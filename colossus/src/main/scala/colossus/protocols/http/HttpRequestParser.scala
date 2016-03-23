package colossus
package protocols.http

import akka.util.ByteString

import core.{DataBuffer, DataOutBuffer}
import parsing._
import DataSize._
import Combinators._

object HttpRequestParser {
  import HttpParse._

  val DefaultMaxSize: DataSize = 1.MB

  def apply(size: DataSize = DefaultMaxSize) = maxSize(size, httpRequest)

  //TODO : don't parse body as a bytestring
  protected def httpRequest: Parser[HttpRequest] = httpHead |> {case HeadResult(head, contentLength, transferEncoding) => 
    transferEncoding match { 
      case None | Some("identity") => contentLength match {
        case Some(0) | None => const(HttpRequest(head, HttpBody.NoBody))
        case Some(n) => bytes(n) >> {body => HttpRequest(head, HttpBody(body))}
      }
      case Some(other)  => chunkedBody >> {body => HttpRequest(head, HttpBody(body))}
    } 
  }

  protected def httpHead = firstLine ~ headers >> {case fl ~ headersBuilder => 
    HeadResult(
      HttpRequestHead(fl, headersBuilder.buildHeaders), 
      headersBuilder.contentLength,
      headersBuilder.transferEncoding
    )
  }

  def firstLine = line(ParsedFL.apply, true)
  
}



case class ParsedFL(data: Array[Byte]) extends FirstLine with LazyParsing {

  protected def parseErrorMessage = "Malformed head"


  def encode(out: DataOutBuffer) {
    out.write(data)
  }

  private lazy val pathStart  = fastIndex(data, ' '.toByte, 3) + 1
  private lazy val pathLength = data.size - 11 - pathStart //assumes the line ends with " HTTP/x/x\r\n", which it always should

  lazy val method     = parsed { HttpMethod(new String(data, 0, pathStart - 1)) } //the -1 is for the space between method and path
  lazy val path       = parsed { new String(data, pathStart, pathLength) }
  lazy val version    = parsed { 
    val vstart = pathStart + pathLength + 1
    HttpVersion(data, vstart, data.size - vstart - 2)
  }
}





