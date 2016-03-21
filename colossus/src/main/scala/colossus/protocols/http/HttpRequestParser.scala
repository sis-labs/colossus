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

  protected def fastFind(headers: Array[EncodedHttpHeader], key: String): Option[HttpHeader] = {
    var i = 0
    var res: Option[HttpHeader] = None
    while (i < headers.size && res == None) {
      if (headers(i) matches key) {
        res = Some(headers(i))
      }
      i += 1
    }
    res
  }

  protected def httpHead = firstLine ~ headers >> {case fl ~ headers => 
    HeadResult(
      HttpRequestHead(fl, new HttpHeaders(headers.asInstanceOf[Array[HttpHeader]])), 
      fastFind(headers, "content-length").map{_.value.toInt}, 
      fastFind(headers, "transfer-encoding").map{_.value}
    )
  }

  implicit val z: Zero[ParsedHeaderLine, EncodedHttpHeader] = HttpHeader.FPHZero

  def firstLine = line(ParsedFL.apply, true)
  def headers = repeatZero[ParsedHeaderLine, EncodedHttpHeader](header)
  def header: Parser[ParsedHeaderLine] = line(HttpHeader.apply, true)

  
}

case class HeadResult(head: HttpRequestHead, contentLength: Option[Int], transferEncoding: Option[String] )


trait LazyParsing {

  protected def parseErrorMessage: String

  def parsed[T](op: => T): T = try {
    op
  } catch {
    case p: ParseException => throw p
    case other : Throwable => throw new ParseException(parseErrorMessage + s": $other")
  }

}

case class ParsedFL(data: Array[Byte]) extends FirstLine with LazyParsing {

  protected val parseErrorMessage = "Malformed head"

  def fastIndex(byte: Byte, start: Int = 0) = {
    var pos = start
    while (pos < data.size && data(pos) != byte) { pos += 1 }
    if (pos >= data.size) -1 else pos
  }

  def encode(out: DataOutBuffer) {
    out.write(data)
  }

  private lazy val pathStart  = fastIndex(' '.toByte) + 1
  private lazy val pathLength = fastIndex(' '.toByte, pathStart) - pathStart

  lazy val method     = parsed { HttpMethod(new String(data, 0, pathStart - 1)) } //the -1 is for the space between method and path
  lazy val path       = parsed { new String(data, pathStart, pathLength) }
  lazy val version    = parsed { 
    val vstart = pathStart + pathLength + 1
    HttpVersion(data, vstart, data.size - vstart - 2)
  }
}





