package colossus
package protocols.http

import akka.util.ByteString

import core.DataBuffer
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

  protected def httpHead = firstLine ~ headers >> {case fl ~ headers => 
    def fastFind(key: String): Option[HttpHeader] = {
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
    HeadResult(
      HttpRequestHead(fl, new HttpHeaders(headers.asInstanceOf[Array[HttpHeader]])), 
      fastFind("content-length").map{_.value.toInt}, 
      fastFind("transfer-encoding").map{_.value}
    )
  }

  implicit val z: Zero[ParsedHeaderLine, FastParseHeader] = FastParseHeader.FPHZero

  def firstLine = line >> ParsedFL
  def headers = repeatZero[ParsedHeaderLine, FastParseHeader](header)
  def header: Parser[ParsedHeaderLine] = line >> FastParseHeader.make

  
}

case class HeadResult(head: HttpRequestHead, contentLength: Option[Int], transferEncoding: Option[String] )


case class ParsedFL(data: Array[Byte]) extends FirstLine {

  def fastIndex(byte: Byte, start: Int = 0) = {
    var pos = start
    while (pos < data.size && data(pos) != byte) { pos += 1 }
    if (pos >= data.size) -1 else pos
  }


  lazy val pathStart = fastIndex(' '.toByte) + 1
  lazy val pathLength = fastIndex(' '.toByte, pathStart) - pathStart
  
  lazy val method = HttpMethod(new String(data, 0, pathStart - 1)) //the -1 is for the space between method and path
  lazy val path = new String(data, pathStart, pathLength)
  lazy val version = HttpVersion(new String(data, pathStart + pathLength + 1, data.size - (pathStart + pathLength + 1)))
}



sealed trait ParsedHeaderLine

case object EndLine extends ParsedHeaderLine

case class FastParseHeader(data: Array[Byte]) extends HttpHeader with ParsedHeaderLine{

  lazy val valueStart = data.indexOf(':'.toByte) + 1
  lazy val key = new String(data, 0, valueStart - 1).toLowerCase
  lazy val value = new String(data, valueStart, data.length - valueStart).trim
  lazy val encoded = data ++ Array('\r'.toByte, '\n'.toByte)

  def matches(matchkey: String) = {
    val c = matchkey(0).toByte
    if (data(0) == c || data(0) + 48 == c) matchkey == key else false
  }
}

object FastParseHeader {
  
  //def apply(data: Array[Byte], segments: Array[Int]): ParsedHeaderLine = if (data.size == 0) EndLine else FastParseHeader(data, segments(0))
  def make(data: Array[Byte]): ParsedHeaderLine = if (data.size == 0) EndLine else FastParseHeader(data)

  implicit object FPHZero extends Zero[ParsedHeaderLine, FastParseHeader] {
    def isZero(t: ParsedHeaderLine) = t == EndLine
    def nonZero(t: ParsedHeaderLine) = t match {
      case EndLine => None
      case h: FastParseHeader => Some(h)
    }
  }
}

