package colossus
package protocols.http

import akka.util.ByteString

import core.DataBuffer
import parsing._
import DataSize._
import HttpParse._
import Combinators._

object HttpRequestParser {
  import HttpBody._

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

  protected def httpHead = new HttpHeadParser
  
}

case class HeadResult(head: HttpRequestHead, contentLength: Option[Int], transferEncoding: Option[String] )

class RequestBuilder {
  var method: String = "GET"
  var path: String = "/plaintext"
  var version: String = "HTTP/1.1"
  var headers: List[HttpHeader] = Nil
  var contentLength: Option[Int] = None
  var transferEncoding: Option[String] = None

  def addHeader(name: String, value: String) {
    headers =  new DecodedHeader(name, value) :: headers
    if (name == HttpHeaders.ContentLength) {
      contentLength = Some(value.toInt)
    } else if (name == HttpHeaders.TransferEncoding) {
      transferEncoding = Some(value)
    }
  }

  def build: HeadResult = {
    val r = HeadResult(
      HttpRequestHead(
        HttpMethod(method),
        path,
        HttpVersion(version),
        new HttpHeaders(headers.toArray)
      ), 
      contentLength,
      transferEncoding
    )
    reset()
    r
  }

  def reset() {
    headers = Nil
    contentLength = None
    transferEncoding = None
  }
}

abstract class MiniParser(val requestBuilder: RequestBuilder) {
  def parse(c: Char)
  def end()
}

case class ParsedFL(data: Array[Byte], pathStart: Int, pathLength: Int) {
  
  lazy val method = HttpMethod(new String(data, 0, pathStart - 1)) //the -1 is for the space between method and path
  lazy val path = new String(data, pathStart, pathLength)
  lazy val version = HttpVersion(new String(data, pathStart + pathLength + 1, data.size - (pathStart + pathLength + 1)))
}


class FastFLParser {

  private val SPACE = ' '.toByte
  private val CR    = '\r'.toByte
  private val LF    = '\n'.toByte
  private var scanChar = SPACE

  //normally we can just create one array and copy the whole first line into it
  //at once, but sometimes the first line may cross packets or DataBuffers
  private var build: Array[Byte] = Array()
  private var pathStart = 0
  private var pathLength = 0
  private var dataPos = 0

  def complete(): ParsedFL = {
    val res = ParsedFL(build, pathStart, pathLength)
    build = Array()
    pathStart = 0
    pathLength = 0
    dataPos = 0
    scanChar = SPACE
    res
  }

  def parse(buffer: DataBuffer): Option[ParsedFL] = {
    var pos = buffer.data.position
    val until = buffer.remaining + pos
    var res: Option[ParsedFL] = None
    while (pos < until && res == None) {
      val byte = buffer.data.get(pos)
      dataPos += 1
      pos += 1
      if (byte == scanChar) {
        scanChar match {
          case SPACE if (pathStart == 0) => {
            pathStart = dataPos
          }
          case SPACE if (pathStart > 0) => {
            pathLength = (dataPos - pathStart) - 1
            scanChar = CR
          }
          case CR => {
            //we're done
            //subtract 1 so we don't copy the \r
            pos -= 1
            val copy = new Array[Byte](pos - buffer.data.position)
            buffer.data.get(copy)
            if (build.length == 0) {
              build = copy
            } else {
              build = build ++ copy
            }
            if (pos < until) {
              //usually we can skip scanning for the \n
              //do an extra get to read in the \n
              buffer.next
              res = Some(complete())
            } else {
              //this would only happen if the \n is in the next packet/buffer,
              //very rare but it can happen, but we can't complete until we've read it in
              scanChar = LF
            }
          }
          case LF => {
            res = Some(complete())
          }
        }
            
      }
    }
    if (pos == until) {
      //copy the whole databuffer, we're not done yet
      build = new Array(until - pos)
      buffer.data.get(build)
    }
    res
  }
}

    


class FirstLineParser(b: RequestBuilder) extends MiniParser(b) {
  val STATE_METHOD  = 0
  val STATE_PATH    = 1
  val STATE_VERSION = 2
  var state = STATE_METHOD
  val builder = new StringBuilder
  def parse(c: Char) {
    if (c == ' ') {
      val res = builder.toString
      builder.setLength(0)
      state match {
        case STATE_METHOD => {
          requestBuilder.method = res
          state = STATE_PATH
        }
        case STATE_PATH   => {
          requestBuilder.path = res
          state = STATE_VERSION
        }
        case _ => {
          throw new ParseException("invalid content in header first line")
        }
      }
    } else {
      builder.append(c)
    }
  }
  def end() {
    val res = builder.toString
    builder.setLength(0)
    requestBuilder.version = res
    state = STATE_METHOD
  }
}

class HeaderParser(b: RequestBuilder) extends MiniParser(b) {
  val STATE_KEY   = 0
  val STATE_VALUE = 1
  val STATE_TRIM = 2
  var state = STATE_KEY
  val builder = new StringBuilder
  var builtKey = ""
  def parse(c: Char) {
    state match {
      case STATE_KEY => {
        if (c == ':') {
          builtKey = builder.toString
          builder.setLength(0)
          state = STATE_TRIM
        } else {
          if (c >= 'A' && c <= 'Z') {
            builder.append((c + 32).toChar)
          } else {
            builder.append(c)
          }
        }
      }
      case STATE_TRIM => {
        if (c != ' ') {
          state = STATE_VALUE
          builder.append(c)
        }
      }
      case STATE_VALUE => {
        builder.append(c)
      }
    }
  }
  def end() {
    requestBuilder.addHeader(builtKey, builder.toString)
    builder.setLength(0)
    state = STATE_KEY
  }
}

class NoParser(b: RequestBuilder) extends MiniParser(b) {

  def parse(c: Char){}

  def end(){}
}



/**
 * This parser is optimized to reduce the number of operations per character
 * read
 */
class HttpHeadParser extends Parser[HeadResult]{

  var requestBuilder = new RequestBuilder
  var headerState = 0 //incremented when parsing \r\n\r\n



  val fparser = new FirstLineParser(requestBuilder)
  val hparser = new NoParser(requestBuilder)
  //val hparser = new HeaderParser(requestBuilder)
        
  var currentParser: MiniParser = fparser

  def parse(d: DataBuffer): Option[HeadResult] = {
    var res: Option[HeadResult] = None
    var pos = d.data.position
    val until = d.remaining + pos
    while (pos < until && res == None) {
      val b = d.data.get(pos).toChar
      pos += 1
      if (b == '\r') {
        headerState += 1
      } else if (b == '\n') {
        headerState += 1
        if (headerState == 2) {
          //currentParser.end()
          if (currentParser == fparser) {
            currentParser = hparser
          }
        } else if (headerState == 4) {
          //two consecutive \r\n indicates the end of the request head
          currentParser = fparser
          headerState = 0
          res = Some(requestBuilder.build)
        } 
      } else {
        //if (currentParser == fparser) currentParser.parse(b)
        headerState = 0
      }
    }
    d.data.position(pos)
    res
  }


}


