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

  //protected def httpHead = new HttpHeadParser
  protected def httpHead = NewParser.parser
  
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
        BuildFL(
          HttpMethod(method),
          path,
          HttpVersion(version)
        ),
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

case class ParsedFL(data: Array[Byte], pathStart: Int, pathLength: Int) extends FirstLine {
  
  lazy val method = HttpMethod(new String(data, 0, pathStart - 1)) //the -1 is for the space between method and path
  lazy val path = new String(data, pathStart, pathLength)
  lazy val version = HttpVersion(new String(data, pathStart + pathLength + 1, data.size - (pathStart + pathLength + 1)))
}

object ParsedFL {

  def apply(b : ByteRange): ParsedFL = ParsedFL(b.data, b.start(1), b.length(1))
  def apply(data: Array[Byte], segments: Array[Int]): ParsedFL = ParsedFL(data, ByteRange.start(segments, 1), ByteRange.length(segments, 1))
}




class FastFLParser extends Parser[ParsedFL] {

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

//note - right now delimiters are always single bytes
case class ByteRange(data: Array[Byte], segments: Array[Int]) {
  def start(range: Int) = if (range == 0) 0 else segments(range - 1)
  def length(range: Int) = if (range == 0) segments(0) else (segments(range) - segments(range - 1) - 1)
}
object ByteRange {
  def start(segments: Array[Int], range: Int) = if (range == 0) 0 else segments(range - 1)
  def length(segments: Array[Int], range: Int) = if (range == 0) segments(0) else (segments(range) - segments(range - 1) - 1)
}

sealed trait ParsedHeaderLine

case object EndLine extends ParsedHeaderLine

case class FastParseHeader(data: Array[Byte], valueStart: Int) extends HttpHeader with ParsedHeaderLine{

  lazy val key = new String(data, 0, valueStart - 1).toLowerCase
  lazy val value = new String(data, valueStart, data.length - valueStart).trim
  lazy val encoded = data ++ Array('\r'.toByte, '\n'.toByte)
}
object FastParseHeader {
  
  def apply(data: Array[Byte], segments: Array[Int]): ParsedHeaderLine = if (data.size == 0) EndLine else FastParseHeader(data, segments(0))

  implicit object Zero extends Zero[ParsedHeaderLine, FastParseHeader] {
    def isZero(t: ParsedHeaderLine) = t == EndLine
    def nonZero(t: ParsedHeaderLine) = t match {
      case EndLine => None
      case h: FastParseHeader => Some(h)
    }
  }
}

trait Zero[T, N <: T] {
  def isZero(t: T): Boolean
  def nonZero(t: T): Option[N]
}



class RepeatZeroParser[T , N <: T : scala.reflect.ClassTag](parser: Parser[T])(implicit zero: Zero[T,N]) extends Parser[Array[N]] {
  val build = new java.util.LinkedList[N]()

  def parse(data: DataBuffer): Option[Array[N]] = {
    var done = false
    while (data.hasUnreadData && !done) {
      parser.parse(data) match {
        case Some(res) => zero.nonZero(res) match {
          case Some(header) => { build.add(header) }
          case None => {done = true }
        }
        case None => {}
      }
    }
    if (done) {
      val h = new Array[N](build.size)
      var i = 0
      while (build.size > 0) {
        h(i) = build.remove()
        i += 1
      }
      Some(h)
    } else {
      None
    }
  }
}
      

//todo: shapeless could really help here
class RangeLineParser[T](delimiters: Array[Byte], constructor: (Array[Byte], Array[Int]) => T) extends Parser[T] {
  private val CR    = '\r'.toByte
  private val LF    = '\n'.toByte
  private val empty = Array[Byte]()

  private var scanChar = delimiters(0)
  private var scanIndex = 0 //the index of the delimiter we're scanning for

  private var ranges = new Array[Int](delimiters.size)

  //normally we can just create one array and copy the whole first line into it
  //at once, but sometimes the first line may cross packets or DataBuffers
  private var build: Array[Byte] = empty
  private var dataPos = 0

  def complete(): T = {
    val res = constructor(build, ranges)
    build = empty
    ranges = new Array[Int](delimiters.size)
    dataPos = 0
    scanIndex = 0
    scanChar = delimiters(0)
    res
  }

  def parse(buffer: DataBuffer): Option[T] = {
    var pos = buffer.data.position
    val until = buffer.remaining + pos
    var res: Option[T] = None
    while (pos < until && res == None) {
      val byte = buffer.data.get(pos)
      dataPos += 1
      pos += 1
      if (byte == scanChar) {
        scanChar match {
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
              buffer.data.position(buffer.data.position + 2)
              res = Some(complete())
            } else {
              //this would only happen if the \n is in the next packet/buffer,
              //very rare but it can happen, but we can't complete until we've read it in
              scanChar = LF
            }
          }
          case LF => {
            buffer.next //skip over it
            res = Some(complete())
          }
          case userChar => {
            ranges(scanIndex) = dataPos
            scanIndex += 1
            if (scanIndex  == delimiters.size) {
              scanChar = CR
            } else {
              scanChar = delimiters(scanIndex)
            }
          }
        }
      } else if (byte == CR) {
        //happens on an empty line or a line that ends too early
        if (pos < until) {
          buffer.data.position(buffer.data.position + 2)
          res = Some(complete())
        } else {
          buffer.data.position(buffer.data.position + 1)
          scanChar = LF
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

object NewParser {

  def firstLine = new RangeLineParser(Array(' '.toByte, ' '.toByte) , ParsedFL.apply)
  def header: Parser[ParsedHeaderLine] = new RangeLineParser(Array(':'.toByte), FastParseHeader.apply)
  def repeat = new RepeatZeroParser[ParsedHeaderLine, FastParseHeader](header)

  def parser = firstLine ~ repeat >> {case fl ~ headers => HeadResult(HttpRequestHead(fl, new HttpHeaders(headers.asInstanceOf[Array[HttpHeader]])), None, None)}

  val data = ByteString("GET /foobar HTTP/1.1\r\nfoo:bar\r\n\r\n")

  def buf = DataBuffer(data)

  def go() {
    val d = buf
    firstLine.parse(d)
    header.parse(d)
    println("start")
    header.parse(d)
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

class HeaderParser {

  def headers: HttpHeaders = {
    val h = new Array[HttpHeader](_headers.size)
    var i = 0
    while (_headers.size > 0) {
      h(i) = _headers.remove()
      i += 1
    } 
    new HttpHeaders(h)
  }

  private val _headers = new java.util.LinkedList[HttpHeader]()

  def reset() {
    _headers.clear()
    builder.setLength(0)
    state = STATE_KEY
  }

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
    _headers.add(new DecodedHeader(builtKey, builder.toString))
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

  var headerState = 0 //incremented when parsing \r\n\r\n

  val hparser = new HeaderParser
        
  var fl: Option[ParsedFL] = None
  val flParser = new FastFLParser

  //val p = FastFLParser ~> skip(2) >> {fl => HeadResult(HttpRequestHead(fl, HttpHeaders()), None, None)}

  def parse(d: DataBuffer): Option[HeadResult] = {
    var res: Option[HeadResult] = None
    if (fl == None) {
      fl = flParser.parse(d)
      headerState = 2
    }
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
          hparser.end()
        } else if (headerState == 4) {
          //two consecutive \r\n indicates the end of the request head
          headerState = 0
          res = Some(HeadResult(HttpRequestHead(fl.get, hparser.headers), None, None))
          fl = None
          hparser.reset()
        } 
      } else {
        hparser.parse(b)
        headerState = 0
      }
    }
    d.data.position(pos)
    res
  }


}


