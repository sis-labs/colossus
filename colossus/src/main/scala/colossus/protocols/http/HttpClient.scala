package colossus
package protocols.http
import scala.language.higherKinds

import service._
trait HttpClient[M[_]] extends LiftedClient[Http, M] {

}

object HttpClient {

  implicit object HttpClientLifter extends ClientLifter[Http, HttpClient] {
    
    def lift[M[_]](client: Sender[Http,M])(implicit async: Async[M]) = new LiftedClient(client) with HttpClient[M]
  }

}