package trackjacket

import com.ning.http.client.Response
import dispatch.as
import org.json4s._

trait Rep[T] {
  def map: Response => T
}

object Rep {
  implicit val Identity: Rep[Response] = new Rep[Response] {
    def map = identity(_)
  }

  implicit val Nada: Rep[Unit] = new Rep[Unit] {
    def map = _ => ()
  }
}
