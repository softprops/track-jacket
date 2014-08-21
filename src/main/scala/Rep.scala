package trackjacket

import com.ning.http.client.Response
import dispatch.as
import org.json4s._

trait Rep[T] {
  def map: Response => T
}

object Rep {
  implicit object Identity extends Rep[Response] {
    def map = identity(_)
  }

  implicit object Nada extends Rep[Unit] {
    def map = _ => ()
  }
}
