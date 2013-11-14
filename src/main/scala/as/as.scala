package trackjacket.as

import trackjacket.Client.Handler
import com.ning.http.client.Response

object Unit extends Handler[Unit] {
  def apply(r: Response): Unit = ()
}
