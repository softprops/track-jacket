package trackjacket

import com.ning.http.client.Response
import dispatch._, dispatch.Defaults._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.render
import org.json4s.native.Printer.compact
import scala.concurrent.Future

object Client {
  type Handler[T] = Response => T
  val Agent = "Trackjacket/0.1.0"
  trait Completion {
    def apply[T](handler: Client.Handler[T]): Future[T]
  }
  val Headers = Map(
    "Content-Type" -> "application/json",
    "Accept" -> "application/json"
  )
}

case class Client(
  host: String, port: Int = 8080,
  credentials: Option[(String, String)] = None) {

  private val http = new Http

  private def base = :/(host, port) / "v1" <:< Client.Headers

  private def request[T](req: Req)(
    handler: Client.Handler[T]): Future[T] =
    http(credentials.map { case (user, pass) => req.as_!(user, pass) }
               .getOrElse(req) OK handler)

  private def complete(req: Req): Client.Completion =
    new Client.Completion {
      override def apply[T](handler: Client.Handler[T]) =
        request(req)(handler)
    }

  def apps =
    complete(base / "apps")

  def endpoints =
    complete(base / "endpoints")

  def endpoint(id: String) =
    complete(base / "endpoints" / id)

  def start(id: String) = AppBuilder(id)

  case class AppBuilder(
    id: String,
    _cmd: Option[String] = None,
    _cpus: Option[Int] = None,
    _mem: Option[Int] = None,
    _instances: Int = 1,
    _uris: List[String] = Nil)
    extends Client.Completion {

    def cmd(str: String) = copy(_cmd = Some(str))

    def cpus(n: Int) = copy(_cpus = Some(n))

    def mem(megs: Int) = copy(_mem = Some(megs))

    def instances(n: Int) = copy(_instances = n)

    def uris(strs: String*) = copy(_uris = strs.toList)

    def apply[T](handler: Client.Handler[T]): Future[T] =
      request(base.POST / "apps" / "start" << compact(
        render(("id"   -> id)    ~ ("cmd"       -> _cmd) ~
               ("cpus" -> _cpus) ~ ("instances" -> _instances) ~
               ("mem"  -> _mem)  ~ ("uris"      -> _uris))))(handler)
  }

  def stop(id: String) =
    complete(base.POST / "apps" / "stop" << compact(
      render(("id" -> id))))

  def scale(id: String, instances: Int) =
    complete(base.POST / "apps" / "scale" << compact(
      render(("id" -> id) ~ ("instances" -> instances))))

  def close() = http.shutdown()
}
