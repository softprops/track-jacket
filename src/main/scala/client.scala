package trackjacket

import com.ning.http.client.Response
import dispatch._, dispatch.Defaults._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.render
import org.json4s.native.Printer.compact
import scala.concurrent.Future

object Client {
  type Handler[T] = Response => T
  val Agent = "Track-jacket/0.1.0"
  trait Completion {
    def apply[T](handler: Client.Handler[T]): Future[T]
  }
  val Headers = Map(
    "Content-Type" -> "application/json",
    "Accept" -> "application/json",
    "User-Agent" -> Agent
  )
  object Default {
    def port = 8080
    val host = "localhost"
    val credentials: Option[(String, String)] = None
    val mem = 10.0
    val cpus = 0.1
    val instances = 1
  }
}

case class Client(
  host: String = Client.Default.host,
  port: Int = Client.Default.port,
  credentials: Option[(String, String)] = Client.Default.credentials) {

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
    _cpus: Double = Client.Default.cpus,
    _mem: Double = Client.Default.mem,
    _instances: Int = Client.Default.instances,
    _uris: List[String] = Nil,
    _env: Map[String, String] = Map.empty[String, String])
    extends Client.Completion {

    def cmd(str: String) = copy(_cmd = Some(str))

    def cpus(n: Double) = copy(_cpus = n)

    def mem(megs: Double) = copy(_mem = megs)

    def instances(n: Int) = copy(_instances = n)

    def uris(strs: String*) = copy(_uris = strs.toList)

    def env(kvs: (String, String)*) = copy(_env = kvs.toMap)

    def apply[T](handler: Client.Handler[T]): Future[T] =
      request(base.POST / "apps" / "start" << compact(
        render(("id"   -> id)    ~ ("cmd"       -> _cmd) ~
               ("cpus" -> _cpus) ~ ("instances" -> _instances) ~
               ("mem"  -> _mem)  ~ ("uris"      -> _uris) ~
               ("env"  -> _env))))(handler)
  }

  def stop(id: String) =
    complete(base.POST / "apps" / "stop" << compact(
      render(("id" -> id))))

  def scale(id: String, instances: Int) =
    complete(base.POST / "apps" / "scale" << compact(
      render(("id" -> id) ~ ("instances" -> instances))))

  /** same as scaling to 0 instances */
  def suspend(id: String) =
    scale(id, 0)

  def close() = http.shutdown()
}
