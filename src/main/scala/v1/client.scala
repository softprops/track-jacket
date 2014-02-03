package trackjacket.v1

import trackjacket.as
import trackjacket.Client.Default
import com.ning.http.client.Response
import dispatch._, dispatch.Defaults._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.render
import org.json4s.native.Printer.compact
import scala.concurrent.Future

// legacy client interface
case class Client(
  host: String = Default.host,
  port: Int    = Default.port,
  credentials: Option[(String, String)] = Default.credentials,
  http: Http   = new Http) {

  private def base = :/(host, port) / "v1" <:< trackjacket.Client.Headers

  private def request[T](req: Req)(
    handler: trackjacket.Client.Handler[T] = as.Unit): Future[T] =
    http(credentials.map { case (user, pass) => req.as_!(user, pass) }
                    .getOrElse(req) OK handler)

  private def complete(req: Req): trackjacket.Client.Completion =
    new trackjacket.Client.Completion {
      override def apply[T](handler: trackjacket.Client.Handler[T] = as.Unit) =
        request(req)(handler)
    }

  /** return all apps registered with marathon */
  def apps =
    complete(base / "apps")

  /** return all endpoints for all apps */
  def endpoints =
    complete(base / "endpoints")

  /** return all endpoints associated with an app id */
  def endpoint(id: String) =
    complete(base / "endpoints" / id)

  /** starts an app by building up an app requirements definition */
  def start(id: String) = AppBuilder(id)

  case class AppBuilder(
    id: String,
    _cmd: Option[String] = None,
    _cpus: Double = Default.cpus,
    _mem: Double = Default.mem,
    _instances: Int = Default.instances,
    _uris: List[String] = Nil,
    _env: Map[String, String] = Map.empty[String, String],
    _taskRateLimit: Option[Double] = None,
    _ports: List[Int] = Default.ports,
    _executor: Option[String] = None)
    extends trackjacket.Client.Completion {

    def cmd(str: String) = copy(_cmd = Some(str))

    def cpus(n: Double) = copy(_cpus = n)

    def mem(megs: Double) = copy(_mem = megs)

    def instances(n: Int) = copy(_instances = n)

    def uris(strs: String*) = copy(_uris = strs.toList)

    def env(kvs: (String, String)*) = copy(_env = kvs.toMap)

    /** Number of new tasks this app may spawn per second in response to
     *  terminated tasks. This prevents frequently failing apps from spamming
     *  the cluster.
     */
    def taskRateLimit(limit: Double) = copy(_taskRateLimit = Some(limit))

    def ports(ps: Int*) = copy(_ports = ps.toList)

    def executor(exec: String) = copy(_executor = Some(exec))

    // future response will have no body
    def apply[T](handler: trackjacket.Client.Handler[T] = as.Unit): Future[T] =
      request(base.POST / "apps" / "start" << compact(
        render(("id"   -> id)    ~ ("cmd"       -> _cmd) ~
               ("cpus" -> _cpus) ~ ("instances" -> _instances) ~
               ("mem"  -> _mem)  ~ ("uris"      -> _uris) ~
               ("taskRateLimit" -> _taskRateLimit) ~
               ("ports" -> _ports) ~ ("executor" -> _executor) ~
               ("env"  -> _env))))(handler)
  }

  /** stop all instances of an app. The response will have no body */
  def stop(id: String) =
    complete(base.POST / "apps" / "stop" << compact(
      render(("id" -> id))))

  /** increase/decrease number of instances of app identified by id.
   *  The response will have no body */
  def scale(id: String, instances: Int) =
    complete(base.POST / "apps" / "scale" << compact(
      render(("id" -> id) ~ ("instances" -> instances))))

  /** same as scaling to 0 instances */
  def suspend(id: String) =
    scale(id, 0)

  /** search apps either by their id or cmd */
  def search = Search()

  case class Search(
    _id: Option[String] = None,
    _cmd: Option[String] = None)
    extends trackjacket.Client.Completion {

    def id(id: String) = copy(_id = Some(id))

    def cmd(str: String) = copy(_cmd = Some(str))

    def apply[T](handler: trackjacket.Client.Handler[T]): Future[T] = {
      val endpoint  = ((base / "apps" / "search") /: Seq(
        _id.map(("id", _)), _cmd.map(("cmd" -> _))).flatten) {
          case (req, (k, v)) => req.addParameter(k, v)
        }
      request(endpoint)(handler)
    }
  }

  /** lists all tasks */
  def tasks = complete(base / "tasks")

  /** kills tasks by app id or by host */
  def killTask = KillTask()

  case class KillTask(
    _id: Option[String] = None,
    _host: Option[String] = None)
    extends trackjacket.Client.Completion {

    def id(id: String) = copy(_id = Some(id))

    def host(str: String) = copy(_host = Some(str))

    def apply[T](handler: trackjacket.Client.Handler[T] = as.Unit): Future[T] = {
      val endpoint  = ((base.POST / "tasks" / "kill") /: Seq(
        _id.map(("appId", _)), _host.map(("host" -> _))).flatten) {
          case (req, (k, v)) => req.addParameter(k, v)
        }
      request(endpoint)(handler)
    }
  }

  def close() = http.shutdown()
}
