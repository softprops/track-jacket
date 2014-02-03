package trackjacket

import com.ning.http.client.Response
import dispatch._, dispatch.Defaults._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.render
import org.json4s.native.Printer.compact
import scala.concurrent.Future

object Client {

  type Handler[T] = Response => T

  val Agent = s"Track-jacket/${BuildInfo.version}"

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
    val mem = 128.0
    val cpus = 1.0
    val instances = 1
    val taskRateLimit = None
    val ports: List[Int] = Nil
  }
}

// v2 client
case class Client(
  host: String = Client.Default.host,
  port: Int = Client.Default.port,
  credentials: Option[(String, String)] = Client.Default.credentials,
  http: Http = new Http) {

  private def base = :/(host, port) / "v2" <:< Client.Headers

  private def request[T](req: Req)(handler: Client.Handler[T] = as.Unit): Future[T] =
    http(credentials.map { case (user, pass) => req.as_!(user, pass) }
                    .getOrElse(req) OK handler)

  private def complete(req: Req): Client.Completion =
    new Client.Completion {
      override def apply[T](handler: Client.Handler[T] = as.Unit) =
        request(req)(handler)
    }

  case class Apps(_cmd: Option[String] = None) extends Client.Completion {
    def cmd(str: String) = copy(_cmd = Some(str))
    override def apply[T](handler: Client.Handler[T] = as.Unit) =
      request(base / "apps" <<? Map.empty[String, String] ++ _cmd.map("command" -> _))(handler)
  }

  case class KillTask(
    appId: String,
    taskId: String,
    _scale: Boolean = false)
    extends Client.Completion {
    override def apply[T](handler: Client.Handler[T] = as.Unit) =
      request(base.DELETE / "apps" / appId / "tasks" / taskId
              <<? Map("scale" -> _scale.toString))(handler)
  }

  case class KillTasks(
    appId: String,
    _host: Option[String] = None,
    _scale: Boolean = false)
    extends Client.Completion {
    def host(h: String) = copy(_host = Some(h))
    def scaleDown = copy(_scale = true)
    override def apply[T](handler: Client.Handler[T] = as.Unit) =
      request(base.DELETE / "apps" / appId / "tasks"
              <<? Map("scale" -> _scale.toString) ++
              _host.map("host" -> _))(handler)
  }

  case class Container(image: String, options: List[String])
  case class Constraint(attr: String, operator: String, value: Option[String] = None)

  case class AppBuilder(
    id: String, create: Boolean = true,
    _cmd: Option[String] = None,
    _cpus: Double = Client.Default.cpus,
    _mem: Double = Client.Default.mem,
    _instances: Int = Client.Default.instances,
    _uris: List[String] = Nil,
    _env: Map[String, String] = Map.empty[String, String],
    _taskRateLimit: Option[Double] = None,
    _ports: List[Int] = Client.Default.ports,
    _executor: Option[String] = None,
    _container: Option[Container] = None,
    _constraints: Option[List[Constraint]] = None)
    extends Client.Completion {

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

    def container(cont: Container) = copy(_container = Some(cont))

    def constraints(consts: Constraint*) = copy(_constraints = Some(consts.toList))

    // future response will have no body
    def apply[T](handler: Client.Handler[T] = as.Unit): Future[T] =
      request((if (create) base.POST / "apps" else base.PUT / "apps" / id)
              << compact(
                render(("id"  -> Option(create).filter(identity).map(Function.const(id.toString))) ~
                       ("cmd" -> _cmd) ~
                       ("cpus" -> _cpus) ~ ("instances" -> _instances) ~
                       ("mem"  -> _mem)  ~ ("uris"      -> _uris) ~
                       ("taskRateLimit" -> _taskRateLimit) ~
                       ("ports" -> _ports) ~ ("executor" -> _executor) ~
                       ("env"  -> _env) ~ ("container" -> _container.map { c =>
                         ("image" -> c.image) ~ ("options" -> c.options)
                       }) ~ ("constraints" -> _constraints.map { 
                         _.map {
                           case Constraint(attr, oper, Some(value)) => List(attr, oper, value)
                           case Constraint(attr, oper, _) => List(attr, oper)
                         }
                       }))))(handler)
  }

  def apps = Apps()

  // responds with 204 no content
  def start(appId: String) =
    AppBuilder(appId)

  // repdons with 20
  def update(appId: String) =
    AppBuilder(appId, create = false)

  // responds with 204 no contnt
  def kill(appId: String) =
    complete(base.DELETE / "apps" / appId)

  // responds with { "app": { def } }
  def app(appId: String) =
    complete(base / "apps" / appId)

  // responses with { tasks: [resolved tasks] }
  def tasks(appId: String) =
    complete(base / "apps" / appId / "tasks")

  // responds with { tasks: [killed tasks] }
  def killTasks(appId: String) =
    KillTasks(appId)

  def killTask(appId: String, taskId: String) =
    KillTask(appId, taskId)
}
