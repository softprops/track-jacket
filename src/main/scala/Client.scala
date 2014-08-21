package trackjacket

import com.ning.http.client.{ AsyncHandler, Response }
import dispatch.{ as, Http, OkFunctionHandler, Req, :/ }
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.render
import org.json4s.native.Printer.compact
import scala.concurrent.{ ExecutionContext, Future }

object Client {
  type Handler[T] = AsyncHandler[T]

  val Agent = s"Track-jacket/${BuildInfo.version}"

  abstract class Completion[T: Rep] {
    /** @return a future of the default representation of the response */
    def apply(): Future[T] =
      apply(implicitly[Rep[T]].map)
    /** @return a future transformed by Response => T */
    def apply[T]
      (f: Response => T): Future[T] =
        apply(new OkFunctionHandler(f))
    def apply[T]
      (handler: Client.Handler[T]): Future[T]
  }

  val Headers = Map(
    "Content-Type" -> "application/json",
    "Accept" -> "application/json",
    "User-Agent" -> Agent
  )

  object Default {
    def port = 8080
    val host = "localhost"
    val mem = 128.0
    val cpus = 1.0
    val instances = 1
    val taskRateLimit = None
    val ports: List[Int] = Nil
  }
}

case class Client(
  host: String = Client.Default.host,
  port: Int = Client.Default.port,
  credentials: Option[(String, String)] = None,
  http: Http = new Http)
  (implicit ec: ExecutionContext) {

  def close() = http.shutdown()


  private def base = :/(host, port) / "v2" <:< Client.Headers

  private def request[T]
    (req: Req)
    (handler: Client.Handler[T]): Future[T] =
      http(credentials.map { case (user, pass) => req.as_!(user, pass) }
           .getOrElse(req) > handler)

  private def complete[T: Rep](req: Req): Client.Completion[T] =
    new Client.Completion[T] {
      override def apply[T](handler: Client.Handler[T]) =
        request(req)(handler)
    }

  case class Apps(_cmd: Option[String] = None)
    extends Client.Completion[Response] {
    def cmd(str: String) = copy(_cmd = Some(str))
    override def apply[T]
      (handler: Client.Handler[T]) =
        request(base / "apps"
                <<? Map.empty[String, String]
                    ++ _cmd.map("command" -> _))(handler)
  }

  case class KillTask(
    appId: String,
    taskId: String,
    _scale: Boolean = false)
    extends Client.Completion[Response] {
    def scalaDown = copy(_scale = true)
    override def apply[T](handler: Client.Handler[T]) =
      request(base.DELETE / "apps" / appId / "tasks" / taskId
              <<? Map("scale" -> _scale.toString))(handler)
  }

  case class KillTasks(
    appId: String,
    _host: Option[String] = None,
    _scale: Boolean       = false)
    extends Client.Completion[Response] {
    def host(h: String) = copy(_host = Some(h))
    def scaleDown = copy(_scale = true)
    override def apply[T](handler: Client.Handler[T]) =
      request(base.DELETE / "apps" / appId / "tasks"
              <<? Map("scale" -> _scale.toString) ++
              _host.map("host" -> _))(handler)
  }

  case class Container(image: String, options: List[String])

  object Constraints {
    sealed trait Constraint {
      def op: String
      def field: String
      def value: Option[String] = None
    }

    /**  to enforce uniqueness of the attribute across all of an app’s tasks
     *   Unique(hostname) */
    case class Unique(field: String) extends Constraint {
      val op = "UNIQUE"
    }

    /** run all of your app’s tasks on slaves that share a certain attribute
     *  Cluster("rack_id", "rack-1") */
    case class Cluster(field: String, _value: String) extends Constraint {
      val op = "CLUSTER"
      override val value = Some(_value)
    }

    /** distribute your tasks evenly across racks or datacenters for high availability
     *  GroupBy("rack_id") */
    case class GroupBy(field: String) extends Constraint {
      val op = "GROUP_BY"
    }
  }

  case class AppBuilder(
    id: String,
    create: Boolean                = true,
    _cmd: Option[String]           = None,
    _cpus: Double                  = Client.Default.cpus,
    _mem: Double                   = Client.Default.mem,
    _instances: Int                = Client.Default.instances,
    _uris: List[String]            = Nil,
    _env: Map[String, String]      = Map.empty[String, String],
    _taskRateLimit: Option[Double] = None,
    _ports: List[Int]              = Client.Default.ports,
    _executor: Option[String]      = None,
    _container: Option[Container]  = None,
    _constraints: Option[List[Constraints.Constraint]] = None)
    extends Client.Completion[Response] {

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

    /** An array of required port resources on the host. To generate one or more arbitrary
     *  free ports for each application instance, pass zeros as port values.*/
    def ports(ps: Int*) = copy(_ports = ps.toList)

    def executor(exec: String) = copy(_executor = Some(exec))

    def container(cont: Container) = copy(_container = Some(cont))

    def constraints(consts: Constraints.Constraint*) = copy(_constraints = Some(consts.toList))

    // future response will have no body
    def apply[T](handler: Client.Handler[T]): Future[T] =
      request((if (create) base.POST / "apps" else base.PUT / "apps" / id)
              << compact(
                render(("id"  -> Option(create).filter(identity).map(Function.const(id.toString))) ~
                       ("cmd" -> _cmd) ~ ("cpus" -> _cpus) ~
                       ("instances" -> _instances) ~
                       ("mem"  -> _mem)  ~ ("uris"      -> _uris) ~
                       ("taskRateLimit" -> _taskRateLimit) ~
                       ("ports" -> _ports) ~ ("executor" -> _executor) ~
                       ("env"  -> _env) ~ ("container" -> _container.map { c =>
                         ("image" -> c.image) ~ ("options" -> c.options)
                       }) ~ ("constraints" -> _constraints.map { 
                         _.map { c => List(c.field, c.op) ++ c.value }
                       }))))(handler)
  }

  def apps = Apps()

  // responds with 204 no content
  def start(appId: String) =
    AppBuilder(appId)

  // respods with 204 no content
  def update(appId: String) =
    AppBuilder(appId, create = false)

  // responds with 204 no content
  def kill(appId: String) =
    complete[Response](base.DELETE / "apps" / appId)

  // responds with { "app": { def } }
  def app(appId: String) =
    complete[Response](base / "apps" / appId)

  // responses with { tasks: [resolved tasks] }
  def tasks(appId: String) =
    complete[Response](base / "apps" / appId / "tasks")

  // responds with { tasks: [killed tasks] }
  def killTasks(appId: String) =
    KillTasks(appId)

  def killTask(appId: String, taskId: String) =
    KillTask(appId, taskId)
}
