package trackjacket.as

import trackjacket.Client.Handler
import com.ning.http.client.Response
import org.json4s._

object Unit extends Handler[Unit] {
  def apply(r: Response): Unit = ()
}

case class App(
  id: String,
  cmd: String,
  env: Map[String, String],
  instances: Int,
  cpus: Double,
  mem: Double,
  executor: String,
  constraints: Iterable[String],
  uris: Iterable[String],
  ports: Iterable[Int])

object Apps extends Handler[Iterable[App]] {
  def apply(r: Response) =
    (dispatch.as.json4s.Json andThen (for {
      JArray(apps) <- _
      JObject(app) <- apps
      ("id", JString(id)) <- app
      ("cmd", JString(cmd)) <- app
      ("env", JObject(env)) <- app
      ("instances", JInt(instances)) <- app
      ("cpus", JDouble(cpus)) <- app
      ("mem", JDouble(mem)) <-  app
      ("executor", JString(exec)) <- app
      ("contraints", JArray(constraints)) <- app
      ("uris", JArray(uris)) <- app
      ("ports", JArray(ports)) <- app
    } yield App(
      id, cmd,
      (for ((k, JString(v)) <- env) yield (k, v)).toMap,
      instances.toInt, cpus, mem, exec,
      for (JString(c) <- constraints) yield c,
      for (JString(uri) <- uris) yield uri,
      for (JInt(p) <- ports) yield p.toInt)))(r)
}
