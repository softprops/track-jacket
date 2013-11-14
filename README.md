# track jacket

A slimly outfitted interface for getting your app up and running with [marathon](https://github.com/mesosphere/marathon#readme)

<p align="center"><img src="http://f.cl.ly/items/3910290t2m2J1f222z2W/il_570xN.515783795_c2ib.jpg" style="height:600px"/></p>

## usage

```scala
import trackjacket._
import dispatch._

val cli = Client()
val start =
 (cli.start("my_app")
  .cmd("java -jar app-1.0.jar")
  .uris("/path/to/app-1.0.jar")
  .cpus(4) 
  .instances(4)
  .env("env" -> "prod")()).either
for (_ <- start.right) yield println("off you go")
for (_ <- start.left) yield println("whooa nelly")
(for { js <- cli.apps(as.json4s.Json) } yield {
  for {
    JArray(apps)                   <- js
    JObject(app)                   <- apps
    ("id", JString(id))            <- app
    ("instances", JInt(instances)) <- app
  } yield {
    log.info(s"service: $id instances requested: $instances")
    for (ejs <- cli.endpoint(id)(as.json4s.Json)) yield {
      for {
        JObject(endpoint) <- ejs
        ("instances", JArray(instances))  <- endpoint
        JObject(instance)                 <- instances
        ("host", JString(host))           <- instance
        ("ports", JArray(JInt(port) :: _)) <- instance
      } yield {
      log.info(s"✈ $host:$port")
    }
   }.apply()
  }.apply()
})
```

Doug Tangren (softprops) 2013
