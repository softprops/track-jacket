# track jacket

A slimly outfitted interface for getting your app up and running with [marathon](https://github.com/mesosphere/marathon#readme)

<p align="center"><img src="http://f.cl.ly/items/3910290t2m2J1f222z2W/il_570xN.515783795_c2ib.jpg" style="height:600px"/></p>

## usage

```
import trackjacket._
import dispatch._

val cli = Client()
val start =
 (cli.start("my_app")
  .cmd("java -jar app-1.0.jar")
  .cpus(4) 
  .instances(4)
  .env("env" -> "prod")(as.String)).either
for (_ <- start.right) yield println("off you go")
for (_ <- start.left) yield println("whooa nelly")
```

Doug Tangren (softprops) 2013
