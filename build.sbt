name := "HomiesOasis"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "mysql" % "mysql-connector-java" % "5.1.26"
)

scalacOptions += "-feature"

play.Project.playScalaSettings
