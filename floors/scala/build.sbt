name := "scala-department-floor"
version := "1.0.0"
scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
)

assembly / mainClass := Some("run")
assembly / assemblyJarName := "department_floor.jar"

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

Compile / run / fork := true
Compile / run / connectInput := true
