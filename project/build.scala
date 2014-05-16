import sbt._
import Keys._

object MacroBuild extends Build {
  lazy val macroSub = Project("macro", file("macro")) settings(
    resolvers ++= Seq(
      "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/"
    ),
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0"
  )
  lazy val main = Project("main", file(".")) dependsOn(macroSub) settings(
    libraryDependencies += "com.h2database" % "h2" % "1.4.178"
  )
}
