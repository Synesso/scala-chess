import sbt._

class ScalaChessProject(info: ProjectInfo) extends DefaultWebProject(info) {
  override def useMavenConfigurations = true

  val specsRepo = "Specs Repository" at "http://specs.googlecode.com/svn/maven2/"

  val geronimoServlet = "org.apache.geronimo.specs" % "geronimo-servlet_2.5_spec" % "1.2" % "compile->default"
  val gae = "com.google.appengine" % "appengine-api-1.0-sdk" % "1.2.1" % "compile->default"

  val specs = "org.scala-tools.testing" % "specs" % "1.5.0" % "test->default"
  val scalacheck = "org.scala-tools.testing" % "scalacheck" % "1.5" % "test->default"
  val mockito = "org.mockito" % "mockito-core" % "1.7" % "test->default"
  val junit = "junit" % "junit" % "4.4" % "test->default"
  val jetty = "org.mortbay.jetty" % "jetty" % "6.1.14" % "test->default"
  val jasper = "org.apache.tomcat" % "jasper" % "6.0.18" % "test->default"
}
