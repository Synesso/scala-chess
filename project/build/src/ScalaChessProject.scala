import sbt._

class ScalaChessProject(info: ProjectInfo) extends DefaultWebProject(info) {
    val specsRepo = "Specs Repository" at "http://specs.googlecode.com/svn/maven2/"

    val gae = "com.google.appengine" % "appengine-api-1.0-sdk" % "1.2.0"
    val jasper = "org.apache.tomcat" % "jasper" % "6.0.18"
    val jetty = "org.mortbay.jetty" % "jetty" % "6.1.14"
    val geronimoServlet = "org.apache.geronimo.specs" % "geronimo-servlet_2.5_spec" % "1.2"
    val specs = "org.scala-tools.testing" % "specs" % "1.4.4"
    val scalacheck = "org.scala-tools.testing" % "scalacheck" % "1.5"
    val mockito = "org.mockito" % "mockito-core" % "1.7"
    val junit = "junit" % "junit" % "4.4"

}
