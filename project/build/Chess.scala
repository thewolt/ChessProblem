import sbt._

class ChessProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest =  "org.scalatest" % "scalatest" % "1.2" % "test"
}