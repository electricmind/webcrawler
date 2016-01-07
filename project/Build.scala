import sbt._

object WebCrawlerBuild extends Build {
  val Name = "webcrawler"
  val utils =
    RootProject(uri("https://github.com/electricmind/utils.git#scala-2_11"))

  val tunevocabulary =
    Project(id="tunevocabulary", base=file("tunevocabulary")).dependsOn(utils)

  val treeapproximator =
    Project(id="treeapproximator", base=file("treeapproximator")).dependsOn(utils)

  override lazy val settings = super.settings

  lazy val root = Project(Name,
    base = file("."),
    settings = Project.defaultSettings
  ).dependsOn(utils).aggregate(tunevocabulary, treeapproximator)
}

