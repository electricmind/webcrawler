import sbt._

object WebCrawlerBuild extends Build {
  val Name = "webcrawler"
  val utils =
    RootProject(uri("https://github.com/electricmind/utils.git#v.2.1"))

  val tunevocabulary =
    Project(id="tunevocabulary", base=file("tunevocabulary")).dependsOn(utils)

  override lazy val settings = super.settings

  lazy val root = Project(Name,
    base = file("."),
    settings = Project.defaultSettings
  ).dependsOn(utils).aggregate(tunevocabulary)
}

