import java.util.Date
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import com.typesafe.tools.mima.core._

lazy val V = new {
  def munit = "0.7.9"
}
val scala212 = "2.12.11"
val scala213 = "2.13.1"
val ScalaVersions = List(scala212, scala213)
inThisBuild(
  List(
    useSuperShell := false,
    scalaVersion := scala212,
    scalacOptions += "-Yrangepos",
    organization := "com.geirsson",
    version ~= { old => old.replace('+', '-') },
    licenses := Seq(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    homepage := Some(url("https://github.com/olafurpg/metaconfig")),
    autoAPIMappings := true,
    apiURL := Some(url("https://github.com/olafurpg/metaconfig")),
    developers += Developer(
      "olafurpg",
      "Ólafur Páll Geirsson",
      "olafurpg@gmail.com",
      url("https://geirsson.com")
    ),
    scalaVersion := ScalaVersions.head,
    crossScalaVersions := ScalaVersions,
    resolvers += Resolver.sonatypeRepo("snapshots")
  )
)

addCommandAlias(
  "scalafixAll",
  s"; ++$scala212 ; scalafixEnable ; all scalafix test:scalafix"
)
addCommandAlias(
  "scalafixCheckAll",
  s"; ++$scala212 ;  scalafixEnable ; scalafix --check ; test:scalafix --check"
)

addCommandAlias(
  "native-image",
  "; tests/graalvm-native-image:packageBin ; taskready"
)
commands += Command.command("taskready") { s =>
  import scala.sys.process._
  "afplay /System/Library/Sounds/Hero.aiff".!
  s
}

lazy val warnUnusedImport = Def.setting {
  if (scalaVersion.value.startsWith("2.13")) "-Wunused:imports"
  else "-Ywarn-unused-import"
}

val languageAgnosticCompatibilityPolicy: ProblemFilter = (problem: Problem) => {
  val (ref, fullName) = problem match {
    case problem: TemplateProblem => (problem.ref, problem.ref.fullName)
    case problem: MemberProblem => (problem.ref, problem.ref.fullName)
  }
  val public = ref.isPublic
  val include = fullName.startsWith("metaconfig.")
  val exclude = fullName.contains(".internal.") ||
    fullName.startsWith("metaconfig.cli")
  public && include && !exclude
}

lazy val sharedSettings = List[Setting[_]](
  scalacOptions ++= List(
    "-Yrangepos",
    warnUnusedImport.value
  ),
  mimaBinaryIssueFilters ++= List[ProblemFilter](
    languageAgnosticCompatibilityPolicy
  ),
  mimaPreviousArtifacts := Set("com.geirsson" %% moduleName.value % "0.9.10")
)

skip.in(publish) := true
disablePlugins(MimaPlugin)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("metaconfig-core"))
  .settings(
    sharedSettings,
    moduleName := "metaconfig-core",
    libraryDependencies ++= List(
      "org.typelevel" %%% "paiges-core" % "0.3.1",
      "org.scala-lang.modules" %%% "scala-collection-compat" % "2.1.6",
      scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
    ) :+ (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => "com.lihaoyi" %%% "pprint" % "0.5.4"
      case _ => "com.lihaoyi" %%% "pprint" % "0.5.9"
    })
  )
lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val json = project
  .in(file("metaconfig-json"))
  .settings(
    sharedSettings,
    moduleName := "metaconfig-json",
    libraryDependencies ++= List(
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11 | 12)) => "com.lihaoyi" %%% "upickle" % "0.7.4"
        case _ => "com.lihaoyi" %% "upickle" % "0.7.5"
      })
    )
  )
  .dependsOn(coreJVM)

lazy val typesafe = project
  .in(file("metaconfig-typesafe-config"))
  .settings(
    sharedSettings,
    moduleName := "metaconfig-typesafe-config",
    description := "Integration for HOCON using typesafehub/config.",
    libraryDependencies += "com.typesafe" % "config" % "1.2.1"
  )
  .dependsOn(coreJVM)

lazy val sconfig = crossProject(JVMPlatform)
  .in(file("metaconfig-sconfig"))
  .settings(
    sharedSettings,
    moduleName := "metaconfig-sconfig",
    description := "Integration for HOCON using ekrich/sconfig.",
    libraryDependencies ++= List(
      "org.ekrich" %%% "sconfig" % "1.0.0"
    )
  )
  .dependsOn(core)
lazy val sconfigJVM = sconfig.jvm

val scalatagsVersion = Def.setting {
  "0.7.0"
}

lazy val tests = crossProject(JVMPlatform, JSPlatform)
  .in(file("metaconfig-tests"))
  .disablePlugins(MimaPlugin)
  .settings(
    sharedSettings,
    skip in publish := true,
    publishArtifact.in(Compile, packageDoc) := false,
    testFrameworks := List(new TestFramework("munit.Framework")),
    libraryDependencies ++= List(
      "org.scalameta" %%% "munit-scalacheck" % V.munit,
      "com.github.alexarchambault" %%% "scalacheck-shapeless_1.14" % "1.2.5"
    )
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )
  .jvmSettings(
    mainClass in GraalVMNativeImage := Some("metaconfig.tests.ExampleMain"),
    sources.in(Compile, doc) := Seq.empty,
    graalVMNativeImageOptions ++= {
      val reflectionFile =
        Keys.sourceDirectory.in(Compile).value./("graal")./("reflection.json")
      assert(reflectionFile.exists, "no such file: " + reflectionFile)
      List(
        "-H:+ReportUnsupportedElementsAtRuntime",
        "--initialize-at-build-time",
        "--initialize-at-run-time=metaconfig",
        "--no-server",
        "--enable-http",
        "--enable-https",
        "-H:EnableURLProtocols=http,https",
        "--enable-all-security-services",
        "--no-fallback",
        s"-H:ReflectionConfigurationFiles=$reflectionFile",
        "--allow-incomplete-classpath",
        "-H:+ReportExceptionStackTraces"
      )
    }
  )
  .jvmConfigure(
    _.enablePlugins(GraalVMNativeImagePlugin)
      .dependsOn(json, typesafe, sconfigJVM, docs)
  )
  .dependsOn(core)
lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val docs = project
  .in(file("metaconfig-docs"))
  .settings(
    sharedSettings,
    moduleName := "metaconfig-docs",
    libraryDependencies ++= List(
      "com.lihaoyi" %% "scalatags" % scalatagsVersion.value
    ),
    mdocVariables := Map(
      "VERSION" -> version.value.replaceFirst("\\+.*", ""),
      "SCALA_VERSION" -> scalaVersion.value
    ),
    mdocOut :=
      baseDirectory.in(ThisBuild).value / "website" / "target" / "docs",
    mdocExtraArguments := List("--no-link-hygiene")
  )
  .dependsOn(coreJVM, json, typesafe, sconfigJVM)
  .enablePlugins(DocusaurusPlugin)
  .disablePlugins(MimaPlugin)
