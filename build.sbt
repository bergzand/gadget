name := "riscv-blinky"

version := "0.1.0"

val spinalVersion = "1.6.4"

lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "com.github.spinalhdl",
      scalaVersion := "2.11.12",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "superproject",
    libraryDependencies ++= Seq(
      "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion,
      "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion,
      compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % spinalVersion)
    )
  ).dependsOn(vexRiscv)

lazy val vexRiscv = RootProject(uri("https://github.com/SpinalHDL/VexRiscv.git"))

fork := true
