scalacOptions ++= Seq("-unchecked", "-deprecation")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.7.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.10")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.9.5")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.4.0")
