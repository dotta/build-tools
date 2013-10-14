package org.scalaide.buildtools

import org.osgi.framework.VersionRange
import org.osgi.framework.Version

object EclipseVersion {
  val Default = EclipseJuno

  def apply(range: String): EclipseVersion = {
    lazy val indigoVersion = new Version(3,7,0)
    lazy val junoVersion = new Version(3,8,0)

    val version = new VersionRange(range)

    if(version.isEmpty()) Default
    else if (version.includes(junoVersion)) EclipseJuno
    else if(version.includes(indigoVersion)) EclipseIndigo
    else throw new UnsupportedEclipseVersion(version)
  }
}

abstract class EclipseVersion(val id: String, val name: String, val repoLocation: String)

case object EclipseIndigo extends EclipseVersion("indigo", "Indigo", "http://download.eclipse.org/releases/indigo/")

case object EclipseJuno extends EclipseVersion("juno", "Juno", "http://download.eclipse.org/releases/juno/")

case class UnsupportedEclipseVersion(version: VersionRange) extends Exception