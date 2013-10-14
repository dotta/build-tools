package org.scalaide.buildtools

import org.osgi.framework.Version
import Ecosystem._
import org.osgi.framework.VersionRange

case class AddOn(conf: PluginDescriptor, iu: InstallableUnit, repository: P2Repository) {
  
  // TODO: need to check for missing information
  
  lazy val scalaIDEVersion: Option[VersionRange] = findDependency(ScalaIDEFeatureIdOsgi).map(d => new VersionRange(d.range))
  
  val version: Version = iu.version
  
  def id: String = conf.featureId
  
  private def findDependency(id: String): Option[DependencyUnit] = iu.dependencies.find(_.id == id)
}
