package org.scalaide.buildtools

import org.osgi.framework.Version
import Ecosystem._
import org.osgi.framework.VersionRange

object ScalaIDEVersion {

  def apply(iu: InstallableUnit, repository: P2Repository, existingAddOns: Map[PluginDescriptor, Seq[AddOn]], availableAddOns: Map[PluginDescriptor, Seq[AddOn]], siteRepo: P2Repository): ScalaIDEVersion = {
    val (associatedExistingAddOns, associatedAvailableAddOns) = latestAssociated(availableAddOns, iu.version).foldLeft(latestAssociated(existingAddOns, iu.version)) {
      (acc, availableAddOn) =>
        acc.get(availableAddOn._1) match {
          case Some(existingAddOn) if (existingAddOn.version.compareTo(availableAddOn._2.version) >= 0) =>
            acc
          case _ =>
            acc + availableAddOn
        }
    }.partition(_._2.repository == siteRepo)

    // the sdt.core bundle
    val sdtCore: Option[InstallableUnit] = (for {
      sdtCoreDep <- iu.dependencies.find(_.id == ScalaIDEId)
      sdtCoreVersion = new VersionRange(sdtCoreDep.range)
      sdtCore <- repository.findIU(ScalaIDEId).find(_.version == sdtCoreVersion)
    } yield sdtCore)

    // the version of eclipse it depends on
    val eclipseVersion: Option[EclipseVersion] = {
      val dependencies = sdtCore.map(_.dependencies) getOrElse Nil
      val jdtCoreDep = dependencies.find(_.id == JDTId)
      jdtCoreDep.map(jdt => EclipseVersion(jdt.range))
    }

    // the version of Scala it depends on
    val scalaVersion: Option[VersionRange] = {
      val scalaLibDep = sdtCore.flatMap(_.dependencies.find(_.id == ScalaLibraryId))
      scalaLibDep.map(scalaLib => new VersionRange(scalaLib.range))
    }

    new ScalaIDEVersion(iu, repository, scalaVersion, eclipseVersion, associatedExistingAddOns, associatedAvailableAddOns)
  }

  private def latestAssociated(addOns: Map[PluginDescriptor, Seq[AddOn]], version: Version): Map[PluginDescriptor, AddOn] = {
    addOns.flatMap {
      case (conf, addOns) =>
        addOns.filter(addOn => addOn.scalaIDEVersion.isEmpty || addOn.scalaIDEVersion.exists(vr => vr.includes(version))).headOption.map(conf -> _)
    }
  }
}

case class ScalaIDEVersion private (
  iu: InstallableUnit,
  repository: P2Repository,
  scalaVersion: Option[VersionRange],
  eclipseVersion: Option[EclipseVersion],
  associatedExistingAddOns: Map[PluginDescriptor, AddOn],
  associatedAvailableAddOns: Map[PluginDescriptor, AddOn]) {

  def version = iu.version

}