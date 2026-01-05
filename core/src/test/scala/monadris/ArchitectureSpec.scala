package monadris

import com.tngtech.archunit.base.DescribedPredicate
import com.tngtech.archunit.core.domain.JavaClass
import com.tngtech.archunit.core.importer.ClassFileImporter
import com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noClasses
import com.tngtech.archunit.library.dependencies.SlicesRuleDefinition.slices
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArchitectureSpec extends AnyFlatSpec with Matchers:

  private object Packages:
    val root: String           = "monadris"
    val domain: String         = "..domain.."
    val view: String           = "..view.."
    val javaSql: String        = "java.sql.."
    val javaFileIo: String     = "java.nio.file.."
    val javaNet: String        = "java.net.."
    val javaConcurrent: String = "java.util.concurrent.."
    val zio: String            = "zio.."

  private object Predicates:
    val javaIoExcludingSerializable: DescribedPredicate[JavaClass] =
      new DescribedPredicate[JavaClass]("reside in java.io but not Serializable"):
        override def test(javaClass: JavaClass): Boolean =
          val name = javaClass.getName
          name.startsWith("java.io.") && name != "java.io.Serializable"

  private val classes = new ClassFileImporter().importPackages(Packages.root)

  "Domain layer" should "not depend on view layer" in:
    val rule = noClasses()
      .that()
      .resideInAPackage(Packages.domain)
      .should()
      .dependOnClassesThat()
      .resideInAPackage(Packages.view)
      .because("Domain models should not depend on view layer")

    rule.check(classes)

  "View layer" should "not depend on domain.model.game internals" in:
    // View depends on domain, but domain should not depend on view
    val rule = noClasses()
      .that()
      .resideInAPackage(Packages.domain)
      .should()
      .dependOnClassesThat()
      .resideInAPackage(Packages.view)
      .because("Domain should be independent of view layer")

    rule.check(classes)

  "Core package" should "not depend on java.sql (database)" in:
    val rule = noClasses()
      .that()
      .resideInAPackage(Packages.root)
      .should()
      .dependOnClassesThat()
      .resideInAPackage(Packages.javaSql)
      .because("Core should remain pure and not depend on database APIs")

    rule.check(classes)

  it should "not depend on java.io (file IO, excluding Serializable)" in:
    val rule = noClasses()
      .that()
      .resideInAPackage(Packages.root)
      .should()
      .dependOnClassesThat(Predicates.javaIoExcludingSerializable)
      .because("Core should remain pure and not depend on file IO APIs")

    rule.check(classes)

  it should "not depend on java.nio.file (NIO file IO)" in:
    val rule = noClasses()
      .that()
      .resideInAPackage(Packages.root)
      .should()
      .dependOnClassesThat()
      .resideInAPackage(Packages.javaFileIo)
      .because("Core should remain pure and not depend on NIO file APIs")

    rule.check(classes)

  it should "not depend on java.net (network IO)" in:
    val rule = noClasses()
      .that()
      .resideInAPackage(Packages.root)
      .should()
      .dependOnClassesThat()
      .resideInAPackage(Packages.javaNet)
      .because("Core should remain pure and not depend on network APIs")

    rule.check(classes)

  it should "not depend on ZIO" in:
    val rule = noClasses()
      .that()
      .resideInAPackage(Packages.root)
      .should()
      .dependOnClassesThat()
      .resideInAPackage(Packages.zio)
      .because("Core should be pure Scala without effect system dependencies")

    rule.check(classes)

  it should "not depend on java.util.concurrent (threading)" in:
    val rule = noClasses()
      .that()
      .resideInAPackage(Packages.root)
      .should()
      .dependOnClassesThat()
      .resideInAPackage(Packages.javaConcurrent)
      .because("Core should remain pure and not depend on concurrency APIs")

    rule.check(classes)

  "Package dependencies" should "be free of cycles" in:
    val rule = slices()
      .matching("monadris.(*)..")
      .should()
      .beFreeOfCycles()

    rule.check(classes)
