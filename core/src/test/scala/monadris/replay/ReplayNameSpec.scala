package monadris.replay

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReplayNameSpec extends AnyFlatSpec with Matchers:

  private object Names:
    val Simple    = "my-replay_01"
    val Japanese  = "テスト再生"
    val Padded    = "  padded  "
    val Trimmed   = "padded"
    val Traversal = "../secret"
    val WithSlash = "a/b"
    val WithDot   = "a.b"
    val WithSpace = "a b"
    val Blank     = "   "
    val LongestOk = "a" * ReplayName.MaxLength
    val TooLong   = "a" * (ReplayName.MaxLength + 1)

  "ReplayName.validate" should "accept letters, digits, hyphens and underscores" in {
    ReplayName.validate(Names.Simple) shouldBe Right(Names.Simple)
  }

  it should "accept non-ASCII letters" in {
    ReplayName.validate(Names.Japanese) shouldBe Right(Names.Japanese)
  }

  it should "trim surrounding whitespace" in {
    ReplayName.validate(Names.Padded) shouldBe Right(Names.Trimmed)
  }

  it should "reject empty and blank names" in {
    ReplayName.validate("").isLeft shouldBe true
    ReplayName.validate(Names.Blank).isLeft shouldBe true
  }

  it should "reject path separators, dots and inner whitespace" in
    List(Names.Traversal, Names.WithSlash, Names.WithDot, Names.WithSpace).foreach { name =>
      ReplayName.validate(name).isLeft shouldBe true
    }

  it should "enforce the maximum length" in {
    ReplayName.validate(Names.LongestOk) shouldBe Right(Names.LongestOk)
    ReplayName.validate(Names.TooLong).isLeft shouldBe true
  }

  "ReplayName.isValid" should "agree with validate" in {
    ReplayName.isValid(Names.Simple) shouldBe true
    ReplayName.isValid(Names.Traversal) shouldBe false
  }
