package monadris.infrastructure.persistence

import zio.test.*

object ReplayErrorSpec extends ZIOSpecDefault:

  private object Sample:
    val Name   = "my-replay"
    val Reason = "Replay name must not be empty."
    val Detail = "unexpected end of input"
    val Cause  = "connection refused"

  def spec = suite("ReplayError.message")(
    test("names the replay that was not found or already exists") {
      assertTrue(
        ReplayError.NotFound(Sample.Name).message.contains(Sample.Name),
        ReplayError.AlreadyExists(Sample.Name).message.contains(Sample.Name)
      )
    },
    test("passes the validation reason through for an invalid name") {
      assertTrue(ReplayError.InvalidName(Sample.Reason).message == Sample.Reason)
    },
    test("describes corrupted data with the decoding detail") {
      assertTrue(ReplayError.Corrupted(Sample.Detail).message.contains(Sample.Detail))
    },
    test("uses the cause's message for a storage failure, or its type when there is none") {
      assertTrue(
        ReplayError.StorageFailure(new RuntimeException(Sample.Cause)).message == Sample.Cause,
        ReplayError.StorageFailure(new IllegalStateException()).message == "IllegalStateException"
      )
    }
  )
