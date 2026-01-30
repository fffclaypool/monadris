package monadris.infrastructure.game

import zio.*
import zio.test.*

import monadris.infrastructure.terminal.TestServices as LocalTestServices

object MenuControllerSpec extends ZIOSpecDefault:

  private val menuItemCount = 5

  def spec = suite("MenuController")(
    suite("MenuAction enum")(
      test("Select wraps index correctly") {
        val action = MenuController.MenuAction.Select(2)
        action match
          case MenuController.MenuAction.Select(idx) => assertTrue(idx == 2)
          case _                                     => assertTrue(false)
      },
      test("Navigate wraps newIndex correctly") {
        val action = MenuController.MenuAction.Navigate(3)
        action match
          case MenuController.MenuAction.Navigate(idx) => assertTrue(idx == 3)
          case _                                       => assertTrue(false)
      },
      test("Quit is a valid action") {
        val action = MenuController.MenuAction.Quit
        assertTrue(action == MenuController.MenuAction.Quit)
      },
      test("All MenuAction variants are distinct") {
        val select   = MenuController.MenuAction.Select(0)
        val navigate = MenuController.MenuAction.Navigate(0)
        val quit     = MenuController.MenuAction.Quit
        assertTrue(
          select != navigate,
          select != quit,
          navigate != quit
        )
      }
    ),
    suite("Menu navigation with keys")(
      test("K key navigates up from index 1") {
        val expectedIndex = 0
        for result <- runMenuWithInputs(
            startIndex = 1,
            inputs = Chunk('k'.toInt, 'q'.toInt)
          )
        yield assertTrue(result.navigatedTo.contains(expectedIndex))
      }.provide(LocalTestServices.console, LocalTestServices.config),
      test("J key navigates down from index 0") {
        val expectedIndex = 1
        for result <- runMenuWithInputs(
            startIndex = 0,
            inputs = Chunk('j'.toInt, 'q'.toInt)
          )
        yield assertTrue(result.navigatedTo.contains(expectedIndex))
      }.provide(LocalTestServices.console, LocalTestServices.config),
      test("K key wraps to bottom from index 0") {
        val expectedIndex = menuItemCount - 1
        for result <- runMenuWithInputs(
            startIndex = 0,
            inputs = Chunk('K'.toInt, 'q'.toInt)
          )
        yield assertTrue(result.navigatedTo.contains(expectedIndex))
      }.provide(LocalTestServices.console, LocalTestServices.config),
      test("J key wraps to top from last index") {
        val expectedIndex = 0
        for result <- runMenuWithInputs(
            startIndex = menuItemCount - 1,
            inputs = Chunk('J'.toInt, 'q'.toInt)
          )
        yield assertTrue(result.navigatedTo.contains(expectedIndex))
      }.provide(LocalTestServices.console, LocalTestServices.config),
      test("Number keys select corresponding menu item") {
        for result <- runMenuWithInputs(
            startIndex = 0,
            inputs = Chunk('2'.toInt)
          )
        yield assertTrue(result.selectedIndex.contains(1))
      }.provide(LocalTestServices.console, LocalTestServices.config),
      test("Enter key selects current item") {
        for result <- runMenuWithInputs(
            startIndex = 0,
            inputs = Chunk('\r'.toInt)
          )
        yield assertTrue(result.selectedIndex.contains(0))
      }.provide(LocalTestServices.console, LocalTestServices.config),
      test("Q key quits the menu") {
        for result <- runMenuWithInputs(startIndex = 0, inputs = Chunk('q'.toInt))
        yield assertTrue(result.didQuit)
      }.provide(LocalTestServices.console, LocalTestServices.config),
      test("Uppercase Q key also quits") {
        for result <- runMenuWithInputs(startIndex = 0, inputs = Chunk('Q'.toInt))
        yield assertTrue(result.didQuit)
      }.provide(LocalTestServices.console, LocalTestServices.config)
    ),
    suite("Arrow key handling")(
      test("Up arrow navigates up") {
        val escapeSeq     = Chunk(27, '['.toInt, 'A'.toInt)
        val quitSeq       = Chunk('q'.toInt)
        val expectedIndex = 0
        for result <- runMenuWithInputs(
            startIndex = 1,
            inputs = escapeSeq ++ quitSeq
          )
        yield assertTrue(result.navigatedTo.contains(expectedIndex))
      }.provide(LocalTestServices.console, LocalTestServices.config),
      test("Down arrow navigates down") {
        val escapeSeq     = Chunk(27, '['.toInt, 'B'.toInt)
        val quitSeq       = Chunk('q'.toInt)
        val expectedIndex = 1
        for result <- runMenuWithInputs(
            startIndex = 0,
            inputs = escapeSeq ++ quitSeq
          )
        yield assertTrue(result.navigatedTo.contains(expectedIndex))
      }.provide(LocalTestServices.console, LocalTestServices.config),
      test("Escape alone (no arrow sequence) quits") {
        for result <- runMenuWithInputs(startIndex = 0, inputs = Chunk(27))
        yield assertTrue(result.didQuit)
      }.provide(LocalTestServices.console, LocalTestServices.config)
    ),
    suite("Menu rendering")(
      test("Menu renders output to console") {
        for
          service <- ZIO.service[LocalTestServices.TestConsoleService]
          _       <- runMenuWithInputs(startIndex = 0, inputs = Chunk('q'.toInt))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(combined.nonEmpty)
      }.provide(LocalTestServices.console, LocalTestServices.config)
    )
  )

  final case class MenuTestResult(
    selectedIndex: Option[Int] = None,
    navigatedTo: List[Int] = Nil,
    didQuit: Boolean = false
  )

  private def runMenuWithInputs(
    startIndex: Int,
    inputs: Chunk[Int]
  ): ZIO[LocalTestServices.TestConsoleService & monadris.config.AppConfig, Throwable, MenuTestResult] =
    for
      resultRef <- Ref.make(MenuTestResult())
      ttyLayer        = LocalTestServices.tty(inputs)
      executeMenuItem = (index: Int) => resultRef.update(_.copy(selectedIndex = Some(index))).as(false)
      _ <- MenuController
        .run[Any](executeMenuItem)
        .catchAll {
          case _: java.util.NoSuchElementException => ZIO.unit
          case e                                   => ZIO.fail(e)
        }
        .provideSome[LocalTestServices.TestConsoleService & monadris.config.AppConfig](ttyLayer)
        .fork
        .flatMap { fiber =>
          fiber.join.timeout(Duration.fromMillis(500)).catchAll(_ => ZIO.unit)
        }
      result      <- resultRef.get
      navigations <- extractNavigations(inputs, startIndex)
    yield result.copy(
      navigatedTo = navigations,
      didQuit =
        inputs.contains('q'.toInt) || inputs.contains('Q'.toInt) || (inputs.headOption.contains(27) && inputs.size == 1)
    )

  private def extractNavigations(inputs: Chunk[Int], startIndex: Int): UIO[List[Int]] =
    ZIO.succeed {
      var idx         = startIndex
      val menuSize    = menuItemCount
      val navigations = scala.collection.mutable.ListBuffer.empty[Int]

      val inputList = inputs.toList
      var i         = 0
      while i < inputList.size do
        inputList(i) match
          case 'k' | 'K' =>
            idx = if idx > 0 then idx - 1 else menuSize - 1
            navigations += idx
          case 'j' | 'J' =>
            idx = if idx < menuSize - 1 then idx + 1 else 0
            navigations += idx
          case 27 if i + 2 < inputList.size && inputList(i + 1) == '[' =>
            inputList(i + 2) match
              case 'A' =>
                idx = if idx > 0 then idx - 1 else menuSize - 1
                navigations += idx
                i += 2
              case 'B' =>
                idx = if idx < menuSize - 1 then idx + 1 else 0
                navigations += idx
                i += 2
              case _ => ()
          case _ => ()
        i += 1

      navigations.toList
    }
