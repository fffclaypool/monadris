package monadris.infrastructure.game

import zio.*
import zio.test.*

import monadris.infrastructure.terminal.TestServices as Mocks
import monadris.view.GameView

object MenuControllerSpec extends ZIOSpecDefault:

  private val menuItems     = GameView.MenuItems.all
  private val menuItemCount = menuItems.size

  private object Key:
    val Escape      = 27
    val Bracket     = '['.toInt
    val ArrowUp     = 'A'.toInt
    val ArrowDown   = 'B'.toInt
    val Unknown     = 'Z'.toInt
    val Enter       = '\r'.toInt
    val LineFeed    = '\n'.toInt
    val Up          = 'k'.toInt
    val Down        = 'j'.toInt
    val Quit        = 'q'.toInt
    val EndOfStream = -1

  private object Item:
    val First     = 0
    val Second    = 1
    val Third     = 2
    val ThirdKey  = '3'.toInt
    val Last: Int = menuItemCount - 1

  private object Shown:
    val Goodbye = "Thanks for playing!"

  private val suiteTimeout = 10.seconds

  /** Which item was executed (if any) and everything the menu drew */
  final private case class MenuRun(selected: Option[Int], screen: String)

  /** Runs the menu on the given keys; executing an item leaves the menu */
  private def runMenu(inputs: Chunk[Int]): Task[MenuRun] =
    val run =
      for
        console  <- ZIO.service[Mocks.TestConsoleService]
        selected <- Ref.make(Option.empty[Int])
        _        <- MenuController.run[Any](index => selected.set(Some(index)).as(false))
        result   <- selected.get
        output   <- console.buffer.get
      yield MenuRun(result, output.mkString)
    run.provide(Mocks.tty(inputs), Mocks.console, Mocks.config)

  private def selects(inputs: Chunk[Int], expected: Int): Task[TestResult] =
    runMenu(inputs).map(menu => assertTrue(menu.selected.contains(expected)))

  private def quitsWithoutSelecting(inputs: Chunk[Int]): Task[TestResult] =
    runMenu(inputs).map(menu => assertTrue(menu.selected.isEmpty, menu.screen.contains(Shown.Goodbye)))

  private def itemSelectionSuite = suite("Item selection")(
    test("Enter selects the first item initially") {
      selects(Chunk(Key.Enter), Item.First)
    },
    test("a line feed selects like Enter") {
      selects(Chunk(Key.LineFeed), Item.First)
    },
    test("the shortcut key shown next to an item selects it directly") {
      selects(Chunk(Item.ThirdKey), Item.Third)
    },
    test("a key that is not a shortcut is ignored") {
      selects(Chunk(Key.Unknown, Key.Enter), Item.First)
    }
  )

  private def vimNavigationSuite = suite("Navigation with K and J")(
    test("j moves the selection down") {
      selects(Chunk(Key.Down, Key.Enter), Item.Second)
    },
    test("k moves the selection back up") {
      selects(Chunk(Key.Down, Key.Down, Key.Up, Key.Enter), Item.Second)
    },
    test("uppercase K and J navigate as well") {
      selects(Chunk('J'.toInt, 'J'.toInt, 'K'.toInt, Key.Enter), Item.Second)
    },
    test("k wraps around from the first item to the last") {
      selects(Chunk(Key.Up, Key.Enter), Item.Last)
    },
    test("j wraps around from the last item to the first") {
      selects(Chunk.fill(menuItemCount)(Key.Down) :+ Key.Enter, Item.First)
    }
  )

  private def arrowNavigationSuite = suite("Navigation with arrow keys")(
    test("the arrow down sequence moves the selection") {
      selects(Chunk(Key.Escape, Key.Bracket, Key.ArrowDown, Key.Enter), Item.Second)
    },
    test("the arrow up sequence wraps around to the last item") {
      selects(Chunk(Key.Escape, Key.Bracket, Key.ArrowUp, Key.Enter), Item.Last)
    },
    test("an unknown arrow sequence leaves the selection unchanged") {
      selects(Chunk(Key.Escape, Key.Bracket, Key.Unknown, Key.Enter), Item.First)
    },
    test("an escape followed by something other than a bracket leaves the selection unchanged") {
      selects(Chunk(Key.Escape, Key.Unknown, Key.Enter), Item.First)
    }
  )

  private def quitSuite = suite("Quitting")(
    test("q quits and shows the goodbye screen") {
      quitsWithoutSelecting(Chunk(Key.Quit))
    },
    test("uppercase Q quits as well") {
      quitsWithoutSelecting(Chunk('Q'.toInt))
    },
    test("a lone escape key quits") {
      quitsWithoutSelecting(Chunk(Key.Escape))
    },
    test("the end of the input stream quits") {
      quitsWithoutSelecting(Chunk(Key.EndOfStream))
    }
  )

  private def renderingSuite = suite("Rendering")(
    test("the menu shows every item with its shortcut key") {
      runMenu(Chunk(Key.Quit)).map { menu =>
        assertTrue(
          menuItems.forall(item => menu.screen.contains(item.label) && menu.screen.contains(item.key.toString))
        )
      }
    }
  )

  def spec = suite("MenuController")(
    itemSelectionSuite,
    vimNavigationSuite,
    arrowNavigationSuite,
    quitSuite,
    renderingSuite
  ) @@ TestAspect.timeout(suiteTimeout)
