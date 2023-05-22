package streams

class StringParserTerrainSuite extends munit.FunSuite:
  trait SPTTest extends StringParserTerrain:
    val level: String =
      """------
        |--oT--
        |--oS--
        |--oo--
        |------""".stripMargin
    val levelVector: Vector[Vector[Char]] = Vector(Vector('o', 'T'), Vector('o', 'S'), Vector('o', 'o'))

  test("findChar() should return correct position") {
    new SPTTest:
      assertEquals(findChar('S', levelVector), Pos(1, 1))
  }

  import scala.concurrent.duration.*
  override val munitTimeout: FiniteDuration = 10.seconds
