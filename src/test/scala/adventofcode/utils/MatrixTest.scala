package adventofcode.utils

import adventofcode.utils.matrix.*
import munit.FunSuite

class MatrixTest extends FunSuite:
  test("rotate"):
      val matrix =
        List(
          List(1, 2, 3),
          List(4, 5, 6),
          List(7, 8, 9)
        )
      assertEquals(
        matrix.rotateLeft,
        List(
          List(7, 4, 1),
          List(8, 5, 2),
          List(9, 6, 3)
        )
      )
      assertEquals(
        matrix.rotateLeft.rotateLeft,
        List(
          List(9, 8, 7),
          List(6, 5, 4),
          List(3, 2, 1)
        )
      )
      assertEquals(
        matrix.rotateLeft.rotateLeft.rotateLeft,
        List(
          List(3, 6, 9),
          List(2, 5, 8),
          List(1, 4, 7)
        )
      )
      assertEquals(matrix.rotateLeft.rotateLeft.rotateLeft.rotateLeft, matrix)
