package adventofcode.utils

object matrix:
  extension [T](matrix: List[List[T]])
    def rotateLeft: List[List[T]] = matrix.transpose.map(_.reverse)
    def rotateRight: List[List[T]] = matrix.map(_.reverse).transpose
    def flip: List[List[T]] = matrix.reverse

  extension [T](matrix: Vector[Vector[T]])
    def rotateLeft: Vector[Vector[T]] = matrix.transpose.map(_.reverse)
    def rotateRight: Vector[Vector[T]] = matrix.map(_.reverse).transpose
    def flip: Vector[Vector[T]] = matrix.reverse
