package rmcode

object Space {

  def apply(n: Int, q: Int) : List[Vector[Int]] = {
    def combinations(size: Int) : List[List[Int]] = {
      if (size == 0) List(List())
      else for {
        x <- (0 to (q - 1)).toList
        xs <- combinations(size - 1)
      } yield x :: xs
    }
    combinations(n).map(_.toVector)
  }

  def add(vector1: Vector[Int], vector2: Vector[Int], q: Int): Vector[Int] = {
    if (vector1.size != vector2.size) throw new Exception("Vector sizes must match")
    vector1.zip(vector2).map(v => (v._1 + v._2) % q)
  }

  def multiply(vector1: Vector[Int], vector2: Vector[Int], q: Int): Vector[Int] = {
    if (vector1.size != vector2.size) throw new Exception("Vector sizes must match")
    vector1.zip(vector2).map(v => (v._1 * v._2) % q)
  }

  def multiplyScalar(vector1: Vector[Int], vector2: Vector[Int], q: Int): Int = {
    multiply(vector1, vector2, q).sum % q
  }

  def multiplyByScalar(vector: Vector[Int], scalar: Int, q: Int): Vector[Int] = {
    vector.map(_ * scalar % q)
  }

  def subspace(position: Int, value: Int, n: Int, q: Int) : List[Vector[Int]] = {
    Space(n, q).filter(vector => vector(position-1) == value)
  }

}
