package rmcode

// Linear space
object Space {

  // Construct a set of F(n, q) words
  def apply(n: Int, q: Int) : List[Vector[Int]] = {
    // Words of F_q with length of n, where each bit belongs to F_q
    def combinations(size: Int) : List[List[Int]] = {
      if (size == 0) List(List())
      else for {
        x <- (0 to (q - 1)).toList
        xs <- combinations(size - 1)
      } yield x :: xs
    }
    combinations(n).map(_.toVector)
  }

  // Vector sum of two F(n, q) words
  def add(vector1: Vector[Int], vector2: Vector[Int], q: Int): Vector[Int] = {
    // Respective vector bits are added in space F_q
    if (vector1.size != vector2.size) throw new Exception("Vector sizes must match")
    vector1.zip(vector2).map(v => (v._1 + v._2) % q)
  }

  // Vector product of two F(n, q) words
  def multiply(vector1: Vector[Int], vector2: Vector[Int], q: Int): Vector[Int] = {
    // Respective vector bits are multiplied in space F_q
    if (vector1.size != vector2.size) throw new Exception("Vector sizes must match")
    vector1.zip(vector2).map(v => (v._1 * v._2) % q)
  }

  // Scalar product of two F(n, q) words
  def multiplyScalar(vector1: Vector[Int], vector2: Vector[Int], q: Int): Int = {
    // Take vector product and then add bits together in F_q
    multiply(vector1, vector2, q).sum % q
  }

  // Scalar multiplication of vector in F(n, q)
  def multiplyByScalar(vector: Vector[Int], scalar: Int, q: Int): Vector[Int] = {
    // Each vector bit is multiplied by scalar in F_q
    vector.map(_ * scalar % q)
  }

  // Computes a subspace of F(n, q), where `position` bit is equal to `value`
  def subspace(position: Int, value: Int, n: Int, q: Int) : List[Vector[Int]] = {
    Space(n, q).filter(vector => vector(position-1) == value)
  }

}
