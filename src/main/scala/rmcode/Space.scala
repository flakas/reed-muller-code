package rmcode

// Tiesinė erdvė
object Space {

  // Sudaro tiesinės erdvės F(n, q) žodžių aibę
  def apply(n: Int, q: Int) : List[Vector[Int]] = {
    // Ilgio n kūno F_q žodžiai, kur kiekvienas žodžio bitas priklauso F_q
    def combinations(size: Int) : List[List[Int]] = {
      if (size == 0) List(List())
      else for {
        x <- (0 to (q - 1)).toList
        xs <- combinations(size - 1)
      } yield x :: xs
    }
    combinations(n).map(_.toVector)
  }

  // Tiesinės erdvės F(n, q) žodžių vektorinė suma
  def add(vector1: Vector[Int], vector2: Vector[Int], q: Int): Vector[Int] = {
    // Atitinkami vektorių bitai sudedami kūne F_q
    if (vector1.size != vector2.size) throw new Exception("Vector sizes must match")
    vector1.zip(vector2).map(v => (v._1 + v._2) % q)
  }

  // Tiesinės erdvės F(n, q) žodžių vektorinė sandauga
  def multiply(vector1: Vector[Int], vector2: Vector[Int], q: Int): Vector[Int] = {
    // Atitinkami vektorių bitai sudauginami kūne F_q
    if (vector1.size != vector2.size) throw new Exception("Vector sizes must match")
    vector1.zip(vector2).map(v => (v._1 * v._2) % q)
  }

  // Tiesinės erdvės F(n, q) žodžių skaliarinė sandauga
  def multiplyScalar(vector1: Vector[Int], vector2: Vector[Int], q: Int): Int = {
    // Du vektoriai sudauginami, ir gauto vektoriaus bitai sudedami kūne F_q
    multiply(vector1, vector2, q).sum % q
  }

  // Tiesinės erdvės F(n, q) žodžio sandauga su skaliaru
  def multiplyByScalar(vector: Vector[Int], scalar: Int, q: Int): Vector[Int] = {
    // Kiekvienas vektoriaus bitas skaliariškai sudauginamas su skaliaru kūne F_q
    vector.map(_ * scalar % q)
  }

  // Tiesinės erdvės F(n, q) poerdvis, kur `position` simbolis yra lygus `value`
  def subspace(position: Int, value: Int, n: Int, q: Int) : List[Vector[Int]] = {
    Space(n, q).filter(vector => vector(position-1) == value)
  }

}
