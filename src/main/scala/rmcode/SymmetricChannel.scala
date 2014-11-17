package rmcode
import java.io.File

// Symmetric channel
class SymmetricChannel(errorRate: Double, q: Int) {

  // Transmits vectors with random seed
  def transmit(data: List[Vector[Int]]): List[Vector[Int]] = {
    transmit(data, (new scala.util.Random).nextInt())
  }
  // Transmits vectors with fixed seed
  def transmit(data: List[Vector[Int]], seed: Int): List[Vector[Int]] = {
    val random = new scala.util.Random(seed)
    data.map(c => {
      // Add an error vector to each transmitted vector
      val errorVector = (1 to c.length)
        .toVector
        .map(x => if (random.nextDouble() <= errorRate) 1 else 0)
      Space.add(c, errorVector, q)
    })
  }

}
