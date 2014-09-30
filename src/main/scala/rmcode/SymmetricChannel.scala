package rmcode
import java.io.File

class SymmetricChannel(errorRate: Double, q: Int) {

  def transmit(data: List[Vector[Int]]) = {
    val random = scala.util.Random
    data.map(c => {
      // Prie kiekvieno duomenų vektoriaus pridedamas klaidos vektorius,
      // Kurio kiekvienas bitas turi errorRate tikimybę tapti 1
      val errorVector = (1 to c.length)
        .toVector
        .map(x => if (random.nextDouble() <= errorRate) 1 else 0)
        Space.add(c, errorVector, q)
    })
  }
}
