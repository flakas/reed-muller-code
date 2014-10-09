package rmcode
import java.io.File

// Simetrinis kanalas
class SymmetricChannel(errorRate: Double, q: Int) {

  // Persiunčia vektorius kanalu su atsitiktiniu "seed"
  def transmit(data: List[Vector[Int]]): List[Vector[Int]] = {
    transmit(data, (new scala.util.Random).nextInt())
  }
  // Persiunčia vektorius kanalu su fiksuotu "seed"
  def transmit(data: List[Vector[Int]], seed: Int): List[Vector[Int]] = {
    val random = new scala.util.Random(seed)
    data.map(c => {
      // Prie kiekvieno duomenų vektoriaus pridedamas klaidos vektorius,
      // Kurio kiekvienas bitas turi errorRate tikimybę pasikeisti
      val errorVector = (1 to c.length)
        .toVector
        .map(x => if (random.nextDouble() <= errorRate) 1 else 0)
      Space.add(c, errorVector, q)
    })
  }

}
