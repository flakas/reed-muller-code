package rmcode

import com.sksamuel.scrimage._
import java.io._

// RM(m=3, r=2).encode(1011010) => 00111001

// Reed-Muller code
class RMCode(m: Int, r: Int) {

  val q = 2
  val n = scala.math.pow(q, m).toInt

  // Generator matrix
  val generatorMatrix = {
    val space = Space(m, q)
    // Construct subspaces based on x_i components
    val subspaces = (1 to m).map(Space.subspace(_, 0, m, q))
    // Construct vectors v_1, ..., v_m
    val baseMatrix : List[Vector[Int]] =
      if (r == 0) List()
      else subspaces.map(
        subspace => space.map(spaceWord => if (subspace.contains(spaceWord)) 1 else 0).toVector
      ).toList
    // If needed, construct C(m, s) product combinations for each s <= r
    val combinationMatrix : List[Vector[Int]] = for {
      s <- (2 to r).toList
      combination <- baseMatrix.combinations(s)
    } yield {
      combination.reduce((v1: Vector[Int], v2: Vector[Int]) => Space.multiply(v1, v2, q))
    }
    val matrix : List[Vector[Int]] = (1 to n).map(x => 1).toVector :: baseMatrix ++ combinationMatrix
    matrix
  }

  val matrixHeight = generatorMatrix.size
  val matrixWidth = generatorMatrix.head.size

  // Encode a vector list with RM code
  def encode(data: List[Vector[Int]]) : List[Vector[Int]]= {
    data.map(dataVector =>
        if (dataVector.size != matrixHeight) throw new Exception("Vector length " + dataVector.size + " should match matrix height " + matrixHeight)
        // Compute scalar product of each word's symbol with the apropriate
        // row of generator matrix and sum all resulting words
        else dataVector.zip(generatorMatrix)
          .map(x => Space.multiplyByScalar(x._2, x._1, q))
          .reduce((v1, v2) => Space.add(v1, v2, q))
    )
  }

  // Decode RM code vector list back to data vector list
  def decode(data: List[Vector[Int]]) : List[Vector[Int]] = {
    val space = Space(m, q)
    // Compute subspaces based on x_i components
    val subspaces = (1 to m).map(Space.subspace(_, 0, m, q)).toList
    val inverseSubspaces = (1 to m).map(Space.subspace(_, 1, m, q)).toList
    // Recursively decode vector with r, r-1, ... 0
    def decodeVector(c: Vector[Int], r: Int): Vector[Int] = {
      if (c.size != matrixWidth) throw new Exception("Vector length " + c.size + " should match matrix height " + matrixWidth)
      // For each list 1 <= i_1 < ... < i_r <= m of length r
      else {
        val coeficients = (1 to m).toVector.combinations(r).toVector.map(set => {
          val l = (1 to m).diff(set)
          val t = Space(m - r, q)
          val w = t.map(t => {
            // Compute intersection between H_l_1(t_1)...H_l_r-m(t_r-m)
            val intersection = l.zip(t).map(x =>
              if (x._2 == 0) subspaces(x._1 - 1)
              else inverseSubspaces(x._1 - 1)
            ).reduce((h1, h2) => h1.intersect(h2))

            // Compute w_<t> = v(intersection)
            space.map(spaceWord => if (intersection.contains(spaceWord)) 1 else 0).toVector
          })
          // Compute values of voting equations
          val votes = w.map(w => Space.multiplyScalar(c, w, q))

          // a(i_1,...,i_r) coeficient values are determined by amount of equal bits
          // in voting equations
          if (votes.count(_ == 1) > votes.size / 2) 1 else 0
        })
        // Subtract r-th order vectors from c
        val dropVectors = (0 to r-1).map(Utils.combinations(m, _)).sum
        val takeVectors = Utils.combinations(m, r)
        val rthVectorSum =
          generatorMatrix.drop(dropVectors).take(takeVectors)
            .zip(coeficients).map(x => Space.multiplyByScalar(x._1, x._2, q))
            .reduce((v1, v2) => Space.add(v1, v2, q))
        val newC = Space.add(c, rthVectorSum, q)

        if (r > 0) decodeVector(newC, r - 1) ++ coeficients
        else coeficients
      }
    }
    data.map(c => {
      decodeVector(c, r)
    })
  }

  // Convert byte list to vector list of apropriate size, and add excess
  // bits if needed
  def convertBytesToVectors(data: List[Byte]): (List[Vector[Int]], Int) = {
    def getBits(byte: Byte): List[Int] = {
      (0 to 7).map(i => (byte >> i) & 1).reverse.toList
    }
    // Bytes are converted to a common bit sequence,
    // An apropriate amount of zeros is added to pad to necessary vector length
    // Bit sequence is grouped to code vector sized vectors
    val bits = data.flatMap(getBits(_)).toVector
    val paddingBits = (matrixHeight - bits.size % matrixHeight)
    (bits.padTo(bits.size + paddingBits, 0).grouped(matrixHeight).toList, paddingBits)
  }

  // Convert vector list to byte list and remove excess bits
  def convertVectorsToBytes(data: (List[Vector[Int]], Int)): List[Byte] = {
    def getBytes(bits: List[Int]): Byte = {
      (0 to 7).zip(bits.reverse).map(x => x._2 << x._1).sum.toByte
    }
    // Vector bits are grouped into one byte (8 bit) vectors, and then bytes
    val bits = data._1.flatten
    bits.take(bits.size - data._2).grouped(8).map(getBytes(_)).toList
  }

  // Encode byte sequence with RM code
  def encodeBytes(data: List[Byte]) = {
    val vectors = convertBytesToVectors(data)
    (encode(vectors._1), vectors._2)
  }

  // Decode RM code vectors to bytes
  def decodeBytes(data: (List[Vector[Int]], Int)): List[Byte] = {
    convertVectorsToBytes((decode(data._1), data._2))
  }

  // Encode text with RM code
  def encodeText(text: String): (List[Vector[Int]], Int) = {
    // Text is converted to bytes and then encoded
    encodeBytes(Utils.textToBytes(text))
  }

  // Decode RM code vectors and turn them to text
  def decodeText(data: (List[Vector[Int]], Int)): String = {
    // Convert byte list to text string
    Utils.bytesToText(decodeBytes(data))
  }

  // Encode image pixels with RM code
  def encodeImage(file: File): (Int, Int, (List[Vector[Int]], Int)) = {
    val image = Image(file)
    (image.width, image.height, encodeBytes(Utils.imageToBytes(image)))
  }

  // Decode RM encoded image
  def decodeImage(data: (Int, Int, (List[Vector[Int]], Int))): Image = {
    Utils.bytesToImage(data._1, data._2, decodeBytes(data._3))
  }
}
