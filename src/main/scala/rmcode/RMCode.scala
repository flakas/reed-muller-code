package rmcode

import com.sksamuel.scrimage._
import java.io._

// RM(m=3, r=2).encode(1011010) => 00111001

class RMCode(m: Int, r: Int) {

  val q = 2
  val n = scala.math.pow(q, m).toInt

  val generatorMatrix = {
    val space = Space(m, q)
    // Sudaromi poerdviai pagal x_i komponentes
    val subspaces = (1 to m).map(Space.subspace(_, 0, m, q))
    // Sudaromi vektoriai v_1, ..., v_m
    val baseMatrix : List[Vector[Int]] =
      if (r == 0) List()
      else subspaces.map(
        subspace => space.map(spaceWord => if (subspace.contains(spaceWord)) 1 else 0).toVector
      ).toList
    // Jei reikia, sudaroma po C(m, s) kombinacijų sandaugų kiekvienam s <= r
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

  def encode(data: List[Vector[Int]]) : List[Vector[Int]]= {
    data.map(dataVector =>
        if (dataVector.size != matrixHeight) throw new Exception("Data vector size " + dataVector.size + " should match matrix height of " + matrixHeight)
        // Žodžio simbolį skaliariškai sudauginam su atitinkama generuojančios
        // matricos eilute ir visus gautus žodžius sudedam
        else dataVector.zip(generatorMatrix)
          .map(x => Space.multiplyByScalar(x._2, x._1, q))
          .reduce((v1, v2) => Space.add(v1, v2, q))
    )
  }

  def decode(data: List[Vector[Int]]) : List[Vector[Int]] = {
    //println("------------------------------")
    val space = Space(m, q)
    // Sudaromi poerdviai pagal x_i komponentes
    val subspaces = (1 to m).map(Space.subspace(_, 0, m, q)).toList
    val inverseSubspaces = (1 to m).map(Space.subspace(_, 1, m, q)).toList
    def decodeVector(c: Vector[Int], r: Int): Vector[Int] = {
      //println("------- c --------")
      //println(c)
      if (c.size != matrixWidth) throw new Exception("Data vector size " + c.size + " should match matrix width of " + matrixWidth)
      // Kiekvienam rinkiniui 1 <= i_1 < ... < i_r <= m ilgio r
      else {
        val coeficients = (1 to m).toVector.combinations(r).toVector.map(set => {
          val l = (1 to m).diff(set)
          val t = Space(m - r, q)
          val w = t.map(t => {
            // Sudaroma sankirta tarp H_l_1(t_1)...H_l_r-m(t_r-m)
            val intersection = l.zip(t).map(x =>
              if (x._2 == 0) subspaces(x._1 - 1)
              else inverseSubspaces(x._1 - 1)
            ).reduce((h1, h2) => h1.intersect(h2))

            // Sudaromas w_<t> = v(sankirta)
            space.map(spaceWord => if (intersection.contains(spaceWord)) 1 else 0).toVector
          })
          // Apskaičiuojamos balsavimo lygčių reikšmės
          val votes = w.map(w => Space.multiplyScalar(c, w, q))

          // a(i_1,...,i_r) koeficiento reikšmė nustatoma pagal vienodų bitų
          // skaičių balsavimo lygtyse
          if (votes.count(_ == 1) > votes.size / 2) 1 else 0
        })
        // Atimami r-os eilės vektoriai iš c
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

  def convertBytesToVectors(data: List[Byte]): (List[Vector[Int]], Int) = {
    def getBits(byte: Byte): List[Int] = {
      (0 to 7).map(i => (byte >> i) & 1).reverse.toList
    }
    // Baitai konvertuojami į bitų sąrašą,
    // Pridedama kiek reikia nulių, kad užpildyti koduojamus vektorius
    // Bitų sąrašas sugrupuojamas į koduojamo vektoriaus dydžio vektorius ir koduojamas
    val bits = data.flatMap(getBits(_)).toVector
    val paddingBits = (matrixHeight - bits.size % matrixHeight)
    (bits.padTo(bits.size + paddingBits, 0).grouped(matrixHeight).toList, paddingBits)
  }

  def convertVectorsToBytes(data: (List[Vector[Int]], Int)): List[Byte] = {
    def getBytes(bits: List[Int]): Byte = {
      (0 to 7).zip(bits.reverse).map(x => x._2 << x._1).sum.toByte
    }
    // Dekoduoti bitai konvertuojami į baitų sąrašą
    val bits = data._1.flatten
    bits.take(bits.size - data._2).grouped(8).map(getBytes(_)).toList
  }

  def encodeBytes(data: List[Byte]) = {
    val vectors = convertBytesToVectors(data)
    (encode(vectors._1), vectors._2)
  }

  def decodeBytes(data: (List[Vector[Int]], Int)): List[Byte] = {
    convertVectorsToBytes((decode(data._1), data._2))
  }

  def encodeText(text: String): (List[Vector[Int]], Int) = {
    // Tekstas išskaidomas baitais ir koduojamas
    //encodeBytes(text.getBytes().toList)
    encodeBytes(Utils.textToBytes(text))
  }

  def decodeText(data: (List[Vector[Int]], Int)): String = {
    // Iš baitų sekos sukonstruojama teksto eilutė
    Utils.bytesToText(decodeBytes(data))
    //new String(decodeBytes(data).toArray)
  }

  def encodeImage(file: File): (Int, Int, (List[Vector[Int]], Int)) = {
    def intToBytes(x: Int): Array[Byte] = {
      Array(x >> 24 & 0xFF, x >> 16 & 0xFF, x >> 8 & 0xFF, x & 0xFF).map(_.toByte)
    }
    val image = Image(file)
    (image.width, image.height, encodeBytes(image.pixels.map(intToBytes(_)).toList.flatten))
  }

  def decodeImage(data: (Int, Int, (List[Vector[Int]], Int))): Image = {
    def bytesToInt(bytes: List[Byte]): Int = {
      (0 to 3).reverse.zip(bytes).map(x => x._2.toInt << 8 * x._1).sum
    }
    val pixels = decodeBytes(data._3).grouped(4).map(bytesToInt(_)).toArray
    Image(data._1, data._2, pixels)
  }
}
