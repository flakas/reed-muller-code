package rmcode

import com.sksamuel.scrimage._

// Pagalbinės funkcijos
object Utils {

  // Matematinis faktorialas
  def factorial(n: Int): Int = {
    def fact(n: Int, acc: Int) : Int = {
      if (n == 0) acc
      else fact(n-1, n*acc)
    }
    fact(n, 1)
  }

  // Funkcija C(n, k)
  def combinations(n: Int, k: Int): Int = {
    factorial(n) / (factorial(k) * factorial(n - k))
  }

  // Teksto transformacija į baitus
  def textToBytes(text: String): List[Byte] = {
    text.getBytes().toList
  }

  // Baitų transformacija į tekstą
  def bytesToText(data: List[Byte]): String = new String(data.toArray)

  // Paveiksliuko pikselių transformacija į baitų kolekciją
  def imageToBytes(image: Image): List[Byte] = {
    // Į baitus konvertuojami paveiksliuko pikseliai
    def intToBytes(x: Int): Array[Byte] = {
      Array(x >> 24 & 0xFF, x >> 16 & 0xFF, x >> 8 & 0xFF, x & 0xFF).map(_.toByte)
    }
    image.pixels.map(intToBytes(_)).toList.flatten
  }

  // Pikselių baitų kolekcijos transformacija į paveiksliuko objektą
  def bytesToImage(width: Int, height: Int, bytes: List[Byte]): Image = {
    // Baitai konvertuojami į pikselių seką
    def bytesToInt(bytes: List[Byte]): Int = {
      (0 to 3).reverse.zip(bytes).map(x => x._2.toInt << 8 * x._1).sum
    }
    val pixels = bytes.grouped(4).map(bytesToInt(_)).toArray
    Image(width, height, pixels)
  }

}
