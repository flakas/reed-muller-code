package rmcode

import com.sksamuel.scrimage._

// Helpers
object Utils {

  def factorial(n: Int): Int = {
    def fact(n: Int, acc: Int) : Int = {
      if (n == 0) acc
      else fact(n-1, n*acc)
    }
    fact(n, 1)
  }

  def combinations(n: Int, k: Int): Int = {
    factorial(n) / (factorial(k) * factorial(n - k))
  }

  def textToBytes(text: String): List[Byte] = {
    text.getBytes().toList
  }

  def bytesToText(data: List[Byte]): String = new String(data.toArray)

  def imageToBytes(image: Image): List[Byte] = {
    // Convert pixel integers to bytes
    def intToBytes(x: Int): Array[Byte] = {
      Array(x >> 24 & 0xFF, x >> 16 & 0xFF, x >> 8 & 0xFF, x & 0xFF).map(_.toByte)
    }
    image.pixels.map(intToBytes(_)).toList.flatten
  }

  def bytesToImage(width: Int, height: Int, bytes: List[Byte]): Image = {
    // Convert bytes to a list of pixels
    def bytesToInt(bytes: List[Byte]): Int = {
      (0 to 3).reverse.zip(bytes).map(x => x._2.toInt << 8 * x._1).sum
    }
    val pixels = bytes.grouped(4).map(bytesToInt(_)).toArray
    Image(width, height, pixels)
  }

}
