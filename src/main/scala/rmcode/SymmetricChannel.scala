package rmcode
import java.io.File

class SymmetricChannel(codeAlgorithm: RMCode, errorRate: Double) {

  def transmit(data: Array[Byte]) = {
    data
  }

  def transmitText(data: String) = {
    transmit(data.getBytes())
  }

  def transmitFile(data: File) = {
    //transmit(scala.io.Source.fromFile(data).map(_.toByte()).toArray())
  }

}
