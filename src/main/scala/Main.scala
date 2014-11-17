import java.io.File
import rmcode._

import com.sksamuel.scrimage._

case class Config(
  m: Int = 3,
  r: Int = 9,
  errorRate: Double = 0,
  verbose: Boolean = false,
  debug: Boolean = false,
  inputText: String = null,
  inputFile: File = null,
  outputFileCoded: File = null,
  outputFileUncoded: File = null,
  inputVector: String = null,
  decodeVector: String = null
)

object Main extends App {

  val parser = new scopt.OptionParser[Config]("RMCode") {
    head("RMCode", "1.0")
    opt[Int]('m', "m") required() action { (x, c) =>
      c.copy(m = x) } text("m parameter for Reed-Muller code") validate { x => if (x > 0) success else failure("Parameter m must be > 0") }

    opt[Int]('r', "r") required() action { (x, c) =>
      c.copy(r = x) } text("r parameter for Reed-Muller code") validate { x => if (x > 0) success else failure("Parameter r must be > 0") }

    opt[Double]('p', "errorRate") required() action { (x, c) =>
      c.copy(errorRate = x) } text("errorRate is the probability of an error during transmission") validate { x => if (x >= 0 && x <= 1) success else failure("Parameter errorRate must be 0<=errorRate<=1") }

    opt[String]('v', "inputVector") action { (x, c) =>
      c.copy(inputVector = x) } text("inputVector is a data vector to be sent via transmission channel")

    opt[String]('d', "decodeVector") action { (x, c) =>
      c.copy(decodeVector = x) } text("decodeVector is a vector of RM code, that will be decoded with given RM code. Use this to manually add transmission errors")

    opt[String]('i', "inputText") action { (x, c) =>
      c.copy(inputText = x) } text("inputText is a text that will be transmitted via transmission channel")

    opt[File]('i', "inputFile") valueName("<file>") action { (x, c) =>
      c.copy(inputFile = x) } text("inputFile is a path to a file (an image of JPEG format), that will be transmitted via a transmission channel")
    opt[File]('o', "outputFileCoded") valueName("<file>") action { (x, c) =>
      c.copy(outputFileCoded = x) } text("outputFileCoded path where the file transmitted via RM coded channel should be stored")
    opt[File]('c', "outputFileUncoded") valueName("<file>") action { (x, c) =>
      c.copy(outputFileUncoded = x) } text("outputFileUncoded is a path where the file transmitted via an uncoded channel should be stored")

    help("help") text("This help message")
    note("""Usage examples:
=====================

# In order to send a vector:
  Mandatory parameters: m, r, errorRate, inputVector
  Command line sample:
    java -jar RMCode.jar --m <parameter m> --r <parameter r> --errorRate <parameter p> --inputVector "<vector to be transmitted>"
  Command line call sample pvz.:
    java -jar RMCode.jar --m 4 --r 2 --errorRate 0.2 --inputVector "10101010101"
  Result:
    An input vector, encoded vector before transmission, encoded vector after transmission, error vector, decoded vector and transmitted vector without encoding will be displayed

# In order to decode a vector:
  Mandatory parameters: m, r, errorRate, decodeVector
  Command line sample:
    java -jar RMCode.jar --m <parameter m> --r <parameter r> --errorRate <parameter p> --decodeVector "<vector of code RM(m, r) to be decoded>"
  Command line call sample:
    java -jar RMCode.jar --m 4 --r 2 --errorRate 0.2 --decodeVector "0010000111101001"
  Result:
    A decoded vector will be displayed. Purpose of this call is to manually insert transmission errors.

# In order to transmit text:
  Mandatory parameters: m, r, errorRate, inputText
  Command line sample:
    java -jar RMCode.jar --m <parameter m> --r <parameter r> --errorRate <parameter p> --inputText "<text to be transmitted>"
  Command line call sample:
    java -jar RMCode.jar --m 4 --r 2 --errorRate 0.05 --inputText "abcdefghijklmnopqrstuvwxyz"
  Result:
    Text transmitted via a coded channel and text transmitted via uncoded channel will be displayed.

# In order to transmit an image:
  Mandatory parameters: m, r, errorRate, inputFile, outputFileCoded, outputFileUncoded
  Command line sample:
    java -jar RMCode.jar --m <parameter m> --r <parameter r> --errorRate <parameter p> --inputFile <path of input JPEG image> --outputFileCoded <path where resulting transmitted coded image will be saved> --outputFileUncoded <path where resulting transmitted uncoded image will be saved>
  Command line call sample:
    java -jar RMCode.jar --m 3 --r 1 --errorRate 0.05 --inputFile source.jpeg --outputFileCoded coded.jpeg --outputFileUncoded uncoded.jpeg
  Result:
    Image transmitted via channel with coding will be saved to outputFileCoded, image transmitted via channel with no coding will be saved to outputFileUncoded.""")
  }

  try {
    // parser.parse returns Option[C]
    parser.parse(args, Config()) map { config =>
      if (config.m > config.r) {
        val channel = new SymmetricChannel(config.errorRate, 2)
        val code = new RMCode(config.m, config.r)
        if (config.inputFile != null && config.outputFileCoded != null && config.outputFileUncoded != null) {
          // Send a file

          println("Processing image transmission with encoding...")
          // Encode, send, decode and save the image
          val encodedImage = code.encodeImage(config.inputFile)
          val transmittedImage = (encodedImage._1, encodedImage._2, (channel.transmit(encodedImage._3._1), encodedImage._3._2))
          val decodedImage = code.decodeImage(transmittedImage)
          decodedImage.writer(Format.JPEG).write(config.outputFileCoded)

          println("Processing image transmission without encoding...")
          // Turn bytes into vectors, transmit, turn resulting vectors back into image
          val uncodedImage = Image(config.inputFile)
          val uncodedImageVectors = code.convertBytesToVectors(Utils.imageToBytes(uncodedImage))
          val transmittedUncodedImage = Utils.bytesToImage(uncodedImage.width, uncodedImage.height, code.convertVectorsToBytes((channel.transmit(uncodedImageVectors._1), uncodedImageVectors._2)))
          transmittedUncodedImage.writer(Format.JPEG).write(config.outputFileUncoded)
          println("Transmitted image using coded channel was saved to     " +  config.outputFileCoded)
          println("Transmitted image not using coded channel was saved to " +  config.outputFileUncoded)

        } else if (config.decodeVector != null) {

          // Decode given vector and show it
          val vector = config.decodeVector.map(x => if (x == '1') 1 else 0).toVector
          val decodedCodedVector = code.decode(List(vector)).head
          val decodedVector = new String(decodedCodedVector.map(x => if (x == 1) '1' else '0').toArray)

          println("Vector to be decoded: " + config.decodeVector)
          println("Decoded vector:       " + decodedVector)

        } else if (config.inputVector != null) {

          // Encode, transmit, decode and display the vector
          val seed = (new scala.util.Random).nextInt()
          val vector = config.inputVector.map(x => if (x == '1') 1 else 0).toVector
          val encodedVector = code.encode(List(vector))
          val encodedVectorString = new String(encodedVector.head.map(x => if (x == 1) '1' else '0').toArray)
          val transmittedCodedVector = channel.transmit(encodedVector, seed)
          val transmittedCodedVectorBits = new String(transmittedCodedVector.head.map(x => if (x == 1) '1' else '0').toArray)
          val decodedCodedVector = code.decode(transmittedCodedVector).head
          val decodedVector = new String(decodedCodedVector.map(x => if (x == 1) '1' else '0').toArray)
          val errorVector = encodedVector.head.zip(transmittedCodedVector.head).map(x =>
              if (x._1 != x._2) 1 else 0)
          val errorVectorString = new String(errorVector.map(x => if (x == 1) '1' else '0').toArray)
          val errorPositions = errorVector.zip((1 to errorVector.length)).filter(_._1 == 1).map(_._2)
          val numberOfErrorsCode = encodedVector.zip(transmittedCodedVector).map(x =>
                x._1.zip(x._2)
                  .map(y => if (y._1 != y._2) 1 else 0)
                  .sum
              ).sum
          val numberOfErrorBitsCoded = config.inputVector
            .zip(decodedVector)
            .map((x: (Char, Char)) => if (x._1 != x._2) 1 else 0).sum

          // Transmit the vector and display it
          val transmittedUncodedVector = channel.transmit(List(vector), seed)
          val receivedVector = new String(transmittedUncodedVector.head.map(x => if (x == 1) '1' else '0').toArray)
          val numberOfErrorsNoncode = List(vector).zip(transmittedUncodedVector).map(x =>
                x._1.zip(x._2)
                  .map(y => if (y._1 != y._2) 1 else 0)
                  .sum
              ).sum
          val numberOfErrorBitsNonCoded = config.inputVector
            .zip(receivedVector)
            .map((x: (Char, Char)) => if (x._1 != x._2) 1 else 0).sum

          val canRestoreErrors = (scala.math.pow(2, config.m - config.r) - 1) / 2

          println("Data vector                                  " + config.inputVector)
          println("Encoded vector, before transmission:         " + encodedVectorString)
          println("Vector received after transmission:          " + transmittedCodedVectorBits)
          println("Error vector:                                " + errorVectorString + " (" + numberOfErrorsCode + " transmission errors, " + canRestoreErrors + " errors can be restored), error positions: " + errorPositions.mkString(", "))
          println("Vector after decoding      :                 " + decodedVector + " (" + numberOfErrorBitsCoded + " incorrect bits)")
          println("Vector received without encoding:            " + receivedVector + " (" + numberOfErrorsNoncode + " transmission errors, " + numberOfErrorBitsNonCoded + " incorrect bits)")

        } else if (config.inputText != null) {

          // Encode, transmit, decode and display text
          val seed = (new scala.util.Random).nextInt()
          val encodedText = code.encodeText(config.inputText)
          val transmittedCodedText = (channel.transmit(encodedText._1, seed), encodedText._2)
          val decodedCodedText = code.decodeText(transmittedCodedText)
          val numberOfErrorsCode = encodedText._1.zip(transmittedCodedText._1).map(x =>
                x._1.zip(x._2)
                  .map(y => if (y._1 != y._2) 1 else 0)
                  .sum
              ).sum

          // Transform text to vectors, transmit, turn vectors back to text and display it
          val textVectors = code.convertBytesToVectors(Utils.textToBytes(config.inputText))
          val transmittedUncodedText = (channel.transmit(textVectors._1, seed), textVectors._2)
          val receivedText = Utils.bytesToText(code.convertVectorsToBytes(transmittedUncodedText))
          val numberOfErrorsNoncode = textVectors._1.zip(transmittedUncodedText._1).map(x =>
                x._1.zip(x._2)
                  .map(y => if (y._1 != y._2) 1 else 0)
                  .sum
              ).sum

          println("Data text:                                " + config.inputText)
          println("Received transmitted text with coding:    " + decodedCodedText + " (" + numberOfErrorsCode + " errors in transmission)")
          println("Received transmitted text without coding: " + receivedText + " (" + numberOfErrorsNoncode + " errors in transmission)")
        } else {
          println("Bad arguments. Please try again with '--help', if you want to see some examples for running this")
        }
      } else {
        println("Condition m > r > 0 must be satisfied")
      }
    } getOrElse {
      println("Bad arguments. Please try again with '--help', if you want to see some examples for running this")
    }
  } catch {
    case e : Exception => println(e.getMessage)
  }

}
