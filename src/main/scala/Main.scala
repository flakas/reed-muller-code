import java.io.File
import rmcode._

import com.sksamuel.scrimage.Format

case class Config(m: Int = 3, r: Int = 9, errorRate: Double = 0, verbose: Boolean = false, debug: Boolean = false, inputText: String = null, inputFile: File = null, outputFileCoded: File = null, outputFileUncoded: File = null, inputVector: String = null, decodeVector: String = null)

object Main extends App {

  val parser = new scopt.OptionParser[Config]("RMCode") {
    head("RMCode", "1.0")
    opt[Int]('m', "m") required() action { (x, c) =>
      c.copy(m = x) } text("m parameter for Reed-Muller code")

    opt[Int]('r', "r") required() action { (x, c) =>
      c.copy(r = x) } text("r parameter for Reed-Muller Code")

    opt[Double]('p', "errorRate") required() action { (x, c) =>
      c.copy(errorRate = x) } text("errorRate is the error rate parameter of the transmission channel")

    opt[String]('v', "inputVector") action { (x, c) =>
      c.copy(inputVector = x) } text("inputVector is vector to be transmitted via the channel")

    opt[String]('d', "decodeVector") action { (x, c) =>
      c.copy(decodeVector = x) } text("decodeVector is an already transmitted vector, to be decoded with the RM code")

    opt[String]('i', "inputText") action { (x, c) =>
      c.copy(inputText = x) } text("inputText is text to be transmitted via the channel")

    opt[File]('i', "inputFile") valueName("<file>") action { (x, c) =>
      c.copy(inputFile = x) } text("inputFile is a file to be transmitted via the channel")
    opt[File]('o', "outputFileCoded") valueName("<file>") action { (x, c) =>
      c.copy(outputFileCoded = x) } text("outputFileCoded is a location where the coded transmitted file should be written to")
    opt[File]('o', "outputFileUncoded") valueName("<file>") action { (x, c) =>
      c.copy(outputFileUncoded = x) } text("outputFileUncoded is a location where the coded transmitted file should be written to")

    opt[Unit]("verbose") action { (_, c) =>
      c.copy(verbose = true) } text("verbose is a flag")
    opt[Unit]("debug") hidden() action { (_, c) =>
      c.copy(debug = true) } text("this option is hidden in the usage text")

    note("some notes.\n")
    help("help") text("prints this usage text")
  }
  try {
    // parser.parse returns Option[C]
    parser.parse(args, Config()) map { config =>
      val channel = new SymmetricChannel(config.errorRate, 2)
      val code = new RMCode(config.m, config.r)
      if (config.inputFile != null && config.outputFileCoded != null && config.outputFileUncoded != null) {
        // Send a file
        val encodedImage = code.encodeImage(config.inputFile)
        val transmittedImage = (encodedImage._1, encodedImage._2, (channel.transmit(encodedImage._3._1), encodedImage._3._2))
        val decodedImage = code.decodeImage(transmittedImage)
        decodedImage.writer(Format.JPEG).write(config.outputFileCoded)
        println("Output from coded transmission has been saved to ", config.outputFileCoded)
      } else if (config.decodeVector != null) {
        // Decode a user-defined vector
        val vector = config.decodeVector.map(x => if (x == '1') 1 else 0).toVector
        val decodedCodedVector = code.decode(List(vector)).head
        val decodedVector = new String(decodedCodedVector.map(x => if (x == 1) '1' else '0').toArray)

        println("Vector to be decoded: " + config.decodeVector)
        println("Vector decoded to:    " + decodedVector)
      } else if (config.inputVector != null) {
        // Send a vector
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

        println("Vector data:                                    " + config.inputVector)
        println("Vector encoded, before transmission:            " + encodedVectorString)
        println("Vector received after transmission with coding: " + transmittedCodedVectorBits)
        println("Error vector:                                   " + errorVectorString + " (" + numberOfErrorsCode + " errors in transmission, can restore " + canRestoreErrors + " errors), error positions: " + errorPositions.mkString(", "))
        println("Vector received with coding:                    " + decodedVector + " (" + numberOfErrorBitsCoded + " incorrect bits)")
        println("Vector received without coding:                 " + receivedVector + " (" + numberOfErrorsNoncode + " errors in transmission, " + numberOfErrorBitsNonCoded + " incorrect bits)")
      } else if (config.inputText != null) {
        // Send text
        val seed = (new scala.util.Random).nextInt()
        val encodedText = code.encodeText(config.inputText)
        val transmittedCodedText = (channel.transmit(encodedText._1, seed), encodedText._2)
        val decodedCodedText = code.decodeText(transmittedCodedText)
        val numberOfErrorsCode = encodedText._1.zip(transmittedCodedText._1).map(x =>
              x._1.zip(x._2)
                .map(y => if (y._1 != y._2) 1 else 0)
                .sum
            ).sum

        val textVectors = code.convertBytesToVectors(Utils.textToBytes(config.inputText))
        val transmittedUncodedText = (channel.transmit(textVectors._1, seed), textVectors._2)
        val receivedText = Utils.bytesToText(code.convertVectorsToBytes(transmittedUncodedText))
        val numberOfErrorsNoncode = textVectors._1.zip(transmittedUncodedText._1).map(x =>
              x._1.zip(x._2)
                .map(y => if (y._1 != y._2) 1 else 0)
                .sum
            ).sum

        println("Text sent:                    " + config.inputText)
        println("Text received with coding:    " + decodedCodedText + " (" + numberOfErrorsCode + " errors in transmission)")
        println("Text received without coding: " + receivedText + " (" + numberOfErrorsNoncode + " errors in transmission)")
      } else {
        println("Bad arguments. Re-run this with '--help' to see correct invocation")
      }
    } getOrElse {
      println("Bad arguments. Re-run this with '--help' to see correct invocation")
    }
  } catch {
    case e : Exception => println(e.getMessage)
  }

}
