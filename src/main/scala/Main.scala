import java.io.File
import rmcode._

import com.sksamuel.scrimage._

// Vartotojo interfeiso konfigūracija
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

  // Komandinės eilutės parametrų "parsinimas"
  val parser = new scopt.OptionParser[Config]("RMCode") {
    head("RMCode", "1.0")
    opt[Int]('m', "m") required() action { (x, c) =>
      c.copy(m = x) } text("m parametras Rydo-Miulerio kodui") validate { x => if (x > 0) success else failure("Parametras m turi būti > 0") }

    opt[Int]('r', "r") required() action { (x, c) =>
      c.copy(r = x) } text("r parametras Rydo-Miulerio kodui") validate { x => if (x > 0) success else failure("Parametras r turi būti > 0") }

    opt[Double]('p', "errorRate") required() action { (x, c) =>
      c.copy(errorRate = x) } text("errorRate yra klaidos tikimybė perdavimo kanale") validate { x => if (x >= 0 && x <= 1) success else failure("Parametras errorRate turi būti 0<=errorRate<=1") }

    opt[String]('v', "inputVector") action { (x, c) =>
      c.copy(inputVector = x) } text("inputVector yra įvedamas duomenų vektorius, persiuntimui kanalu")

    opt[String]('d', "decodeVector") action { (x, c) =>
      c.copy(decodeVector = x) } text("decodeVector yra RM kodo vektorius, kuris bus dekoduojamas naudojant RM kodą, skirtas pridėti kiek reikia klaidų")

    opt[String]('i', "inputText") action { (x, c) =>
      c.copy(inputText = x) } text("inputText yra įvesties tekstas, kuris bus siunčiamas kanalu")

    opt[File]('i', "inputFile") valueName("<file>") action { (x, c) =>
      c.copy(inputFile = x) } text("inputFile yra duomenų failo kelias (JPEG formato paveiksliukas), kuris bus persiunčiamas kanalu")
    opt[File]('o', "outputFileCoded") valueName("<file>") action { (x, c) =>
      c.copy(outputFileCoded = x) } text("outputFileCoded yra koduojant kanalu persiųsto paveiksliuko išsaugojimo vieta")
    opt[File]('c', "outputFileUncoded") valueName("<file>") action { (x, c) =>
      c.copy(outputFileUncoded = x) } text("outputFileUncoded yra nekoduojant kanalu persiųsto paveiksliuko išsaugojimo vieta")

    help("help") text("Išspausdina šį pagalbos pranešimą")
    note("""Naudojimo pavyzdžiai:
=====================

# Norint persiųsti vektorių:
  Privaloma nurodyti parametrus: m, r, errorRate, inputVector
  Komandinės eilutės pvz.:
    java -jar RMCode.jar --m <parametras m> --r <parametras r> --errorRate <parametras p> --inputVector "<persiunčiamas vektorius>"
  Iškvietimo pvz.:
    java -jar RMCode.jar --m 4 --r 2 --errorRate 0.2 --inputVector "10101010101"
  Rezultatas:
    Parodomas įvestas, užkoduotas nepersiųstas, užkoduotas persiųstas, klaidos, dekoduotas ir siųstas nekoduotas vektoriai

# Norint tik dekoduoti vektorių:
  Privaloma nurodyti parametrus: m, r, errorRate, decodeVector
  Komandinės eilutės pvz.:
    java -jar RMCode.jar --m <parametras m> --r <parametras r> --errorRate <parametras p> --decodeVector "<dekoduojamas RM(m, r) kodo žodis>"
  Iškvietimo pvz.:
    java -jar RMCode.jar --m 4 --r 2 --errorRate 0.2 --decodeVector "0010000111101001"
  Rezultatas:
    Parodomas dekoduotas vektorius decodeVector. Ši funkcija skirta norint rankiniu būdu įterpti klaidas prieš dekodavimą.

# Norint persiųsti tekstą:
  Privaloma nurodyti parametrus: m, r, errorRate, inputText
  Komandinės eilutės pvz.:
    java -jar RMCode.jar --m <parametras m> --r <parametras r> --errorRate <parametras p> --inputText "<persiunčiamas tekstas>"
  Iškvietimo pvz.:
    java -jar RMCode.jar --m 4 --r 2 --errorRate 0.05 --inputText "abcdefghijklmnopqrstuvwxyz"
  Rezultatas:
    Parodomas tekstas, gautas persiunčiant koduotą tekstą ir nekoduotą tekstą.

# Norint persiųsti paveiksliuką:
  Privaloma nurodyti parametrus: m, r, errorRate, inputFile, outputFileCoded, outputFileUncoded
  Komandinės eilutės pvz.:
    java -jar RMCode.jar --m <parametras m> --r <parametras r> --errorRate <parametras p> --inputFile <duomenų JPEG formato paveiksliuko adresas> --outputFileCoded <koduoto JPEG formato paveiksliuko išvesties adresas> --outputFileUncoded <nekoduoto JPEG formato paveiksliuko išvesties adresas>
  Iškvietimo pvz.:
    java -jar RMCode.jar --m 3 --r 1 --errorRate 0.05 --inputFile source.jpeg --outputFileCoded coded.jpeg --outputFileUncoded uncoded.jpeg
  Rezultatas:
    Išsaugomi persiųsto paveiksliuko inputFile rezultatai: koduojant paveiksliukas išsaugomas keliu outputFileCoded, nekoduojant paveiksliukas išsaugomas keliu outputFileUncoded""")
  }

  try {
    // parser.parse returns Option[C]
    parser.parse(args, Config()) map { config =>
      if (config.m > config.r) {
        val channel = new SymmetricChannel(config.errorRate, 2)
        val code = new RMCode(config.m, config.r)
        if (config.inputFile != null && config.outputFileCoded != null && config.outputFileUncoded != null) {
          // Send a file

          println("Apdorojama koduojama paveiksliuko versija...")
          // Koduojam, siunčiam, dekoduojam ir išsaugom paveiksliuką
          val encodedImage = code.encodeImage(config.inputFile)
          val transmittedImage = (encodedImage._1, encodedImage._2, (channel.transmit(encodedImage._3._1), encodedImage._3._2))
          val decodedImage = code.decodeImage(transmittedImage)
          decodedImage.writer(Format.JPEG).write(config.outputFileCoded)

          println("Apdorojama nekoduojama paveiksliuko versija...")
          // Skaidom baitus į vektorius, siunčiam, išsaugom paveiksliuką
          val uncodedImage = Image(config.inputFile)
          val uncodedImageVectors = code.convertBytesToVectors(Utils.imageToBytes(uncodedImage))
          val transmittedUncodedImage = Utils.bytesToImage(uncodedImage.width, uncodedImage.height, code.convertVectorsToBytes((channel.transmit(uncodedImageVectors._1), uncodedImageVectors._2)))
          transmittedUncodedImage.writer(Format.JPEG).write(config.outputFileUncoded)
          println("Koduojamo persiuntimo rezultatas buvo išsaugotas į " +  config.outputFileCoded)
          println("Nekoduojamo persiuntimo rezultatas buvo išsaugotas į " +  config.outputFileUncoded)

        } else if (config.decodeVector != null) {

          // Dekoduojam nurodytą vektorių ir jį parodom
          val vector = config.decodeVector.map(x => if (x == '1') 1 else 0).toVector
          val decodedCodedVector = code.decode(List(vector)).head
          val decodedVector = new String(decodedCodedVector.map(x => if (x == 1) '1' else '0').toArray)

          println("Dekoduojamas vektorius: " + config.decodeVector)
          println("Dekoduotas vektorius:   " + decodedVector)

        } else if (config.inputVector != null) {

          // Koduojam, siunčiam, dekoduojam ir parodom vektorių
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

          // Vektorių siunčiam ir parodom be kodavimo
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

          println("Duomenų vektorius                            " + config.inputVector)
          println("Užkoduotas vektorius, prieš persiuntimą:     " + encodedVectorString)
          println("Gautas užkoduotas vektorius, po persiuntimo: " + transmittedCodedVectorBits)
          println("Klaidų vektorius:                            " + errorVectorString + " (" + numberOfErrorsCode + " klaidų persiuntime, galima atstatyti " + canRestoreErrors + " klaidų), klaidų pozicijos: " + errorPositions.mkString(", "))
          println("Vektorius, gautas dekodavus:                 " + decodedVector + " (" + numberOfErrorBitsCoded + " neteisingi bitai)")
          println("Vektorius, gautas persiuntus nekoduojant:    " + receivedVector + " (" + numberOfErrorsNoncode + " klaidų persiuntime, " + numberOfErrorBitsNonCoded + " neteisingi bitai)")

        } else if (config.inputText != null) {

          // Tekstą koduojam, siunčiam, dekoduojam ir parodom
          val seed = (new scala.util.Random).nextInt()
          val encodedText = code.encodeText(config.inputText)
          val transmittedCodedText = (channel.transmit(encodedText._1, seed), encodedText._2)
          val decodedCodedText = code.decodeText(transmittedCodedText)
          val numberOfErrorsCode = encodedText._1.zip(transmittedCodedText._1).map(x =>
                x._1.zip(x._2)
                  .map(y => if (y._1 != y._2) 1 else 0)
                  .sum
              ).sum

          // Tekstą transformuojam į vektorius, siunčiam, verčiam atgal į tekstą ir parodom
          val textVectors = code.convertBytesToVectors(Utils.textToBytes(config.inputText))
          val transmittedUncodedText = (channel.transmit(textVectors._1, seed), textVectors._2)
          val receivedText = Utils.bytesToText(code.convertVectorsToBytes(transmittedUncodedText))
          val numberOfErrorsNoncode = textVectors._1.zip(transmittedUncodedText._1).map(x =>
                x._1.zip(x._2)
                  .map(y => if (y._1 != y._2) 1 else 0)
                  .sum
              ).sum

          println("Duomenų tekstas:                " + config.inputText)
          println("Persiųstas dekoduotas tekstas:  " + decodedCodedText + " (" + numberOfErrorsCode + " klaidų perdavime)")
          println("Persiųstas tekstas be kodavimo: " + receivedText + " (" + numberOfErrorsNoncode + " klaidų perdavime)")
        } else {
          println("Blogi argumentai. Paleiskite dar kartą su '--help', jei norite pamatyti teisingo iškvietimo pavyzdžių")
        }
      } else {
        println("Turi būti patenkinta sąlyga m > r > 0")
      }
    } getOrElse {
      println("Blogi argumentai. Paleiskite dar kartą su '--help', jei norite pamatyti teisingo iškvietimo pavyzdžių")
    }
  } catch {
    case e : Exception => println(e.getMessage)
  }

}
