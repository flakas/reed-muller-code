import java.io.File
import rmcode._

case class Config(m: Int = 1, r: Int = 1, errorRate: Double = 0.9, verbose: Boolean = false, debug: Boolean = false, inputText: String = "", inputFile: File = null, outputFile: File = null)

object Main extends App {
  val parser = new scopt.OptionParser[Config]("RMCode") {
    head("RMCode", "1.0")
    opt[Int]('m', "m") required() action { (x, c) =>
      c.copy(m = x) } text("m parameter for Reed-Muller code")

    opt[Int]('r', "r") required() action { (x, c) =>
      c.copy(r = x) } text("r parameter for Reed-Muller Code")

    opt[Double]('p', "errorRate") required() action { (x, c) =>
      c.copy(errorRate = x) } text("errorRate is the error rate parameter of the transmission channel")

    opt[String]('i', "inputText") action { (x, c) =>
      c.copy(inputText = x) } text("inputText is text to be transmitted via the channel")

    opt[File]('i', "inputFile") valueName("<file>") action { (x, c) =>
      c.copy(inputFile = x) } text("inputFile is a file to be transmitted via the channel")
    opt[File]('o', "outputFile") valueName("<file>") action { (x, c) =>
      c.copy(outputFile = x) } text("outputFile is a location where the transmitted file should be written to")

    opt[Unit]("verbose") action { (_, c) =>
      c.copy(verbose = true) } text("verbose is a flag")
    opt[Unit]("debug") hidden() action { (_, c) =>
      c.copy(debug = true) } text("this option is hidden in the usage text")

    note("some notes.\n")
    help("help") text("prints this usage text")
  }
  // parser.parse returns Option[C]
  parser.parse(args, Config()) map { config =>
    val channel = new SymmetricChannel(new RMCode(config.m, config.r), config.errorRate)
    if (config.inputFile != null && config.outputFile != null) {
      val received = channel.transmitFile(config.inputFile)
      println("Output file has been saved to ", config.outputFile)
    } else {
      val received = channel.transmitText(config.inputText)
      println(received)
    }
  } getOrElse {
    println("Bad arguments. Re-run this with '--help' to see correct invocation")
  }
}
