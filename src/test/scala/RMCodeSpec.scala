package rmcode

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Arbitrary._

class RMCodeSpec extends FlatSpec with Matchers with Checkers {
    "A Function" should "correctly calculate output with valid input" in {
      5 should be (5)
    }
}
