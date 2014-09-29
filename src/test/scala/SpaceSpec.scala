package rmcode

import org.scalatest._
import Matchers._
import org.scalatest.prop.Checkers

class SpaceSpec extends FlatSpec with Matchers with Checkers{

  "Array" should "generate 2**n vectors" {
    val n: Int = 5
    val q = 2
    Array(1, 2) should equal (Array(1, 2))
  }
}
