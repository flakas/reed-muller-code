package rmcode

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

}
