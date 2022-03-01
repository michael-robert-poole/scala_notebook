object ProjectEuler extends App{
  //https://projecteuler.net/problem=15
  def lattice(h: Int, w: Int):BigDecimal = {
    val a = h+w
    val b = fact(a)
    val c  = fact(h) * fact(a-h)
    b/c
  }
  println(lattice(20, 20))

  def fact(n: BigDecimal, acc: BigDecimal=1): BigDecimal =
    if (n < 2) {
      acc
    } else {
      val tAcc: BigDecimal = n * acc
      fact(n - 1, tAcc)
    }
  println(fact(40, 1))



  //https://projecteuler.net/problem=1
  //sum of multiples of 3 and 5 between 1 and 999(inclusive)
  val d = (1 to 999).filter(x => x % 3 == 0 || x % 5 == 0).sum
  println(d)


}
