object HackerRank extends App {
  //https://www.hackerrank.com/challenges/encryption/problem
  def encryption(s: String): String = {
    val sw = s.replace(" ", "")
    val scol = math.ceil(math.sqrt(sw.length)).toInt
    bString(sw,0,scol)("").trim
  }

  def bString (s:String, n:Int, index:Int)(acc:String): String = {
    if (n==index) acc else {
      val fls =  (0 to s.length-1).filter(idx => idx% index == 0 && idx < s.length-n).map(_+n)
      val temp = for (idx <- fls) yield s(idx)
      bString (s, n+1, index) (acc+temp.mkString("") + " ")
    }
  }



  println(encryption("feedthedog"))

}