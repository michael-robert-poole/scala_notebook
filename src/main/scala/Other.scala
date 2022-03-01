object Other extends App {

  //Voting algorithm first past the post system
  // - create unique list not using sets
  // -count occurances
  // -build results (tuple)
  //sort results by occurances

  // val votes:Seq[Int] = Seq(1,2,2,2,3,3,3,3,3,3,4,5, 5)
  val votes:List[String] = List("Green", "Red", "Green", "Green", "Blue", "Green", "Red", "Yellow","Yellow", "Yellow")

  def count[A](ls:Seq[A])(f: A => Boolean) (acc:Int):Int = if (ls.length == 0) acc else if (f(ls.head)) count(ls.tail)(f)(acc +1) else count(ls.tail)(f)(acc)

  def rmdups[A](ls:Seq[A], acc:Seq[A]= Seq.empty):Seq[A] = ls match {
    case Nil => acc
    case x :: xs => if(xs.contains(x)) rmdups(xs,acc) else rmdups(xs,acc :+x)
  }
  def result[A](ls:Seq[A], acc:Seq[A]=Seq.empty):Seq[(Int, A)] = rmdups(ls, acc).map(t => (count(ls)(a => a.equals(t))(0), t))

  def sort[A](ls: Seq[(Int, A)]): Seq[(Int, A)] = {
    def innerSort(elem:(Int, A), accI:Seq[(Int,A)]): Seq[(Int, A)] = {
      if(accI.isEmpty || elem._1 < accI.head._1) elem +: accI
      else accI.head +: innerSort(elem, accI.tail)
    }
    ls.foldLeft(Seq.empty[(Int,A)]) ((x,y) => innerSort(y, x))
  }

  println(count(votes)(a => a.equals("Red"))(0))
  println(rmdups(votes))
  println(result(votes))
  println(sort(result(votes)))
  println(sort(result(votes)).reverse.head._2)
}
