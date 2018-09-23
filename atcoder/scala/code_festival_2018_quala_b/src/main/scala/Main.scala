import scala.io.StdIn._

object Main extends App {
  val Array(n,m,a,b)=readLine().split(" ").map(_.toInt)
  val list=(0 until m).map(_=>readLine().split(" ").map(_.toInt)).map{case Array(l,r)=>(l,r)}
  val aCount=list.flatMap{case (a,b)=>(a to b)}.toSet.size
  val bCount=n-aCount
  println(aCount*a+bCount*b)
}