import scala.io.StdIn._

object Main extends App {
  val Array(n,k)=readLine().split(" ").map(_.toLong)
  val list=readLine().split(" ").map(_.toLong)
  //パターン数
  //パターン数-1回割ると0になる
  val patternCount=list.map(x=>(0l until 100).scan(x)((x, _) => x/2).takeWhile(_!=0).size.toLong+1).sorted.toList

  //kが(パターン数-1).sumより大きくても意味ないので
  val k2=k min patternCount.map(_-1).sum
}