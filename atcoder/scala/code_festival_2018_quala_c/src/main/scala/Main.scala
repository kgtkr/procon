import scala.io.StdIn._

object Main extends App {
  val Array(n,k1)=readLine().split(" ").map(_.toLong)
  val list=readLine().split(" ").map(_.toLong)
  //パターン数
  //パターン数-1回割ると0になる
  val patternCount=list.map(x=>(0l until 100).scan(x)((x, _) => x/2).takeWhile(_!=0).size.toLong+1).sorted.toArray

  //kが(パターン数-1).sumより大きくても意味ないので
  val k=k1 min patternCount.map(_-1).sum

  //現在の操作回数をcountとしたときのi番目以降のパターン数
  def f(i:Long,count:Long)=Long{
    val remain=k-count

    (0 to remain).map(x=>f(i+1,count+x))
    1l
  }
}