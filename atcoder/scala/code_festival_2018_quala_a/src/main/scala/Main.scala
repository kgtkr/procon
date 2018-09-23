import scala.io.StdIn._

object Main extends App {
  val a=readLine().toInt
  val b=readLine().toInt
  val c=readLine().toInt
  val s=readLine().toInt
  val sum=a+b+c
  println(if(sum<=s&&s<=sum+3){
    "Yes"
  }else{
    "No"
  })
}