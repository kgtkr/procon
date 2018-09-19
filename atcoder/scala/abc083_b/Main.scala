object Main{
  def main(args: Array[String]){
    if (sys.env.getOrElse("TEST", "")=="1"){
      println(test());
    }else{
      val input=io.Source.stdin.getLines().mkString("\n");
      println(solve(input).trim());
    }
  }
 
  def solve(input:String):String={
    val List(n,a,b)=input.split(" ").map(_.toInt).toList;
    (1 to n).filter(x=>bitween(x.toString().split("").filter(_.length!=0).map(_.toInt).sum,a,b)).sum.toString()
  }
 
  def bitween(x:Int,min:Int,max:Int)=min<=x&&x<=max;
 
  val tests=List("""20 2 5""" -> """84""",
    """10 1 2""" -> """13""",
"""100 4 16"""->"""4554""");
 
  def test():String= {
    return tests.map{case (i,o)=>(i.trim(),o.trim())}.zipWithIndex.map{case ((input,outputExpect),i)=>{
      val output=solve(input).trim();
      s"test${i+1}:"+(if(output==outputExpect){
        "Passed"
      }else{
        s"Failed\nexpect:\n${outputExpect}\noutput:\n${output}"
      })
    }}.mkString("\n");
  }
}