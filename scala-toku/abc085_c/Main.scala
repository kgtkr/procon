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
    val List(n,y)=input.split(" ").map(_.toInt).toList;
    for (a <- 0 to n){
      for (b <- 0 to n-a){
        val c=n-a-b;
        if(a*10000+b*5000+c*1000==y){
          return s"${a} ${b} ${c}";
        }
      }
    }

    "-1 -1 -1"
  }
 
  val tests=List("""9 45000""" -> """0 9 0""",
    """20 196000""" -> """-1 -1 -1""",
"""1000 1234000"""->"""14 27 959""",
"""2000 20000000"""->"""2000 0 0""");
 
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