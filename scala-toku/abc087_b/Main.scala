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
    val List(a,b,c,x)=input.split("\n").map(_.toInt).toList;
    val ay=(0 to a).map(_*500);
    val by=(0 to b).map(_*100);
    val cy=(0 to c).map(_*50);
    ay.flatMap(i=>by.flatMap(j=>cy.map(k=>i+j+k))).filter(_==x).size.toString()
  }

  val tests=List("""2
2
2
100""" -> """2""",
    """5
1
0
150""" -> """0""",
"""30
40
50
6000"""->"""213""");

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