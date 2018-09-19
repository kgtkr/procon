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
    val list=input.split("\n").drop(1).map(_.split(" ").map(_.toInt)).map{case Array(t,x,y)=>(t,x,y)}.toList;
    if(((0,0,0)::list).zip(list).forall{case ((t,x,y),(nextT,nextX,nextY))=>{
      val dt = nextT - t;
      val dist = (nextX-x).abs + (nextY-y).abs;
      dt >= dist&&dist % 2 == dt % 2
    }}){
      "Yes"
    }else{
      "No"
    }
  }
 
  val tests=List("""2
3 1 2
6 1 1""" -> """Yes""",
    """1
2 100 100""" -> """No""",
"""2
5 1 1
100 1 1"""->"""No""");
 
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