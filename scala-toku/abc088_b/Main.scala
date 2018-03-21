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
    val (a,b)=input.split("\n")(1).split(" ").map(_.toInt).sorted.reverse.zipWithIndex.foldRight((0,0)){case ((x,i),(an,bo)) => if(i%2==0){(an+x,bo)}else{(an,bo+x)}};
    (a-b).toString()
  }

  def bitween(x:Int,min:Int,max:Int)=min<=x&&x<=max;

  val tests=List("""2
3 1""" -> """2""",
    """3
2 7 4""" -> """5""",
"""4
20 18 2 18
"""->"""18""");

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