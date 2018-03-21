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
    input.split("").map(_.toInt).sum.toString()
  }

  val tests=List("""101""" -> """2""",
    """000""" -> """0""");

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