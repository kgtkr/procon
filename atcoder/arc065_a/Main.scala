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
    if(f(input.toList.reverse)){
      "YES"
    }else{
      "NO"
    }
  }

  def f(s:List[Char]):Boolean={
    s match{
      case List('m','a','e','r','d',x @ _*)=>f(x.toList)
      case List('r','e','m','a','e','r','d',x @ _*)=>f(x.toList)
      case List('e','s','a','r','e',x @ _*)=>f(x.toList)
      case List('r','e','s','a','r','e',x @ _*)=>f(x.toList)
      case List()=>true
      case _=>false
    }
  }
 
  val tests=List("""erasedream""" -> """YES""",
    """dreameraser""" -> """YES""",
"""dreamerer"""->"""NO""");
 
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