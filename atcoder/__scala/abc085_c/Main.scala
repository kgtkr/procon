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
    f1(n,y,0) match{
      case Some((a,b,c))=>s"${a} ${b} ${c}"
      case None=>"-1 -1 -1"
    }
  }

  def f1(n:Int,y:Int,a:Int):Option[(Int,Int,Int)]={
    if(a<=n){
      f2(n,y,a,0) match{
        case Some(v)=>Some(v)
        case None=>f1(n,y,a+1)
      }
    }else{
      None
    }
  }

  def f2(n:Int,y:Int,a:Int,b:Int):Option[(Int,Int,Int)]={
    if(b<=n-a){
      val c=n-a-b;
      if(a*10000+b*5000+c*1000==y){
        Some((a,b,c))
      }else{
        f2(n,y,a,b+1)
      }
    }else{
      None
    }
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