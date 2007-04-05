package mx.util
{
  public class Vector { intrinsic::print("new Vector") } 
  public var x = {y:10}
}

var mx

{
  import mx.util.*
  var v : mx.util.Vector = new mx.util.Vector
  intrinsic::print(mx.util.x.y)
}

