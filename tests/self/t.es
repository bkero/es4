/*
{
   use namespace Decode
   var decoded = program (ast)
}

{
   use namespace Encode
   var encoded = program (decoded)
}

print (encoded)

*/

/*
var o = {y:10}
print (o.y)
({x:o.y} = {x:20})
print (o.y)
*/

/*
var x = {};
[x.y] = [10]
print (x.y)
*/

class A { 
  var x = 10
  function A (x) 
    : x = x 
    {}
  function m () {
    return this.x
  }
}
print(new A(20).m())
