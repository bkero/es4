/*
class Object {}
class A {
var [x,y] = 10
   // $t = 10
   // x = $t[0]
   // y = $t[1]
   //   [InitStmt {[$t=10,x=$t[0],y=$t[1]]}}
}
new A
*/

use namespace intrinsic
function f() { return 30 }
function g() { return 20 }
function h() { return 10 }
class Object { }
class A { var x=f(); function A(x=g):x=x() { print("making A\n") } }
var a = new A(h)
print(a.x,"\n")
a = new A()
print(a.x,"\n")


/*
class Object {}
class foo {
function foo([a,b],[c,d]=bar()) : z = zug() {}
}
*/



/*
type ft = function .<A,B> (A?,B,boolean)
var f : ft.<int,string> = function (i:int?,s:string,b:boolean) { }
f(null,"hi",true)
*/
/*
namespace ns = "http"
class Object {}
ns class A {
  static var x = 10
  var y = x
  prototype var z = x
}
class B extends ns::A {}

var o = new ns::A
intrinsic::print(o.y,"\n")
intrinsic::print(ns::A.prototype.z,"\n")
intrinsic::print(o.z,"\n")

*/
//Object.prototype.u = 40
//intrinsic::print(Object.y)
//intrinsic::print(Object.prototype.u)



/*
let [x,[y],[z]]:[int,[int],[int]] = o
let {i:x,j:{k:y,l:z}}:{i:I,j:{k:K,l:L}} = o
var {a:i,b:j}:{a:int,b2:string} = o
[x,[y],[z]] = o
({i:x,j:{k:y,l:z}} = o)
var f:function (_:[]):* = function .<t,u>(i,j,k) {var x=10; let y=20; print('foo')}
*/


/*
class B extends A
{
  var x = 40
  override function f.<t>(a,b,c):Number { return "B.f" }
}
*/

/*
class A
{
  var x = 10
  static var y = 20
  let z = 30

  function A([a,b,c]=o):x=10,y=20 {}
  function m () {}
  static function n() {}

}
*/


/*
{
	use namespace foo
	namespace bar = foo
}
*/

/*

function withAsterisks(y) {
	return "*** " + y + " ***";
}

function printWithLine(x) {
	intrinsic::print(withAsterisks(x));
	intrinsic::print("\n");
}

class bar {
	var p;
}

namespace magic;

class foo extends bar {	
	prototype var k = 22;
	function foo(x) : p = x { magic::q = p; }
	magic var q;
}

var x = 10;

var y = new foo(10);

printWithLine(y.k);

while (x != 20) {
	printWithLine(x)
	x += 1;
}

*/