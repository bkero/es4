/*
let [x,[y],[z]]:[int,[int],[int]] = o
let {i:x,j:{k:y,l:z}}:{i:I,j:{k:K,l:L}} = o
var {a:i,b:j}:{a:int,b2:string} = o
[x,[y],[z]] = o
({i:x,j:{k:y,l:z}} = o)
var f:function (_:[]):* = function .<t,u>(i,j,k) {var x=10; let y=20; print('foo')}
*/

class f 
{
function f([a,b,c]=o):x=10,y=20 {}
}

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