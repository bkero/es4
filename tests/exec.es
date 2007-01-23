
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
