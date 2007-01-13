
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
	function foo(x) : p = x { magic::q = p; }
	magic var q;
}

var x = 10;

var y = new foo(10);

while (x != 20) {
	printWithLine(x)
	x += 1;
}
