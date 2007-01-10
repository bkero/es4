
function withAsterisks(y) {
	return "*** " + y + " ***";
}

function printWithLine(x) {
	intrinsic::print(withAsterisks(x));
	intrinsic::print("\n");
}

class bar {
}

class foo extends bar {
}

var x = 10;

while (x != 20) {
	printWithLine(x)
	x += 1;
}
