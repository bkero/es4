
function withAsterisks(y) {
	return "*** " + y + " ***";
}

function printWithLine(x) {
	intrinsic::print(withAsterisks(x));
	intrinsic::print("\n");
}

var x = 10;

while (x != 20) {
	printWithLine(x)
	x += 1;
}
