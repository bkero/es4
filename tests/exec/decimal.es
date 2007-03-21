use decimal;

var x = 1.5;
var y = 1.5;
var z = 3;

intrinsic::assert(x + y == z);
intrinsic::assert(x + 1.5 == z);
intrinsic::assert(z / 2 == x);
