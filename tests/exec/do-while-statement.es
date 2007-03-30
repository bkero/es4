

x = 10;
y = 0;

do {
	y++;
	x--;
} while ( x > 0 )

intrinsic::assert(x == 0);
intrinsic::assert(y == 10);