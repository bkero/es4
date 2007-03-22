

x = 10;
y = 0;

while ( x > 0 ) {
	y++;
	x--;
}

intrinsic::assert(x == 0);
intrinsic::assert(y == 10);