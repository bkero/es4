use namespace intrinsic;

function fibo(n:int) : int {
	if (n < 2) {
		return n;
	} else {
		var a : int = fibo(n-1);
		var b : int = fibo(n-2);
		return a + b;
	}
}

intrinsic::assert(fibo(8) == 21);