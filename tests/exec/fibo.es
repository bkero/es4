use namespace intrinsic;

function fibo(n : double) : double {
	if (n < 2) {
		return n;
	} else {
		var a = fibo(n-1);
		var b = fibo(n-2);
		return a + b;
	}
}

assert(fibo(8) == 21);
print ("PASSED!");