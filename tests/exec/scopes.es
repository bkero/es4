
var c;

function foo() 
{
	intrinsic::assert(false);
}

class C 
{
    var x
    function C(x) : x=x {
        intrinsic::assert(this.x == x)
    }

	function foo() 
	{
		intrinsic::assert(this === c);
		intrinsic::assert(this !== global);
	}

	function bar()
	{	
		intrinsic::assert(this === c);
		intrinsic::assert(this !== global);
		foo();
		this.foo();
	}

	function outer() {

		function inner1() {
			intrinsic::assert(this !== c);
			intrinsic::assert(this === global);
		}

		function inner2() {
			intrinsic::assert(this !== c);
			intrinsic::assert(this === global);
			inner1();
		}

		intrinsic::assert(this === c);
		intrinsic::assert(this !== global);
		return inner2;
	}

	prototype function proto()
	{
		this.foo();
	}
}


c = new C(10);
c.bar();
var y = c.outer();
y();

