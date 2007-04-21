
var c;

function foo() 
{
	intrinsic::assert(false);
}

class C 
{
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
}


c = new C;
c.bar();
var y = c.outer();
y();

