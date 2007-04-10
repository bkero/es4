package p 
{
	use namespace intrinsic
    public var x = 10
    public function f() { print('f') }
    public class A { function A() { print('A') } public static var y = 20 }
	public var y = 'oops'
}
use namespace intrinsic
import x2=p.x
import f2=p.f
import p.A
f2()
new A
print(A.y)
print(x2)
x2=20
print(x2)
print(p.y)