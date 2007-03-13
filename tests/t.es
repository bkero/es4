use default namespace intrinsic
var x = 10
print(intrinsic::x,"\n")
public class Object {}
class A {
static var y = 20
var z = 30
}
print(A.intrinsic::y)
print(new A().intrinsic::z)
