use namespace intrinsic
class Object {}
class A 
{
	prototype var [x,[y,[z]]] = [1,[2,[3]]]
}
class B extends A {}
let b = new B
print(b.x," ",b.y," ",b.z,"\n")
A.prototype.x=10
delete A.prototype.y
print(b.x," ",b.y," ",b.z,"\n")
