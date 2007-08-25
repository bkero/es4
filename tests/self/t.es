// array literal
print("Array literal");
var a = [10,20]
var y = a[0]
var z = a[1]
print(y,z)
print("");


print("Qualified Ident");
// qualified ident
namespace foo
class C
{
    foo var bar = 10
}
print(new C().foo::bar)
print("");

print("object literal");
// object literal
var o = {x:10,y:20}
print(o.x,o.y)
print("");

print("is op");
// is op
print (10 is int)
print("");

print("this expr");
// this expr
print (this)
print("");

// math ops
print("math ops");
print (1*1,2/2,4%3,0+1,6-5)
print("");

// comparison ops
print("comparison ops");
print (1<2,2>1,1<=1,1>=1,1==1,1===1,1!=2,1!==2);
print("");

// logical ops
print("logical ops");
print (true && true, false || true)
print("");

// unary ops
print("unary ops");
var x = 0
print (++x,--x,!false)
print("");
