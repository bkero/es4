use namespace intrinsic
namespace N
class A
{
  N var x : int = 10
  function m() {
    return N::x
  }
}
print((new A).m())
