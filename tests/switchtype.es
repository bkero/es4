a=1;
b=2;
c="hello";
switch type ( a,b,c:Object )
{
   case ( y : intrinsic::int ) { intrinsic::assert(false); }
   case ( z : String ) { intrinsic::assert(z===c); }
   default { intrinsic::assert(false); }
}
