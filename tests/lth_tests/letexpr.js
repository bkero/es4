var y=0;
function f() {
   var x = 10;
   y = let (x=x+5) x*2;
   return x;
}
var tmp = f();
tmp + y;  // 40
