var y=0;
function f() {
   var x = 10;
   let (x=x+5) {
     y = x;
   }
   return x;
}
var tmp = f();
tmp + y;  // 25
