var y = 0;
function f() {
   var x = 10;
   if (x > 5) {
      let x = 15;
      y = x*2;
   }
   return x;
}
var tmp = f();
tmp + y; // 40

