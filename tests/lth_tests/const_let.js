var x = 10;
var y = 0;
function f(n) {
   if (n > 10) {
     let const x = 20;
     y = x;
   }
   return x;
}
f(37) + y;  // 30
