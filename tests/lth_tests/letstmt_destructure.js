var y=0;
function f() {
   var x = { a: 10, b: 20 };
   let ({a:x,b:z} = x) {
     y = x+z;
   }
   return x;
}
var tmp = f();
tmp + y;  // 30
