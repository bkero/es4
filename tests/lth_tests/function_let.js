var y = 0;
function f(n) 
{
   function h() { return 13; }
   if (n > 10) {
       let function h() { return g(); }
       let function g() { return 12; }
       y = h();
   }
   return h();
}
var tmp = f(37);
tmp + y; // 25
