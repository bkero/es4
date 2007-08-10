/*
class A { var x = 10; function A (x) : x=x {}; print('static hi') }
let x = 10;
var y = 20;
function f(a=1) { let b=2; var c=3 }
*/

class Fib {
    function Fib(n) {
       if (n < 2)
           val = n;
       else
           val = (new Fib(n-1)).val + (new Fib(n-2)).val
    }
    var val;
    function m() { print('m'); }
}
print((new Fib(10)).val)
