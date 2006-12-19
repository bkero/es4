1;
1.23;
null;
true;
false;
"string";
[0, true, "blue"] : [int, Boolean, String];
//[1, 2, "foo"] : Array;
//[1, 2, "bar"] : *;
//[1, 2, "baz"] : Object;
[1, 2, "foo"];
//{} : {};
//{} : *;
//{};
//{ x: 0, y: true, z: "blue" } : { x: int, y: Boolean };
//{ x: 0, y: true, z: "blue" } : { x: int, y: Boolean, z: String };
let (x=1) { x };
let (x=1) let (y=x) y;
let (z=2,y=3) y;
1,2,"abc";
this;
(function f(x:int):int { return 1 });
(function f(x:int):int { return 1 }) (4);
(function f(x:int,y):int { return 1 }) (4,5);
let (f=function f() {}) f(3);
let (f:function(int):int=function f() {}) f(3);

//(type function(int):int);

(function f(...x):int { return 1 }) (4);

(function f.<X>(x:X):X { return x});

let (f=1) f(3);




