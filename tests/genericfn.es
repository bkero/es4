// Test cases for the generic functions system.
//
// Load this file interactively from the root dir.
//
// For extra interesting output, turn on the DEBUG flag in
// builtins/GenericFunction.es.

intrinsic::load("builtins/GenericFunction.es");

/*
generic function fn(x);
generic function fn(x) { print("any: " + x) }
generic function fn(x:int) { print("int: " + x) }
*/
fn = new GenericFunction([AnyType], null, false);
fn.addMethod([AnyType], function (nextMethod, x) { print("any: " + x) });
fn.addMethod([int], function (nextMethod, x) { print("int: " + x) });

// Now try:
//  fn(1)
//  fn("hi there")


/*
generic function plus(x,y);
generic function plus(x:String,y) { return string(x) + string(y) }
generic function plus(x,y:String) { return string(x) + string(y) }
generic function plus(x:(int,double), y:(int,double)) { return x+y }
generic function plus(x,y) { return double(x) + double(y) }
*/
plus = new GenericFunction([AnyType,AnyType], null, false);
plus.addMethod([String,AnyType], function(nextMethod, x, y) { return string(x) + string(y) });
plus.addMethod([AnyType,String], function(nextMethod, x, y) { return string(x) + string(y) });
// Typical translation of a union type dispatch
let (f = function(nextMethod, x, y) { return x+y }) {
    plus.addMethod([int,int], f);
    plus.addMethod([int,double], f);
    plus.addMethod([double,int], f);
    plus.addMethod([double,double], f);
}
plus.addMethod([AnyType,AnyType], function(nextMethod, x, y) { return double(x)+double(y) });

/* Typical user addition:
generic function plus(x:Number,y) { return 2*nextMethod() }
*/
plus.addMethod([Number,AnyType], function(nextMethod,x,y) { return 2*nextMethod() })

// Now try:
//  plus("x", 3)
//  plus(1, 0.5)
//  plus(new Number(7), 8)
