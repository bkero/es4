// Does the var statement introduce bindings into the variable object?
let a, b, c;
var { x: a, y: b, z: c } = { x: 1, y: 2, z: 4 };
this.a + this.b + this.c; // 7
