package com.opera.ecma4 
{
    public var y = "ecma4";
    public function f() { return y; }
}
import zappa = com.opera.ecma4.y;
import com.opera.ecma4.f;
zappa = "ecma5";
f();
