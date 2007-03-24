//* (number 50)

package com.opera.foo
{
    public var x = 20;
    public var y = 10;
}

var y = 30;
{
    import com.opera.foo.x;
    x + y;
}