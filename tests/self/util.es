namespace Util

{
    use default namespace Util;

    function assert(cond) {
        if (!cond)
            throw "Assertion failed!";
    }

    function map(fn, a) {
        var b = [];
        for ( var i=0 ; i < a.length ; i++ )
            if (i in a)
                b[i] = fn(a[i]);
        return b;
    }

    function forEach(fn, a) {
        for ( var i=0 ; i < a.length ; i++ )
            if (i in a)
                fn(a[i]);
    }

    function memberOf(x, ys) {
        for ( var i=0 ; i < ys.length ; i++ )
            if (ys[i] === x)
                return true;
        return false;
    }

    function copyArray(c) {
        var a = new Array;
        for ( var i=0 ; i < c.length ; i++ )
            a[i] = c[i];
        return a;
    }

}
