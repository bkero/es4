package es4 {

    function assert(cond) {
        if (!cond)
            throw "Assertion failed!";
    }

    function copyArray(c) {
        var a = new Array;
        for ( var i=0 ; i < c.length ; i++ ) 
            a[i] = c[i];
        return a;
    }

    function dumpByteStream(bytes, result=null) {
        function f(n, r=16) {
            return (n + 0x100).toString(r).substring(1);
        }
        var a = bytes.writeToArray([]);
        if (result == null)
            result = [];
        for ( var i=0 ; i < a.length ; i++ )
            print((i+100).toString(10).substring(1) + ": " + 
                  (i in result ? " " + result[i] : (result.length == 0 ? "   " : " **")) + " " + 
                  f(a[i]) + " " + 
                  f(a[i],2) + " " +
                  (a[i] >= 32 && a[i] <= 126 ? "'" + String.fromCharCode(a[i]) + "'" : "") );
    }
}
