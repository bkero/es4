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

    function dumpByteStream(bytes, result=null) {
        function f(n, r=16) {
            return (n + 0x100).toString(r).substring(1);
        }

        var a;
        if (bytes.int32) /*FIXME ES4: really "bytes is ABCByteStream"*/
            a = bytes.writeToArray([]);
        else if (bytes.push) /*FIXME ES4: really "bytes is Array"*/
            a = copyArray(bytes);
        else
            throw "Unknown type of array in dumpByteStream";

        if (result == null)
            result = [];
        for ( var i=0 ; i < a.length ; i++ )
            print((i+100).toString(10).substring(1) + ": " +
                  (i in result ? " " + result[i] : (result.length == 0 ? "   " : " **")) + " " +
                  f(a[i]) + " " +
                  f(a[i],2) + " " +
                  (a[i] >= 32 && a[i] <= 126 ? "'" + String.fromCharCode(a[i]) + "'" : "") );
    }

    function dumpABCFile(abcfile, filename) {
        var bytes = abcfile.getBytes();
        assert( bytes.push );  /*FIXME ES4: really "bytes is Array" */

        let s = ""
        let len = bytes.length;
	beginBytes();
        for (let i = 0; i<len; ++i) {
	    pushByte (uint(bytes[i]));
        }

        print (filename, ", writing ",len," bytes");        
        writeBytes(filename);
    }
}
