package util
{
    import bytestream.*;

    public function assert(cond) {
        if (!cond)
            throw "Assertion failed!";
    }

    public function map(fn, a) {
        var b = [];
        for ( var i=0 ; i < a.length ; i++ )
            if (i in a)
                b[i] = fn(a[i]);
        return b;
    }

    public function forEach(fn, a) {
        for ( var i=0 ; i < a.length ; i++ )
            if (i in a)
                fn(a[i]);
    }

    public function memberOf(x, ys) {
        for ( var i=0 ; i < ys.length ; i++ )
            if (ys[i] === x)
                return true;
        return false;
    }

    public function copyArray(c) {
        var a = new Array;
        for ( var i=0 ; i < c.length ; i++ )
            a[i] = c[i];
        return a;
    }

    public function dumpByteStream(bytes, result=null) {
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

    public function dumpABCFile(abcfile, filename) {
        var bytes = abcfile.getBytes();
        assert( bytes.push );  /*FIXME ES4: really "bytes is Array" */
        var s = ["(function (xs) {",
                 " import flash.utils.ByteArray;",
                 " import avmplus.Domain;",
                 " var ba = new ByteArray;",
                 " for ( var i=0 ; i < xs.length ; i++ )",
                 "   ba.writeByte(xs[i]);",
                 " var domain = Domain.currentDomain;",
                 " domain.loadBytes(ba);",
                 "})([" + bytes.join(",") + "])" ].join("\n");
        writeStringToFile(s, filename);
    }
}
