/* E262-4 draft proposals:bytearray 
 *
 * Lars and Brendan agree (2007-01-26) that this class is final,
 * non-dynamic, and nullable.
 */
final class ByteArray
{
    function ByteArray(n : uint = 0) {
        for ( let i : uint ; i < n ; i++ )
            magic::setByteArrayByte(this, i, 0);
        _length = n;
    }

    static function to(a : Array!) : ByteArray!
        this.intrinsic::to(a);

    static intrinsic function to(a : Array!) : ByteArray! {
        let n  : uint = a.length;
        let ba : ByteArray = new ByteArray(n);
        for ( let i : uint = 0 ; i < n ; i++ )
            ba[i] = a[i];
        return ba;
    }

    static function fromString(s : string) : ByteArray! {
        let n  : uint = s.length;
        let ba : ByteArray = new ByteArray(n);
            
        for ( let i : uint = 0; i < n; i++ )
            ba[i] = s.charCodeAt(i);
        return a;
    }

    static function fromArray(a:Array) : ByteArray!
        a to ByteArray;

    function get length() : uint
        _length;

    function set length(n : uint) : void {
        for ( let i : uint = _length ; i < n ; i++ )
            magic::setByteArrayByte(this, n, 0);
        _length = n;
    }

    function get *(k) : uint {
        if (k is uint)
            return magic::getByteArrayByte(this, k to uint);
        else
            return intrinsic::get(this, k);
    }

    function set *(k, v) : void {
        if (k is uint) {
            let idx : uint = k to uint;
            if (idx >= _length)
                length = idx+1;  // Setting "length" (not "_length") performs zero-filling
            magic::setByteArrayByte(this, idx, (v to uint) & 255);
        }
        else
            intrinisic::set(this, k, v);
    }

    prototype function toString(this: ByteArray) 
        this.toString();

    intrinsic function toString() : string {
        var n : uint = _length;
        var s : string = "";
        for ( let i:int = 0; i < n; i++ )
            s += String.fromCharCode(this[i]);
        return s;
    }

    var _length : uint = 0;
}
