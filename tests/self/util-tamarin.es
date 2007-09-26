{
    import flash.utils.*;
    
    public function writeFile(s:String, filename:String) {
        var b = new ByteArray();
        
        b.writeUTFBytes(s);
        
        b.writeFile(filename);
        
    }
    
    public function readFile(filename:String) {
        
        var b = ByteArray.readFile(filename);
        return b.readUTFBytes(b.length);
    }
}

{
    import flash.utils.*;
    
    public function dumpABCFile(abcfile, filename) {
        var bytes = abcfile.getBytes();
        Util::assert( bytes.push );  /*FIXME ES4: really "bytes is Array" */

        let len = bytes.length;
        let b = new ByteArray();
        b.endian = "littleEndian";

        for (let i = 0; i<len; ++i) {
	        b.writeByte((uint(bytes[i])));
        }

        print (filename, ", writing ",len," bytes");        
        b.writeFile(filename);
    }
}
