{
    import flash.utils.*;
    
    function writeFile(s:String, filename:String) {
        var b = new ByteArray();
        
        b.writeUTFBytes(s);
        
        b.writeFile(filename);
        
    }
    
    function readFile(filename:String) {
        
        var b = ByteArray.readFile(filename);
        return b.readUTFBytes(b.length);
    }
/*
    var s = readFile("tmp.txt");
    print("File contents:");
    print(s);
    print("Writing file to out.txt");
    writeFile(s,"out.txt");
*/    
}


