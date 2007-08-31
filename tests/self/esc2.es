import avmplus.*;
{
    import flash.utils.*;
    use namespace Decode;
    var fname = System.argv[0];
print ("reading ", fname+".abc")
    var bytes = ByteArray.readFile (fname+".abc");
    Domain.currentDomain.loadBytes(bytes);   // defines global var 'ast'
    print ("decoding", fname);
    var nd = program (ast);
}
{
    use namespace Encode;
    print ("encoding");
    var tx = "public var ast = "+program (nd);
    print ("writing ",tx);  //use print in tamarin for now
    writeFile (tx,"esc-tmp.ast");
    print (tx.length+" chars written");
}
