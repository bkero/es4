{
    import avmplus.*;
    var fname = System.argv[0];
    var str = File.read (fname);
    print ("compiling ", fname);
}
{
    import avmplus.*;
    import flash.utils.*;
    use namespace Decode;
    use namespace Ast;
    print ("decoding");
    var bytes = ByteArray.readFile ("esc-env.ast.abc");
    Domain.currentDomain.loadBytes(bytes);   // defines global var 'ast'
    var nd = program (ast);
    var topFixtures = nd.head.fixtures;
}
{
    use namespace Parse;
    print ("parsing");
    var parser = initParser(str,topFixtures);
    var [ts,nd] = program();
}
{
    use namespace Encode;
    print ("encoding");
    var tx = "public var ast = "+program (nd);
    print ("writing");
    writeFile (tx,fname+".ast");
    print (tx.length+" chars written");
}
