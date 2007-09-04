/*
{
    import flash.utils.*;
    use namespace Decode;
    var bytes = ByteArray.readFile (fname+"esc-env.ast.abc");
    Domain.currentDomain.loadBytes(bytes);   // defines global var 'ast'
    print ("decoding", fname);
    var nd = program (ast);
}
*/
{
//    import util.*;
    use namespace Ast;
    use namespace Parse;
    load ("esc-env.ast")
    print ("decoding: ", esc_env);
    var nd = Decode::program (esc_env);
    var topFixtures = nd.head.fixtures;

    var file = "esc-tmp";
    var str = readFile (file+".es");
    var parser = initParser(str,topFixtures);
    print ("parsing");
    var [ts,nd] = program();
}
{
    use namespace Encode;
    print ("encoding");
    var tx = "public var ast = "+program (nd);
    print ("writing");
    writeFile (tx,file+".ast");
    print (tx.length+" chars written");
}
