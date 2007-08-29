{
//    import util.*;
    use namespace Ast;
    load ("esc-env.ast")
    print ("decoding: ", ast);
    var nd = Decode::program (ast);
    var topFixtures = nd.head.fixtures;

    var file = "esc-tmp";
    var str = readFile (file+".es");
    var parser = Parse::initParser(str,topFixtures);
    print ("parsing");
    var [ts,nd] = Parse::program();
    print ("encoding");
    var tx = "var ast = "+Encode::program (nd);
    print ("writing");
    writeFile (tx,file+".ast");
    print (tx.length+" chars written");
}
