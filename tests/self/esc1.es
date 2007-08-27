{
//    import util.*;
    use namespace Parse;
    load ("esc-env.ast")
    print ("decoding: ", ast);
    var nd = Decode::program (ast);
    var topFixtures = nd.Ast::head.fixtures;

    var file = "esc-tmp";
    var str = readFile (file+".es");
    var parser = new Parser(str,topFixtures);
    print ("parsing");
    var [ts,nd] = parser.program();
    print ("encoding");
    var tx = "var ast = "+Encode::program (nd);
    print ("writing");
    writeFile (tx,file+".ast");
    print (tx.length+" chars written");
}
