{
    import util.*;
    use namespace Parser;
    var file = "esc-tmp";
    var str = readFile (file+".es");
    var parser = new Parser(str);
    print ("parsing");
    var [ts,nd] = parser.program();
    print ("encoding");
    var tx = Encode::program (nd);
    writeFile (tx,file+".ast");
    print (tx.length+" chars written");
}
