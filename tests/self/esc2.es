{
    import util.*;
    use namespace Parser;
    var file = "esc-tmp";
    print("reading");
    load (file+".ast");
    print ("decoding");
    var nd = Decode::program (ast);
    print ("encoding");
    var tx = "var ast = "+Encode::program (nd);
    print ("writing");
    writeFile (tx,file+".ast");
    print (tx.length+" chars written");
}
