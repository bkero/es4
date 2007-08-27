{
    print ("decoding");
    var nd = Decode::program (ast);
    print ("encoding");
    var tx = "var ast = "+Encode::program (nd);
    print ("writing");
    writeFile (tx,"esc-tmp.ast");  // what's this in tamarin
    print (tx.length+" chars written");
}
