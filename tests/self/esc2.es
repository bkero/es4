{
    print ("reading");
    load ("esc-tmp.ast");
    print ("decoding");
    var nd = Decode::program (ast);
    print ("encoding");
    var tx = "var ast = "+Encode::program (nd);
    print ("writing");
    writeFile (tx,"esc-tmp.ast");
    print (tx.length+" chars written");
}
