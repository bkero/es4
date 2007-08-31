print("start esc2");
{
    print ("decoding");
    var nd = Decode::program (ast);
    print ("encoding");
    var tx = "var ast = "+Encode::program (nd);
    print ("writing ",tx);  //use print in tamarin for now
    writeFile (tx,"esc-tmp.ast");
    print (tx.length+" chars written");
}
