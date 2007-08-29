print("start esc2");

{
    use namespace Decode;
    var Decode_program = program;
}

{
    use namespace Encode;
    var Encode_program = program;
}

{
    print ("decoding");
    var nd = Decode_program (ast);
    print ("encoding");
    var tx = "var ast = "+Encode_program (nd);
    print ("writing ",tx);
    writeFile (tx,"esc-tmp.ast");  // what's this in tamarin
    print (tx.length+" chars written");
}
