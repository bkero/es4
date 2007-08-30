print("start esc2");
/*
{
    use namespace Decode;
    var Decode_program = program;
}

{
    use namespace Encode;
    var Encode_program = program;
}
*/
{
    print ("decoding");
    var nd = Decode::program (ast);
    print ("encoding");
    var tx = "var ast = "+Encode::program (nd);
    print ("writing ",tx);  //use print in tamarin for now
    //writeFile (tx,"esc-tmp.ast");  // what's this in tamarin
    print (tx.length+" chars written");
}
