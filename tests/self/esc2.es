{
    import util.*;
    use namespace Parser;
    var file = "esc-tmp";
    print("reading");
    var ast = readFile (file+".ast");
    print ("decoding");
    var nd = Decode::program (eval("("+ast+")"));
    print ("encoding");
    var ast = Encode::program (nd);
    writeFile (ast,file+".ast");
    print (ast.length+" chars written");
}
