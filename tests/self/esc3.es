{
    import util.*;
    import cogen.*;
    var file = "esc-tmp";
    var ast = readFile (file+".ast");
    print ("eval");
    var ob = eval("("+ast+")");
    print ("decode");
    var nd3 = Decode::program (ob);
    print("cogen");
    dumpABCFile(cogen.cg(nd3), "esc-tmp.as");
}
