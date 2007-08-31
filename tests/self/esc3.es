{
    use namespace Util;

    print ("reading")
    load ("esc-tmp.ast");  // read and eval 
    print ("decoding");
    var nd3 = Decode::program (ast);
    dumpABCFile(Gen::cg(nd3), "esc-tmp.abc");
}