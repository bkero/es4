{
    import util.*;
    import cogen.*;

    print ("reading")
    load ("esc-tmp.ast");  // read and eval 
    print ("decoding");
    var nd3 = Decode::program (ast);
    dumpABCFile(cogen.cg(nd3), "esc-tmp.abc");
}