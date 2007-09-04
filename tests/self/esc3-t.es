import avmplus.*;

{
    import flash.utils.*;
    var fname = System.argv[0];
    if (fname==undefined) throw "no file name given";
    print ("reading ", fname+".abc")
    var bytes = ByteArray.readFile (fname+".ast.abc");
    Domain.currentDomain.loadBytes(bytes);   // defines global var 'ast'
    print ("decoding", fname);
    var nd = Decode::program (ast);
    dumpABCFile(Gen::cg(nd), fname+".abc");
}