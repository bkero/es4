{
    import avmplus.*;
    var fname = System.argv[0];
    var str = File.read (fname);
    print ("compiling ", fname);
}
{
    import avmplus.*;
    import flash.utils.*;
    use namespace Decode;
    use namespace Ast;
    print ("decoding");
    var bytes = ByteArray.readFile ("esc-env.ast.abc");
    Domain.currentDomain.loadBytes(bytes);   // defines global var 'ast'
    var nd = program (ast);
    var topFixtures = nd.head.fixtures;
}
{
    use namespace Parse;
    print ("parsing");
    var parser = initParser(str,topFixtures);
    var [ts,nd] = program();
}
{
    import avmplus.*;
    import flash.utils.*;
    var fname = System.argv[0];
    if (fname==undefined) throw "no file name given";
    print ("reading ", fname+".abc")
    var bytes = ByteArray.readFile (fname+".ast.abc");
    Domain.currentDomain.loadBytes(bytes);   // defines global var 'ast'
    print ("decoding", fname);
    var nd = Decode::program (ast);
    Util::dumpABCFile(Gen::cg(nd), fname+".abc");
}