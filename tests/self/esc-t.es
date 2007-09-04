{
    import avmplus.*;
    var fname = System.argv[0];
    var str = File.read (fname);
    print ("compiling ", fname);
}

var topFixtures = [];

{
    use namespace Parse;
    print ("parsing");
    var parser = initParser(str,topFixtures);
    var [ts,nd] = program();
}

{
    import avmplus.*;
    import flash.utils.*;
    dumpABCFile(Gen::cg(nd), fname+".abc");
}