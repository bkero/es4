{
    import util.*;
    import cogen.*;

    use namespace Ast;
    use namespace Parser;

    var file = "fib";

    var str1 = readFile ("./tests/self/"+file+".es");
    var parser = new Parser(str1);
    var [ts1,nd1] = parser.program();
    var str2 = Encode::program (nd1);
    print(str2);
    writeFile (str2,"./tests/self/"+file+".ast");
    var str3 = readFile ("./tests/self/"+file+".ast");
    print(str3);
    print ("eval");
    var ob = eval("("+str3+")");
    print ("decode");
    var nd3 = Decode::program (ob);
    print("cogen");
    dumpABCFile(cogen.cg(nd3), "esc-tmp.es");
}
