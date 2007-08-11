{
    import util.*;
    import cogen.*;

    use namespace Ast;
    use namespace Parser;


    var str = readFile ("./tests/self/cls.es");
    var parser = new Parser(str);
    var [ts1,nd1] = parser.program();
    print(Ast::encodeProgram (nd1));

    dumpABCFile(cogen.cg(nd1), "esc-tmp.es");
}
