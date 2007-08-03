{
    import util.*;
    import cogen.*;

    use namespace Ast;
    use namespace Parser;


    var parser = new Parser("var x = 10; print(x);");
    var [ts1,nd1] = parser.program();
    print(Ast::encodeProgram (nd1));

    dumpABCFile(cogen.cg(nd1), "esc-tmp.es");
}
