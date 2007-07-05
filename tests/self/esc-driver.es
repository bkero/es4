{
    import util.*;
    import cogen.*;

    use namespace Ast;
    use namespace Parser;

/*
    var parser = new Parser("print(3+4)");
    var [ts1,nd1] = parser.program();
    print(Ast::encodeProgram (nd1));
*/
/*
    {
        import emitter.*;
        var e = new ABCEmitter();

        cogen.cgStmt({ "emitter": e, "asm": e.newScript().init.asm, "cp": e.constants },
                     nd1.block.stmts[0]);

        dumpABCFile(e.finalize(), "hello-test.es");
    }
*/

    let fn = new FunctionDefn(new Func({kind: new Ordinary(), ident: "fn"},  // name
                                       new FunctionSignature,  // fsig - FIXME
                                       false, // isNative
                                       new Block([],  // pragmas
                                                 [],  // defns
                                                 null, // head
                                                 [new ReturnStmt(new BinaryExpr(plusOp,
                                                                                new LexicalRef(new Identifier("m")),
                                                                                new LexicalRef(new Identifier("n"))))
                                                  ], // stmts
                                                 null // pos
                                                 ), // block
                                       { fixtures: [[new PropName(new PropIdent("m")),
                                                     new ValFixture(new SpecialType(new AnyType()))],
                                                    [new PropName(new PropIdent("n")),
                                                     new ValFixture(new SpecialType(new AnyType()))]],
                                         inits: []
                                       }, // params
                                       [], // defaults
                                       null // ty
                                       ));

    let main = new ExprStmt(new CallExpr(new LexicalRef(new Identifier("print")),
                                         [new CallExpr(new LexicalRef(new Identifier("fn")),
                                                       [new LiteralExpr(new LiteralInt(3)),
                                                        new LiteralExpr(new LiteralInt(4))])]));

    dumpABCFile(cogen.cg(new Program([],
                                     new Block([], [fn], null, [main], null),
                                     [[new PropName(new PropIdent("fn")),
                                       new ValFixture(new SpecialType(new AnyType()))]] // fixtures
                                     )),
                "fn-test.es");

//    dumpABCFile(cogen.cg(nd1), "hello-test.es");
}
