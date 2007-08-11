/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is [Open Source Virtual Machine.].
 *
 * The Initial Developer of the Original Code is
 * Adobe System Incorporated.
 * Portions created by the Initial Developer are Copyright (C) 2004-2006
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Adobe AS3 Team
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

package cogen
{
    import util.*;
    import emitter.*;
    use namespace Ast;

    /*
     * This generates code equivalent to source like:
     * 
     * print("Hello World");
     * 
     */
    public function testHelloWorld() {
        var e = new ABCEmitter();

        cgStmt({ "asm": e.newScript().init.asm, "emitter": e, "cp": e.constants },
               new ExprStmt(new CallExpr(new LexicalRef(new Identifier("print")),
                                         [new LiteralExpr(new LiteralString("Hello, world!"))])));

        dumpABCFile(e.finalize(), "hello-test.es");
    }

    /*
     * This generates code equivalent to source like:
     * 
     * function f() { print("Hello World"); };
     * f();
     *
     */
    public function testHelloWorldFunc() {
        // Function Body 
        var f_func : FUNC = new Func({kind:new Ordinary(), ident:"f"}, //name
                                    false, //isNative
                                    //block:
                                    new Block( [] // pragmas
                                      , [] // defns 
                                      , null// head
                                        // stmts:
                                      , [ new ExprStmt(new ListExpr([new CallExpr(new LexicalRef(new Identifier("print")),
                                                                                        [new LiteralExpr(new LiteralString("Hello, world!"))])]))
                                              ,]
                                      , null // pos 
                                    ),
                                    // params:
                                    {fixtures:[], inits:[]},
                                    // defaults:
                                    [],
                                    // type:
                                    {typeParams:[], params:[], result:new ObjectType(), thisType:null, hasRest:false, minArgs:0}
                                    );



        // Program
        var prog = cg( new Program([],
                        new Block([], //pragmas
                                  //defns
                                  [new FunctionDefn(new Const(), 
                                                    new LiteralExpr(new LiteralNamespace(new PublicNamespace("") ) ),
                                                    false, // final
                                                    false, // override
                                                    false, // prototype
                                                    false, // static
                                                    false, // abstract
                                                    f_func
                                                    ) 
                                   ],
                                  //heads
                                  null,
                                  //stmts
                                  [ new ExprStmt(new ListExpr([new CallExpr(new LexicalRef(new Identifier("f")),[ ] ) ] ) ) ], 
                                  //pos
                                  null
                                ), 
                        [[new PropName({ns: new PublicNamespace("") , id:"f"}), 
                          new MethodFixture( f_func  // FUNC
                                             , new ObjectType() // type - shouldn't this be function?
                                             , true  // isReadOnly
                                             , false // isOverride
                                             , false // isFinal
                                           )]
                        ]
                        )
        );
                          
        dumpABCFile(prog, "hello-test.es");
    }


    /*
     * This generates code equivalent to source like:
     *
     * var x = "Hello World";
     * function f() { print(x); };
     * f();
     *
     */
    public function testHelloWorldVar() {
        // Function Body 
        var f_func : FUNC = new Func({kind:new Ordinary(), ident:"f"}, //name
                                    false, //isNative
                                    //block:
                                    new Block( [] // pragmas
                                      , [] // defns 
                                      , null// head
                                        // stmts:
                                      , [ new ExprStmt(new ListExpr([new CallExpr(new LexicalRef(new Identifier("print")),
                                                                                        [new LexicalRef(new Identifier("x"))])]))
                                              ,]
                                      , null // pos 
                                    ),
                                    // params:
                                    {fixtures:[], inits:[]},
                                    // defaults:
                                    [],
                                    // type:
                                    {typeParams:[], params:[], result:new ObjectType(), thisType:null, hasRest:false, minArgs:0}
                                    );



        // Program
        var prog = cg( new Program([],
                        new Block([], //pragmas
                                  //defns
                                  [new VariableDefn(null, false, false, new Var(), [ [new Binding(new PropIdent("x"), null)], [] ] )
                                   , new FunctionDefn(new Const(), 
                                                    new LiteralExpr(new LiteralNamespace(new PublicNamespace("") ) ),
                                                    false, // final
                                                    false, // override
                                                    false, // prototype
                                                    false, // static
                                                    false, // abstract
                                                    f_func
                                                    ) 
                                   ],
                                  //heads
                                  null,
                                  //stmts
                                  [ new ExprStmt(new ListExpr([new SetExpr(new Assign(), new LexicalRef(new Identifier("x")), new LiteralExpr(new LiteralString("Hello World")))]) )
                                  , new ExprStmt(new ListExpr([new CallExpr(new LexicalRef(new Identifier("f")),[ ] ) ] ) ) ], 
                                  //pos
                                  null
                                ), 
                        [[ new PropName({ns: new PublicNamespace("") , id:"x"}), 
                           new ValFixture(new TypeName(new Identifier("String")), //Type
                                          false //isReadOnly
                                          )]
                        , [new PropName({ns: new PublicNamespace("") , id:"f"}), 
                           new MethodFixture( f_func  // FUNC
                                             , new ObjectType() // type - shouldn't this be function?
                                             , true  // isReadOnly
                                             , false // isOverride
                                             , false // isFinal
                                           )]
                        ]
                        )
        );
                          
        dumpABCFile(prog, "hello-test.es");
    }

    /*
     * This generates code equivalent to source like:
     *
     * var x = "Hello World";
     * function f(h) { print(h); };
     * f(x);
     *
     */
    public function testHelloWorldArgs() {
        // Function Body 
        var f_func : FUNC = new Func({kind:new Ordinary(), ident:"f"}, //name
                                    false, //isNative
                                    //block:
                                    new Block( [] // pragmas
                                      , [] // defns 
                                      , null// head
                                        // stmts:
                                      , [ new ExprStmt(new ListExpr([new CallExpr(new LexicalRef(new Identifier("print")),
                                                                                        [new LexicalRef(new Identifier("h"))])]))
                                              ,]
                                      , null // pos 
                                    ),
                                    // params:
                                    {fixtures:[[ new PropName({ns: new PublicNamespace("") , id:"h"}), 
                                                 new ValFixture(new TypeName(new Identifier("String")), //Type
                                                                false //isReadOnly
                                                                )
                                               ]]
                                     , inits:[]},
                                    // defaults:
                                    [],
                                    // type:
                                    {typeParams:[], params:[], result:new ObjectType(), thisType:null, hasRest:false, minArgs:0}
                                    );



        // Program
        var prog = cg( new Program([],
                        new Block([], //pragmas
                                  //defns
                                  [new VariableDefn(null, false, false, new Var(), [ [new Binding(new PropIdent("x"), null)], [] ] )
                                   , new FunctionDefn(new Const(), 
                                                    new LiteralExpr(new LiteralNamespace(new PublicNamespace("") ) ),
                                                    false, // final
                                                    false, // override
                                                    false, // prototype
                                                    false, // static
                                                    false, // abstract
                                                    f_func
                                                    ) 
                                   ],
                                  //heads
                                  null,
                                  //stmts
                                  [ new ExprStmt(new ListExpr([new SetExpr(new Assign(), new LexicalRef(new Identifier("x")), new LiteralExpr(new LiteralString("Hello World")))]) )
                                  , new ExprStmt(new ListExpr([new CallExpr(new LexicalRef(new Identifier("f")),[new LexicalRef(new Identifier("x"))] ) ] ) ) ], 
                                  //pos
                                  null
                                ), 
                        [[ new PropName({ns: new PublicNamespace("") , id:"x"}), 
                           new ValFixture(new TypeName(new Identifier("String")), //Type
                                          false //isReadOnly
                                          )]
                        , [new PropName({ns: new PublicNamespace("") , id:"f"}), 
                           new MethodFixture( f_func  // FUNC
                                             , new ObjectType() // type - shouldn't this be function?
                                             , true  // isReadOnly
                                             , false // isOverride
                                             , false // isFinal
                                           )]
                        ]
                        )
        );
                          
        dumpABCFile(prog, "hello-test.es");
    }

    /*
     * This generates code equivalent to source like:
     *
     * var x = 0;
     * function f() { while(x<5) { print('hi'); ++x} };
     * f();
     *
     */
    public function testHelloWorldLoop() {
        
        var w : WhileStmt = new WhileStmt( new BinaryExpr( new Less(), new LexicalRef(new Identifier("x")), new LiteralExpr(new LiteralInt(5))) 
                                         , new BlockStmt( new Block( [] // pragmas
                                                                   , [] // defns 
                                                                   , null// head
                                                                   // stmts:
                                                                   , [ new ExprStmt(new ListExpr([new CallExpr(new LexicalRef(new Identifier("print")),
                                                                                       [new LiteralExpr(new LiteralString("Hello, world!"))])]))
                                                                       , new ExprStmt(new ListExpr([new UnaryExpr(new PreIncr(), new LexicalRef(new Identifier("x")))]))]
                                                                   , null // pos 
                                                                   )
                                                        ) 
                                         , []
                                         , null );
        // Function Body 
        var f_func : FUNC = new Func({kind:new Ordinary(), ident:"f"}, //name
                                    false, //isNative
                                    //block:
                                    new Block( [] // pragmas
                                      , [] // defns 
                                      , null// head
                                        // stmts:
                                      , [ w ]
                                      , null // pos 
                                    ),
                                    // params:
                                    {fixtures:[]
                                     , inits:[]},
                                    // defaults:
                                    [],
                                    // type:
                                    {typeParams:[], params:[], result:new ObjectType(), thisType:null, hasRest:false, minArgs:0}
                                    );



        // Program
        var prog = cg( new Program([],
                        new Block([], //pragmas
                                  //defns
                                  [new VariableDefn(null, false, false, new Var(), [ [new Binding(new PropIdent("x"), null)], [] ] )
                                   , new FunctionDefn(new Const(), 
                                                    new LiteralExpr(new LiteralNamespace(new PublicNamespace("") ) ),
                                                    false, // final
                                                    false, // override
                                                    false, // prototype
                                                    false, // static
                                                    false, // abstract
                                                    f_func
                                                    ) 
                                   ],
                                  //heads
                                  null,
                                  //stmts
                                  [ new ExprStmt(new ListExpr([new SetExpr(new Assign(), new LexicalRef(new Identifier("x")), new LiteralExpr(new LiteralInt(0)))]) )
                                  , new ExprStmt(new ListExpr([new CallExpr(new LexicalRef(new Identifier("f")),[] ) ] ) ) ], 
                                  //pos
                                  null
                                ), 
                        [[ new PropName({ns: new PublicNamespace("") , id:"x"}), 
                           new ValFixture(new TypeName(new Identifier("int")), //Type
                                          false //isReadOnly
                                          )]
                        , [new PropName({ns: new PublicNamespace("") , id:"f"}), 
                           new MethodFixture( f_func  // FUNC
                                             , new ObjectType() // type - shouldn't this be function?
                                             , true  // isReadOnly
                                             , false // isOverride
                                             , false // isFinal
                                           )]
                        ]
                        )
        );
                          
        dumpABCFile(prog, "hello-test.es");
    }

    /*
    public function testFib() {
        dumpABCFile(new Program([],
                                new Block([],
                                          [],
                                          [],
                                          [new ExprStmt(new CallExpr(new LexicalRef(new Identifier("print")),
                                                                     [new LiteralExpr(new LiteralString("Hello, world!"))]))],
                                          null),
                                [[new PropName,
                                  new ClassFixture(new Cls({ ns: new PublicNamespace(""), id: "Fib" },
                                  { ns: new PublicNamespace(""), id: "Object" },
                                                           [],
                                                           new Ctor([],
                                                                    [],
                                                                    new Func({ ns: new PublicNamespace(""), id: "Fib" },
                                                                             ...,
                                                                             false,
                                                                             ...,
                                                                             ...,
                                                                             [],
                                                                             ...))))]]),
                    "fib-test.es");
    }
    */
}
