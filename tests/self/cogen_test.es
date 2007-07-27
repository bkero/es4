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

    public function testHelloWorld() {
        var e = new ABCEmitter();

        cgStmt({ "asm": e.newScript().init.asm, "emitter": e, "cp": e.constants },
               new ExprStmt(new CallExpr(new LexicalRef(new Identifier("print")),
                                         [new LiteralExpr(new LiteralString("Hello, world!"))])));

        dumpABCFile(e.finalize(), "hello-test.es");
    }

/*
{ 'ast::class': 'Program'
, 'packages': []
, 'fixtures': null
, 'block': { 'ast::class': 'Block'
           , 'pragmas': []
           , 'defns': [ { 'ast::class': 'FunctionDefn'
                        , 'ns': { 'ast::class': 'LiteralExpr'
                                , 'literal': { 'ast::class': 'LiteralNamespace'
                                             , 'namespaceValue': { 'ast::class': 'PublicNamespace'
                                                                 , 'name': '' } } }
                        , 'func': { 'ast::class': 'Func'
                                  , 'name': { 
                                            , 'kind': Ordinary
                                            , 'ident': f }
                                  , 'isNative': false
                                  , 'block': { 'ast::class': 'Block'
                                             , 'pragmas': []
                                             , 'defns': [ 
                                             , 'head': []
                                             , 'stmts': [ { 'ast::class': 'ExprStmt'
                                                          , 'expr': { 'ast::class': 'ListExpr'
                                                                    , 'exprs': [ { 'ast::class': 'CallExpr'
                                                                                 , 'func': { 'ast::class': 'LexicalRef'
                                                                                           , 'ident': { 'ast::class': 'Identifier'
                                                                                                      , 'ident': print } }
                                                                                 , 'args': [ { 'ast::class': 'LiteralExpr'
                                                                                             , 'literal': { 'ast::class': 'LiteralString'
                                                                                                          , 'strValue': 'hello } }
                                                                                           ,  ] }
                                                                               ,  ] } }
                                                        ,  ] } }
                      , 
           , 'head': []
           , 'stmts': [ { 'ast::class': 'ExprStmt'
                        , 'expr': { 'ast::class': 'ListExpr'
                                  , 'exprs': [ { 'ast::class': 'CallExpr'
                                               , 'func': { 'ast::class': 'LexicalRef'
                                                         , 'ident': { 'ast::class': 'Identifier'
                                                                    , 'ident': f } }
                                               , 'args': [  ] }
                                             ,  ] } }
                      ,  ] }
    
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
