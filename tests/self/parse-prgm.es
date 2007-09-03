/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */
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

use namespace Release;
use namespace intrinsic;

{
    use default namespace Parse;
    use namespace Lex;

    {
        // DIRECTIVES

        /*
          Directives(tau)
              empty
              DirectivesPrefix(tau) Directives(tau,full)

        */

        function directives (ts: TOKENS, tau: TAU)
            : [TOKENS, Ast::PRAGMAS, Ast::STMTS]
        {
            enter("Parser::directives ", ts);

            switch (hd (ts)) {
            case Token::RightBrace:
            case Token::EOS:
                var [ts1,nd1] = [ts,[],[]];
                break;
            default:
                var [ts1,nd1] = directivesPrefix (ts,tau);
                break;
            }

            exit("Parser::directives ", ts1);
            return [ts1,nd1];
        }

        /*

          DirectivesPrefix (tau)
              empty
              Pragmas
              DirectivesPrefix(tau) Directive(tau,full)

          right recursive:

          DirectivesPrefix(tau)
              empty
              Pragmas DirectivePrefix'(tau)

          DirectivesPrefix'(tau)
              empty
              Directive(tau,full) DirectivesPrefix'(tau)

          add var fixtures to the vhead and let fixtures to the bhead. the
          context provides a reference to the current vhead and bhead, as
          well as the whole environment, for convenient name addition and
          lookup.


        */

        function directivesPrefix (ts: TOKENS, tau: TAU)
            : [TOKENS, Ast::PRAGMAS, Ast::STMTS]
        {
            enter("Parser::directivesPrefix ", ts);

            switch (hd (ts)) {
            case Token::Use:
            case Token::Import:
                var [ts1] = pragmas (ts); 
                var [ts2,nd2] = directivesPrefixPrime (ts1,tau);
                break;
            default:
                var [ts2,nd2] = directivesPrefixPrime (ts,tau);
                break;
            }

            exit("Parser::directivesPrefix ", ts2);
            return [ts2,nd2];
        }

        function directivesPrefixPrime (ts: TOKENS, tau: TAU)
            : [TOKENS, Ast::STMTS]
        {
            enter("Parser::directivesPrefixPrime ", ts);

            var nd1 = [];
            var ts1 = ts;

            while (hd (ts1) !== Token::RightBrace &&
                   hd (ts1) !== Token::EOS ) 
            {
                var [ts1,ndx] = directive (ts1,tau,fullStmt);
                for (var i=0; i<ndx.length; ++i) nd1.push (ndx[i]);
            }

            exit("Parser::directivesPrefixPrime ", ts1);
            return [ts1,nd1];
        }

        function isCurrentClassName (ts: TOKENS) 
            : boolean {
            let text = Token::tokenText (ts.head());
            if (text === currentClassName) 
            {
                return true;
            }
            else 
            {
                return false;
            }
        }

        /*

        Directive(t, w)
            EmptyStatement
            Statement(w)
            AnnotatableDirective(t, w)
            Attributes(t)  [no line break]  AnnotatableDirective(t, w)

        */

        function directive (ts: TOKENS, tau: TAU, omega: OMEGA)
            : [TOKENS, Ast::STMTS]
        {
            enter("Parser::directive ", ts);

            printLn(ts);

            switch (hd(ts)) {
            case Token::SemiColon:
                var [ts1,nd1] = [tl (ts), [new Ast::EmptyStmt]];
                break;
            case Token::Let: // FIXME might be function
            case Token::Var:
            case Token::Const:
                var [ts1,nd1]
                    = variableDefinition (ts, allowIn, tau
                                  , cx.pragmas.defaultNamespace
                                  , false, false);

                ts1 = semicolon (ts1,omega);
                break;
            case Token::Function:
                if (isCurrentClassName (tl (ts))) 
                {
                    var [ts1,nd1] = constructorDefinition (ts, omega, cx.pragmas.defaultNamespace);
                }
                else 
                {
                    var [ts1,nd1] = functionDefinition (ts, tau, omega, new Ast::Var
                                  , cx.pragmas.defaultNamespace
                                  , false, false, false, false, false);
                }
                ts1 = semicolon (ts1,omega);
                break;
            case Token::Class:
                var isDynamic = false;
                var [ts1,nd1] = classDefinition (ts, cx.pragmas.defaultNamespace, isDynamic);
                break;
            case Token::Namespace:
                var [ts1,nd1] = namespaceDefinition (ts, omega, cx.pragmas.defaultNamespace);
                break;
            case Token::Type:
                var [ts1,nd1] = typeDefinition (ts, omega, cx.pragmas.defaultNamespace);
                break;
            case Token::LeftBrace:
            case Token::Break:
            case Token::Continue:
            case Token::Default:
            case Token::Do:
            case Token::For:
            case Token::If:
            case Token::Let:
            case Token::Return:
            case Token::Switch:
            case Token::Throw:
            case Token::Try:
            case Token::While:
            case Token::With:
                var [ts1,nd1] = statement (ts,tau,omega);
                nd1 = [nd1];
                break;
            case Token::Dynamic:
            case Token::Final:
            case Token::Native:
            case Token::Override:
            case Token::Prototype:
            case Token::Static:
            case Token::Public:
            case Token::Private:
            case Token::Protected:
            case Token::Internal:
            case Token::Intrinsic:
                var [ts1,nd1] = attribute (ts,tau,defaultAttrs());
                var [ts1,nd1] = annotatableDirective (ts1,tau,omega,nd1);
                break;
            default:  // label, attribute, or expr statement
                var [ts1,nd1] = listExpression (ts,allowIn);
                switch (hd (ts1)) {
                case Token::Colon:  // label
                    //print ("label=",Encode::encodeExpr (nd1));
                    // FIXME check label
                    break;
                case Token::SemiColon:
                    var [ts1,nd1] = [tl (ts1), [new Ast::ExprStmt (nd1)]];
                    break;
                case Token::RightBrace:
                case Token::EOS:
                    var nd1 = [new Ast::ExprStmt (nd1)];
                    break;
                default:
                    if (newline (ts1)) 
                    { // stmt
                        var nd1 = [new Ast::ExprStmt (nd1)];
                    }
                    else 
                    {
                        switch (hd (ts1)) {
                        case Token::Dynamic:
                        case Token::Final:
                        case Token::Native:
                        case Token::Override:
                        case Token::Prototype:
                        case Token::Static:
                        case Token::Let:
                        case Token::Var:
                        case Token::Const:
                        case Token::Function:
                        case Token::Class:
                        case Token::Namespace:
                        case Token::Type:
                            // FIXME check ns attr
                            let ie = nd1.Ast::exprs[0].Ast::ident;  
                            var attrs = defaultAttrs ();
                            attrs.ns = cx.evalIdentExprToNamespace (ie);
                            var [ts1,nd1] = annotatableDirective (ts1,tau,omega,attrs);
                            break;
                        default:
                            throw "directive should never get here";
                            var nd1 = [new Ast::ExprStmt (nd1)];
                            break;
                        }
                    }
                }
            }

            exit("Parser::directive ", ts1);
            return [ts1,nd1];
        }

        function annotatableDirective (ts: TOKENS, tau: TAU, omega: OMEGA, attrs)
            : [TOKENS, Ast::STMTS]
        {
            enter("Parser::annotatableDirective ", ts);

            switch (hd(ts)) {
            case Token::Let: // FIXME might be function
            case Token::Var:
            case Token::Const:
                var [ts2,nd2]
                    = variableDefinition (ts, allowIn, tau
                                          , attrs.ns
                                          , attrs.prototype
                                          , attrs.static);

                var ts2 = semicolon (ts2,omega);
                break;
            case Token::Function:
                if (isCurrentClassName (tl (ts))) 
                {
                    var [ts2,nd2] = constructorDefinition (ts, omega, attrs.ns);
                }
                else 
                {
                    var [ts2,nd2] = functionDefinition (ts, tau, omega, new Ast::Var
                                                           , attrs.ns, attrs.final, attrs.override
                                                           , attrs.prototype, attrs.static, attrs.abstract);
                }
                ts2 = semicolon (ts2,omega);
                break;
            case Token::Class:
                var [ts2,nd2] = classDefinition (ts, attrs.ns, attrs.dynamic);
                break;
            case Token::Namespace:
                print ("found namespace");
                var [ts2,nd2] = namespaceDefinition (ts, omega, attrs.ns);
                break;
            case Token::Type:
                var [ts2,nd2] = typeDefinition (ts, omega, attrs.ns);
                break;
            default:  // label, attribute, or expr statement
                var [ts1,nd1] = attribute (ts,tau,defaultAttrs());
                if (newline (ts1)) throw "error unexpected newline before "+Token::tokenText (hd (ts));
                var [ts2,nd2] = annotatableDirective (ts1,tau,omega,nd1);
            }

            exit("Parser::annotatableDirective ", ts2);
            return [ts2,nd2];
        }

//        /*
//
//        Attributes
//            Attribute
//            Attribute [no line break] Attributes
//
//        Attribute
//            SimpleTypeIdentifier
//            ReservedNamespace
//            dynamic
//            final
//            native
//            override
//            prototype
//            static
//            [  AssignmentExpressionallowIn  ]
//
//        */
//

        type ATTRS = Object;  // FIXME object type

        function defaultAttrs ()
            : ATTRS {
            return { ns: cx.pragmas.defaultNamespace
                   , 'true': false
                   , 'false': false
                   , dynamic: false
                   , final: false
                   , native: false
                   , override: false
                   , prototype: false
                   , static: false }
        }

        function attribute (ts: TOKENS, tau: TAU, nd: ATTRS)
            : [TOKENS, *]
        {
            enter("Parser::attribute tau="+tau+" ", ts);

            switch (tau) {
            case classBlk:
                switch (hd (ts)) {
                case Token::Final:
                    nd.final = true;
                    var [ts1,nd1] = [tl (ts), nd];
                    break;
                case Token::Native:
                    nd.native = true;
                    var [ts1,nd1] = [tl (ts), nd];
                    break;
                case Token::Override:
                    nd.override = true;
                    var [ts1,nd1] = [tl (ts), nd];
                    break;
                case Token::Prototype:
                    nd.prototype = true;
                    var [ts1,nd1] = [tl (ts), nd];
                    break;
                case Token::Static:
                    nd.static = true;
                    var [ts1,nd1] = [tl (ts), nd];
                    break;
                case Token::Public:
                case Token::Private:
                case Token::Protected:
                case Token::Internal:
                case Token::Intrinsic:
                    var [ts1,nd1] = reservedNamespace (ts);
                    nd.ns = nd1;
                    var [ts1,nd1] = [tl (ts), nd];
                    break;
                default:
                    var [ts1,nd1] = primaryName (ts);
                    nd.ns = cx.evalIdentExprToNamespace (nd1);
                    var [ts1,nd1] = [ts1,nd];
                    break;
                }
            case globalBlk:
                switch (hd (ts)) {
                case Token::True:
                    nd['true'] = true;  // FIXME RI bug
                    var [ts1,nd1] = [tl (ts), nd];
                case Token::False:
                    nd['false'] = false;
                    var [ts1,nd1] = [tl (ts), nd];
                case Token::Dynamic:
                    nd.dynamic = true;
                    var [ts1,nd1] = [tl (ts), nd];
                    break;
                case Token::Final:
                    nd.final = true;
                    var [ts1,nd1] = [tl (ts), nd];
                    break;
                case Token::Native:
                    nd.native = true;
                    var [ts1,nd1] = [tl (ts), nd];
                    break;
                case Token::Public:
                case Token::Internal:
                case Token::Intrinsic:
                    var [ts1,nd1] = reservedNamespace (ts);
                    nd.ns = nd1;
                    var [ts1,nd1] = [ts1, nd];
                    break;
                default:
                    var [ts1,nd1] = primaryName (ts);
                    nd.ns = cx.evalIdentExprToNamespace (nd1);
                    var [ts1,nd1] = [ts1, nd];
                    break;
                }
                break;
            case localBlk:
                var [ts1,nd1] = [ts,nd];
                break;
            default:
                throw "error attribute tau " + tau;
            }

            exit("Parser::attribute ", ts1);
            return [ts1,nd1];
        }


        // PRAGMAS

        function pragmas (ts: TOKENS)
            : [TOKENS]
        {
            enter("Parser::pragmas ", ts);

            while (hd (ts)===Token::Use || hd (ts)===Token::Import) {
                [ts] = pragma (ts);
                ts = semicolon (ts,fullStmt);
            }

            var ts1 = ts;

            exit("Parser::pragmas ", ts1);
            return [ts1];
        }

        function pragma (ts: TOKENS)
            : [TOKENS]
        {
            enter("Parser::pragma ", ts);

            switch (hd (ts)) {
            case Token::Use:
                var [ts1] = pragmaItems (tl (ts));
                break;
            case Token::Import:
                var [ts1] = importName (tl (ts));
                break;
            }

            exit("Parser::pragma ", ts1);
            return [ts1];
        }

        function pragmaItems (ts: TOKENS)
            : [TOKENS]
        {
            enter("Parser::pragmaItems ", ts);

            var ts1 = ts;

            while (true) {
            switch (hd (ts1)) {
            case Token::Decimal:
                break;
            case Token::Namespace:
                var [ts1,nd1] = primaryName (tl (ts1));
                cx.openNamespace (nd1);
                break;
            case Token::Double:
                break;
            case Token::Int:
                break;
            case Token::Default:
                switch (hd (tl (ts1))) {
                case Token::Namespace:
                    var [ts1,nd1] = primaryName (tl (tl (ts1)));
                    cx.defaultNamespace (nd1);
                    cx.openNamespace (nd1);
                    break;
                default:
                    throw "unexpected token after 'use default'";
                }
                break;
                //            case Token::Number
                //                break;
            case Token::Precision:
                break;
            case Token::Rounding:
                break;
            case Token::Standard:
                break;
            case Token::Strict:
                break;
            case Token::UInt:
                break;
            case Token::Unit:
                break;
            default:
                throw "unknown token in PragmaItem";
            }

            if (hd (ts1) !== Token::Comma) {
                break;
            }

            ts1 = eat (ts1,Token::Comma);
            }

            exit("Parser::pragmaItems ", ts1);
            return [ts1];
        }

        /*

        ImportName
            PackageName  .  *
            PackageName  .  Identifier

        */

        function importName (ts: TOKENS)
            : [TOKENS]
        {
            enter("Parser::importName ", ts);

            var [ts1,nd1] = identifier (ts);
            nd1 = [nd1];
            while (hd (ts1)===Token::Dot) {
                nd1.push(Token::tokenText(tl (ts1).head()));
                ts1 = tl (tl (ts1));
            }

            let ns = namespaceFromPath (nd1);
            cx.openNamespace (ns);

            exit ("Parser::importName ", ts1);
            return [ts1];

            function namespaceFromPath (path) 
            {
                var str = "";
                for (var i=0; i<path.length-1; ++i) { // -1 to skip last ident
                    if (i!=0) 
                        str = str + ".";
                    str = str + path[i];
                }

                return new Ast::ReservedNamespace (new Ast::PublicNamespace (str));  // FIXME ReservedNamespace is a misnomer
            }

        }

        // BLOCKS and PROGRAMS

        function block (ts:TOKENS, tau: TAU)
            : [TOKENS, Ast::BLOCK]
        {
            enter("Parser::block ",ts);

            ts = eat (ts, Token::LeftBrace);
            cx.enterLetBlock ();
            var [ts1,nd1] = directives (ts, tau);
            let head = cx.exitLetBlock ();
            ts1 = eat (ts1, Token::RightBrace);

            exit ("Parser::block ", ts1);
            return [ts1, new Ast::Block (head,nd1)];
        }

        function program ()
            : [TOKENS, Ast::PROGRAM]
        {
            enter("Parser::program ","");

            let [ts,cs] = scan.tokenList (scan.start);
            coordList = cs;
            ts = new TokenStream (ts,0);

            cx.enterVarBlock ();
            var publicNamespace = new Ast::ReservedNamespace (new Ast::PublicNamespace (""));
            cx.openNamespace (publicNamespace);
            cx.defaultNamespace (publicNamespace);

            if (hd (ts) == Token::Internal || 
                hd (ts) == Token::Package)
            {
                var [ts1, nd1] = packages (ts);
            }
            else
            {
                var [ts1, nd1] = [ts, []];
            }

            currentPackageName = "";
            currentClassName = "";

            cx.enterLetBlock ();
            var [ts2,nd2] = directives (ts1, globalBlk);
            var bhead = cx.exitLetBlock ();
            var vhead = cx.exitVarBlock ();

            switch (hd (ts2)) {
            case Token::EOS:
                break;
            default:
                throw "extra tokens after end of program: " + ts2;
            }

            exit ("Parser::program ", ts2);
            return [ts2, new Ast::Program (nd1,new Ast::Block (bhead,nd2),vhead)];
        }
    }

    function test ()
    {
        var programs =
            [ "print('hi')"
              // , readFile ("./tests/self/t.es")
              /*
            , "x<y"
            , "x==y"
            , "m-n;n+m"
            , "10"
            , "p.q.r.x"
            , "q::id"
            , "f() ()"
            , "new A()"
            , "(new Fib(n-1)).val + (new Fib(n-2)).val"
            , "var x = 10, y = 20"
            , "var x = 10; var y"
            , "if (x) y; else z"
            , "new new x (1) (2) . x"
            , "var x : int = 10; var y: string = 'hi'"
            , "function f(x,y,z) { return 10 }"
            , "new new y"
            , "z (1) (2)"
            , "new new x (1) (2)"
            , "new new x (1) (2) . x"
            , "let x = 10"
            , "let const x"
            , "const x"
            , "x.y.z"
            , "while (x) { print(x); x-- }"
            , "function f (x=10) { return x }"
            , "function f (x) { return x }"
              , "x = y"
            , readFile ("./tests/self/prime.es")
              */
              /*
            , "class A { function A() {} }"
            , "class Fib { function Fib (n) { } }"
            , readFile ("./tests/self/hello.es")
            "a .< t .< u .< v > , w .< x > > > >",
            "q::[expr]",
            "(expr)::id",
            "(expr)::[expr]",
            "@a",
            "@q::id",
            "@q::[expr]",
            "@(expr)::id",
            "@(expr)::[expr]",
            "@[expr]",
            "/abcdefg/g",
            "/abcdefg/",
            "/abcdefg/i",
            "/abcdefg/x",
            "true",
            "false",
            "null",
            "(a)::x",
            "(function(a,b,c){})",
            "{x:a,y:b,z:c}",
            "[a,b,c]",
            "{(x):y}",
            "(function(){})",
            "(function f(a:A,b:B){})",
            "(function f.<T,U,V>(a:T,b:U,c:V){})",

            // type expressions

            "T",
            "?T",
            "T!",
            "T~",
            "T.<U>",
            "T.<U.<V>>",
            "T.<{a:A,t:{i:I,s:S}}>",
            "T.<{x:[A,B,C]}>",
            "T.<{x:(A,B,C)}>",
            "T.<U.<V.<W.<[,,,]>>>>",
            "T.<U>!",
            "?T.<U>",

            // Postfixx expressions

            "x.y",
            "new x",
            "new x()",
            "x()",
            "x.y()",
            "x++",
            "x--",
            "x.y++",
            "x.y()++",
            "new x.y++",
            */
        ]

        var n = 0;
            //        for each ( var p in programs )
        for (;n<programs.length;n++)
        {
            var p = programs[n];
            try {
                var parser = initParser(p);
                var [ts1,nd1] = parser.program ();

                //                dumpABCFile(cogen.cg(nd1), "hello-test.es");

                var tx1 = Encode::program (nd1);
                print(n, "-1> ", p, tx1);
                var nd2 = Decode::program (eval("("+tx1+")"));
                var tx2 = Encode::program (nd2);
                print(n, "-2> ", p, tx2);

                print("tx1.length=",tx1.length);
                print("tx2.length=",tx2.length);
                for (var i = 0; i < tx1.length; ++i) {
                    if (tx1[i] != tx2[i]) throw "error at pos "+i+" "+tx1[i]+ " != "+tx2[i]+" prefix: "+tx1.slice(i,tx1.length);
                }
                print("txt==tx2");
            }
            catch(x)
            {
                print(x)
            }
        }
    }

    //Parse::test ()
}

