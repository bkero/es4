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

namespace Parse;

{
    use default namespace Parse;
    use namespace intrinsic;
    use namespace Release;

    type PATTERNS = [PATTERN];
    type PATTERN =
          ( ObjectPattern
          , ArrayPattern
          , SimplePattern
          , IdentifierPattern );

    type FIELD_PATTERNS = [FIELD_PATTERN];
    type FIELD_PATTERN = FieldPattern;

    class FieldPattern {
        use default namespace public;
        const ident: Ast::IDENT_EXPR;
        const ptrn: PATTERN;
        function FieldPattern (ident,ptrn)
            : ident = ident
            , ptrn = ptrn { }
    }

    class ObjectPattern {
        const ptrns //: FIELD_PATTERNS;
        function ObjectPattern (ptrns)
            : ptrns = ptrns { }
    }

    class ArrayPattern { 
        const ptrns //: PATTERNS;
        function ArrayPattern (ptrns)
            : ptrns = ptrns { }
    }

    class SimplePattern
    {
        const expr : Ast::EXPR;
        function SimplePattern (expr)
            : expr = expr { }
    }

    class IdentifierPattern
    {
        const ident : Ast::IDENT;
        function IdentifierPattern (ident)
            : ident = ident { }
    }

    type ALPHA = (NoColon, AllowColon);
    class NoColon {}
    class AllowColon {}
    const noColon = new NoColon;
    const allowColon = new AllowColon;

    type BETA = (NoIn, AllowIn);
    class NoIn {}
    class AllowIn {}
    const noIn = new NoIn;
    const allowIn = new AllowIn;

    type GAMMA = (NoExpr, AllowExpr);
    class NoExpr {}
    class AllowExpr {}
    const noExpr = new NoExpr;
    const allowExpr = new AllowExpr;

    type TAU = (GlobalBlk, ClassBlk, InterfaceBlk, LocalBlk);
    class GlobalBlk {}
    class ClassBlk {}
    class InterfaceBlk {}
    class LocalBlk {}
    const globalBlk = new GlobalBlk;
    const classBlk = new ClassBlk;
    const interfaceBlk = new InterfaceBlk;
    const localBlk = new LocalBlk;

    /*
    const AbbrevIfElse = 0;
    const AbbrevDoWhile = AbbrevIfElse + 1;
    const AbbrevFunction = AbbrevDoWhile + 1;
    const Abbrev = AbbrevFunction + 1;
    const Full = Abbrev + 1;
    */

    type OMEGA = (FullStmt, AbbrevStmt);
    class FullStmt {}
    class AbbrevStmt {}
    const fullStmt = new FullStmt;
    const abbrevStmt = new AbbrevStmt;

    type ENV = [Ast::FIXTURES];

    type PRAGMAS = Pragmas;
    class Pragmas 
    {
        var openNamespaces //: [[Ast::NAMESPACE]];
        var defaultNamespace: Ast::NAMESPACE;
        function Pragmas (pragmas) 
        {
            enter ("Pragma ",pragmas);
            if (pragmas===null)
            {
                this.openNamespaces = [[]];
                this.defaultNamespace = new Ast::InternalNamespace ("");
            }
            else
            {
                this.openNamespaces = util.copyArray (pragmas.openNamespaces);
                this.defaultNamespace = pragmas.defaultNamespace;
            }

            if (this.openNamespaces[this.openNamespaces.length-1].length !== 0) { 
                this.openNamespaces.push ([]);  // otherwise reuse the last one pushed
            }
            exit ("Pragma");
        }
    }

    type PRAGMA_ENV = [Ast::PRAGMAS];

    class Context
    {
        var env: ENV;
        var varHeads  //: [Ast::HEAD];
        var letHeads  //: [Ast::HEAD];
        var ctor: Ast::CTOR;
        var pragmas: PRAGMAS;
        var pragmaEnv: PRAGMA_ENV; // push one PRAGMAS for each scope

        function Context (topFixtures)
            : env = [topFixtures]
            , varHeads = []
            , letHeads = [] 
            , ctor = null
            , pragmas = null
            , pragmaEnv = []
        {
            //print ("topFixtures.length=",topFixtures.length);
            //            print ("env[0].length=",env[0].length);
        }

        function enterVarBlock () 
        {
            enter ("enterVarBlock");
            let varHead = {fixtures:[],inits:[]};
            this.varHeads.push(varHead);
            this.env.push (varHead.fixtures);
            this.pragmas = new Pragmas (this.pragmas);
            this.pragmaEnv.push (this.pragmas);
            exit ("exitVarBlock");
        }

        function exitVarBlock () 
        {
            enter ("exitVarBlock");
            let varHead = this.varHeads.pop ();
            this.env.pop ();
            this.pragmaEnv.pop ();
            if (this.pragmaEnv.length === 0) {
                this.pragmas = null;
            }
            else {
                this.pragmas = this.pragmaEnv[this.pragmaEnv.length-1];
            }
            exit ("exitVarBlock");
            return varHead;
        }

        function addVarFixtures (fxtrs) 
        {
            let varHead = this.varHeads[this.varHeads.length-1];
            for (let n = 0, len = fxtrs.length; n < len; ++n)  // until array conact works
                varHead.fixtures.push (fxtrs[n]);
        }

        function addVarInits (inits) 
        {
            let varHead = this.varHeads[this.varHeads.length-1];
            for (let n = 0, len = inits.length; n < len; ++n)  // until array conact works
                varHead.inits.push (inits[n]);
        }

        function enterLetBlock () 
        {
            enter ("enterLetBlock");
            let letHead = {fixtures:[],inits:[]};
            this.letHeads.push(letHead);
            this.env.push (letHead.fixtures);
            this.pragmas = new Pragmas (this.pragmas);
            this.pragmaEnv.push (this.pragmas);
            exit ("enterLetBlock");
        }

        function exitLetBlock () 
        {
            enter ("exitLetBlock");
            let letHead = this.letHeads.pop ();
            this.env.pop ();
            this.pragmaEnv.pop ();
            this.pragmas = this.pragmaEnv[this.pragmaEnv.length-1];
            exit ("exitLetBlock");
            return letHead;
        }

        function addLetFixtures (fxtrs) 
        {
            let letHead = this.letHeads[this.letHeads.length-1];
            for (let n = 0, len = fxtrs.length; n < len; ++n)  // until array conact works
                letHead.fixtures.push (fxtrs[n]);
        }

        function addLetInits (inits) 
        {
            let letHead = this.letHeads[this.letHeads.length-1];
            for (let n = 0, len = inits.length; n < len; ++n)  // until array conact works
                letHead.inits.push (inits[n]);
        }

        function openNamespace (nd: Ast::IDENT_EXPR) {
            enter ("openNamespace");
            let ns = evalIdentExpr (nd);
            //print("this.pragmas=",this.pragmas);
            let opennss = this.pragmas.openNamespaces;
            //print ("opennss=",opennss);
            //print ("opennss.length=",opennss.length);
            //print ("adding ns ",ns);
            opennss[opennss.length-1].push (ns);
            exit ("openNamespace");
        }

        function defaultNamespace (nd: Ast::IDENT_EXPR) {
            enter ("defaultNamespace");
            let ns = evalIdentExpr (nd);
            this.pragmas.defaultNamespace = ns;
            exit ("defaultNamespace");
        }

        function hasName (fxtrs,id,ns) 
        {
            enter ("hasName ",id);
            if (fxtrs.length===0) 
            {
                exit ("hasName false");
                return false;
            }

            let pn = fxtrs[0][0];
            //print ("pn..id=",pn.Ast::name.id," id=",id);
            //print ("pn..ns=",pn.Ast::name.ns.Ast::hash()," ns=",ns.Ast::hash());
            if (pn.Ast::name.id==id && pn.Ast::name.ns.Ast::hash()==ns.Ast::hash())  // FIXME: need ns compare
            {
                exit ("hasName true");
                return true;
            }
            else 
            {
                exit ("hasName looking");
                return hasName (fxtrs.slice (1,fxtrs.length),id,ns);
            }
        }

        function getFixture (fxtrs,id,ns) 
        {
            enter ("getFixture ");
            if (fxtrs.length===0) 
            {
                throw "name not found " + ns + "::" + id;
            }

            let pn = fxtrs[0][0];
            if (pn.Ast::name.id==id && pn.Ast::name.ns.toString()==ns.toString()) 
            {
                exit ("getFixture");
                return fxtrs[0][1];
            }
            else 
            {
                exit ("getFixture");
                return getFixture (fxtrs.slice (1,fxtrs.length),id,ns);
            }
        }

        /*

        two dimensional search

        repeat for each shadowed name
            each name in each head
                dup is error

        for each namespace set
            find all names in the inner most head

        */

        function findFixtureWithNames (id,nss) {
            enter ("findFixtureWithNames");
            // for each name in each head
            let env = this.env;
            //print ("env.length=",env.length);
            for (var i=env.length-1; i>=0; --i)   // for each head
            {
                let ns = null;
                let fxtrs = env[i];
                //print ("nss.length=",nss.length);
                for (var j=nss.length-1; j>=0; --j)   // for each name
                {
                    //print ("nss[",j,"]=",nss[j]);
                    if (hasName (fxtrs,id,nss[j])) 
                    {
                        if (ns !== null) {
                            throw "ambiguous reference to " + id;
                        }
                        ns = nss[j];
                    }
                }
                if (ns!==null) {
                    exit ("findFixtureWithNames");
                    return getFixture (fxtrs,id,ns);
                }
            }
            exit ("findFixtureWithNames");
            return null;
        }

        function findFixtureWithIdentifier (id: Ast::IDENT)
        {
            enter ("findFixtureWithIdentifier ", id);
            //print ("this.pragmas=",this.pragmas);
            let nsss = this.pragmas.openNamespaces;
            //print ("nsss.length=",nsss.length);
            for (var i=nsss.length-1; i>=0; --i) 
            {
                //print ("nsss[",i,"]=",nsss[i]);
                let fx = findFixtureWithNames (id,nsss[i]);
                if (fx !== null) 
                {
                    exit ("findFixtureWithIdentifier");
                    return fx;
                }
            }
            throw "fixture not found: " + id;
        }

        function evalIdentExpr (nd: Ast::IDENT_EXPR)
            : Ast::NAMESPACE
        {
            enter ("evalIdentExpr");
            switch type (nd) {
            case (nd: Ast::Identifier) {
                let fxtr = findFixtureWithIdentifier (nd.Ast::ident);
                switch type (fxtr) {
                case (fxtr:Ast::NamespaceFixture) {
                    var val = fxtr.Ast::ns;
                }
                case (fxtr:*) {
                    throw "fixture with unknown value " + fxtr;
                }
                }
            }
            case (nd: Ast::ReservedNamespace) {
                var val = nd.Ast::ns;
            }
            case (nd: Ast::QualifiedIdentifier) {
                throw "evalIdentExpr: case not implemented " + nd;
            }
            }
            exit ("evalIdentExpr ", val);
            return val;
        }
    };

    type TOKENS = TokenStream;  // [int];
    class TokenStream {
        var ts: Array;
        var n: int;
        function TokenStream (ts,n)
            : ts = ts
            , n = n { }
        
        function head () : int{
            return ts[n];
        }
        
        function next () : void {
            ++n;
        }

        public function toString () { return ts.slice(n,ts.length).toString() }
    }

    class Parser
    {
        var scan : Lexer::Scanner;
        var cx: Context;

        public function Parser(src,topFixtures=[])
            : cx = new Context (topFixtures)
            , scan = new Lexer::Scanner (src)
        {
        }

        var defaultNamespace: Ast::NAMESPACE;
        var currentPackageName: string;
        var currentClassName: string;

        private var coordList;

        function hd (ts:TokenStream) 
        {
            //enter ("hd ",ts.head());
            var tk = Token::tokenKind (ts.head());
            //exit ("hd ",tk);
            return tk;
        }

        function eat (ts:TokenStream,tc) {
            //print("eating ",Token::tokenText(tc));
            let tk = hd (ts);
            if (tk === tc) {
                return tl (ts);
            }
            throw "expecting "+Token::tokenText(tc)+" found "+Token::tokenText(tk);
        }

        /*
          Replace the first token in the stream with another one. Raise an exception
          if the first token is not of a specified kind.
        */

        function swap (ts,t0,t1) {
            let tk = hd (ts);
            if (tk === t0) {
                return ts.ts[ts.n] = t1;
            }
            throw "expecting "+Token::tokenText(t0)+" found "+Token::tokenText(tk);
        }

        function tl (ts:TOKENS) : TOKENS {
            return new TokenStream (ts.ts,ts.n+1);
        }  //ts.slice (1,ts.length);

        /*

        var x = y    [x], init var () [x=y]
        x = y        [],  set x=y
        var [x] = y  [x], init var ([t0],[t0=y]) x=t0[0]
        [x] = y      [],  let ([t0],[t0=y]) x=t0[0]

        let (x=y) ...  let ([x], init let () [x=y]) ...
        let x=y             [x], init let () [x=y]


        assignment, create a let for each aggregate, a temp for
        each level of nesting

        x = y              set x=y
        [x] = y            let (t0=y) set x=t0[0]
        [[x]] = y          let (t0=y) let (t1=t0[0]) set x=t1[0]
        [[x],[x]] = y      let (t0=y) let (t1=t0[0]) set x=t1[0]
                                    , let (t1=t0[1]) set x=t1[0]
        
        initialization, create an init rather than a set for the
        leaf nodes

        var x = v            let (t0=v) init () [x=t0]
        var [x] = v          let (t0=v) init () [x=t0[0]]
        var [x,[y,z]] = v    let (t0=v) init () [x=t0[0]]
                                      , let (t1=t0[1]) init () [y=t1[0], z=t1[1]]
        var [x,[y,[z]]] = v  let (t0=v) init () [x=t0[0]]
                                      , let (t1=t0[1]) init () [y=t1[0]
                                                     , let (t2=t1[0]) init () [z=t2[0]]

        for initialization, we need to know the namespace and the target 
        so we make INITS to go into the InitExpr inside the LetExpr

        let x = y          init x=y

        flattening.

        var [x,[y,z]] = v  let (t0=v) init () [x=t0[0]]
                                    , let (t1=t0[1]) init () [y=t1[0], z=t1[0]]

                           t0=v
                           x=t0[0]
                           t1=t0[1]
                           y=t1[0]
                           z=t1[1]
        head = {[t0,x,t1,y,z],

        flattening doesn't work because it mixes named and temporary
        fixtures

        lets and params have the same problem. both allow destructuring
        patterns that can expand into a nested expression.

        let ([x,[y,z]]=v) ...

        top heads only have named fixtures. sub heads only have temporaries.
        temporaries are always immediately initialized. a head is a list of
        fixtures and a list of expressions. the expressions get evaluated
        in the scope outside the head.

        settings is a sub head. it has temporary fixtures and init exprs that
        target instance variables

        */

        function desugarAssignmentPattern (p: PATTERN, t: Ast::TYPE_EXPR, e: Ast::EXPR, op: Ast::ASSIGNOP)
            : [Ast::FIXTURES, Ast::EXPR]
            desugarPattern (p,t,e,null,null,null,op);

        function desugarBindingPattern (p: PATTERN, t: Ast::TYPE_EXPR, e: Ast::EXPR,
                                        ns: Ast::NAMESPACE?, it: Ast::INIT_TARGET?, ro: boolean?)
            : [Ast::FIXTURES, Ast::EXPR]
            desugarPattern (p,t,e,ns,it,ro,null);

        function desugarPattern (p: PATTERN, t: Ast::TYPE_EXPR, e: Ast::EXPR,
                                 ns: Ast::NAMESPACE?, it: Ast::INIT_TARGET?, ro: boolean?, op: Ast::ASSIGNOP?)
            : [Ast::FIXTURES, Ast::EXPR]
        {
            return desugarSubPattern (p,t,e,0);

            function desugarSubPattern (p: PATTERN, t: Ast::TYPE_EXPR, e: Ast::EXPR, n: int) 
                : [Ast::FIXTURES, Ast::EXPR]
            {
                switch type (p) : PATTERN {
                case (p:IdentifierPattern) {
                    let nm = new Ast::PropName ({ns:ns,id:p.ident});
                    let fx = new Ast::ValFixture (t,ro);
                    var fxtrs = [[nm,fx]];
                    if (e !== null) {
                        var inits = [[nm,e]];
                    }
                    else {
                        var inits = [];
                    }
                    var expr = new Ast::InitExpr (it, new Ast::Head ([],[]), inits);
                }
                case (p:SimplePattern) {
                    if (e === null) throw "simple pattern without initializer";
                    var fxtrs = [];
                    var expr = new Ast::SetExpr (op,p.expr,e);
                }
                case (p: (ArrayPattern, ObjectPattern)) {
                    let tn = new Ast::TempName (n);
                    var fxtrs = [];
                    let exprs = [];
                    let ptrns = p.ptrns;
                    for (let i=0; i<ptrns.length; ++i) {
                        let sub = ptrns[i];
                        switch type (sub) {
                        case (pat: FieldPattern) {
                            var typ = new Ast::FieldTypeRef (t,sub.ident);
                            var exp = new Ast::ObjectRef (new Ast::GetTemp (n), sub.ident);
                            var pat = sub.ptrn;
                        }
                        case (pat: *) {
                            var typ = new Ast::ElementTypeRef (t,i);
                            var exp = new Ast::ObjectRef (new Ast::GetTemp (n), new Ast::Identifier (i,[[Ast::noNS]]));
                                      // FIXME what is the ns of a temp and how do we refer it
                            var pat = sub;
                        }
                        }

                        let [fx,ex] = desugarSubPattern (pat,typ,exp,n+1);
                        for (let n in fx) fxtrs.push(fx[n]);
                        exprs.push(ex);
                    }
                    let head = new Ast::Head ([[tn,new Ast::ValFixture (Ast::anyType,false)]],[[tn,e]]);
                    var expr = new Ast::LetExpr (head, new Ast::ListExpr (exprs));
                }
                case (x: *) {
                    throw "internal error: desugarPattern " + p;
                }
                }
                return [fxtrs,expr];
            }
        }

        // Parse rountines

        /*

        Identifier
            Identifier
            call
            debugger
            dynamic
            each
            final
            get
            goto
            include
            namespace
            native
            override
            prototype
            set
            static
            type
            xml

        */

        public function identifier (ts: TOKENS)
            : [TOKENS, Ast::IDENT]
        {
            enter("Parser::identifier ", ts);

            var str = "";   // fixme: evaluator isn't happy if this is inside of the switch

            switch (hd (ts)) {
            case Token::Identifier:
            case Token::Call:
            case Token::Cast:
            case Token::Const:
            case Token::Decimal:
            case Token::Double:
            case Token::Dynamic:
            case Token::Each:
            case Token::Eval:
            case Token::Final:
            case Token::Get:
            case Token::Has:
            case Token::Implements:
            case Token::Import:
            case Token::Int:
            case Token::Interface:
            case Token::Internal:
            case Token::Intrinsic:
            case Token::Is:
            case Token::Let:
            case Token::Namespace:
            case Token::Native:
            case Token::Number:
            case Token::Override:
            case Token::Package:
            case Token::Precision:
            case Token::Private:
            case Token::Protected:
            case Token::Prototype:
            case Token::Public:
            case Token::Rounding:
            case Token::Standard:
            case Token::Strict:
            case Token::Set:
            case Token::Static:
            case Token::To:
            case Token::Type:
            case Token::UInt:
            case Token::Undefined:
            case Token::Use:
            case Token::Xml:
            case Token::Yield:
                var str = Token::tokenText (ts.head());
                break;
            default:
                throw "expecting identifier, found " + Token::tokenText (ts.head());
            }
            exit ("Parser::identifier ", str);
            return [tl (ts), str];
        }

        function isReserved (tk: int) {
            switch (tk) {
            case Token::Break:
                break;
            // FIXME more of these
            default:
                return false;
                break;
            }
        }

        function reservedOrOrdinaryIdentifier (ts: TOKENS)
            : [TOKENS, Ast::IDENT]
        {
            enter ("Parser::reservedOrOrdinaryIdentifer");

            if (isReserved (hd (ts))) 
            {
                var [ts1,nd1] = Token::tokenText (hd (ts));
            }
            else 
            {
                var [ts1,nd1] = identifier (ts);
            }

            exit ("Parser::reservedOrOrdinaryIdentifier");
            return [ts1,nd1];
        }

        /*
            Qualifier
                *
                ReservedNamespace
                Identifier
        */

        function qualifier(ts)
            : [TOKENS, (Ast::IDENT,Ast::NAMESPACE)]
        {
            enter("Parser::qualifier ",ts);

            switch (hd(ts)) {
            case Token::Internal:
            case Token::Intrinsic:
            case Token::Private:
            case Token::Protected:
            case Token::Public:
                var [ts1,nd1] = reservedNamespace(ts);
                break;
            case Token::Mult:
                let id = Token::tokenText (ts.head());
                var [ts1,nd1] = [tl (ts), id];
                break;
            default:
                var [ts1,nd1] = identifier (ts);
                break;
            }

            exit("Parser::qualifier ",nd1);
            return [ts1,nd1];
        }

        /*
            ReservedNamespace
                internal
                intrinsic
                private
                protected
                public
        */

        function reservedNamespace (ts: TOKENS)
			: [TOKENS, Ast::NAMESPACE]
        {
            enter("Parser::reservedNamespace ", ts);

            switch (hd (ts)) {
            case Token::Internal:
                var [ts1,nd1] = [tl (ts), new Ast::InternalNamespace (current_package)];
                break;
            case Token::Public:
                var [ts1,nd1] = [tl (ts), new Ast::PublicNamespace (current_package)];
                break;
            case Token::Intrinsic:
                var [ts1,nd1] = [tl (ts), new Ast::IntrinsicNamespace];
                break;
            case Token::Private:
                var [ts1,nd1] = [tl (ts), new Ast::PrivateNamespace (current_class)];
                break;
            case Token::Protected:
                var [ts1,nd1] = [tl (ts), new Ast::ProtectedNamespace (current_class)];
                break;
            }

            exit("Parser::reservedNamespace ", ts1);
            return [ts1,nd1];
        }

        /*
          QualifiedNameIdentifier
              *
              Identifier
              ReservedIdentifier
              String
              Number
              Brackets
        */

        function qualifiedNameIdentifier (ts1: TOKENS, nd1: Ast::EXPR)
            : [TOKENS, Ast::IDENT_EXPR]
        {
            enter("Parser::qualifiedNameIdentifier ", ts1);

            switch (hd(ts1)) {
                case Token::Mult:
                    var [ts2,nd2] = [tl(ts1), "*"];
                    var [ts3,nd3] = [ts1, new Ast::QualifiedIdentifier (nd1,nd2)];
                    break;
                case Token::StringLiteral:
                case Token::DecimalLiteral:
                    let str = Token::tokenText (ts1.head());
                    var [ts2,nd2] = [tl(ts1), str];
                    var [ts3,nd3] = [ts1, new Ast::QualifiedIdentifier (nd1,nd2)];
                    break;
                case Token::LeftBracket:
                    var [ts2,nd2] = brackets (ts1);
                    var [ts3,nd3] = [ts1, new Ast::QualifiedExpression (nd1,nd2)];
                    break;
                default:
                    var [ts2,nd2] = identifier (ts1);
                    var [ts3,nd3] = [ts2, new Ast::QualifiedIdentifier (nd1,nd2)];
                    break;
            }

            exit("Parser::qualifiedNameIdentifier ", nd3);
            return [ts3,nd3];
        }

        /*
          SimpleQualifiedName
              Identifier
              Qualifier  ::  QualifiedNameIdentifier
        */

        function simpleQualifiedName (ts: TOKENS)
            : [TOKENS, Ast::IDENT_EXPR]
        {
            enter ("Parser::simpleQualifiedName ", ts);

            var [ts1,nd1] = qualifier (ts);
            switch (hd (ts1)) {
            case Token::DoubleColon:
                switch type (nd1) {
                case (ns:Ast::NAMESPACE) {
                    nd1 = new Ast::LiteralExpr (new Ast::LiteralNamespace (nd1));
                }
                case (id:Ast::IDENT) {
                    nd1 = new Ast::LexicalRef (new Ast::Identifier (nd1,cx.pragmas.openNamespaces))
                }
                }
                var [ts2,nd2] = qualifiedNameIdentifier (tl(ts1), nd1);
                break;
            default:
                switch type (nd1) {
                case (ns:Ast::NAMESPACE) {
                    var [ts2,nd2] = [ts1,new Ast::ReservedNamespace (nd1)];
                }
                case (id:Ast::IDENT) {
                    var [ts2,nd2] = [ts1,new Ast::Identifier (nd1,cx.pragmas.openNamespaces)];
                }
                }
                break;
            }

            exit ("Parser::simpleQualifiedName ", ts2);
            return [ts2,nd2];
        }

        /*
            ExpressionQualifiedName
                ParenListExpression :: QualifiedName
        */

        /*
            NonAttributeQualifiedIdentifier
                SimpleQualifiedName
                ExpressionQualifiedName
        */

        function nonAttributeQualifiedName (ts: TOKENS)
            : [TOKENS, Ast::IDENT_EXPR]
        {
            enter("Parser::nonAttributeQualifiedName ", ts);

            switch (hd (ts)) {
            case Token::LeftParen:
                var [ts1,nd1] = expressionQualifiedIdentifier (ts);
                break;
            default:
                var [ts1,nd1] = simpleQualifiedName (ts);
            }

            exit ("Parser::nonAttributeQualifiedName ", ts1);
            return [ts1,nd1];
        }

        /*
            AttributeQualifiedIdentifier
                @ Brackets
                @ NonAttributeQualifiedIdentifier
        */

        /*
            QualifiedName
                AttributeName
                NonAttributeQualifiedName
        */

        function qualifiedName (ts: TOKENS)
            : [TOKENS, Ast::IDENT_EXPR]
        {
            enter("Parser::qualifiedName ", ts);

            switch (hd (ts)) {
            case Token::LeftParen:
                var [ts1,nd1] = expressionQualifiedIdentifier (ts);
                break;
            default:
                var [ts1,nd1] = simpleQualifiedName (ts);
            }

            exit ("Parser::qualifiedName ", ts1);
            return [ts1,nd1];
        }

        /*
            PropertyName
                NonAttributeQualifiedName
                NonAttributeQualifiedName  .<  TypeExpressionList  >
                (  TypeExpression  )  .<  TypeExpressionList  >

            e.g.
                A.<B.<C.<t>,D.<u>>>
        */

        function propertyName (ts: TOKENS)
            : [TOKENS, Ast::IDENT_EXPR]
        {
            enter("Parser::propertyName ", ts);

            switch (hd (ts)) {
/*  FIXME: this is a grammar bug
            case Token::LeftParen:
                var [ts1,nd1] = typeExpression (tl (ts));
                ts1 = eat (ts1,Token::RightParen);
                break;
*/
            default:
                var [ts1,nd1] = nonAttributeQualifiedName (ts);
            }

            switch (hd (ts1)) {
            case Token::LeftDotAngle:
                var [ts2,nd2] = typeExpressionList (tl (ts1));
                switch (hd (ts2)) {
                case Token::UnsignedRightShift:
                    // downgrade >>> to >> to eat one >
                    ts2 = swap (ts2,Token::UnsignedRightShift,Token::RightShift);
                    break;
                case Token::RightShift:
                    // downgrade >> to > to eat one >
                    ts2 = swap (ts2,Token::RightShift,Token::GreaterThan);
                    break;
                default:
                    ts2 = eat (ts2,Token::GreaterThan);
                    break;
                }
                break;
            default:
                var [ts2,nd2] = [ts1,nd1];
                break;
            }

            exit ("Parser::propertyName ", ts2);
            return [ts2,nd2];
        }

        /*
            PrimaryName
                Path  .  PropertyName
                PropertyName
        */

        function primaryName (ts: TOKENS)
            : [TOKENS, Ast::IDENT_EXPR]
        {
            enter("Parser::primaryName ", ts);

            switch (hd (ts)) {
            case Token::Identifier:
                switch (hd (tl (ts))) {
                case Token::Dot:
                    var [ts1,nd1] = path (tl (tl (ts)), [Token::tokenText(ts.head())]);
                    var [ts2,nd2] = propertyName (ts1);
                    nd2 = new Ast::UnresolvedPath (nd1,nd2);
                    break;
                default:
                    var [ts2,nd2] = propertyName (ts);
                    break;
                }
                break;
            default:
                var [ts2,nd2] = propertyName (ts);
                break;
            }

            exit ("Parser::primaryName ", ts2);
            return [ts2,nd2];
        }

        /*
            Path
                Identifier
                Path  .  Identifier
        */

        function path (ts: TOKENS, nd /*: [Ast::IDENT]*/ )  /* FIXME: verifier bug */
            : [TOKENS, [Ast::IDENT]]
        {
            enter("Parser::path ", ts);

            switch (hd (ts)) {
            case Token::Identifier:
                switch (hd (tl (ts))) {
                case Token::Dot:
                    nd.push(Token::tokenText(ts.head()));
                    var [ts1,nd1] = path (tl (tl (ts)), nd);
                    break;
                default:
                    var [ts1,nd1] = [ts,nd];
                    break;
                }
                break;
            default:
                var [ts1,nd1] = [ts,nd];
                break;
            }

            exit ("Parser::path ", ts1);
            return [ts1,nd1];
        }

        (1/2)

        function parenExpression (ts: TOKENS)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::parenExpression ", ts);

            var ts1 = eat (ts,Token::LeftParen);
            var [ts2,ndx] = assignmentExpression (ts1, allowIn);
            var tsx = eat (ts2,Token::RightParen);

            exit ("Parser::parenExpression ", tsx);
            return [tsx, ndx];
        }

        function parenListExpression (ts: TOKENS)
            : [TOKENS, [Ast::EXPR]]
        {
            enter("Parser::parenListExpression ", ts);

            var ts1 = eat (ts,Token::LeftParen);
            var [ts2,ndx] = listExpression (ts1, allowIn);
            var tsx = eat (ts2,Token::RightParen);

            exit ("Parser::parenListExpression ", tsx);
            return [tsx, ndx];
        }

        /*

        ObjectLiteral(noColon)
            {  FieldList  }

        ObjectLiteral(allowColon)
            {  FieldList  }
            {  FieldList  }  :  TypeExpression

        */

        function objectLiteral (ts: TOKENS /*, alpha: ALPHA*/)
            : [TOKENS, Ast::TYPE_EXPR]
        {
            enter("Parser::objectLiteral ", ts);

            var alpha: ALPHA = allowColon;    // FIXME need to get this from caller
            ts = eat (ts,Token::LeftBrace);
            var [ts1,nd1] = fieldList (ts);
            ts1 = eat (ts1,Token::RightBrace);
            switch (alpha) {
            case allowColon:
                switch (hd (ts1)) {
                case Token::Colon:
                    var [ts2,nd2] = typeExpression (tl (ts1));
                    break;
                default:
                    var [ts2,nd2] = [ts1,new Ast::ObjectType ([])]; // FIXME I mean {*}
                    break;
                }
                break;
            default:
                var [ts2,nd2] = [ts1,new Ast::ObjectType ([])]; // FIXME I mean {*}
                break;
            }

            exit("Parser::objectLiteral ", ts2);
            return [ts2,new Ast::LiteralExpr (new Ast::LiteralObject (nd1,nd2))];
        }

        /*

        FieldList
            empty
            LiteralField
            LiteralField  ,  LiteralFieldList

        */

        function fieldList (ts: TOKENS)
            //            : [TOKENS, [Ast::FIELD_TYPE]]
        {
            enter("Parser::fieldList ", ts);

            var nd1 = [];
            var ts1 = ts;

            if (hd (ts) !== Token::RightBrace) 
            {
                var [ts1,ndx] = literalField (ts);
                nd1.push (ndx);
                while (hd (ts1) === Token::Comma) {
                    var [ts1,ndx] = literalField (tl (ts1));
                    nd1.push (ndx);
                }
            }

            exit ("Parser::fieldList ", ts1);
            return [ts1,nd1];
        }

        /*

          LiteralField
              FieldKind  FieldName  :  AssignmentExpressionallowColon, allowIn
              get  FieldName  FunctionSignature  FunctionExpressionBodyallowColon, allowIn
              set  FieldName  FunctionSignature  FunctionExpressionBodyallowColon, allowIn

        */

        function literalField (ts: TOKENS)
            : [TOKENS, Ast::FIELD_TYPE]
        {
            enter ("Parser::literalField");

            switch (hd (ts)) {
            case Token::Const:
                var [ts1,nd1] = [tl (ts), constTag];
                break;
            default:
                var [ts1,nd1] = [ts,Ast::varTag];
                break;
            }

            var [ts2,nd2] = fieldName (ts);
            ts2 = eat (ts2,Token::Colon);
            var [ts3,nd3] = assignmentExpression (ts2,allowIn);

            exit ("Parser::literalField", ts3);
            return [ts3, new Ast::LiteralField (nd1,nd2,nd3)];
        }

        /*

        FieldName
            NonAttributeQualifiedName
            StringLiteral
            NumberLiteral
            ReservedIdentifier

        */

        function fieldName (ts: TOKENS)
            : [TOKENS, Ast::IDENT_EXPR]
        {
            enter ("Parser::fieldName");

            switch (hd (ts)) {
            case Token::StringLiteral:
                var [ts1,nd1] = [tl (ts), new Ast::Identifier (Token::tokenText (ts.head()),cx.pragmas.openNamespaces)];
                break;
            case Token::DecimalLiteral:
            case Token::DecimalIntegerLiteral:
            case Token::HexIntegerLiteral:
                throw "unsupported fieldName " + hd(ts);
                break;
            default:
                if (isReserved (hd (ts))) {
                    var [ts1,nd1] = [tl (ts), new Ast::Identifier (Token::tokenText (ts.head()),cx.pragmas.openNamespaces)];
                                     // NOTE we use openNamespaces here to indicate that the name is 
                                     //      unqualified. the generator should use the expando namespace,
                                     //      which is probably Public "".
                }
                else {
                    var [ts1,nd1] = nonAttributeQualifiedName (ts);
                }
                break;
            }

            exit ("Parser::fieldName");
            return [ts1,nd1];
        }

        /*

        ArrayLiteral(noColon)
            [  Elements  ]
        
        ArrayLiteral(allowColon)
            [  Elements  ]
            [  Elements  ]  :  TypeExpression
        
        Elements
            ElementList
            ElementComprehension

        */

        function arrayLiteral (ts: TOKENS)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::arrayLiteral ", ts);

            ts = eat (ts,Token::LeftBracket);
            var [ts1,nd1] = elementList (ts);
            ts1 = eat (ts1,Token::RightBracket);

            exit ("Parser::arrayLiteral ", ts1);
            return [ts1, new Ast::LiteralExpr (new Ast::LiteralArray (nd1,new Ast::ArrayType ([])))];
        }

        /*

        ElementList
            empty
            LiteralElement
            ,  ElementList
             LiteralElement  ,  ElementList

        LiteralElement
            AssignmentExpression(allowColon,allowIn)

        */

        function elementList (ts: TOKENS)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::elementList ", ts);

            var nd1 = [];
            var ts1 = ts;

            if (hd (ts) !== Token::RightBracket) 
            {
                switch (hd (ts)) {
                case Token::Comma:
                    var [ts1,ndx] = [tl (ts),new Ast::LiteralExpr (new Ast::LiteralUndefined)];
                    break;
                default:
                    var [ts1,ndx] = assignmentExpression (ts,allowIn);
                    break;
                }
                nd1.push (ndx);
                while (hd (ts1) === Token::Comma) {
                    ts1 = eat (ts1,Token::Comma);
                    switch (hd (ts1)) {
                    case Token::Comma:
                        var [ts1,ndx] = [ts1,new Ast::LiteralExpr (new Ast::LiteralUndefined)];
                        break;
                    case Token::RightBracket:
                        continue;  // we're done
                    default:
                        var [ts1,ndx] = assignmentExpression (ts1,allowIn);
                        break;
                    }
                    nd1.push (ndx);
                }
            }

            exit ("Parser::elementList ", ts1);
            return [ts1, nd1];
        }

        /*

        PrimaryExpression
            null
            true
            false
            NumberLiteral
            StringLiteral
            this
            RegularExpression
            XMLInitialiser
            ParenListExpression
            ArrayLiteral
            ObjectLiteral
            FunctionExpressionb
            AttributeIdentifier
            PrimaryIdentifier
        */

        function primaryExpression(ts:TOKENS)
            : [TOKENS,Ast::EXPR]
        {
            enter("Parser::primaryExpression ",ts);

            switch (hd (ts)) {
            case Token::Null:
                var [ts1,nd1] = [tl (ts), new Ast::LiteralExpr (new Ast::LiteralNull ())];
                break;
            case Token::True:
                var [ts1,nd1] = [tl (ts), new Ast::LiteralExpr (new Ast::LiteralBoolean (true))];
                break;
            case Token::False:
                var [ts1,nd1] = [tl (ts), new Ast::LiteralExpr (new Ast::LiteralBoolean (false))];
                break;
            case Token::DecimalLiteral:
                var [ts1,nd1] = [tl (ts), new Ast::LiteralExpr (new Ast::LiteralDecimal (Token::tokenText (ts.head())))];
                break;
            case Token::StringLiteral:
                var [ts1,nd1] = [tl (ts), new Ast::LiteralExpr (new Ast::LiteralString (Token::tokenText (ts.head())))];
                break;
            case Token::This:
                var [ts1,nd1] = [tl (ts), new Ast::ThisExpr ()];
                break;
//            else
//            if( lookahead(regexpliteral_token) )
//            {
//                var result = <LiteralRegExp value={scan.tokenText(match(regexpliteral_token))}/>
//            }
//            else
//            if( lookahead(function_token) )
//            {
//                match(function_token);
//                var first = null
//                if( lookahead(identifier_token) )
//                {
//                    first = parseIdentifier();
//                }
//                var result = parseFunctionCommon(first);
//            }
            case Token::LeftParen:
                var [ts1,nd1] = parenListExpression(ts);
                break;
            case Token::LeftBracket:
                var [ts1,nd1] = arrayLiteral (ts);
                break;
            case Token::LeftBrace:
                var [ts1,nd1] = objectLiteral (ts);
                break;
            default:
                var [ts1,nd1] = primaryName (ts);
                switch type (nd1) {
                case (nd: Ast::UnresolvedPath) {
                    base = resolvePath (nd.Ast::path,null);
                    nd1 = new Ast::ObjectRef (base,nd.Ast::ident);  // FIXME: not good for package qualified refs
                }
                case (nd:*) {
                    nd1 = new Ast::LexicalRef (nd1);
                }
                }
                break;
            }

            exit("Parser::primaryExpression ",ts1);
            return [ts1,nd1];
        }

        function resolvePath (path/*: [Ast::IDENT]*/, expr: Ast::EXPR)
        {
            return resolveObjectPath (path,expr);
        }

        function resolveObjectPath (path /*: [Ast::IDENT]*/, expr: Ast::EXPR)
            : Ast::EXPR
        {
            if (path.length === 0) {
                return expr;
            }
            else
            if (expr === null) 
            {
                var base = new Ast::LexicalRef (new Ast::Identifier (path[0],cx.pragmas.openNamespaces));
                return resolveObjectPath (path.slice (1,path.length), base);
            }
            else 
            {
                var base = new Ast::ObjectRef (expr, new Ast::Identifier (path[0],cx.pragmas.openNamespaces));
                return resolveObjectPath (path.slice (1,path.length), base);
            }
        }



        /*

        SuperExpression
            super
            super  Arguments

        */


        /*

        PropertyOperator
            .  ReservedIdentifier
            .  PropertyName
            .  AttributeName
            ..  QualifiedName
            .  ParenListExpression
            .  ParenListExpression  ::  QualifiedNameIdentifier
            Brackets

        */

        function propertyOperator (ts: TOKENS, nd: Ast::EXPR)
            : [TOKENS, [Ast::EXPR]]
        {
            enter("Parser::propertyOperator ", ts);

            switch (hd (ts)) {
            case Token::Dot:
                switch (hd (tl (ts))) {
                case Token::LeftParen:
                    throw "filter operator not implemented";
                    break;
                default:
                    //                    if (isReservedIdentifier (hd (ts))) {
                    //                    }
                    let [ts1,nd1] = propertyName (tl (ts));
                    var [tsx,ndx] = [ts1, new Ast::ObjectRef (nd,nd1)];
                    break;
                }
                break;
            case Token::LeftBracket:
                let [ts1,nd1] = listExpression (tl (ts), allowIn);
                ts1 = eat (ts1,Token::RightBracket);
                var [tsx,ndx] = [ts1, new Ast::ObjectRef (nd,new Ast::ExpressionIdentifier (nd1,cx.pragmas.openNamespaces))];
                break;
            case Token::DoubleDot:
                throw "descendents operator not implemented";
                break;
            default:
                throw "internal error: propertyOperator";
                break;
            }

            exit ("Parser::propertyOperator ", tsx);
            return [tsx, ndx];
        }

        /*

        Arguments
            (  )
            (  ArgumentList  )

        ArgumentList
            AssignmentExpression(allowIn)
            ArgumentList  ,  AssignmentExpression(allowIn)

        */

        function arguments (ts: TOKENS)
            : [TOKENS, * /*[Ast::EXPR]*/]
        {
            enter("Parser::arguments ", ts);

            var ts1 = eat (ts,Token::LeftParen);
            switch (hd (ts1)) {
            case Token::RightParen:
                var tsx = eat (ts1,Token::RightParen);
                var ndx = [];
                break;
            default:
                let [ts2,nd2] = listExpression (ts1, allowIn);
                var tsx = eat (ts2,Token::RightParen);
                var ndx = nd2.Ast::exprs;
                break;
            }
            exit ("Parser::arguments ", tsx);
            return [tsx, ndx];
        }

        /*

        MemberExpression(beta)
            PrimaryExpression(beta)
            new  MemberExpression(beta)  Arguments
            SuperExpression  PropertyOperator
            MemberExpression(beta)  PropertyOperator

        Refactored:

        MemberExpression(beta)
            PrimaryExpression(beta) MemberExpressionPrime(beta)
            new MemberExpression(beta) Arguments MemberExpressionPrime(beta)
            SuperExpression  PropertyOperator  MemberExpressionPrime(beta)

        MemberExpressionPrime(beta)
            PropertyOperator MemberExpressionPrime(beta)
            empty

        Note: member expressions always have balanced new and (). The LHS parser is
        responsible for dispatching extra 'new' or '()' to 

        */

        function memberExpression (ts: TOKENS, beta:BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::memberExpression ", ts);

            switch (hd (ts)) {
            case Token::New:
                let [ts1,nd1] = memberExpression (tl (ts), beta);
                let [ts2,nd2] = this.arguments (ts1);
                var [tsx,ndx] = memberExpressionPrime (ts2, beta, new Ast::NewExpr (nd1,nd2));
                break;
            case Token::Super:
                let [ts1,nd1] = superExpression (ts);
                let [ts2,nd2] = propertyOperator (ts1,nd1);
                var [tsx,ndx] = memberExpressionPrime (ts2, beta, nd2);
            default:
                let [ts1,nd1] = primaryExpression (ts,beta);
                var [tsx,ndx] = memberExpressionPrime (ts1, beta, nd1);
                break;
            }

            exit ("Parser::memberExpression ", tsx);
            return [tsx, ndx];
        }

        function memberExpressionPrime (ts: TOKENS, beta:BETA, nd: Ast::EXPR)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::memberExpressionPrime ", ts);

            switch (hd (ts)) {
            case Token::LeftBracket:
            case Token::Dot:
            case Token::DoubleDot:
                let [ts1,nd1] = propertyOperator (ts,nd);
                var [tsx,ndx] = memberExpressionPrime (ts1, beta, nd1);
                break;
            default:
                var [tsx,ndx] = [ts,nd]
                break;
            }

            exit ("Parser::memberExpressionPrime ", tsx);
            return [tsx, ndx];
        }

        /*

        CallExpression(beta)
            MemberExpression(beta) Arguments CallExpressionPrime(beta) 

        CallExpressionPrime(beta)
            Arguments CallExpressionPrime(beta)
            [ Expression ] CallExpressionPrime(beta)
            . Identifier CallExpressionPrime(beta)
            empty

        */

        function callExpression (ts: TOKENS, beta:BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::callExpression ", ts);

            var [ts1,nd1] = memberExpression (ts,beta);
            var [ts2,nd2] = this.arguments (ts);
            var [tsx,ndx] = callExpressionPrime (ts2, beta, new Ast::CallExpr (nd1,nd2));

            exit ("Parser::callExpressionPrime ", ndx);
            return [tsx, ndx];
        }

        function callExpressionPrime (ts: TOKENS, beta:BETA, nd: Ast::EXPR)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::callExpressionPrime ", ts);

            switch (hd (ts)) {
            case Token::LeftParen:
                let [ts1,nd1] = this.arguments (ts);
                var [tsx,ndx] = callExpressionPrime (ts1, beta, new Ast::CallExpr (nd,nd1));
                break;
            case Token::LeftBracket:
            case Token::Dot:
            case Token::DoubleDot:
                let [ts1,nd1] = propertyOperator (ts,nd);
                var [tsx,ndx] = callExpressionPrime (ts1, beta, nd1);
                break;
            default:
                var [tsx,ndx] = [ts,nd]
                break;
            }

            exit ("Parser::callExpressionPrime ", ndx);
            return [tsx, ndx];
        }

        /*

        NewExpression
            MemberExpression
            new  NewExpression

        */

        function newExpression (ts: TOKENS, beta:BETA, new_count=0)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::newExpression ", ts);

            switch (hd (ts)) {
            case Token::New:
                let [ts1,nd1] = newExpression (tl (ts), beta, new_count+1);
                switch (hd (ts1)) {
                case Token::LeftParen:  // no more new exprs so this paren must start a call expr
                    let [ts2,nd2] = this.arguments (ts1); // refer to parser method
                    if (new_count == 0)
                    {
                        var [tsx,ndx] = callExpressionPrime (ts2,beta,new Ast::CallExpr (nd1,nd2));
                    }
                    else
                    {
                        var [tsx,ndx] = [ts2,new Ast::NewExpr (nd1,nd2)];
                    }
                    break;
                default:
                    if (new_count == 0)
                    {
                        var [tsx,ndx] = memberExpressionPrime (ts1,beta,nd1);
                    }
                    else
                    {
                        var [tsx,ndx] = [ts1,new Ast::NewExpr (nd1,[])];
                    }
                    break;
                }
                break;
            default:
                let [ts1,nd1] = memberExpression (ts,beta);
                switch (hd (ts1)) {
                case Token::LeftParen:
                    let [ts2,nd2] = this.arguments (ts1); // refer to parser method
                    if( new_count == 0 )
                    {
                       var [tsx,ndx] = callExpressionPrime (ts2,beta,new Ast::CallExpr (nd1,nd2));
                    }
                    else
                    {
                        var [tsx,ndx] = [ts2,new Ast::NewExpr (nd1,nd2)];
                    }
                    break;
                default:
                    if( new_count == 0 ) 
                    {
                        var [tsx,ndx] = [ts1,nd1];
                    }
                    else 
                    {
                        var [tsx,ndx] = [ts1,new Ast::NewExpr (nd1,[])];
                    }
                    break;
                }
                break;
            }

            exit ("Parser::newExpression ", ndx);
            return [tsx, ndx];
        }

        /*

        LeftHandSideExpression
            NewExpression
            CallExpression

        Refactored:

        LeftHandSideExpression
            NewExpression
            MemberExpression Arguments CallExpressionPrime
            MemberExpression

        */

        function leftHandSideExpression (ts: TOKENS, beta:BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::leftHandSideExpression ", ts);

            switch (hd (ts)) {
            case Token::New:
                var [tsx,ndx] = newExpression (ts,beta,0);
                break;
            default:
                let [ts1,nd1] = memberExpression (ts,beta);
                switch (hd (ts1)) {
                case Token::LeftParen:
                    let [ts2,nd2] = this.arguments (ts1); // refer to parser method
                    var [tsx,ndx] = callExpressionPrime (ts2, beta, new Ast::CallExpr (nd1,nd2));
                    break;
                default:
                    var [tsx,ndx] = [ts1,nd1];
                    break;
                }
                break;
            }

            exit ("Parser::leftHandSideExpression ", ndx);
            return [tsx, ndx];
        }

        /*

        PostfixExpression(beta)
            LeftHandSideExpression(beta)
            LeftHandSideExpression(beta)  [no line break]  ++
            LeftHandSideExpression(beta)  [no line break]  --

        */

        function postfixExpression (ts: TOKENS, beta:BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::postfixExpression ", ts);

            let [ts1, nd1] = leftHandSideExpression (ts, beta);
            switch (hd (ts1)) {
            case Token::PlusPlus:
                var [tsx,ndx] = [tl (ts1), new Ast::UnaryExpr (Ast::postIncrOp,nd1)];
                break;
            case Token::MinusMinus:
                var [tsx,ndx] = [tl (ts1), new Ast::UnaryExpr (Ast::postDecrOp,nd1)];
                break;
            default:
                var [tsx,ndx] = [ts1,nd1];
                break;
            }

            exit ("Parser::postfixExpression ", tsx);
            return [tsx, ndx];
        }

        /*

        UnaryExpression(beta)
            PostfixExpression(beta)
            delete  PostfixExpression(beta)
            void  UnaryExpression(beta)
            typeof  UnaryExpression(beta)
            ++   PostfixExpression(beta)
            --  PostfixExpression(beta)
            +  UnaryExpression(beta)
            -  UnaryExpression(beta)
            ~  UnaryExpression(beta)
            !  UnaryExpression(beta)
            type  NullableTypeExpression

        */

        function unaryExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::unaryExpression ", ts);

            switch (hd (ts)) {
            case Token::Delete:
                let [ts1,nd1] = postfixExpression (tl (ts),beta);
                var [tsx,ndx] = [ts1,new Ast::UnaryExpr (Ast::deleteOp,nd1)];
                break;
            case Token::Void:
                let [ts1,nd1] = unaryExpression (tl (ts),beta);
                var [tsx,ndx] = [ts1,new Ast::UnaryExpr (Ast::voidOp,nd1)];
                break;
            case Token::TypeOf:
                let [ts1,nd1] = unaryExpression (tl (ts),beta);
                var [tsx,ndx] = [ts1,new Ast::UnaryExpr (Ast::typeOfOp,nd1)];
                break;
            case Token::PlusPlus:
                let [ts1,nd1] = postfixExpression (tl (ts),beta);
                var [tsx,ndx] = [ts1,new Ast::UnaryExpr (Ast::preIncrOp,nd1)];
                break;
            case Token::MinusMinus:
                let [ts1,nd1] = postfixExpression (tl (ts),beta);
                var [tsx,ndx] = [ts1,new Ast::UnaryExpr (Ast::preDecrOp,nd1)];
                break;
            case Token::Plus:
                let [ts1,nd1] = unaryExpression (tl (ts),beta);
                var [tsx,ndx] = [ts1,new Ast::UnaryExpr (Ast::unaryPlusOp,nd1)];
                break;
            case Token::Minus:
                let [ts1,nd1] = unaryExpression (tl (ts),beta);
                var [tsx,ndx] = [ts1,new Ast::UnaryExpr (Ast::unaryMinusOp,nd1)];
                break;
            case Token::BitwiseNot:
                let [ts1,nd1] = unaryExpression (tl (ts),beta);
                var [tsx,ndx] = [ts1,new Ast::UnaryExpr (Ast::bitwiseNotOp,nd1)];
                break;
            case Token::Not:
                let [ts1,nd1] = unaryExpression (tl (ts),beta);
                var [tsx,ndx] = [ts1,new Ast::UnaryExpr (Ast::logicalNotOp,nd1)];
                break;
            case Token::Type:
                let [ts1,nd1] = nullableTypeExpression (tl (ts),beta);
                var [tsx,ndx] = [ts1,new Ast::TypeExpr (nd1)];
                break;
            default:
                var [tsx,ndx] = postfixExpression (ts,beta);
                break;
            }

            exit ("Parser::unaryExpression ", tsx);
            return [tsx,ndx];
        }

        /*

        MultiplicativeExpression
            UnaryExpression
            MultiplicativeExpression  *  UnaryExpression
            MultiplicativeExpression  /  UnaryExpression
            MultiplicativeExpression  %  UnaryExpression

        */

        function multiplicativeExpression (ts: TOKENS, beta:BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::multiplicativeExpression ", ts);

            var [ts1,nd1] = unaryExpression (ts, beta);

            done:
            while (true) {

                if (hd (ts1) === Token::BREAK) {
                    let tsx;
                    [tsx,this.coordList] = scan.tokenList (scan.div);
                    ts1 = new TokenStream (tsx,0);
                }

                switch (hd (ts1)) {
                case Token::Mult:
                    var op = Ast::timesOp;
                    break;
                case Token::Div:
                    var op = Ast::divideOp;
                    break;
                case Token::Remainder:
                    var op = Ast::remainderOp;
                    break;
                default:
                    break done;
                }
                let [ts2, nd2] = unaryExpression (tl (ts1), beta);
                ts1 = ts2;
                nd1 = new Ast::BinaryExpr (op, nd1, nd2);
            }

            exit ("Parser::multiplicativeExpression ", ts1);
            return [ts1, nd1];
        }

        /*

        AdditiveExpression
            MultiplicativeExpression
            AdditiveExpression + MultiplicativeExpression
            AdditiveExpression - MultiplicativeExpression

        */

        function additiveExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::additiveExpression ", ts);

            var [ts1, nd1] = multiplicativeExpression (ts, beta);
            done:
            while (true) {
                switch (hd (ts1)) {
                case Token::Plus:
                    var op = Ast::plusOp;
                    break;
                case Token::Minus:
                    var op = Ast::minusOp;
                    break;
                default:
                    break done;
                }
                let [ts2, nd2] = multiplicativeExpression (tl (ts1), beta);
                [ts1, nd1] = [ts2, new Ast::BinaryExpr (op, nd1, nd2)];
            }

            exit ("Parser::additiveExpression ", ts1);
            return [ts1, nd1];
        }

        /*

        ShiftExpression
            AdditiveExpression
            ShiftExpression << AdditiveExpression
            ShiftExpression >> AdditiveExpression
            ShiftExpression >>> AdditiveExpression

        */

        function shiftExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::shiftExpression ", ts);

            var [ts1, nd1] = additiveExpression (ts, beta);
            done:
            while (true) {
                switch (hd (ts1)) {
                case Token::LeftShift:
                    var op = Ast::leftShiftOp;
                    break;
                case Token::RightShift:
                    var op = Ast::rightShiftOp;
                    break;
                case Token::UnsignedRightShift:
                    var op = Ast::unsignedRightShiftOp;
                    break;
                default:
                    break done;
                }
                let [ts2, nd2] = additiveExpression (tl (ts1), beta);
                var [ts1, nd1] = [ts2, new Ast::BinaryExpr (op, nd1, nd2)];
            }

            exit ("Parser::shiftExpression ", ts1);
            return [ts1, nd1];
        }

        /*

        RelationalExpression(noIn)
            ShiftExpression(noIn)
            RelationalExpression(noIn) < ShiftExpression(noIn)
            RelationalExpression(noIn) > ShiftExpression(noIn)
            RelationalExpression(noIn) <= ShiftExpression(noIn)
            RelationalExpression(noIn) >= ShiftExpression(noIn)
            RelationalExpression(noIn) instanceof ShiftExpression(noIn)
            RelationalExpression(noIn) is TypeExpression
            RelationalExpression(noIn) to TypeExpression
            RelationalExpression(noIn) cast TypeExpression

        RelationalExpression(allowIn)
            ShiftExpression(allowIn)
            RelationalExpression(allowIn) < ShiftExpression(allowIn)
            RelationalExpression(allowIn) > ShiftExpression(allowIn)
            RelationalExpression(allowIn) <= ShiftExpression(allowIn)
            RelationalExpression(allowIn) >= ShiftExpression(allowIn)
            RelationalExpression(allowIn) in ShiftExpression(allowIn)
            RelationalExpression(allowIn) instanceof ShiftExpression(allowIn)
            RelationalExpression(allowIn) is TypeExpression
            RelationalExpression(allowIn) to TypeExpression
            RelationalExpression(allowIn) cast TypeExpression

        */

        function relationalExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::relationalExpression ", ts);

            var [ts1, nd1] = shiftExpression (ts, beta);
            done:
            while (true) {
                switch (hd (ts1)) {
                case Token::LessThan:
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    nd2 = new Ast::BinaryExpr (Ast::lessOp,nd1,nd2);
                    break;
                case Token::GreaterThan:
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    nd2 = new Ast::BinaryExpr (Ast::greaterOp,nd1,nd2);
                    break;
                case Token::LessThanOrEqual:
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    nd2 = new Ast::BinaryExpr (Ast::lessOrEqualOp,nd1,nd2);
                    break;
                case Token::GreaterThanOrEqual:
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    nd2 = new Ast::BinaryExpr (Ast::greaterOrEqualOp,nd1,nd2);
                    break;
                case Token::In:
                    if (beta == noIn) {
                        break done;
                    }
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    nd2 = new Ast::BinaryExpr (Ast::inOp,nd1,nd2);
                    break;
                case Token::InstanceOf:
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    nd2 = new Ast::BinaryExpr (Ast::instanceOfOp,nd1,nd2);
                    break;
                case Token::Is:
                    var [ts2, nd2] = typeExpression (tl (ts1), beta);
                    nd2 = new Ast::BinaryTypeExpr (Ast::isOp,nd1,nd2);
                    break;
                case Token::To:
                    var [ts2, nd2] = typeExpression (tl (ts1), beta);
                    nd2 = new Ast::BinaryTypeExpr (Ast::toOp,nd1,nd2);
                    break;
                case Token::Cast:
                    var [ts2, nd2] = typeExpression (tl (ts1), beta);
                    nd2 = new Ast::BinaryTypeExpr (Ast::castOp,nd1,nd2);
                    break;
                default:
                    break done;
                }
                var [ts1, nd1] = [ts2,nd2];
            }

            exit ("Parser::equalityExpression ", ts1);
            return [ts1, nd1];
        }

        /*

        EqualityExpression(beta)
            RelationalExpression(beta)
            EqualityExpression(beta) == RelationalExpression(beta)
            EqualityExpression(beta) != RelationalExpression(beta)
            EqualityExpression(beta) === RelationalExpression(beta)
            EqualityExpression(beta) !== RelationalExpression(beta)

        */

        function equalityExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::equalityExpression ", ts);

            var [ts1, nd1] = relationalExpression (ts, beta);
            done:
            while (true) {
                switch (hd (ts1)) {
                case Token::Equal:
                    var op = Ast::equalOp;
                    break;
                case Token::NotEqual:
                    var op = Ast::notEqualOp;
                    break;
                case Token::StrictEqual:
                    var op = Ast::strictEqualOp;
                    break;
                case Token::StrictNotEqual:
                    var op = Ast::strictNotEqualOp;
                    break;
                default:
                    break done;
                }
                let [ts2, nd2] = relationalExpression (tl (ts1), beta);
                var [ts1, nd1] = [ts2, new Ast::BinaryExpr (op, nd1, nd2)];
            }

            exit ("Parser::equalityExpression ", ts1);
            return [ts1, nd1];
        }

        /*

        BitwiseAndExpression(beta)
            EqualityExpression(beta)
            BitwiseAndExpressionr(beta) & EqualityExpression(beta)

        */

        function bitwiseAndExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::bitwiseAndExpression ", ts);

            var [ts1, nd1] = equalityExpression (ts, beta);
            while (hd (ts1) === Token::BitwiseAnd) {
                var [ts2, nd2] = equalityExpression (tl (ts1), beta);
                var [ts1, nd1] = [ts2, new Ast::BinaryExpr (Ast::bitwiseAndOp, nd1, nd2)];
            }

            exit ("Parser::bitwiseAndExpression ", ts1);
            return [ts1, nd1];
        }

        /*

        BitwiseXorExpressionb
            BitwiseAndExpressionb
            BitwiseXorExpressionb ^ BitwiseAndExpressionb

        */

        function bitwiseXorExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::bitwiseXorExpression ", ts);

            var [ts1, nd1] = bitwiseAndExpression (ts, beta);
            while (hd (ts1) === Token::BitwiseOr) {
                var [ts2, nd2] = bitwiseAndExpression (tl (ts1), beta);
                var [ts1, nd1] = [ts2, new Ast::BinaryExpr (Ast::bitwiseXorOp, nd1, nd2)];
            }

            exit ("Parser::bitwiseXorExpression ", ts1);
            return [ts1, nd1];
        }

        /*

        BitwiseOrExpression(beta)
            BitwiseXorExpression(beta)
            BitwiseOrExpression(beta) | BitwiseXorExpression(beta)

        */

        function bitwiseOrExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::bitwiseOrExpression ", ts);

            var [ts1, nd1] = bitwiseXorExpression (ts, beta);
            while (hd (ts1) === Token::BitwiseOr) {
                var [ts2, nd2] = bitwiseXorExpression (tl (ts1), beta);
                var [ts1, nd1] = [ts2, new Ast::BinaryExpr (Ast::bitwiseOrOp, nd1, nd2)];
            }

            exit ("Parser::bitwiseOrExpression ", ts1);
            return [ts1, nd1];
        }

        /*

        LogicalAndExpression(beta)
            BitwiseOrExpression(beta)
            LogicalAndExpression(beta) && BitwiseOrExpression(beta)

        */

        function logicalAndExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::logicalAndExpression ", ts);

            var [ts1, nd1] = bitwiseOrExpression (ts, beta);
            while (hd (ts1) === Token::LogicalAnd) {
                var [ts2, nd2] = bitwiseOrExpression (tl (ts1), beta);
                var [ts1, nd1] = [ts2, new Ast::BinaryExpr (Ast::logicalAndOp, nd1, nd2)];
            }

            exit ("Parser::logicalAndExpression ", ts1);
            return [ts1, nd1];
        }

        /*

        LogicalXorExpressionb
            LogicalAndExpressionb
            LogicalXorExpressionb ^^ LogicalAndExpressionb

        */

        function logicalXorExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::logicalXorExpression ", ts);

            var [ts1, nd1] = logicalAndExpression (ts, beta);
            while (hd (ts1) === Token::LogicalXor) {
                var [ts2, nd2] = logicalAndExpression (tl (ts1), beta);
                var [ts1, nd1] = [ts2, new Ast::BinaryExpr (Ast::logicalXor, nd1, nd2)];
            }

            exit ("Parser::logicalXorExpression ", ts1);
            return [ts1, nd1];
        }

        /*

            LogicalOrExpression(beta)
                LogicalXorExpression(beta)
                LogicalOrExpression(AllowIn) || LogicalXorExpression(beta)

        */

        function logicalOrExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::logicalOrExpression ", ts);

            var [ts1, nd1] = logicalXorExpression (ts, beta);
            while (hd (ts1) === Token::LogicalOr) {
                var [ts2, nd2] = logicalXorExpression (tl (ts1), beta);
                var [ts1, nd1] = [ts2, new Ast::BinaryExpr (Ast::logicalOrOp, nd1, nd2)];
            }

            exit ("Parser::logicalOrExpression ", ts1);
            return [ts1, nd1];
        }

        /*

        YieldExpression
            UnaryExpression
            yield  UnaryExpression

        */


        /*

        NonAssignmentExpressiona, b
            LetExpressiona, b
            YieldExpressiona, b
            LogicalOrExpressiona, b
            LogicalOrExpressiona, b  ?  NonAssignmentExpressiona, b  
                                                    :  NonAssignmentExpressiona, b

        */

        function nonAssignmentExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::nonAssignmentExpression ", ts);

            switch (hd (ts)) {
            case Token::Let:
                var [ts1,nd1] = letExpression (ts,beta);
                break;
            case Token::Yield:
                var [ts1,nd1] = yieldExpression (ts,beta);
                break;
            default:
                var [ts1,nd1] = logicalOrExpression (ts,beta);
                switch (hd (ts1)) {
                case Token::QuestionMark:
                    var [ts2,nd2] = nonAssignmentExpression (tl (ts1),beta);
                    match (ts2,Token::Colon);
                    var [ts3,nd3] = nonAssignmentExpression (tl (ts2),beta);
                    break;
                default:
                    var [ts3,nd3] = [ts1,nd1];
                    break;
                }
            }

            exit ("Parser::nonAssignmentExpression ", ts1);
            return [ts1,nd1];
        }

        /*

        ConditionalExpression(beta)
            LetExpression(beta)
            YieldExpression(beta)
            LogicalOrExpression(beta)
            LogicalOrExpression(beta)  ?  AssignmentExpression(beta)
                                       :  AssignmentExpression(beta)

        */

        function conditionalExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::conditionalExpression ", ts);

            switch (hd (ts)) {
            case Token::Let:
                var [ts1,nd1] = letExpression (ts,beta);
                break;
            case Token::Yield:
                var [ts1,nd1] = yieldExpression (ts,beta);
                break;
            default:
                var [ts1,nd1] = logicalOrExpression (ts,beta);
                switch (hd (ts1)) {
                case Token::QuestionMark:
                    var [ts2,nd2] = assignmentExpression (tl (ts1),beta);
                    match (ts2,Token::Colon);
                    var [ts3,nd3] = assignmentExpression (tl (ts2),beta);
                    break;
                default:
                    var [ts3,nd3] = [ts1,nd1];
                    break;
                }
            }

            exit ("Parser::conditionalExpression ", ts1);
            return [ts1,nd1];
        }

        /*

        AssignmentExpression(beta)
            ConditionalExpression(beta)
            Pattern(beta, allowExpr)  =  AssignmentExpression(beta)
            SimplePattern(beta, allowExpr)  CompoundAssignmentOperator  AssignmentExpression(beta)

        */

        function assignmentExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::assignmentExpression ", ts);

            var [ts1,nd1] = conditionalExpression (ts, beta);
            switch (hd (ts1)) {
            case Token::Assign:
                var [ts1,nd1] = [tl (ts1), patternFromExpr (nd1)];
                var [ts2,nd2] = assignmentExpression (ts1,beta);
                var [fxtrs,expr,head] = desugarAssignmentPattern (nd1,Ast::anyType,nd2,Ast::assignOp);
                break;
            default:
                var [ts2,expr] = [ts1,nd1];
                break;
            }

            exit ("Parser::assignmentExpression ", ts1);
            return [ts2,expr];

            // expression to pattern converters

            function patternFromExpr (e: Ast::EXPR) {
                switch type (e) {
                case (e: Ast::LiteralExpr) {
                    switch type (e.Ast::literal) {
                    case (l: Ast::LiteralArray) {
                        var p = arrayPatternFromLiteral (l);
                    }
                    case (l: Ast::LiteralObject) {
                        var p = objectPatternFromLiteral (l);
                    }
                    case (l: *) {
                        throw "invalid lhs expr " + e;
                    }
                    }
                }
                case (e: (Ast::LexicalRef, Ast::ObjectRef)) {
                    var p = new SimplePattern (e);
                }
                case (e: *) {
                    throw "error patternFromExpr, unhandled expression kind " + e;
                }
                }
                return p;
            }

            function arrayPatternFromLiteral (nd: Ast::LITERAL)
                : PATTERN
            {
                enter("Parser::arrayPatternFromLiteral ", ts);
                
                var nd1 = elementListPatternFromLiteral (nd.Ast::exprs);
                
                exit ("Parser::arrayPatternFromLiteral ", ts1);
                return new ArrayPattern (nd1);
            }

            function elementListPatternFromLiteral (nd: Ast::EXPRS)
                : PATTERNS
            {
                enter("Parser::elementListPatternFromLiteral ", nd);
                
                var nd1 = [];
                
                for (let i=0; i<nd.length; ++i) {
                    var ndx = patternFromExpr (nd[i]);
                    nd1.push (ndx);
                }
                
                exit ("Parser::elementListPatternFromLiteral ", nd1);
                return nd1;
            }
                    
            function objectPatternFromLiteral (l: Ast::LITERAL)
                : PATTERN
            {
                enter("Parser::objectPatternFromLiteral ", l);
                
                switch type (l) {
                case (nd: Ast::LiteralObject) {
                    var p = fieldListPatternFromLiteral (nd.Ast::fields);
                }
                case (nd: *) {
                    throw "error objectPatternFromLiteral " + nd;
                }
                }
                        
                exit ("Parser::objectPatternFromLiteral ", p);
                return new ObjectPattern (p);
            }
                    
            function fieldListPatternFromLiteral (nd: Ast::LITERAL_FIELDS)
                : FIELD_PATTERNS
            {
                enter("Parser::fieldListPatternFromLiteral ", nd);
                
                var nd1 = [];
                
                for (let i=0; i<nd.length; ++i) {
                    var ndx = fieldPatternFromLiteral (nd[i]);
                    nd1.push (ndx);
                }
                
                exit ("Parser::fieldListPatternFromLiteral ", nd1);
                return nd1;
            }
                    
            function fieldPatternFromLiteral (nd: Ast::LITERAL_FIELD)
                : FIELD_PATTERN
            {
                enter("Parser::fieldPatternFromLiteral ", ts);
                
                var nd1 = nd.Ast::ident;
                var nd2 = patternFromExpr (nd.Ast::expr);
                
                exit ("Parser::fieldPatternFromLiteral ", ts2);
                return new FieldPattern (nd1,nd2);
            }
        }

        /*

        ListExpression(b)
            AssignmentExpression(b)
            ListExpression(b)  ,  AssignmentExpression(b)

        right recursive:

        ListExpression(b)
            AssignmentExpression(b) ListExpressionPrime(b)

        ListExpressionPrime(b)
            empty
            , AssignmentExpression(b) ListExpressionPrime(b)

        */

        function listExpression (ts: TOKENS, beta: BETA )
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::listExpression ", ts);

            function listExpressionPrime (ts: TOKENS )
                : [TOKENS, Ast::EXPR]
            {
                enter("Parser::listExpressionPrime ", ts);
        
                switch (hd (ts)) {
                case Token::Comma:
                    var [ts1,nd1] = assignmentExpression (tl (ts), beta);
                    var [ts2,nd2] = listExpressionPrime (ts1);
                    nd2.unshift (nd1);
                    break;
                default:
                    var [ts2,nd2] = [ts,[]];
                    break;
                }

                exit ("Parser::listExpressionPrime ", ts2);
                return [ts2,nd2];
            }

            var [ts1,nd1] = assignmentExpression (ts, beta);
            var [ts2,nd2] = listExpressionPrime (ts1);
            // print("nd2.length=",nd2.length);
            nd2.unshift (nd1);
            // print("nd2.length=",nd2.length);

            exit ("Parser::listExpression ", ts2);
            return [ts2,new Ast::ListExpr (nd2)];
        }

//        /*
//
//        LetExpressionb
//            let  (  LetBindingList  )  AssignmentExpressionb
//
//        LetBindingList
//            empty
//            NonemptyLetBindingList
//
//        NonemptyLetBindingList
//            VariableBinding
//            VariableBinding , NonemptyLetBindingList
//
//        */
//
//        function parseLetExpression(mode)
//        {
//            enter("parseLetExpression")
//
//            var prologue = <Prologue/>
//            match(let_token)
//            match(leftparen_token)
//            if( lookahead(rightparen_token) )
//            {
//                var first = <></>
//            }
//            else
//            {
//                var first = <></>
//                first += parseVariableBinding(<Attributes><Let/></Attributes>,var_token,allowIn_mode,prologue)
//                while( lookahead(comma_token) )
//                {
//                    match(comma_token)
//                    first += parseVariableBinding(<Attributes><Let/></Attributes>,var_token,allowIn_mode,prologue)
//                }
//                prologue.* += first
//            }
//            match(rightparen_token)
//            var second = parseAssignmentExpression(mode)
//            var result = <LetExpression>{prologue}{second}</LetExpression>
//
//            exit("parseLetExpression",result)
//            return result
//        }
//
//        /*
//
//        YieldExpressionb
//            yield  AssignmentExpressionb
//
//        */
//
///*
//        function parseYieldExpression(mode)
//        {
//            enter("parseYieldExpression")
//
//            exit("parseYieldExpression",result)
//            return result
//        }
//*/

        // PATTERNS

        /*

          Pattern(beta,gamma)
              SimplePattern(beta,gamma)
              ObjectPattern(gamma)
              ArrayPattern(gamma)

        */

        function pattern (ts: TOKENS, beta: BETA, gamma: GAMMA)
            : [TOKENS, PATTERN]
        {
            enter("Parser::pattern", ts);

            switch (hd (ts)) {
            case Token::LeftBrace:
                var [ts1,nd1] = objectPattern (ts, gamma);
                break;
            case Token::LeftBracket:
                var [ts1,nd1] = arrayPattern (ts, gamma);
                break;
            default:
                var [ts1,nd1] = simplePattern (ts, beta, gamma);
                break;
            }

            exit("Parser::pattern ", ts1);
            return [ts1,nd1];
        }

        /*

          SimplePattern(beta, noExpr)
              Identifier

          SimplePattern(beta, allowExpr)
              LeftHandSideExpression(beta)

          */

        function simplePattern (ts: TOKENS, beta: BETA, gamma: GAMMA)
            : [TOKENS, PATTERN]
        {
            enter("Parser::simplePattern", ts);

            switch (gamma) {
            case noExpr:
                let [ts1,nd1] = identifier (ts);
                var [tsx,ndx] = [ts1, new IdentifierPattern (nd1)];
                break;
            case allowExpr:
                let [ts1,nd1] = leftHandSideExpression (ts,beta);
                var [tsx,ndx] = [ts1, new SimplePattern (nd1)];
                break;
            }

            exit("Parser::simplePattern", tsx);
            return [tsx,ndx];
        }

        /*

        ArrayPattern(gamma)
            [  ElementListPattern(gamma)  ]
        
        */

        function arrayPattern (ts: TOKENS, gamma: GAMMA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::arrayPattern ", ts);

            ts = eat (ts,Token::LeftBracket);
            var [ts1,nd1] = elementListPattern (ts,gamma);
            ts1 = eat (ts1,Token::RightBracket);

            exit ("Parser::arrayPattern ", ts1);
            return [ts1, new ArrayPattern (nd1)];
        }

        /*

        ElementListPattern(gamma)
            empty
            LiteralElementPattern
            ,  ElementListPattern
             LiteralElementPattern  ,  ElementListPattern

        LiteralElementPattern
            Pattern(allowColon,allowIn,gamma)

        */

        function elementListPattern (ts: TOKENS, gamma:GAMMA)
            : [TOKENS, Ast::EXPRS]
        {
            enter("Parser::elementListPattern ", ts);

            var nd1 = [];

            if (hd (ts) !== Token::RightBracket) 
            {
                switch (hd (ts)) {
                case Token::Comma:
                    var [ts1,ndx] = [tl (ts),new Ast::LiteralExpr (new Ast::LiteralUndefined)];
                    break;
                default:
                    var [ts1,ndx] = pattern (ts,allowIn,gamma);
                    break;
                }
                nd1.push (ndx);
                while (hd (ts1) === Token::Comma) {
                    ts1 = eat (ts1,Token::Comma);
                    switch (hd (ts1)) {
                    case Token::Comma:
                        var [ts1,ndx] = [ts1,new Ast::LiteralExpr (new Ast::LiteralUndefined)];
                        break;
                    default:
                        var [ts1,ndx] = pattern (ts1,allowIn,gamma);
                        break;
                    }
                    nd1.push (ndx);
                }
            }

            exit ("Parser::elementListPattern ", ts1);
            return [ts1, nd1];
        }

        /*

        ObjectPattern(gamma)
            [  FieldListPattern(gamma)  ]
        
        */

        function objectPattern (ts: TOKENS, gamma: GAMMA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::objectPattern ", ts);

            ts = eat (ts,Token::LeftBrace);
            var [ts1,nd1] = fieldListPattern (ts,gamma);
            ts1 = eat (ts1,Token::RightBrace);

            exit ("Parser::objectPattern ", ts1);
            return [ts1, new ObjectPattern (nd1)];
        }

        /*

        FieldListPattern(gamma)
            empty
            FieldPattern
            FieldPattern  ,  FieldListPattern

        FieldPattern
            FieldName
            FieldName  :  Pattern(allowColon,allowIn,gamma)

        */

        function fieldListPattern (ts: TOKENS, gamma:GAMMA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::fieldListPattern ", ts);

            var nd1 = [];

            if (hd (ts) !== Token::RightBrace) 
            {
                var [ts1,ndx] = fieldPattern (ts,gamma);
                nd1.push (ndx);
                while (hd (ts1) === Token::Comma) {
                    ts1 = eat (ts1,Token::Comma);
                    var [ts1,ndx] = fieldPattern (ts1,gamma);
                    nd1.push (ndx);
                }
            }

            exit ("Parser::fieldListPattern ", ts1);
            return [ts1, nd1];
        }

        function fieldPattern (ts: TOKENS, gamma:GAMMA)
            : [TOKENS, FIELD_PATTERN]
        {
            enter("Parser::fieldPattern ", ts);

            var [ts1,nd1] = fieldName (ts);
            switch (hd (ts1)) {
            case Token::Colon:
                var [ts2,nd2] = pattern (tl (ts1),allowIn,gamma);
                break;
            default:
                switch type (nd1) {
                case (nd1: Ast::Identifier) {
                    var [ts2,nd2] = [ts1, new IdentifierPattern (nd1.Ast::ident)];
                }
                case (nd1:*) {
                    throw "unsupported fieldPattern " + nd1;
                }
                }
                break;
            }

            exit ("Parser::fieldPattern ", ts2);
            return [ts2, new FieldPattern (nd1,nd2)];
        }

        /*

          TypedIdentifier(beta)
              SimplePattern(beta, noExpr)
              SimplePattern(beta, noExpr)  :  NullableTypeExpression

          TypedPattern(beta)
              Pattern(beta, noExpr)
              Pattern(beta, noExpr)  :  NullableTypeExpression

        */

        function typedPattern (ts: TOKENS, beta: BETA)
            : [TOKENS, [PATTERN,Ast::TYPE_EXPR]]
        {
            enter("Parser::typedPattern ", ts);

            var [ts1,nd1] = pattern (ts,beta,noExpr);
            switch (hd (ts1)) {
            case Token::Colon:
                var [ts2,nd2] = nullableTypeExpression (tl (ts1));
                break;
            default:
                var [ts2,nd2] = [ts1,new Ast::SpecialType (new Ast::AnyType)];
                break;
            }

            exit("Parser::typedPattern ", ts2);
            return [ts2,[nd1,nd2]];
        }

        (1/2)

        // TYPE EXPRESSIONS

        /*

        NullableTypeExpression
            TypeExpression
            TypeExpression  ?
            TypeExpression  !

        */

        function nullableTypeExpression (ts: TOKENS)
            : [TOKENS, Ast::TYPE_EXPR]
        {
            enter("Parser::nullableTypeExpression ", ts);

            var [ts1,nd1] = typeExpression (ts);
            switch (hd (ts1)) {
            case Token::QuestionMark:
                var [ts1,nd1] = [tl (ts1), new Ast::NullableType (nd1,true)];
                break;
            case Token::Not:
                var [ts1,nd1] = [tl (ts1), new Ast::NullableType (nd1,false)];
                break;
            default:
                // do nothing
                break;
            }

            exit("Parser::nullableTypeExpression ", ts1);
            return [ts1,nd1];
        }

        /*

        TypeExpression
            null
            undefined
            FunctionType
            UnionType
            RecordType
            ArrayType
            PrimaryName

        */

        function typeExpression (ts: TOKENS)
            : [TOKENS, Ast::TYPE_EXPR]
        {
            enter("Parser::typeExpression ", ts);

            switch (hd (ts)) {
            case Token::Mult:
                var [ts1,nd1] = [tl (ts), new Ast::SpecialType (new Ast::AnyType)];
                break;
            case Token::Null:
                var [ts1,nd1] = [tl (ts), new Ast::SpecialType (new Ast::NullType)];
                break;
            case Token::Undefined:
                var [ts1,nd1] = [tl (ts), new Ast::SpecialType (new Ast::UndefinedType)];
                break;
            case Token::Function:
                var [ts1,nd1] = functionType (ts);
                break;
            case Token::LeftParen:
                var [ts1,nd1] = unionType (ts);
                break;
            case Token::LeftBrace:
                var [ts1,nd1] = objectType (ts);
                break;
            case Token::LeftBracket:
                var [ts1,nd1] = arrayType (ts);
                break;
            default:
                var [ts1,nd1] = primaryName (ts);
                nd1 = new Ast::TypeName (nd1);
                break;
            }

            exit("Parser::typeExpression ", ts1);
            return [ts1,nd1];
        }

        /*

        UnionType
            (  TypeExpressionList  )

        */

        function unionType (ts: TOKENS)
            : [TOKENS, Ast::TYPE_EXPR]
        {
            enter("Parser::unionType ", ts);

            ts = eat (ts,Token::LeftParen);
            var [ts1,nd1] = typeExpressionList (ts);
            ts1 = eat (ts1,Token::RightParen);

            exit("Parser::unionType ", ts1);
            return [ts1,new Ast::UnionType (nd1)];
        }

        /*

        ObjectType
            {  FieldTypeTypeList  }

        */

        function objectType (ts: TOKENS)
            : [TOKENS, Ast::TYPE_EXPR]
        {
            enter("Parser::objectType ", ts);

            ts = eat (ts,Token::LeftBrace);
            var [ts1,nd1] = fieldTypeList (ts);
            ts1 = eat (ts1,Token::RightBrace);

            exit("Parser::objectType ", ts1);
            return [ts1,new Ast::ObjectType (nd1)];
        }

        /*

        FieldTypeList
            empty
            NonemptyFieldTypeList

        NonemptyFieldTypeList
            FieldType
            FieldType  ,  NonemptyFieldTypeList

        */

        function fieldTypeList (ts: TOKENS)
            //            : [TOKENS, [Ast::FIELD_TYPE]]
        {
            enter("Parser::fieldTypeList ", ts);

            var nd1 = [];

            if (hd (ts) !== Token::RightBrace) 
            {
                var [ts1,ndx] = fieldType (ts);
                nd1.push (ndx);
                while (hd (ts1) === Token::Comma) {
                    var [ts1,ndx] = fieldType (tl (ts1));
                    nd1.push (ndx);
                }
            }

            exit ("Parser::fieldTypeList ", ts1);
            return [ts1,nd1];
        }

        function fieldType (ts: TOKENS)
            : [TOKENS, Ast::FIELD_TYPE]
        {
            enter ("Parser::fieldType");

            var [ts1,nd1] = fieldName (ts);
            ts1 = eat (ts1,Token::Colon);
            var [ts2,nd2] = nullableTypeExpression (ts1);

            exit ("Parser::fieldType");
            return [ts2, new Ast::FieldType (nd1,nd2)];
        }

        /*

        ArrayType
            [  ElementTypeList  ]

        ElementTypeList
            empty
            NullableTypeExpression
            ,  ElementTypeList
            NullableTypeExpression  ,  ElementTypeList

        */

        function arrayType (ts: TOKENS)
            : [TOKENS, Ast::TYPE_EXPR]
        {
            enter("Parser::arrayType ", ts);

            ts = eat (ts,Token::LeftBracket);
            var [ts1,nd1] = elementTypeList (ts);
            ts1 = eat (ts1,Token::RightBracket);

            exit("Parser::arrayType ", ts1);
            return [ts1,new Ast::ArrayType (nd1)];
        }

        function elementTypeList (ts: TOKENS)
            //            : [TOKENS, [Ast::ELEMENT_TYPE]]
        {
            enter("Parser::elementTypeList ", ts);

            var nd1 = [];

            if (hd (ts) !== Token::RightBracket) 
            {
                switch (hd (ts)) {
                case Token::Comma:
                    var [ts1,ndx] = [tl (ts),new Ast::LiteralExpr (new Ast::LiteralUndefined)];
                    break;
                default:
                    var [ts1,ndx] = nullableTypeExpression (ts);
                    break;
                }
                nd1.push (ndx);
                while (hd (ts1) === Token::Comma) {
                    ts1 = eat (ts1,Token::Comma);
                    switch (hd (ts1)) {
                    case Token::Comma:
                        var [ts1,ndx] = [ts1,new Ast::LiteralExpr (new Ast::LiteralUndefined)];
                        break;
                    default:
                        var [ts1,ndx] = nullableTypeExpression (ts1);
                        break;
                    }
                    nd1.push (ndx);
                }
            }

            exit ("Parser::elementTypeList ", ts1);
            return [ts1,nd1];
        }

        /*

        TypeExpressionList
            NullableTypeExpression
            TypeExpressionList  ,  NullableTypeExpression

        refactored

        TypeExpressionList
            NullableTypeExpression  TypeExpressionListPrime

        TypeExpressionListPrime
            empty
            ,  NullableTypeExpression  TypeExpressionListPrime

        */

        function typeExpressionList (ts: TOKENS)
            //            : [TOKENS, [Ast::TYPE_EXPR]]
        {
            enter("Parser::typeExpressionList ", ts);

            var nd1 = [];
            var [ts1,ndx] = nullableTypeExpression (ts);
            nd1.push (ndx);
            while (hd (ts1) === Token::Comma) {
                var [ts1,ndx] = nullableTypeExpression (tl (ts1));
                nd1.push (ndx);
            }

            exit ("Parser::typeExpressionList ", ts1);
            return [ts1,nd1];
        }

        (1/2)

        // STATEMENTS

        /*

        Statement(tau, omega)
            BlockStatement(tau)
            BreakStatement Semicolon(omega)
            ContinueStatement Semicolon(omega)
            DefaultXMLNamespaceStatement Semicolon(omega)
            DoStatement Semicolon(omega)
            ExpressionStatement Semicolon(omega)
            ForStatement(omega)
            IfStatement(omega)
            LabeledStatement(omega)
            LetStatement(omega)
            ReturnStatement Semicolon(omega)
            SwitchStatement
            ThrowStatement Semicolon(omega)
            TryStatement
            WhileStatement(omega)
            WithStatement(omega)

        */

        function statement (ts: TOKENS, tau: TAU, omega: OMEGA)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::statement ", ts);

            switch (hd(ts)) {
            case Token::If:
                var [ts2,nd2] = ifStatement (ts,omega);
                break;
            case Token::While:
                var [ts2,nd2] = whileStatement (ts,omega);
                break;
            case Token::For:
                var [ts2,nd2] = forStatement (ts,omega);
                break;
            case Token::Return:
                var [ts1,nd1] = returnStatement (ts,omega);
                var [ts2,nd2] = [semicolon (ts1,omega),nd1];
                break;
            case Token::Break:
                var [ts1,nd1] = breakStatement (ts,omega);
                var [ts2,nd2] = [semicolon (ts1,omega),nd1];
                break;
            case Token::Continue:
                var [ts1,nd1] = continueStatement (ts,omega);
                var [ts2,nd2] = [semicolon (ts1,omega),nd1];
                break;
            case Token::LeftBrace:
                var [ts1,nd1] = block (ts, tau);
                var [ts2,nd2] = [ts1,new Ast::BlockStmt (nd1)];
                break;
            case Token::Switch:
                switch (hd (tl (ts))) {
                case Token::Type:
                    var [ts2,nd2] = switchTypeStatement (ts);
                    break;
                default:
                    var [ts2,nd2] = switchStatement (ts);
                    break;
                }
                break;
            case Token::Throw:
                var [ts1,nd1] = throwStatement (ts,omega);
                var [ts2,nd2] = [semicolon (ts1,omega),nd1];
                break;
            case Token::Try:
                var [ts2,nd2] = tryStatement (ts,omega);
                break;
            default:
                let [ts1,nd1] = expressionStatement (ts);
                var [ts2,nd2] = [semicolon (ts1,omega),nd1];
                break;
            }

            exit("Parser::statement ", ts2);
            return [ts2,nd2];
        }

        function substatement (ts: TOKENS, omega: OMEGA)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::substatement ", ts);

            switch (hd(ts)) {
            case Token::SemiColon:
                var [ts1,nd1] = [tl (ts), new Ast::EmptyStmt];
                break;
            default:
                var [ts1,nd1] = statement (ts,localBlk,omega);
                break;
            }

            exit("Parser::substatement ", ts1);
            return [ts1,nd1];
        }

        function printLn (ts:TokenStream) {
            enter ("printLn ",ts.n);
            if (coordList.length <= ts.n)
                print("line eos");
            else {
                let coord = coordList[ts.n];
                print ("ln ",coord[0]+1);
            }
            exit ("printLn");
        }

        function newline (ts: TOKENS)
            : boolean
        {
            let offset = ts.n;

            if (offset == 0)
                return true;  // first token, so follows newline, but whose asking?

            let coord = coordList[offset];
            let prevCoord = coordList[offset-1];
            //print("coord=",coord);
            //print("prevCoord=",prevCoord);

            if(coord[0] != prevCoord[0]) // do line coords match?
                return true;
            else 
                return false;
        }

        function semicolon (ts: TOKENS, omega: OMEGA)
            : [TOKENS]
        {
            enter("Parser::semicolon ", ts);

            switch (omega) {
            case fullStmt:
                switch (hd (ts)) {
                case Token::SemiColon:
                    // print ("semicolon found");
                    var ts1 = tl (ts);
                    break;
                case Token::EOS:
                case Token::RightBrace:
                    var ts1 = ts;
                    break;
                default:
                    if (newline (ts)) { 
                        var ts1=ts; 
                        //print ("inserting semicolon") 
                    }
                    else { 
                        throw "** error: expecting semicolon" 
                    }
                    break;
                }
                break;
            case abbrevStmt:  // Abbrev, ShortIf
                //print("abbrevStmt");
                switch (hd (ts)) {
                case Token::SemiColon:
                    var ts1 = tl (ts);
                    break;
                default:
                    var ts1 = ts;
                    break;
                }
                break;
            default:
                throw "unhandled statement mode";
            }

            exit("Parser::semicolon ", ts1);
            return ts1;
        }

        function expressionStatement (ts: TOKENS)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::expressionStatement ", ts);

            var [ts1,nd1] = listExpression (ts,allowIn);

            exit("Parser::expressionStatement ", ts1);
            return [ts1, new Ast::ExprStmt (nd1)];
        }

        function returnStatement (ts: TOKENS)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::returnStatement ", ts);

            ts = eat (ts, Token::Return);

            var [ts1,nd1] = listExpression (ts,allowIn);

            exit("Parser::returnStatement ", ts1);
            return [ts1, new Ast::ReturnStmt (nd1)];
        }

        function breakStatement (ts: TOKENS)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::breakStatement ", ts);

            ts = eat (ts, Token::Break);
            switch (hd (ts)) {
            case Token::SemiColon:
                var [ts1,nd1] = [tl (ts),null];
                break;
            case Token::RightBrace:
                var [ts1,nd1] = [ts,null];
                break;
            default:
                if (newline(ts)) {
                    var [ts1,nd1] = [ts,null];
                }
                else {
                    var [ts1,nd1] = identifier (ts);
                } 
                break;
            }

            exit("Parser::breakStatement ", ts1);
            return [ts1, new Ast::BreakStmt (nd1)];
        }

        function continueStatement (ts: TOKENS)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::continueStatement ", ts);

            ts = eat (ts, Token::Continue);
            switch (hd (ts)) {
            case Token::SemiColon:
                var [ts1,nd1] = [tl (ts),null];
                break;
            case Token::RightBrace:
                var [ts1,nd1] = [ts,null];
                break;
            default:
                if (newline(ts)) {
                    var [ts1,nd1] = [ts,null];
                }
                else {
                    var [ts1,nd1] = identifier (ts);
                } 
                break;
            }

            exit("Parser::continueStatement ", ts1);
            return [ts1, new Ast::ContinueStmt (nd1)];
        }

        function ifStatement (ts: TOKENS, omega)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::ifStatement ", ts);

            ts = eat (ts,Token::If);
            var [ts1,nd1] = parenListExpression (ts);
            var [ts2,nd2] = substatement (ts1, omega);
            switch (hd (ts2)) {
            case Token::Else:
                var [ts3,nd3] = substatement (tl (ts2), omega);
                break;
            default:
                var [ts3,nd3] = [ts2,null];
                break;
            }

            exit("Parser::ifStatement ", ts3);
            return [ts3, new Ast::IfStmt (nd1,nd2,nd3)];
        }

        /*

        WhileStatement(omega)
            while ParenListExpression Substatement(omega)

        */

        function whileStatement (ts: TOKENS, omega)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::whileStatement ", ts);

            ts = eat (ts,Token::While);
            var [ts1,nd1] = parenListExpression (ts);
            var [ts2,nd2] = substatement (ts1, omega); 
            var labels = [];
 
            exit("Parser::whileStatement ", ts2);
            return [ts2, new Ast::WhileStmt (nd1,nd2,labels)];
        }

        /*

            ForStatement(omega)
                for  (  ForInitialiser  ;  OptionalExpression  ;  OptionalExpression  )  Substatement(omega)
                for  (  ForInBinding  in  ListExpression(allowColon, allowIn)  )  Substatement(omega)
                for  each  ( ForInBinding  in  ListExpression(allowColon, allowIn)  )  Substatement(omega)
            
        */

        function forStatement (ts: TOKENS, omega: OMEGA)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::forStatement ", ts);

            cx.enterLetBlock ();

            ts = eat (ts,Token::For);
            ts = eat (ts,Token::LeftParen);
            var [ts1,nd1] = forInitialiser (ts);
            ts1 = eat (ts1,Token::SemiColon);
            var [ts2,nd2] = optionalExpression (ts1);
            ts2 = eat (ts2,Token::SemiColon);
            var [ts3,nd3] = optionalExpression (ts2);
            ts3 = eat (ts3,Token::RightParen);
            var [ts4,nd4] = substatement (ts3, omega); 
            var labels = [];

            var head = cx.exitLetBlock ();
 
            exit("Parser::forStatement ", ts4);
            return [ts4, new Ast::ForStmt (head,nd1,nd2,nd3,nd4,labels)];
        }

        /*

            ForInitialiser
                empty
                ListExpression(allowColon, noIn)
                VariableDefinition(noIn)
            
            ForInBinding
                Pattern(allowColon, noIn, allowExpr)
                VariableDefinitionKind VariableBinding(noIn)

        */

        function forInitialiser (ts: TOKENS)
            : [TOKENS, Ast::EXPR?]
        {
            enter("Parser::forInitialiser ", ts);

            switch (hd (ts)) {
            case Token::SemiColon:
                var [ts1,nd1] = [ts,null];
                break;
            case Token::Var:
                var [ts1,nd1] = variableDefinition (ts,noIn,localBlk,cx.pragmas.defaultNamespace,false,false);
                //assert (nd1.length==1);
                switch type (nd1[0]) {
                case (nd:Ast::ExprStmt) { nd1 = nd.Ast::expr }
                case (nd:*) { throw "error forInitialiser " + nd }
                }
                break;
            default:
                var [ts1,nd1] = listExpression (ts,noIn);
                break;
            }
            //print ("nd1=",nd1);
 
            exit("Parser::forInitialiser ", ts1);
            return [ts1,nd1];
        }

        /*

        OptionalExpression
            empty
            ListExpression(allowColon, allowIn)

        */

        function optionalExpression (ts: TOKENS)
            : [TOKENS, Ast::EXPR?]
        {
            enter("Parser::optionalExpression ", ts);

            switch (hd (ts)) {
            case Token::SemiColon:
            case Token::RightBrace:
                var [ts1,nd1] = [ts,null]
                break;
            default:
                var [ts1,nd1] = listExpression (ts,noIn);
                break;
            }
 
            exit("Parser::optionalExpression ", ts1);
            return [ts1,nd1];
        }

        /*

        SwitchStatement
            switch  ParenListExpression  {  CaseElements  }

        CaseElements
            empty
            CaseLabel
            CaseLabel  CaseElementsPrefix  CaseLabel
            CaseLabel  CaseElementsPrefix  Directives(abbrev)

        CaseElementsPrefix
            empty
            CaseElementsPrefix  CaseLabel
            CaseElementsPrefix  Directives(full)

        right recursive:

        CaseElementsPrefix
            empty
            CaseLabel  CaseElementsPrefix
            Directives(full)  CaseElementsPrefix

        */

        function switchStatement (ts: TOKENS)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::switchStatement ", ts);

            ts = eat (ts,Token::Switch);
            var [ts1,nd1] = parenListExpression (ts);
            ts1 = eat (ts1,Token::LeftBrace);
            switch (hd (ts1)) {
            case Token::Case:
            case Token::Default:
                var [ts2,nd2] = caseElementsPrefix (ts1);
                break;
            default:
                // do nothing
                break;
            }
            ts2 = eat (ts2,Token::RightBrace);

            var nd3 = []; // FIXME labels

            exit("Parser::switchStatement ", ts2);
            return [ts2, new Ast::SwitchStmt (nd1,nd2,nd3)];
        }

        function caseElementsPrefix (ts: TOKENS)
            : [TOKENS, Ast::CASES]
        {
            enter("Parser::caseElements ", ts);

            var ts1 = ts;
            var nd1 = [];
            while (hd (ts1) !== Token::RightBrace) {
                switch (hd (ts1)) {
                case Token::Case:
                case Token::Default:
                    var [ts1,ndx] = caseLabel (ts1);
                    nd1.push (new Ast::Case (ndx,[]));
                    break;
                default:
                    var [ts1,ndx] = directive (ts1,localBlk,fullStmt);  // 'abbrev' is handled by RightBrace check in head
                    for (var i=0; i<ndx.length; ++i) nd1[nd1.length-1].Ast::stmts.push (ndx[i]);
                    break;
                }
            }

            exit("Parser::caseElementsPrefix ", ts1);
            return [ts1,nd1];
        }

        /*

        CaseLabel
            case  ListExpression(allowColon,allowIn)
            default  :

        */

        function caseLabel (ts: TOKENS)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::caseLabel ", ts);

            switch (hd (ts)) {
            case Token::Case:
                var [ts1,nd1] = listExpression (tl (ts),allowIn);
                break;
            case Token::Default:
                var [ts1,nd1] = [tl (ts),null];
                break;
            default:
                throw "error caseLabel expecting case";
            }

            ts1 = eat (ts1,Token::Colon);

            exit("Parser::caseLabel ", ts1);
            return [ts1,nd1];
        }

        function throwStatement (ts: TOKENS)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::throwStatement ", ts);

            ts = eat (ts, Token::Throw);

            var [ts1,nd1] = listExpression (ts,allowIn);

            exit("Parser::throwStatement ", ts1);
            return [ts1, new Ast::ThrowStmt (nd1)];
        }

        function tryStatement (ts: TOKENS)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::tryStatement ", ts);

            ts = eat (ts, Token::Try);

            var [ts1,nd1] = block (ts,localBlk);
            var [ts2,nd2] = catches (ts1);
            switch (hd (ts2)) {
            case Token::Finally:
                var [ts3,nd3] = block (tl (ts2),localBlk);
                break;
            default:
                var [ts3,nd3] = [ts2,null];
                break;
            }

            exit("Parser::tryStatement ", ts3);
            return [ts3, new Ast::TryStmt (nd1,nd2,nd3)];
        }

        function catches (ts: TOKENS)
            : [TOKENS,Ast::CATCHES]
        {
            enter("Parser::catches ", ts);

            var ts1 = ts;
            var nd1 = [];
            while (hd (ts1)===Token::Catch) {
                [ts1,ndx] = catchClause (tl (ts1));
                nd1.push (ndx);
            }

            exit("Parser::catches ", ts1);
            return [ts1,nd1];
        }

        function catchClause (ts: TOKENS)
            : [TOKENS,Ast::CATCH]
        {
            enter("Parser::catchClause ", ts);

            ts = eat (ts,Token::LeftParen);
            var [ts1,nd1] = parameter (ts);
            ts1 = eat (ts1,Token::RightParen);
            var [ts2,nd2] = block (ts1,localBlk);

            var [k,[p,t]] = nd1;
            var [f,i] = desugarBindingPattern (p, t, new Ast::GetParam (0), Ast::noNS, Ast::varInit, false);
            let head = new Ast::Head (f,[]);

            exit("Parser::catchClause ", ts2);
            return [ts2,new Ast::Catch (head,i,nd2)];
        }

        /*

        SwitchTypeStatement
            switch  type  TypedExpression {  TypeCaseElements }
        
        TypeCaseElements
            TypeCaseElement
            TypeCaseElements  TypeCaseElement
            
        TypeCaseElement
            case  (  TypedPattern(allowColon, allowIn)  )  Blocklocal

        */

        function switchTypeStatement (ts: TOKENS)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::switchTypeStatement ", ts);

            ts = eat (ts,Token::Switch);
            ts = eat (ts,Token::Type);
            var [ts1,nd1] = typedExpression (ts);
            var [e,t] = nd1;
            ts1 = eat (ts1,Token::LeftBrace);
            var [ts2,nd2] = typeCases (ts1);
            ts2 = eat (ts2,Token::RightBrace);

            exit("Parser::switchTypeStatement ", ts2);
            return [ts2, new Ast::SwitchTypeStmt (e,t,nd2)];
        }

        /*

        TypedExpression
            ParenListExpression
            ParenListExpression  :  NullableTypeExpression

        */

        function typedExpression (ts: TOKENS)
            : [TOKENS,[Ast::EXPR,Ast::TYPE_EXPR]]
        {
            enter("Parser::typedExpression ", ts);

            var [ts1,nd1] = parenListExpression (ts);
            switch (hd (ts1)) {
            case Token::Colon:
                var [ts2,nd2] = nullableTypeExpression (tl (ts1));
                break;
            default:
                var [ts2,nd2] = [ts1,Ast::anyType];
                break;
            }

            exit("Parser::typedExpression ", ts2);
            return [ts2,[nd1,nd2]];
        }

        function typeCases (ts: TOKENS)
            : [TOKENS,Ast::CATCHES]
        {
            enter("Parser::typeCases ", ts);

            var ts1 = ts;
            var nd1 = [];
            while (hd (ts1)==Token::Case) {
                [ts1,ndx] = catchClause (tl (ts1));
                nd1.push (ndx);
            }

            exit("Parser::typeCases ", ts1);
            return [ts1,nd1];
        }

        (1/2)

        // DEFINITIONS

        /*

        VariableDefinition(beta)
            VariableDefinitionKind  VariableBindingList(beta)


        returns a statement, a list of block fixtures and var fixtures. if the caller
        is a class then it checks the static attribute to know if the var fixtures are
        class fixtures or instance fixtures

        */

        function variableDefinition (ts: TOKENS, beta: BETA, tau: TAU, ns, isPrototype, isStatic)
            : [TOKENS, Ast::STMTS]
        {
            enter("Parser::variableDefinition ", ts);

            let [ts1,nd1] = variableDefinitionKind (ts);

            switch (nd1) {
            case Ast::letConstTag:
                var it = Ast::letInit;
                var ro = true;
                break;
            case Ast::letVarTag:
                var it = Ast::letInit;
                var ro = false;
                break;
            case Ast::constTag:
                var it = Ast::varInit;
                var ro = true;
                break;
            case Ast::varTag:
                var it = Ast::varInit;
                var ro = false;
                break;
            default:
                throw "error variableDefinition kind " + nd1;
            }

            let [ts2,nd2] = variableBindingList (ts1, beta, ns, it, ro);
            let [fxtrs,init] = nd2;

            switch (nd1) {
            case Ast::letConstTag:
            case Ast::letVarTag:
                cx.addLetFixtures (fxtrs);
                var stmts = [new Ast::ExprStmt (init)];
                break;
            default:
                switch (tau) {
                case classBlk:
                    cx.addVarFixtures (fxtrs);
                    //cx.addVarInits ([init]);  // FIXME these aren't inits, they are a kind of settings
                    var stmts = [];
                    break;
                default:
                    cx.addVarFixtures (fxtrs);
                    var stmts = [new Ast::ExprStmt (init)];
                    break;
                }
            }

            exit("Parser::variableDefinition ", ts2);
            return [ts2,stmts];
        }

        /*

        VariableDefinitionKind
            const
            let
            let const
            var

        */

        function variableDefinitionKind (ts: TOKENS)
            : [TOKENS, Ast::VAR_DEFN_TAG]
        {
            enter("Parser::variableDefinitionKind ", ts);

            switch (hd (ts)) {
            case Token::Const:
                var [tsx,ndx] = [tl (ts), Ast::constTag];
                break;
            case Token::Var:
                var [tsx,ndx] = [tl (ts), Ast::varTag];
                break;
            case Token::Let:
                switch (hd (tl (ts))) {
                case Token::Const:
                    var [tsx,ndx] = [tl (tl (ts)), Ast::letConstTag];
                    break;
                case Token::Function:
                    throw "internal error: variableDefinitionKind after let";
                    break;
                default:
                    var [tsx,ndx] = [tl (ts), Ast::letVarTag];
                    break;
                }
                break;
            default:
                throw "internal error: variableDefinitionKind";
                break;
            }

            exit("Parser::variableDefinitionKind ", tsx);
            return [tsx,ndx];
        }

        /*

        VariableBindingList(beta)
            VariableBinding(beta)
            VariableBindingList(beta)  ,  VariableBinding(beta)

        VariableBinding(beta)
            TypedIdentifier
            TypedPattern(noIn)  VariableInitialisation(beta)

        VariableInitialisation(beta)
            =  AssignmentExpression(beta)

        */

        function variableBindingList (ts: TOKENS, beta: BETA, ns: Ast::NAMESPACE, 
                                      it: Ast::INIT_TARGET, ro: boolean )
            : [TOKENS, [Ast::FIXTURES, Ast::EXPR]]
        {
            enter("Parser::variableBindingList ", ts);

            var [ts1,nd1] = variableBinding (ts, beta);
            var [ts2,nd2] = variableBindingListPrime (ts1, beta);

            var [f1,i1] = nd1;  // FIXME: fold into patterns above when it works in the RI
            var [f2,i2] = nd2;

            i1 = [i1];   // i1 is actually only a single expression

            for (let n in f2) f1.push (f2[n]);  // FIXME: use concat when it works in the RI
            for (let n in i2) i1.push (i2[n]);

            exit ("Parser::variableBindingList ", ts2);
            return [ts2,[f1,new Ast::ListExpr(i1)]];  // FIXME if only one element don't create listexpr

            function variableBindingListPrime (ts: TOKENS)
                : [TOKENS, [Ast::FIXTURES, Ast::EXPRS]]
            {
                enter("Parser::variableBindingListPrime ", ts);
        
                switch (hd (ts)) {
                case Token::Comma:
                    var [ts1,nd1] = variableBinding (tl (ts), beta);
                    var [ts2,nd2] = variableBindingListPrime (ts1);

                    var [f1,i1] = nd1;  // FIXME: fold into patterns above when it works in the RI
                    var [f2,i2] = nd2;

                    for (let n in f2) f1.push (f2[n]);  // FIXME: use concat when it works in the RI
                    for (let n in i2) i1.push (i2[n]);
                    break;
                default:
                    var [ts2,nd2] = [ts,[[],[]]];
                    var [f1,i1] = nd2;
                    break;
                }

                exit ("Parser::variableBindingListPrime ", ts2);
                return [ts2,[f1,i1]];
            }

            function variableBinding (ts: TOKENS, beta: BETA)
                : [TOKENS, [Ast::FIXTURES, Ast::EXPR]]
            {
                enter("Parser::variableBinding ", ts);
                    
                let [ts1,nd1] = typedPattern (ts,beta);
                let [p,t] = nd1;
                switch (hd (ts1)) {
                case Token::Assign:
                    let [ts2,nd2] = assignmentExpression (tl (ts1), beta);
                    switch (hd (ts2)) {
                    case Token::In:
                        if (beta === noIn) {
                            // in a binding form
                            break;
                        } // else fall through
                    default:
                        var [tsx,ndx] = [ts2,desugarBindingPattern (p,t,nd2,ns,it,ro)];
                        break;
                    }
                    break;
                default:
                    switch (hd (ts1)) {
                    case Token::In:
                        if (beta === noIn) {
                            // in a binding form
                            break;
                        } // else fall through
                    default:
                        switch type (p) {
                        case (p: IdentifierPattern) {
                            var [tsx,ndx] = [ts1,desugarBindingPattern (p,t,null,ns,it,ro)];
                        }
                        case (x : *) {
                            throw "destructuring pattern without initializer";
                        }
                        }
                        break;
                    }
                }
                exit("Parser::variableBinding ", tsx);
                return [tsx,ndx];
            }
        }

        function variableBinding (ts: TOKENS, beta: BETA, ns: Ast::NAMESPACE, it: Ast::INIT_TARGET)
            : [TOKENS, [Ast::FIXTURES, Ast::EXPRS]]
        {
            enter("Parser::variableBinding ", ts);

            let [ts1,nd1] = typedPattern (ts,beta);
            let [p,t] = nd1;
            switch (hd (ts1)) {
            case Token::Assign:
                let [ts2,nd2] = assignmentExpression (tl (ts1), beta);
                switch (hd (ts2)) {
                case Token::In:
                    if (beta === noIn) {
                        // in a binding form
                        break;
                    } // else fall through
                default:
                    var [tsx,ndx] = [ts2,desugarBindingPattern (p,t,nd2,ns,it,ro)];
                    break;
                }
                break;
            default:
                switch (hd (ts1)) {
                case Token::In:
                    if (beta === noIn) {
                        // in a binding form
                        break;
                    } // else fall through
                default:
                    switch type (p) {
                    case (p: IdentifierPattern) {
                        var [tsx,ndx] = [ts1,desugarPattern (p,t,null,ns,it)];
                    }
                    case (x : *) {
                        throw "destructuring pattern without initializer";
                    }
                    }
                break;
                }
            }
            exit("Parser::variableBinding ", tsx);
            return [tsx,ndx];
        }

        /*

        FunctionDefinition(class)
            function  ClassName  ConstructorSignature  FunctionBody(allowIn)
            function  FunctionName  FunctionSignature  FunctionBody(allowIn)
            
        FunctionDefinition(tau)
            function  FunctionName  FunctionSignature  FunctionBody(allowIn)
            let  function  FunctionName  FunctionSignature  FunctionBody(allowIn)
            const  function  FunctionName  FunctionSignature  FunctionBody(allowIn)

        */

        function functionDefinition (ts: TOKENS, tau: TAU, omega: OMEGA, kind, ns, isFinal, isOverride, isPrototype, isStatic, isAbstract)
            : [TOKENS, Ast::STMTS]
        {
            enter("Parser::functionDefinition ", ts);

            ts = eat (ts, Token::Function);

            var [ts1,nd1] = functionName (ts);
            var [ts2,nd2] = functionSignature (ts1);

            cx.enterVarBlock ();
            var [ts3,nd3] = functionBody (ts2, allowIn, omega);
            var vars = cx.exitVarBlock ();

            var {params:params,paramSettings:paramSettings,defaults:defaults,resultType:resultType,thisType:thisType,hasRest:hasRest} = nd2;
            var func = new Ast::Func (nd1,false,nd3,params,paramSettings,vars,defaults,resultType);

            var name = new Ast::PropName ({ns:ns,id:nd1.ident});
            var fxtr = new Ast::MethodFixture (func,new Ast::SpecialType (new Ast::AnyType),true,isOverride,isFinal);
            cx.addVarFixtures ([[name,fxtr]]);

            exit("Parser::functionDefinition ", ts3);
            return [ts3, []];
        }

        /*

        ConstructorDefinition
            function  ClassName  ConstructorSignature  FunctionBody(allowIn)

        */

        function constructorDefinition (ts: TOKENS, omega, ns)
            : [TOKENS, Ast::STMTS]
        {
            enter("Parser::constructorDefinition ", ts);

            ts = eat (ts, Token::Function);

            var [ts1,nd1] = identifier (ts);
            var [ts2,nd2] = constructorSignature (ts1);

            cx.enterVarBlock ();
            var [ts3,nd3] = functionBody (ts2, allowIn, omega);
            var vars = cx.exitVarBlock ();

            var {params:params,paramSettings:paramSettings,defaults:defaults,hasRest:hasRest,settings:settings,superArgs:superArgs} = nd2;

            // print ("superArgs=",superArgs);
            // print ("settings=",settings);
            var func = new Ast::Func ({kind:new Ast::Ordinary,ident:nd1},false,nd3,params,paramSettings,vars,defaults,Ast::voidType);
            var ctor = new Ast::Ctor (settings,superArgs,func);

            if (cx.ctor !== null) {
                throw "constructor already defined";
            }

            cx.ctor = ctor;

            exit("Parser::constructorDefinition ", ts3);

            return [ts3, []];
        }

        /*

        ConstructorSignature
            TypeParameters  (  Parameters  )  ConstructorInitialiser
        
        */

        type CTOR_SIG = 
          { typeParams : [Ast::IDENT]
          , params : Ast::HEAD
          , paramSettings : Ast::EXPR
          , paramTypes : [Ast::TYPE_EXPR]
          , defaults : [Ast::EXPR]
          , hasRest: boolean
          , settings : [Ast::EXPR]
          , superArgs: [Ast::EXPR] }

        type FUNC_SIG = 
          { typeParams : [Ast::IDENT]
          , params : Ast::HEAD
          , paramSettings : Ast::EXPR
          , paramTypes : [Ast::TYPE_EXPR]
          , defaults : [Ast::EXPR]
          , returnType : Ast::TYPE_EXPR
          , thisType : Ast::TYPE_EXPR?
          , hasRest : boolean }


        function constructorSignature (ts: TOKENS)
            : [TOKENS, CTOR_SIG]
        {
            enter("Parser::constructorSignature ", ts);

            var [ts1,nd1] = typeParameters (ts);
            ts1 = eat (ts1, Token::LeftParen);
            var [ts2,nd2,hasRest] = parameters (ts1);
            ts2 = eat (ts2, Token::RightParen);
            var [ts3,settings,superArgs] = constructorInitialiser (ts2);

            // Translate bindings and init steps into fixtures and inits (HEAD)
            let [[f,i],e,t] = nd2;

            var ndx = { typeParams: []
                      , params: new Ast::Head (f,[])
                      , paramSettings: i
                      , paramTypes: t
                      , defaults: e
                      , hasRest: hasRest
                      , settings: settings
                      , superArgs: superArgs };

            exit("Parser::constructorSignature ", ts3);

            return [ts3,ndx]
        }

        /*

        ConstructorInitialiser
            empty
            : SettingList
            : SettingList  ,  SuperInitialiser
            : SuperInitialiser
        
        SuperInitialiser
            super  Arguments

        constructor initializers are represented by two lists. the first
        list represents the initializers and will consist of set exprs or
        let exprs (if there are temps for destructuring). the second list
        represents the arguments to the call the the super constructor

        */

        function constructorInitialiser (ts: TOKENS)
            : [TOKENS, [Ast::EXPR], [Ast::EXPR]]
        {
            enter("Parser::constructorInitialiser ", ts);

            switch (hd (ts)) {
            case Token::Colon:
                switch (hd (tl (ts))) {
                case Token::Super:
                    var [ts1,nd1] = [tl (tl (ts)),[]]; // no settings
                    var [ts2,nd2] = this.arguments (ts1);
                    break;
                default:
                    var [ts1,nd1] = settingList (tl (ts));
                    switch (hd (ts1)) {
                    case Token::Super:
                        var [ts2,nd2] = this.arguments (tl (ts1));
                        break;
                    default:
                        var [ts2,nd2] = [ts1,[]];
                        break;
                    }
                    break;
                }
                break;
            default:
                var ts2 = ts;
                var nd1 = [];
                var nd2 = [];
                break;
            }

            exit("Parser::constructorInitialiser ", ts2);
            return [ts2,nd1,nd2];
        }


        /*

        SettingList
            Setting
            SettingList  ,  Setting
        
        Setting
            Pattern(noIn, noExpr)  VariableInitialisation(allowIn)
        
        */

        function settingList (ts: TOKENS)
            : [TOKENS, [Ast::EXPR]]
        {
            enter("Parser::settingList ", ts);

            function settingListPrime (ts: TOKENS )
                : [TOKENS,[Ast::EXPR]]
            {
                enter("Parser::settingListPrime ", ts);
        
                switch (hd (ts)) {
                case Token::Comma:
                    switch (hd (tl (ts))) {
                    case Token::Super:
                        var [ts2,nd2] = [tl (ts), []];  // eat the comma
                        break;
                    default:
                        var [ts1,nd1] = setting (tl (ts));
                        var [ts2,nd2] = settingListPrime (ts1);
                        nd2.unshift (nd1);
                        break;
                    }
                    break;
                default:
                    var [ts2,nd2] = [ts,[]];
                    break;
                }

                exit ("Parser::settingListPrime ", ts2);
                return [ts2,nd2];
            }

            var [ts1,nd1] = setting (ts);
            var [ts2,nd2] = settingListPrime (ts1);

            nd2.unshift (nd1);

            exit ("Parser::settingList ", ts2);
            return [ts2,nd2];
        }

        /*

        Setting
            Pattern(noIn, allowExpr)  VariableInitialisation(allowIn)


            function A (a) : [q::x,r::y] = a { }


            let ($t0 = a) q::x = $t0[0], r::y = $t0[1]

            let ($t0 = a)
                init (This,q,[x,$t0[0]),
                init (This,r,[y,$t0[1])


        */

        function initFromAssignStep (as : Ast::AssignStep) {
            enter ("initFromAssignStep");

            switch type ((as.Ast::le).Ast::ident) {
            case (ident: Ast::Identifier) {
                var ns = cx.pragmas.defaultNamespace;
                var name = new Ast::PropName ({ns:ns,id:ident.Ast::ident});
                var init = new Ast::InitExpr (Ast::instanceInit,new Ast::Head ([],[]),[[name,as.Ast::re]]);
            }
            case (le: Ast::QualifiedIdentifier) {
                var qual = le.Ast::qual;
                var ident = le.Ast::ident;
                var ns = cx.pragmas.defaultNamespace; //cx.evalIdentExpr (qual);
                var name = new Ast::PropName ({ns:ns,id:ident});
                var init = new Ast::InitExpr (Ast::instanceInit,new Ast::Head([],[]),[[name,as.Ast::re]]);
            }
            case (le: *) {
                throw "invalid setting target " + (as.Ast::le).Ast::ident;
            }
            }

            exit ("initFromAssignStep");
            return init;
        }

        function setting (ts: TOKENS)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::setting ", ts);

            var [ts1,nd1] = pattern (ts,allowIn,allowExpr);
            ts1 = eat (ts1,Token::Assign);
            var [ts2,nd2] = assignmentExpression (ts1,allowIn);

            var [tsx,[fxtrs,ndx]] = [ts2,desugarBindingPattern (nd1, new Ast::SpecialType (new Ast::AnyType), nd2, null, Ast::instanceInit)];
            // assert fxtrs is empty

            exit("Parser::setting ", tsx);
            return [tsx,ndx];
        }

        /*

        FunctionName
            Identifier
            OverloadedOperator
            get  Identifier
            set  Identifier

        */

        function functionName (ts: TOKENS)
            : [TOKENS, Ast::FUNC_NAME]
        {
            enter("Parser::functionName ", ts);

            switch (hd (ts)) {
            case Token::Get:
                var [ts1,nd1] = identifier (tl (ts));
                var tsx = ts1;
                var ndx = {kind: new Ast::Get, ident: nd1};
                break;
            case Token::Set:
                var [ts1,nd1] = identifier (tl (ts));
                var tsx = ts1;
                var ndx = {kind: new Ast::Set, ident: nd1};
                break;
            case Token::Plus:
            case Token::Minus:
                // FIXME add other operators here
                break;
            default:
                var [ts1,nd1] = identifier (ts);
                var tsx = ts1;
                var ndx = {kind: new Ast::Ordinary, ident: nd1};
                break;
            }

            exit("Parser::functionName ", ts1);

            return [tsx,ndx]
        }

        /*

        FunctionSignature
            TypeParameters  (  Parameters  )  ResultType
            TypeParameters  (  this  :  PrimaryIdentifier  )  ResultType
            TypeParameters  (  this  :  PrimaryIdentifier  ,  NonemptyParameters  )  ResultType

        there are two differences between a BINDING_IDENT and a FIXTURE_NAME: the namespace on
        properties, and the offset on parameter indicies.

        */


        function fixtureNameFromBindingIdent (ns,bi: Ast::BINDING_IDENT)
            : Ast::FIXTURE_NAME 
        {
            switch type (bi) {
            case (bi:Ast::PropIdent) {
                var fn = new Ast::PropName ({ns:ns,id:bi.Ast::ident});
            }
            case (bi:Ast::ParamIdent) {
                var fn = new Ast::TempName (bi.Ast::index);
            }
            case (bi:Ast::TempIdent) {
                var fn = new Ast::TempName (bi.Ast::index);
            }
            }
            return fn;
        }

        function fixturesFromBindings (ns,bs /*: [Ast::BINDING]*/) // FIXME: RI bug
            : Ast::FIXTURES {
            if(bs.length === 0) {
                return [];
            }
            var b0 = bs[0];
            var n0 = fixtureNameFromBindingIdent (ns,b0.Ast::ident);
            var f0 = new Ast::ValFixture (b0.Ast::type,false);
            var fs = fixturesFromBindings (ns,bs.slice(1,bs.length));
            fs.unshift ([n0,f0]);
            return fs;
        }

        function initsFromInitSteps (ns,ss /*: [Ast::INIT_STEP]*/ )
            /* : Ast::INITS */ 
        {

            if(ss.length === 0) {
                return [];
            }
            var s0 = ss[0];

            var n0 = fixtureNameFromBindingIdent (ns,s0.Ast::ident);
            var i0 = [n0,s0.Ast::expr];
            var is = initsFromInitSteps (ns,ss.slice(1,ss.length));
            is.unshift (i0);

            return is;
        }

        function headFromBindingInits ([bindings,steps] /*: Ast::BINDING_INITS*/, ns )
            // : Ast::HEAD  
        {
            var fixtures = fixturesFromBindings (cx.pragmas.defaultNamespace,bindings);
            var inits = initsFromInitSteps (cx.pragmas.defaultNamespace, steps);
            return {fixtures:fixtures, inits:inits}
        }

        function functionSignature (ts: TOKENS)
            : [TOKENS, FUNC_SIG]
        {
            enter("Parser::functionSignature ", ts);

            var [ts1,nd1] = typeParameters (ts);
            ts1 = eat (ts1, Token::LeftParen);
            switch (hd (ts1)) {
            case Token::This:
                // FIXME
                break;
            default:
                var [ts2,nd2,hasRest] = parameters (ts1);
                break;
            }
            ts2 = eat (ts2, Token::RightParen);
            var [ts3,nd3] = resultType (ts2);

            // Translate bindings and init steps into fixtures and inits (HEAD)
            let [[f,i],e,t] = nd2;

            var ndx = { typeParams: []
                      , params: new Ast::Head (f,[])
                      , paramSettings: i
                      , paramTypes: t
                      , defaults: e
                      , ctorInits: null
                      , resultType: nd3
                      , thisType: null
                      , hasRest: hasRest };

            exit("Parser::functionSignature ", ts3);

            return [ts3,ndx]
        }

        /*

        TypeParameters
            empty
            .<  TypeParameterList  >

        */

        function typeParameters (ts: TOKENS)
            : [TOKENS, [Ast::IDENT]]
        {
            enter("Parser::typeParameters ", ts);

            switch (hd (ts)) {
            case Token::LeftDotAngle:
                ts = eat (ts, Token::LeftDotAngle);
                var [ts1,nd1] = typeParameterList (ts);
                ts1 = eat (ts1, Token::GreaterThan);
                break;
            default:
                var [ts1,nd1] = [ts,[]];
                break;
            }

            exit("Parser::typeParameters ", ts1);
            return [ts1,nd1];
        }

        /*

        TypeParameterList
            Identifier
            Identifier  ,  TypeParameterList

        */

        function typeParameterList (ts: TOKENS)
            : [TOKENS, [Ast::IDENT]]
        {
            enter("Parser::typeParameterList ", ts);

            function typeParameterListPrime (ts)
                : [TOKENS, [Ast::IDENT]] {

                switch (hd (ts)) {
                case Token::Comma:
                    ts = eat (ts, Token::Comma);
                    var [ts1,nd1] = identifier (ts);
                    var [ts2,nd2] = typeParameterListPrime (ts1);
                    break;
                default:
                    var [ts2,nd2] = [ts,[]];
                    break;
                }
            }

            var [ts1,nd1] = identifier (ts);
            var [ts2,nd2] = typeParameterListPrime (ts1);

            nd2.unshift (nd1);

            exit("Parser::typeParameterList ", ts2);
            return [ts2,nd2];
        }

        /*

        Parameters
            empty
            NonemptyParameters

        */

        function parameters (ts: TOKENS)
            : [TOKENS, [[Ast::FIXTURES, Ast::EXPRS], [Ast::EXPR], [Ast::TYPE_EXPR]], boolean]
        {
            enter("Parser::parameters ", ts);

            switch (hd (ts)) {
            case Token::RightParen:
                let b1 = [];
                let i1 = [];
                let e1 = [];
                let t1 = [];
                var [ts1,nd1,hasRest] = [ts,[[[],[]],e1,t1],false];
                break;
            default:
                var [ts1,nd1,hasRest] = nonemptyParameters (ts,0,false);
                break;
            }

            exit("Parser::parameters ", ts1);
            return [ts1,nd1,hasRest];
        }

        /*

        NonemptyParameters
            ParameterInit
            ParameterInit  ,  NonemptyParameters
            RestParameter

        */

        function nonemptyParameters (ts: TOKENS, n:int, initRequired)
            : [TOKENS, [[Ast::FIXTURES,Ast::EXPRS], Ast::EXPRS, Ast::TYPE_EXPRS], boolean]
        {
            enter("Parser::nonemptyParameters ", ts);

            switch (hd (ts)) {
            case Token::TripleDot:
                var [ts1,nd1] = restParameter (ts,n);
                /* var */ hasRest = true;
                break;
            default:
                var [ts1,nd1] = parameterInit (ts,n,initRequired);
                switch (hd (ts1)) {
                case Token::Comma:
                    ts1 = eat (ts1, Token::Comma);
                    let [[f1,i1],e1,t1] = nd1;
                    var [ts2,nd2,hasRest] = nonemptyParameters (ts1, n+1, e1.length!=0);
                    let [[f2,i2],e2,t2] = nd2;
                    // FIXME when Array.concat works
                    for (let p in f2) f1.push(f2[p]);
                    for (let p in i2) i1.push(i2[p]);
                    for (let p in e2) e1.push(e2[p]);
                    for (let p in t2) t1.push(t2[p]);
                    var [ts1,nd1,hasRest] = [ts2,[[f1,i1],e1,t1],hasRest];
                    break;
                case Token::RightParen:
                    /* var */ hasRest = false;
                    break;
                default:
                    throw "unexpected token in nonemptyParameters";
                }
                break;
            }

            exit("Parser::nonemptyParameters ", ts1);
            return [ts1,nd1,hasRest];
        }

        /*

        ParameterInit
            Parameter
            Parameter = NonAssignmentExpression(AllowIn)

        */

        function parameterInit (ts: TOKENS, n: int, initRequired)
            : [TOKENS,[[Ast::FIXTURES,Ast::EXPRS], Ast::EXPR, Ast::TYPE_EXPR]]
        {
            enter("Parser::parameterInit ", ts);

            var [ts1,nd1] = parameter (ts,n);
            switch (hd (ts1)) {
            case Token::Assign:
                ts1 = eat (ts1, Token::Assign);
                var [ts2,nd2] = nonAssignmentExpression(ts1,allowIn);
                nd2 = [nd2];
                break;
            default:
                if (initRequired) {
                    throw "expecting default value expression";
                }
                var [ts2,nd2] = [ts1,[]];
                break;
            }

            var [k,[p,t]] = nd1;
            var [f,i] = desugarBindingPattern (p, t, new Ast::GetParam (n), Ast::noNS, Ast::letInit, false);
            f.push ([new Ast::TempName (n), new Ast::ValFixture (t,false)]); // temp for desugaring
            exit("Parser::parameterInit ", ts2);
            return [ts2,[[f,[i]],nd2,t]];
        }

        /*

        Parameter
            ParameterKind  TypedIdentifier(AllowIn)
            ParameterKind  TypedPattern

        */

        function parameter (ts: TOKENS, n)
            : [TOKENS, [Ast::VAR_DEFN_TAG, [PATTERN, Ast::TYPE_EXPR]]]
        {
            enter("Parser::parameter ", ts);

            var [ts1,nd1] = parameterKind (ts);
            var [ts2,nd2] = typedPattern (ts1,allowIn);

            exit("Parser::parameter ", ts2);
            return [ts2,[nd1,nd2]];
        }

        /*

        ParameterKind
            empty
            const

        */

        function parameterKind (ts: TOKENS)
            : [TOKENS, Ast::VAR_DEFN_TAG]
        {
            enter("Parser::parameterKind ", ts);

            switch (hd (ts)) {
            case Ast::Const:
                ts = eat (ts, Ast::Const);
                var [ts1,nd1] = [ts, new Ast::Const];
                break;
            default:
                var [ts1,nd1] = [ts, new Ast::Var];
                break;
            }

            exit("Parser::parameterKind ", ts1);
            return [ts1,nd1];
        }

        /*

        ResultType
            empty
            :  void
            :  NullableTypeExpression

        */

        function resultType (ts: TOKENS)
            : [TOKENS, [Ast::IDENT]]
        {
            enter("Parser::resultType ", ts);

            switch (hd (ts)) {
            case Token::Colon:
                ts = eat (ts, Token::Colon);
                switch (hd (ts)) {
                case Token::Void:
                    ts = eat (ts, Token::Void);
                    var [ts1,nd1] = [ts,new Ast::SpecialType (new Ast::VoidType)];
                    break;
                default:
                    var [ts1,nd1] = nullableTypeExpression (ts);
                    break;
                }
                break;
            default:
                var [ts1,nd1] = [ts,new Ast::SpecialType (new Ast::AnyType)];
                break;
            }

            exit("Parser::resultType ", ts1);
            return [ts1,nd1];
        }

        /*

            FunctionBody(beta)
                Block(local)
                AssignmentExpression(beta)

        */

        function functionBody (ts: TOKENS, beta: BETA, omega)
            : [TOKENS, Ast::BLOCK]
        {
            enter("Parser::functionBody ", ts);

            switch (hd (ts)) {
            case Token::LeftBrace:
                var [ts1,nd1] = block (ts,localBlk);
                break;
            default:
                var [ts1,nd1] = assignmentExpression (ts,beta);
                ts1 = semicolon (ts1,omega);
                var nd1 = new Ast::Block ({fixtures:[],inits:[]},[new Ast::ReturnStmt (nd1)],null);
                break;
            }

            exit("Parser::functionBody ", ts1);
            return [ts1,nd1];
        }

        function classDefinition (ts: TOKENS, ns: Ast::NAMESPACE, isDynamic)
            : [TOKENS, Ast::STMTS]
        {
            enter("Parser::classDefinition ", ts);

            ts = eat (ts, Token::Class);

            var [ts1,nd1] = identifier (ts);
            // print ("class ",nd1);
            var [ts2,nd2] = typeSignature (ts1);
            var [ts3,nd3] = classInheritance (ts2);
            currentClassName = nd1;
            cx.enterVarBlock ();
            var [ts4,blck] = classBody (ts3);
            var ihead = cx.exitVarBlock ();
            currentClassName = "";

            var name = {ns:ns,id:nd1};

            var ctor = cx.ctor;
            if (ctor===null)
            {
                let isNative = false;
                let blck = new Ast::Block ({fixtures:[],inits:[]},[]);
                let params = {fixtures:[],inits:[]};
                let paramSettings = [];
                let vars = {fixtures:[],inits:[]};
                let defaults = [];
                let type = Ast::anyType;
                let func = new Ast::Func ({kind:new Ast::Ordinary,ident:nd1},isNative,blck,params,paramSettings,vars,defaults,type);
                var ctor = new Ast::Ctor ([],[],func);
            }
            
            // var [i,j] = o
            // var $t = o
            // var i = $t[0]
            // var j = $t[1]

            // let ($t=o) init

            var baseName = {ns: new Ast::PublicNamespace (""), id: "Object"}
            var interfaceNames = [];
            var chead = new Ast::Head ([],[]);
            var ctype = Ast::anyType;
            var itype = Ast::anyType;
            var cls = new Ast::Cls (name,baseName,interfaceNames,ctor,chead,ihead,ctype,itype);

            var fxtrs = [[new Ast::PropName(name),new Ast::ClassFixture (cls)]];
            cx.addVarFixtures (fxtrs);
            cx.ctor = null;

            var ss4 = [new Ast::ClassBlock (name,blck)];

            exit("Parser::classDefinition ", ts4);

            return [ts4, ss4];
        }

        /*

        Typesignature
            TypeParameters
            TypeParameters  !

        */

        function typeSignature (ts: TOKENS)
            : [TOKENS, [Ast::IDENT], boolean]
        {
            enter("Parser::className ", ts);

            var [ts1,nd1] = typeParameters (ts);

            switch (hd (ts1)) {
            case Token::Not:
                var [ts2,nd2] = [tl (ts1), true];
                break;
            default:
                var [ts2,nd2] = [ts1, false];
                break;
            }

            exit("Parser::typeSignature ", ts2);

            return [ts2,nd1,nd2];
        }

        function classInheritance (ts: TOKENS)
            : [TOKENS, [Ast::IDENT_EXPR]]
        {
            enter("Parser::classInheritance ", ts);

            switch (hd (ts)) {
            case Token::Extends:
                var [ts1,nd1] = primaryName (tl (ts));
                switch (hd (ts)) {
                case Token::Implements:
                    var [ts2,nd2] = primaryNameList (tl (ts));
                    break;
                default:
                    var [ts2,nd2] = [ts1,[]];
                    break;
                }
                break;
            case Token::Implements:
                var [ts1,nd1] = [ts,[]];
                var [ts2,nd2] = primaryNameList (tl (ts1));
                break;
            default:
                var [ts1,nd1] = [ts,[]];
                var [ts2,nd2] = [ts1,[]];
                break;
            }

            exit("Parser::classInheritance ", ts2);

            return [ts2,nd2];
        }

        function classBody (ts: TOKENS)
            : [TOKENS, Ast::BLOCK]
        {
            enter("Parser::classBody ", ts);

            var [ts1,blck] = block (ts,classBlk);

            exit("Parser::classBody ", ts1);

            return [ts1,blck];
        }

        /*

        NamespaceDefinition(omega)
            namespace  Identifier  NamespaceInitialisation  Semicolon(omega)

        NamespaceInitialisation
            empty
            =  StringLiteral
            =  PrimaryName

        */

        function namespaceDefinition (ts: TOKENS, omega: OMEGA, ns: Ast::NAMESPACE )
            : [TOKENS, Ast::STMTS]
        {
            enter("Parser::namespaceDefinition ", ts);

            function getAnonymousName (seedStr) {
                return seedStr;  // FIXME
            }

            ts = eat (ts,Token::Namespace);
            var [ts1,nd1] = identifier (ts);
            var [ts2,nd2] = namespaceInitialisation (ts1);
            ts2 = semicolon (ts2,omega);

            if (nd2 === null) 
            {
                var nsVal = new Ast::AnonymousNamespace (getAnonymousName(nd1));
            }
            else 
            {
                var nsVal = new Ast::UserNamespace (nd2);
            }

            var name = new Ast::PropName ({ns:ns, id:nd1});
            var fxtr = new Ast::NamespaceFixture (nsVal);
            cx.addVarFixtures ([[name,fxtr]]);

            exit("Parser::namespaceDefinition ", ts2);
            return [ts2,[]];
        }

        function namespaceInitialisation (ts: TOKENS)
            : [TOKENS, Ast::IDENT]
        {
            enter("Parser::namespaceInitialisation ", ts);

            switch (hd (ts)) {
            case Token::Assign:
                switch (hd (tl (ts))) {
                case Token::StringLiteral:
                    var [ts1,nd1] = [tl (tl (ts)), tokenText (tl (ts).head())];
                    break;
                default:
                    var [ts1,nd1] = primaryName (tl (ts));
                    nd1 = cx.resolveNamespaceFromIdentExpr (nd1);  // FIXME not implemented
                    break;
                }
                break;
            default:
                var [ts1,nd1] = [ts,null];
                break;
            }

            exit("Parser::namespaceInitialisation ", ts1);
            return [ts1,nd1];
        }


        /*

        TypeDefinition(omega)
            type  Identifier  TypeInitialisation  Semicolon(omega)

        TypeInitialisation
            =  NullableTypeExpression

        */

        function typeDefinition (ts: TOKENS, omega: OMEGA, ns: Ast::NAMESPACE)
            : [TOKENS, Ast::STMTS]
        {
            enter("Parser::typeDefinition ", ts);

            ts = eat (ts,Token::Type);
            var [ts1,nd1] = identifier (ts);
            // print ("type ",nd1);

            ts1 = eat (ts1,Token::Assign);
            var [ts2,nd2] = nullableTypeExpression (ts1);
            ts2 = semicolon (ts2, omega);

            var name = new Ast::PropName ({ns:ns, id:nd1});
            var fxtr = new Ast::TypeFixture (nd2);
            cx.addVarFixtures ([[name,fxtr]]);

            exit("Parser::typeDefinition ", ts2);
            return [ts2,[]];
        }

        (1/2)

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
                    var nd1 = [new Ast::ExprStmt (nd1)];
                    ts1 = tl (ts1);
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
                            attrs.ns = cx.evalIdentExpr (ie);
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
                   , true: false
                   , false: false
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
                    var [ts1,nd1] = primaryName (ts,allowIn);
                    nd.ns = cx.evalIdentExpr (nd1);
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
                    var [ts1,nd1] = primaryName (ts,allowIn);
                    nd.ns = cx.evalIdentExpr (nd1);
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

            while (hd (ts)===Token::Use) {
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
            this.coordList = cs;
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

            current_package = "";
            current_class = "";

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

            // Postfix expressions

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
                var parser = new Parser(p);
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

    //    test ()
}

