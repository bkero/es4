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

public namespace Parse;

{
    use default namespace Parse;
    use namespace Lex;

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
            if (pragmas==null)
            {
                this.openNamespaces = [[]];
                this.defaultNamespace = new Ast::PublicNamespace ("");
            }
            else
            {
                this.openNamespaces = Util::copyArray (pragmas.openNamespaces);
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
        var pragmas: PRAGMAS
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
            use namespace Ast;
            enter ("enterVarBlock");
            let varHead = new Ast::Head ([],[]);
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

        function hasFixture (fxtrs,fb) {
            use namespace Ast;
            let [fn,f1] = fb;
            switch type (fn) {
            case (fn: Ast::PropName) {
                if (hasName (fxtrs,fn.name.id,fn.name.ns)) {
                    print("hasName ",ns,"::",id);
                    let f2 = getFixture (fxtrs,id,ns);
                    if (f1 is Ast::ValFixture && f2 is Ast::ValFixture) {
                        if (f1.Ast::type==Ast::anyType) return true;
                        else if (f2.Ast::type==Ast::anyType) return true;
                        // other positive cases here
                    }
                    throw "incompatible fixture redef "+fn.id;
                }
            }
            case (fn: Ast::TempName) {
                return false;  // for now
            }
            }
        }

        function addVarFixtures (fxtrs) 
        {
            use namespace Ast;
            let varHead = this.varHeads[this.varHeads.length-1];
            for (let n = 0, len = fxtrs.length; n < len; ++n)  // until array conact works
            {
                let fb = fxtrs[n];
                /// if (!hasFixture (varHead.Ast::fixtures,fb)) {
                    varHead.fixtures.push (fxtrs[n]);
                /// }
            }
        }

        function addVarInits (inits) 
        {
            let varHead = this.varHeads[this.varHeads.length-1];
            for (let n = 0, len = inits.length; n < len; ++n)  // until array conact works
                varHead.Ast::exprs.push (inits[n]);
        }

        function enterLetBlock () 
        {
            use namespace Ast;
            enter ("enterLetBlock");
            let letHead = new Ast::Head ([],[]);
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
            use namespace Ast;
            let letHead = this.letHeads[this.letHeads.length-1];
            for (let n = 0, len = fxtrs.length; n < len; ++n)  // until array conact works
                letHead.fixtures.push (fxtrs[n]);
        }

        function addLetInits (inits) 
        {
            let letHead = this.letHeads[this.letHeads.length-1];
            for (let n = 0, len = inits.length; n < len; ++n)  // until array conact works
                letHead.exprs.push (inits[n]);
        }

        function openNamespace (nd: Ast::IDENT_EXPR) {
            enter ("openNamespace");
            let ns = evalIdentExprToNamespace (nd);
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
            let ns = evalIdentExprToNamespace (nd);
            this.pragmas.defaultNamespace = ns;
            exit ("defaultNamespace");
        }

        function hasName (fxtrs,id,ns) 
        {
            use namespace Ast;

            enter ("hasName ",id);
            if (fxtrs.length===0) 
            {
                exit ("hasName false");
                return false;
            }

            let pn = fxtrs[0][0];
            // print ("pn..id=",pn.Ast::name.id," id=",id);
            // print ("pn..ns=",pn.Ast::name.ns.Ast::hash()," ns=",ns.Ast::hash());
            if (pn.name.id==id && pn.name.ns.hash()==ns.hash())  // FIXME: need ns compare
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
            use namespace Ast;

            enter ("getFixture ");
            if (fxtrs.length===0) 
            {
                throw "name not found " + ns + "::" + id;
            }

            let pn = fxtrs[0][0];
            if (pn.name.id==id && pn.name.ns.toString()==ns.toString()) 
            {
                exit ("getFixture");
                return fxtrs[0];
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

        function findFixtureWithNames (id,nss, it: Ast::INIT_TARGET?) {
            enter ("findFixtureWithNames");

            let env = this.env;

            switch (it) {
            case Ast::instanceInit:
                var start = env.length-2;
                var stop = start;
                break;
            case null:
                var start = env.length-1;
                var stop = 0;
                break;
            default:
                throw "error findFixtureWithName: unimplemented target";
            }

            //print ("env.length=",env.length);
            for (var i=start; i>=stop; --i)   // for each head
            {
                let ns = null;
                let fxtrs = env[i];
                //print ("nss.length=",nss.length);
                for (var j=nss.length-1; j>=0; --j) {
                    //print ("nss[",j,"]=",nss[j]);
                    if (hasName (fxtrs,id,nss[j])) {
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

        function findFixtureWithIdentifier (id: Ast::IDENT, it: Ast::INIT_TARGET?)
        {
            enter ("findFixtureWithIdentifier ", id);
            //print ("this.pragmas=",this.pragmas);
            let nsss = this.pragmas.openNamespaces;
            //print ("nsss.length=",nsss.length);
            for (var i=nsss.length-1; i>=0; --i) 
            {
                //print ("nsss[",i,"]=",nsss[i]);
                let fx = findFixtureWithNames (id,nsss[i],it);
                if (fx !== null) 
                {
                    exit ("findFixtureWithIdentifier");
                    return fx;
                }
            }
            throw "fixture not found: " + id;
        }

        function evalIdentExprToNamespace (nd: Ast::IDENT_EXPR)
            : Ast::NAMESPACE
        {
            use namespace Ast;

            enter ("evalIdentExprToNamespace");

            var fxtr = null;
            var val = null;

            switch type (nd) {
            case (nd: Identifier) {
                var fxtr = findFixtureWithIdentifier (nd.ident,null);
                switch type (fxtr[1]) {
                case (fxtr:NamespaceFixture) {
                    var val = fxtr.ns;
                    return fxtr.ns;
                }
                case (fxtr:*) {
                    throw "fixture with unknown value " + fxtr;
                }
                }
            }
            case (nd: ReservedNamespace) {
                var val = nd.ns;
                return val;
            }
            case (nd: *) {
                throw "evalIdentExprToNamespace: case not implemented " + nd;
            }
            }
            exit ("evalIdentExprToNamespace ", val);
            return val;
        }

        function resolveIdentExpr (nd: Ast::IDENT_EXPR, it: Ast::INIT_TARGET)
            : Ast::FIXTURE_NAME
        {
            enter ("resolveIdentExpr");
            switch type (nd) {
            case (nd: Ast::Identifier) {
                var fxtr = findFixtureWithIdentifier (nd.Ast::ident, it);
            }
            case (nd: *) {
                throw "resolveIdentExpr: case not implemented " + nd;
            }
            }
            exit ("resolveIdentExpr ", fxtr);
            return fxtr[0];
        }
    }

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

    //    class Parser
    {
        var scan : Scanner;
        var cx: Context;

        function initParser(src,topFixtures)
        {
            this.cx = new Context (topFixtures)
            this.scan = new Scanner (src,"")
        }

        var defaultNamespace: Ast::NAMESPACE;
        var currentPackageName: string;
        var currentClassName: string;

        var coordList;

        function hd (ts:TokenStream) 
        {
            //enter ("hd ",ts.head());
            var tk = Token::tokenKind (ts.head());
            //print ("hd ",tk);
            return tk;
        }

        function eat (ts:TokenStream,tc) {
            //print("eating ",Token::tokenText(tc));
            let tk = hd (ts);
            if (tk == tc) {
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

        Notation

        []             list
        (fl,el)        head
        fl             fixture list
        el             expr list
-        il             init list
        sl             stmt list
        it             init target = VAR, LET (default=LET)
        ie             init expr
        se             set expr

        initexpr       init it (fl,el) il
        letexpr        let (fl,el) el
        block          (fl,el) sl

      

        Bindings

        var x = y      [x], init VAR () [x=y]
        var [x] = y    [x], init VAR ([t0],[init t0=y]) [x=t0[0]]

        let (x=y) ...  let ([x], init x=y) ...
        let x=y             [x], init x=y]

        Assignments

        x = y          [],  set x=y
        [x] = y        [],  let ([t0],[init t0=y]) [set x=t0[0]]

        Blocks

        { }            () {}
        {stmt}         () {stmt}
        {let x}        ([x],[x=undef]) {}       is undef the right val?
        let (x) {}     ([x],[x=undef]) {}       what about reping uninit?

        Mixture

        { var x = y; let z = y }  =>
            ([x],[]) { blkstmt ([z],[]) { init VAR () x=y; init LET () z=y } }


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
            desugarPattern (p,t,e,null,null,null,op,false);

        function desugarBindingPattern (p: PATTERN, t: Ast::TYPE_EXPR, e: Ast::EXPR,
                                        ns: Ast::NAMESPACE?, it: Ast::INIT_TARGET?, ro: boolean?)
            : [Ast::FIXTURES, Ast::EXPR]
            desugarPattern (p,t,e,ns,it,ro,null);

        function desugarPattern (p: PATTERN, t: Ast::TYPE_EXPR, e: Ast::EXPR,
                                 ns: Ast::NAMESPACE?, it: Ast::INIT_TARGET?, ro: boolean?, op: Ast::ASSIGNOP?)
            : [Ast::FIXTURES, Ast::EXPR]
        {
            return desugarSubPattern (p,t,e,0);

            function identExprFromExpr (e: Ast::EXPR) 
                : Ast::IDENT_EXPR {
                switch type (e) {
                case (e: Ast::LexicalRef) {
                    var ie = e.Ast::ident;
                }
                case (e: *) {
                    throw "invalid init lhs " + e;
                }
                }
                return ie;
            }

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
                    if (it != null) { // we have an init target so must be an init
                        var ie = identExprFromExpr (p.expr);
                        var nm = cx.resolveIdentExpr (ie,it);
                        var expr = new Ast::InitExpr (it, new Ast::Head ([],[]), [[nm,e]]);
                    }
                    else {
                        var expr = new Ast::SetExpr (op,p.expr,e);
                    }
                }
                //case (p: (ArrayPattern, ObjectPattern)) {
                case (p: *) {
                    let tn = new Ast::TempName (n);
                    var fxtrs = [];
                    let exprs = [];
                    let ptrns = p.ptrns;
                    for (let i=0; i<ptrns.length; ++i) {
                        let sub = ptrns[i];
                        /// switch type (sub) {
                        /// case (sub: FieldPattern) {
                        if (sub is FieldPattern) {
                            var typ = new Ast::FieldTypeRef (t,sub.ident);
                            var exp = new Ast::ObjectRef (new Ast::GetTemp (n), sub.ident);
                            var ptn = sub.ptrn;
                        }
                        /// case (pat: *) {
                        else {
                            var typ = new Ast::ElementTypeRef (t,i);
                            var exp = new Ast::ObjectRef (new Ast::GetTemp (n), new Ast::Identifier (i,[[Ast::noNS]]));
                                      // FIXME what is the ns of a temp and how do we refer it
                            var ptn = sub;
                        }
                        /// }

                        let [fx,ex] = desugarSubPattern (ptn,typ,exp,n+1);
                        for (let j=0; j<fx.length; ++j) fxtrs.push(fx[j]);
                        exprs.push(ex);
                    }
                    let head = new Ast::Head ([[tn,new Ast::ValFixture (Ast::anyType,false)]],[new Ast::InitExpr (Ast::letInit,new Ast::Head([],[]),[[tn,e]])]);
                    var expr = new Ast::LetExpr (head, new Ast::ListExpr (exprs));
                }
                }
                return [fxtrs,expr];
            }
        }
    }

    function test () {
        print ("testing parse-util.es");
        var x = initParser ("hi",[]);
    }

    //Parse::test ();
}


