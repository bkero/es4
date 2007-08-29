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
            let [fxtrs,exprs] = nd2;

            switch (nd1) {
            case Ast::letConstTag:
            case Ast::letVarTag:
                cx.addLetFixtures (fxtrs);
                var stmts = [new Ast::ExprStmt (new Ast::ListExpr(exprs))];
                break;
            default:
                switch (tau) {
                case classBlk:
                    cx.addVarFixtures (fxtrs);
                    cx.addVarInits (exprs);  // FIXME these aren't inits, they are a kind of settings
                    var stmts = [];
                    break;
                default:
                    cx.addVarFixtures (fxtrs);
                    var stmts = [new Ast::ExprStmt (new Ast::ListExpr(exprs))];
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
            : [TOKENS, [Ast::FIXTURES, Ast::EXPRS]]
        {
            enter("Parser::variableBindingList ", ts);

            var [ts1,nd1] = variableBinding (ts, beta);
            var [ts2,nd2] = variableBindingListPrime (ts1, beta);

            var [f1,i1] = nd1;  // FIXME: fold into patterns above when it works in the RI
            var [f2,i2] = nd2;

            for (let i=0; i<f2.length; ++i) f1.push (f2[i]);  // FIXME: use concat when it works in the RI
            for (let i=0; i<i2.length; ++i) i1.push (i2[i]);

            exit ("Parser::variableBindingList ", ts2);
            return [ts2,[f1,i1]];

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

                    for (let i=0; i<f2.length; ++i) f1.push (f2[i]);  // FIXME: use concat when it works in the RI
                    for (let i=0; i<i2.length; ++i) i1.push (i2[i]);
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
                        var [tsx,[f,i]] = [ts2,desugarBindingPattern (p,t,nd2,ns,it,ro)];
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
                            var [tsx,[f,i]] = [ts1,desugarBindingPattern (p,t,null,ns,it,ro)];
                        }
                        case (x : *) {
                            throw "destructuring pattern without initializer";
                        }
                        }
                        break;
                    }
                }
                exit("Parser::variableBinding ", tsx);
                return [tsx,[f,[i]]];
            }
        }

        /*
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
        */

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

            var {params:params,defaults:defaults,resultType:resultType,thisType:thisType,hasRest:hasRest} = nd2;
            var func = new Ast::Func (nd1,false,nd3,params,vars,defaults,resultType);

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

            var {params:params,defaults:defaults,hasRest:hasRest,settings:settings,superArgs:superArgs} = nd2;

            // print ("superArgs=",superArgs);
            // print ("settings=",settings);
            var func = new Ast::Func ({kind:new Ast::Ordinary,ident:nd1},false,nd3,params,vars,defaults,Ast::voidType);
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
          , paramTypes : [Ast::TYPE_EXPR]
          , defaults : [Ast::EXPR]
          , hasRest: boolean
          , settings : [Ast::EXPR]
          , superArgs: [Ast::EXPR] }

        type FUNC_SIG = 
          { typeParams : [Ast::IDENT]
          , params : Ast::HEAD
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
                      , params: new Ast::Head (f,i)
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
                      , params: new Ast::Head (f,i)
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
                    for (let i=0; i<f2.length; ++i) f1.push(f2[i]);
                    for (let i=0; i<i2.length; ++i) i1.push(i2[i]);
                    for (let i=0; i<e2.length; ++i) e1.push(e2[i]);
                    for (let i=0; i<t2.length; ++i) t1.push(t2[i]);
                    var [ts1,nd1,hasRest] = [ts2,[[f1,i1],e1,t1],hasRest];
                    break;
                case Token::RightParen:
                    // nothing to do
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
            : [TOKENS,[[Ast::FIXTURES,Ast::EXPRS], Ast::EXPRS, Ast::TYPE_EXPRS]]
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
            return [ts2,[[f,[i]],nd2,[t]]];
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
                var nd1 = new Ast::Block (new Ast::Head ([],[]),[new Ast::ReturnStmt (nd1)],null);
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
                let blck = new Ast::Block (new Ast::Head([],[]),[]);
                let params = new Ast::Head([],[]);
                let vars = new Ast::Head([],[]);
                let defaults = [];
                let type = Ast::anyType;
                let func = new Ast::Func ({kind:new Ast::Ordinary,ident:nd1},isNative,blck,params,vars,defaults,type);
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

}
}
