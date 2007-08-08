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

//use module ast "tests/self/ast.es";
//use module ast_encoder "tests/self/ast_encoder.es";
//use module lexer "tests/self/lexer.es";

//module parser
{
use namespace intrinsic;
namespace Parser;
type TOKENS = Array;  // [int];

{
    use default namespace Parser;
    use namespace Release;

    type PATTERN =
          ( ObjectPattern
          , ArrayPattern
          , SimplePattern
          , IdentifierPattern );

    class ObjectPattern { }
    class ArrayPattern { }
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

    class Full {}
    class Abbrev {}
    type OMEGA = (Full, Abbrev);
    const full = new Full;
    const abbrev = new Abbrev;

    class Parser
    {
        /*
        static const AbbrevIfElse = 0;
        static const AbbrevDoWhile = AbbrevIfElse + 1;
        static const AbbrevFunction = AbbrevDoWhile + 1;
        static const Abbrev = AbbrevFunction + 1;
        static const Full = Abbrev + 1;
        */

        type BETA = int;  // NoIn, AllowIn
        type TAU = int;   // Global, Class, Interface, Local
        type GAMMA = int; // NoExpr, AllowExpr

        static const NoIn = 0;
        static const AllowIn = 1;

        static const NoExpr = 0;
        static const AllowExpr = 1;

        static const Global = 0;
        static const Class = 1;
        static const Interface = 2;
        static const Local = 3;

        var scan : Lexer::Scanner

        public function Parser(src)
        {
            this.scan = new Lexer::Scanner(src)
        }

        var defaultNamespace: Ast::NAMESPACE;
        var currentPackageName: string;
        var currentClassName: string;

        private var coordList;

        function hd (ts) 
        {
            var tk = Token::tokenKind (ts[0]);
            return tk;
        }

        function eat (ts,tc) {
            // print("eating",Token::tokenText(tc));
            let tk = hd (ts);
            if (tk === tc) {
                return tl (ts);
            }
            throw "expecting "+Token::tokenText(tc)+" found "+Token::tokenText(tk);
        }

        function match (ts,tc) : void {
            // print("matching",Token::tokenText(tc));
            let tk = hd (ts);
            if (tk !== tc) {
                throw "expecting "+Token::tokenText(tc)+" found "+Token::tokenText(tk);
            }
            return;
        }

        /*
          Replace the first token in the stream with another one. Raise an exception
          if the first token is not of a specified kind.
        */

        function swap (ts,t0,t1) {
            let tk = hd (ts);
            if (tk === t0) {
                return ts[0] = t1;
            }
            throw "expecting "+Token::tokenText(t0)+" found "+Token::tokenText(tk);
        }

        function tl (ts:TOKENS) : TOKENS ts.slice (1,ts.length);

        /*

        desugaring can result in bindings and inits for temporaries,
        bindings for the declared names, and inits for the declared names.

        the temp bindings and inits from destructuring end up in the head of an 
        InitExpr, the property init, if any, ends up in the InitExpr inits, and 
        the property bindings get hoisted into the appropriate block head

        */

        function desugarPattern (p: PATTERN, t: Ast::TYPE_EXPR, e: Ast::EXPR)
            : [[Ast::BINDING], [Ast::INIT_STEP], Ast::HEAD]
        {
            enter ("desugarPattern");
            var bs, ss;
            switch type (p) : PATTERN {
            case (p:IdentifierPattern) {
                let i = new Ast::PropIdent (p.ident);
                var binds = [new Ast::Binding (i,t)];
                if (e !== null) {
                    var inits = [new Ast::InitStep (i,e)];
                    var head = {fixtures:[],inits:[]};
                }
                else {
                    var inits = [];
                    var head = {fixtures:[],inits:[]};
                }
            }
            case (p:SimplePattern) {
                if (e !== null) {
                    var binds = [];
                    var inits = [new Ast::AssignStep (p.expr,e)];
                    var head = {fixtures:[],inits:[]};
                }
                else {
                    throw "simple pattern without initializer";
                }
            }
            case (x: *) {
                throw "internal error: desugarPattern " + p;
            }
            }
            exit ("desugarPattern");

            return [binds,inits,head];  // FIXME: RI allows [b,[]], which should be an error
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
                var str = Token::tokenText (ts[0]);
                break;
            default:
                throw "expecting identifier, found " + Token::tokenText (ts[0]);
            }
            exit ("Parser::identifier ", str);
            return [tl (ts), str];
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
                let id = Token::tokenText (ts[0]);
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

            exit("Parser::reservedNamespace ", nd1);
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
                    var [ts2,nd2] = [tl(ts1), new Ast::Identifier ("*")];
                    var [ts3,nd3] = [ts1, new Ast::QualifiedIdentifier (nd1,nd2)];
                    break;
                case Token::StringLiteral:
                case Token::DecimalLiteral:
                    let str = Token::tokenText (ts1[0]);
                    var [ts2,nd2] = [tl(ts1), new Ast::Identifier (str)];
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
                    nd1 = new Ast::LexicalRef (new Ast::Identifier (nd1))
                }
                }
                var [ts2,nd2] = qualifiedNameIdentifier (tl(ts1), nd1);
                break;
            default:
                var [ts2,nd2] = [ts1,new Ast::Identifier (nd1)];
                break;
            }

            exit ("Parser::simpleQualifiedName ", ts2);
            return [ts2,nd2];
        }

        /*
            ExpressionQualifiedName
                ParenListExpression :: QualifiedName
        */

//        function expressionQualifiedIdentifier()
//        {
//            enter("parseExpressionQualifiedIdentifier")
//
//            var first = parseParenListExpression()
//            match(doublecolon_token)
//            if( lookahead(leftbracket_token) )
//            {
//                var second = parseBrackets()
//                var result = <QualifiedExpression><Qualifier>{first}</Qualifier><Expr>{second}</Expr></QualifiedExpression>
//            }
//            else
//            {
//                var second = parsePropertyIdentifier()
//                var result = <QualifiedIdentifier><Qualifier>{first}</Qualifier>{second}</QualifiedIdentifier>
//            }
//
//            exit("parseExpressionQualifiedIdentifier",result)
//            return result
//        }
//
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

//        /*
//            AttributeQualifiedIdentifier
//                @ Brackets
//                @ NonAttributeQualifiedIdentifier
//        */
//
//        function parseAttributeIdentifier()
//        {
//            enter("parseAttributeIdentifier")
//
//            match(at_token)
//            if( lookahead(leftbracket_token) )
//            {
//                var result = parseBrackets()
//            }
//            else
//            {
//                var result = parseNonAttributeQualifiedIdentifier()
//            }
//            result.@is_attr="true"
//
//            exit("parseAttributeIdentifier",result)
//            return result
//        }
//
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
                    var [ts1,nd1] = path (tl (tl (ts)), [Token::tokenText(ts[0])]);
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
                    nd.push(Token::tokenText(ts[0]));
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

        function parenExpression (ts: TOKENS)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::parenExpression ", ts);

            var ts1 = eat (ts,Token::LeftParen);
            var [ts2,ndx] = assignmentExpression (ts1, AllowIn);
            var tsx = eat (ts2,Token::RightParen);

            exit ("Parser::parenExpression ", tsx);
            return [tsx, ndx];
        }

        function parenListExpression (ts: TOKENS)
            : [TOKENS, [Ast::EXPR]]
        {
            enter("Parser::parenListExpression ", ts);

            var ts1 = eat (ts,Token::LeftParen);
            var [ts2,ndx] = listExpression (ts1, AllowIn);
            var tsx = eat (ts2,Token::RightParen);

            exit ("Parser::parenListExpression ", tsx);
            return [tsx, ndx];
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
                var [ts1,nd1] = [tl (ts), new Ast::LiteralExpr (new Ast::LiteralDecimal (Token::tokenText (ts[0])))];
                break;
            case Token::StringLiteral:
                var [ts1,nd1] = [tl (ts), new Ast::LiteralExpr (new Ast::LiteralString (Token::tokenText (ts[0])))];
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
//            else
//            if( lookahead(leftbracket_token) )
//            {
//                var result = parseArrayLiteral()
//            }
//            else
//            if( lookahead(leftbrace_token) )
//            {
//                var result = parseObjectLiteral()
//            }
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
                var base = new Ast::LexicalRef (new Ast::Identifier (path[0]));
                return resolveObjectPath (path.slice (1,path.length), base);
            }
            else 
            {
                var base = new Ast::ObjectRef (expr, new Ast::Identifier (path[0]));
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
                let [ts1,nd1] = brackets (ts);
                var [tsx,ndx] = [ts1, new Ast::ObjectRef (nd,new Ast::ExpressionIdentifier (nd1))];
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
                let [ts2,nd2] = listExpression (ts1, AllowIn);
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
                    [ts1,this.coordList] = scan.tokenList (scan.div);
                }

                switch (hd (ts1)) {
                case Token::Mult:
                    var op = Ast::multOp;
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
                    var op = Ast::lessOp;
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    break;
                case Token::GreaterThan:
                    var op = Ast::greaterOp;
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    break;
                case Token::LessThanOrEqual:
                    var op = Ast::lessOrEqualOp;
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    break;
                case Token::GreaterThanOrEqual:
                    var op = Ast::greaterOrEqualOp;
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    break;
                case Token::In:
                    if (beta == NoIn) {
                        break done;
                    }
                    var op = Ast::inOp;
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    break;
                case Token::InstanceOf:
                    var op = Ast::instanceOfOp;
                    var [ts2, nd2] = shiftExpression (tl (ts1), beta);
                    break;
                case Token::Is:
                    var op = Ast::isOp;
                    var [ts2, nd2] = typeExpression (tl (ts1), beta);
                    break;
                case Token::To:
                    var op = Ast::toOp;
                    var [ts2, nd2] = typeExpression (tl (ts1), beta);
                    break;
                case Token::Cast:
                    var op = Ast::castOp;
                    var [ts2, nd2] = typeExpression (tl (ts1), beta);
                    break;
                default:
                    break done;
                }
                var [ts1, nd1] = [ts2, new Ast::BinaryExpr (op, nd1, nd2)];
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

        function exprFromAssignStep (as : Ast::AssignStep) {
            return new Ast::SetExpr (new Ast::Assign,as.Ast::le,as.Ast::re);
        }

        function assignmentExpression (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::assignmentExpression ", ts);

            function patternFromExpr (e: Ast::EXPR) {
                return new SimplePattern (e);  // FIXME: handle destructuring patterns
            }

            var [ts1,nd1] = conditionalExpression (ts, beta);
            switch (hd (ts1)) {
            case Token::Assign:
                var [ts1,nd1] = [tl (ts1), patternFromExpr (nd1)];
                var [ts2,nd2] = assignmentExpression (ts1,beta);
                var [binds,inits,head] = desugarPattern (nd1,Ast::anyType,nd2,0);
                //var expr = new Ast::LetExpr (head, new Ast::ListExpr ([exprFromAssignStep (inits[0])]));  
                var expr = exprFromAssignStep (inits[0]);
                            // FIXME: map exprFromAssignStep over all elements
                            // assert binds is empty
                break;
            default:
                var [ts2,expr] = [ts1,nd1];
                break;
            }

            exit ("Parser::assignmentExpression ", ts1);
            return [ts2,expr];
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

        function listExpression (ts: TOKENS, beta )
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
                var [ts1,nd1] = objectPattern (tl (ts), gamma);
                break;
            case Token::LeftBracket:
                var [ts1,nd1] = arrayPattern (tl (ts), gamma);
                break;
            default:
                var [ts1,nd1] = simplePattern (ts, beta, gamma);
                break;
            }

            exit("Parser::pattern", ts1);
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
            case NoExpr:
                let [ts1,nd1] = identifier (ts);
                var [tsx,ndx] = [ts1, new IdentifierPattern (nd1)];
                break;
            case AllowExpr:
                let [ts1,nd1] = leftHandSideExpression (ts,beta);
                var [tsx,ndx] = [ts1, new SimplePattern (nd1)];
                break;
            }

            exit("Parser::simplePattern", tsx);
            return [tsx,ndx];
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

            var [ts1,nd1] = pattern (ts,beta,NoExpr);
            switch (hd (ts1)) {
            case Token::Colon:
                var [ts2,nd2] = typeExpression (tl (ts1));
                break;
            default:
                var [ts2,nd2] = [ts1,new Ast::SpecialType (new Ast::AnyType)];
                break;
            }

            exit("Parser::typedPattern ", ts2);
            return [ts2,[nd1,nd2]];
        }

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

//        /*
//
//        UnionType
//            (  TypeExpressionList  )
//
//        */
//
//        function parseUnionType()
//        {
//            enter("parseUnionType")
//
//            match(leftparen_token)
//            var first = parseTypeExpressionList()
//            var result = <UnionType>{first}</UnionType>
//            match(rightparen_token)
//
//            exit("parseUnionType",result)
//            return result
//        }
//
//        /*
//
//        RecordType
//            {  FieldTypeList  }
//
//        */
//
//        function parseRecordType()
//

        /*

        DestructuringElement(gamma)
            , DestructuringElementList(gamma)
            DestructuringElementg  ,  DestructuringElementListg

        DestructuringElementg
            Patterng

        TypedIdentifier
            SimplePattern(noIn)
            SimplePattern(beta)  :  NullableTypeExpression

        TypedPattern(beta)
            SimplePattern(beta)
            SimplePattern(beta)  :  NullableTypeExpression
            ObjectPattern
            ObjectPattern  :  TypeExpression
            ArrayPatternn
            ArrayPatternn  :  TypeExpression

        */

//            enter("parseRecordType")
//
//            match(leftbrace_token)
//            if( lookahead(rightbrace_token) )
//            {
//                var first = <></>
//            }
//            else
//            {
//                var first = parseFieldTypeListPrime(<>{parseFieldType()}</>)
//            }
//            var result = <RecordType>{first}</RecordType>
//            match(rightbrace_token)
//
//            exit("parseRecordType",result)
//            return result
//        }
//
//        /*
//
//        NonemptyFieldTypeList
//            FieldType
//            FieldType  ,  NonemptyFieldTypeList
//
//        */
//
//        function parseFieldTypeListPrime(first)
//        {
//            enter("parseNonemptyFieldTypeList",first)
//
//            if( lookahead(comma_token) )
//            {
//                match(comma_token)
//                var second = parseFieldType()
//                var result = parseFieldTypeListPrime(<>{first}{second}</>)
//            }
//            else
//            {
//                var result = first
//            }
//
//            exit("parseFieldListPrime",result)
//            return result
//        }
//
//        /*
//            FieldType
//                FieldName  :  TypeExpression
//        */
//
//        function parseFieldType()
//        {
//            enter("parseFieldType")
//
//            var first = parseFieldName()
//            match(colon_token)
//            var second = parseTypeExpression()
//            var result = <FieldType>{first}{second}</FieldType>
//
//            exit("parseFieldType",result)
//            return result
//        }
//
//        /*
//
//        ArrayType
//            [  ElementTypeList  ]
//
//        ElementTypeList
//            empty
//            TypeExpression
//            ,  ElementTypeList
//            TypeExpression  ,  ElementTypeList
//
//        */
//
//        function parseArrayType()
//        {
//            enter("parseArrayType")
//
//            enterSlashContext(regexpliteral_token)
//            match(leftbracket_token)
//            if( lookahead(rightbracket_token) )
//            {
//                var first = <></>
//            }
//            else
//            {
//                var temp = parseElementType()
//                var first = parseElementTypeListPrime(<>{temp}</>)
//            }
//
//            exitSlashContext(regexpliteral_token)
//            match(rightbracket_token)
//            var result = <LiteralType>{first}</LiteralType>
//
//            exit("parseArrayLiteral",result)
//            return result
//        }
//
//        function parseElementTypeListPrime(first)
//        {
//            enter("parseElementTypeListPrime",first)
//
//            while( lookahead(comma_token) )
//            {
//                match(comma_token)
//                var second = parseElementType()
//                if( second == null )
//                {
//                    // do nothing
//                }
//                else
//                {
//                    var first = <>{first}{second}</>
//                }
//            }
//            var result = first
//
//            exit("parseElementTypeListPrime",result)
//            return result
//        }
//
//        function parseElementType()
//        {
//            enter("parseElementType")
//
//            if( lookahead(comma_token) )
//            {
//                var result = <EmptyElementType/>
//            }
//            else
//            if( lookahead(rightbracket_token) )
//            {
//                var result = null
//            }
//            else
//            {
//                var result = parseTypeExpression()
//            }
//
//            exit("parseElementType",result)
//            return result
//        }
//
//
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
            case Token::Return:
                var [ts1,nd1] = returnStatement (ts,omega);
                var [ts2,nd2] = [semicolon (ts1,omega),nd1];
                break;
            case Token::LeftBrace:
                var [ts1,nd1] = block (ts, tau);
                var [ts2,nd2] = [ts1,new Ast::BlockStmt (nd1)];
                break;
            default:
                let [ts1,nd1] = expressionStatement (ts);
                var [ts2,nd2] = [semicolon (ts1,omega), nd1];
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
                var [ts1,nd1] = statement (ts,Local,omega);
                break;
            }

            exit("Parser::substatement ", ts1);
            return [ts1,nd1];
        }

        function newline (ts: TOKENS)
            : boolean
        {
            let offset = ts.length;

            if (offset == coordList.length)
                return true;  // first token, so follows newline, but whose asking?

            let coord = coordList[coordList.length-offset];
            let prevCoord = coordList[coordList.length-offset-1];
            print("coord=",coord);
            print("prevCoord=",prevCoord);

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
            case full:
                switch (hd (ts)) {
                case Token::SemiColon:
                    var ts1 = tl (ts);
                    break;
                case Token::EOS:
                case Token::RightBrace:
                    var ts1 = ts;
                    break;
                default:
                    if (newline (ts)) { var ts1=ts; trace("inserting semicolon") }
                    else { throw "** error: expecting semicolon" }
                    break;
                }
                break;
            case abbrev:  // Abbrev, ShortIf
                print("abbrev");
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

            var [ts1,nd1] = listExpression (ts,AllowIn);

            exit("Parser::expressionStatement ", ts1);
            return [ts1, new Ast::ExprStmt (nd1)];
        }

        function returnStatement (ts: TOKENS)
            : [TOKENS, Ast::STMT]
        {
            enter("Parser::returnStatement ", ts);

            ts = eat (ts, Token::Return);

            var [ts1,nd1] = listExpression (ts,AllowIn);

            exit("Parser::returnStatement ", ts1);
            return [ts1, new Ast::ReturnStmt (nd1)];
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
 
            exit("Parser::whileStatement ", ts2);
            return [ts2, new Ast::WhileStmt (nd1,nd2)];
        }


        /*

        VariableDefinition(beta)
            VariableDefinitionKind  VariableBindingList(beta)


        returns a statement, a list of block fixtures and var fixtures. if the caller
        is a class then it checks the static attribute to know if the var fixtures are
        class fixtures or instance fixtures

        */

        function variableDefinition (ts: TOKENS, beta: BETA, ns, isPrototype, isStatic)
            : [TOKENS, [Ast::STMT], Ast::FIXTURES]
        {
            enter("Parser::variableDefinition ", ts);

            let [ts1,nd1] = variableDefinitionKind (ts);
            let [ts2,nd2] = variableBindingList (ts1, beta);

            let [b,i,h] = nd2;

            // i => initexprs
            // b => fixtures

            var fx = fixturesFromBindings (ns, b);
            var st = new Ast::ExprStmt (new Ast::InitExpr (new Ast::HoistedInit, h, initsFromInitSteps (ns, i)));

            exit("Parser::variableDefinition ", ts2);

            return [ts2,[st],fx];
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

        function variableBindingList (ts: TOKENS, beta: BETA )
            : [TOKENS, [[Ast::BINDING], [Ast::INIT_STEP], Ast::HEAD]]
        {
            enter("Parser::variableBindingList ", ts);

            function variableBindingListPrime (ts: TOKENS )
                : [TOKENS,[[Ast::BINDING], [Ast::INIT_STEP], Ast::HEAD]]
            {
                enter("Parser::variableBindingListPrime ", ts);
        
                switch (hd (ts)) {
                case Token::Comma:
                    var [ts1,nd1] = variableBinding (tl (ts), beta);
                    var [ts2,nd2] = variableBindingListPrime (ts1);

                    var [b1,i1,h1] = nd1;  // FIXME: fold into patterns above when it works in the RI
                    var [b2,i2,h2] = nd2;

                    for (var n in b2) b1.push (b2[n]);  // FIXME: use concat when it works in the RI
                    for (var n in i2) i1.push (i2[n]);
                    for (var n in h2.fixtures) h1.fixtures.push (h2.fixtures[n]);
                    for (var n in h2.inits) h1.inits.push (h2.inits[n]);

                    break;
                default:
                    var [ts2,nd2] = [ts,[[],[],{fixtures:[],inits:[]}]];
                    var [b1,i1,h1] = nd2;
                    break;
                }

                exit ("Parser::variableBindingListPrime ", ts2);
                return [ts2,[b1,i1,h1]];
            }

            var [ts1,nd1] = variableBinding (ts, beta);
            var [ts2,nd2] = variableBindingListPrime (ts1, beta);

            var [b1,i1,h1] = nd1;  // FIXME: fold into patterns above when it works in the RI
            var [b2,i2,h2] = nd2;

            for (var n in b2) b1.push (b2[n]);  // FIXME: use concat when it works in the RI
            for (var n in i2) i1.push (i2[n]);
            for (var n in h2.fixtures) h1.fixtures.push (h2.fixtures[n]);
            for (var n in h2.inits) h1.inits.push (h2.inits[n]);

            exit ("Parser::variableBindingList ", ts2);
            return [ts2,[b1,i1,h1]];
        }

        function variableBinding (ts: TOKENS, beta: BETA)
            : [TOKENS, [[Ast::BINDING], [Ast::INIT_STEP], Ast::HEAD]]
        {
            enter("Parser::variableBinding ", ts);

            let [ts1,nd1] = typedPattern (ts,beta);
            let [p,t] = nd1;
            switch (hd (ts1)) {
            case Token::Assign:
                let [ts2,nd2] = assignmentExpression (tl (ts1), beta);
                switch (hd (ts2)) {
                case Token::In:
                    if (beta === NoIn) {
                        // in a binding form
                        break;
                    } // else fall through
                default:
                    var [tsx,ndx] = [ts2,desugarPattern (p, t, nd2, 0)];
                    break;
                }
                break;
            default:
                switch (hd (ts1)) {
                case Token::In:
                    if (beta === NoIn) {
                        // in a binding form
                        break;
                    } // else fall through
                default:
                    switch type (p) {
                    case (p: IdentifierPattern) {
                        var [tsx,ndx] = [ts1,desugarPattern (p, t, null, 0)];
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

        function functionDefinition (ts: TOKENS, tau: TAU, kind, ns, isFinal, isOverride, isPrototype, isStatic, isAbstract)
            : [TOKENS, Ast::STMTS, Ast::FIXTURES]
        {
            enter("Parser::functionDefinition ", ts);

            ts = eat (ts, Token::Function);

            var [ts1,nd1] = functionName (ts);
            var [ts2,nd2] = functionSignature (ts1);
            var [ts3,nd3] = functionBody (ts2, AllowIn);

            var {params:params,defaults:defaults,resultType:resultType,thisType:thisType,hasRest:hasRest} = nd2;
            var func = new Ast::Func (nd1,false,nd3,params,defaults,resultType);
            var fxtr = new Ast::MethodFixture (func,new Ast::SpecialType (new Ast::AnyType),true,isOverride,isFinal);
            var name = new Ast::PropName ({ns:ns,id:nd1.ident});
            var fx3 = [[name,fxtr]];

            exit("Parser::functionDefinition ", ts3);

            return [ts3, [], fx3];
        }

        /*

        ConstructorDefinition
            function  ClassName  ConstructorSignature  FunctionBody(allowIn)

        */

        function constructorDefinition (ts: TOKENS, ns)
            : [TOKENS, Ast::FIXTURE_BINDING]
        {
            enter("Parser::constructorDefinition ", ts);

            ts = eat (ts, Token::Function);

            var [ts1,nd1] = identifier (ts);
            var [ts2,nd2] = constructorSignature (ts1);
            var [ts3,nd3] = functionBody (ts2, AllowIn);

            var {params:params,defaults:defaults,hasRest:hasRest,settings:settings,superArgs:superArgs} = nd2;

            var func = new Ast::Func ({kind:new Ast::Ordinary,ident:nd1},false,nd3,params,defaults,Ast::voidType);
            var ctor = new Ast::Ctor (settings,superArgs,func);
            var fxtr = new Ast::CtorFixture (ctor,new Ast::SpecialType (new Ast::AnyType));
            var name = new Ast::PropName ({ns:ns,id:nd1});
            var fx3 = [[name,fxtr]];

            exit("Parser::constructorDefinition ", ts3);

            return [ts3, [], fx3];
        }

        /*

        ConstructorSignature
            TypeParameters  (  Parameters  )  ConstructorInitialiser
        
        */

        type CTOR_SIG = 
          { typeParams : [Ast::IDENT]
          , params : Ast::HEAD  //BINDING_INITS
          , paramTypes : [Ast::TYPE_EXPR]
          , defaults : [Ast::EXPR]
          , hasRest: boolean
          , settings : [Ast::EXPR]
          , superArgs: [Ast::EXPR] }

        type FUNC_SIG = 
          { typeParams : [Ast::IDENT]
          , params : Ast::HEAD  //BINDING_INITS
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
            let [[b,i],e,t] = nd2;
            let p = headFromBindingInits ([b,i]);

            var ndx = { typeParams: []
                      , params: p
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
                    var [ts2,nd2] = arguments (ts1);
                    break;
                default:
                    var [ts1,nd1] = settingList (tl (ts));
                    switch (hd (ts1)) {
                    case Token::Super:
                        var [ts2,nd2] = arguments (tl (ts1));
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

        function namespaceFromExpr (qual : Ast::EXPR) 
            : Ast::NAMESPACE {
            return new Ast::PublicNamespace ("");
        }

        function initFromAssignStep (as : Ast::AssignStep) {
            enter ("initFromAssignStep");

            switch type ((as.Ast::le).Ast::ident) {
            case (ident: Ast::Identifier) {
                var ns = new Ast::PublicNamespace ("");
                var name = new Ast::PropName ({ns:ns,id:ident.Ast::ident});
                var init = new Ast::InitExpr (new Ast::InstanceInit,{fixtures:[],inits:[]},[[name,as.Ast::re]]);
            }
            case (le: Ast::QualifiedIdentifier) {
                var qual = le.Ast::qual;
                var ident = le.Ast::ident;
                var ns = namespaceFromExpr (qual);  // user namespace require lookup
                var name = new Ast::PropName ({ns:ns,id:ident});
                var init = new Ast::InitExpr (new Ast::InstanceInit,[[],[]],[[name,as.Ast::re]]);
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

            var [ts1,nd1] = pattern (ts,AllowIn,AllowExpr);
            ts1 = eat (ts1,Token::Assign);
            var [ts2,nd2] = assignmentExpression (ts1,AllowIn);

            var [tsx,[binds,inits,head]] = [ts2,desugarPattern (nd1, new Ast::SpecialType (new Ast::AnyType), nd2, 0)];
            // assert binds is empty

            var init = initFromAssignStep (inits[0]);  // FIXME might be more than one init for destructuring
            var ndx = new Ast::LetExpr (head,init);

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
            var fixtures = fixturesFromBindings (new Ast::PublicNamespace (""),bindings);
            var inits = initsFromInitSteps (new Ast::PublicNamespace (""), steps);
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
            let [[b,i],e,t] = nd2;
            let p = headFromBindingInits ([b,i]);

            var ndx = { typeParams: []
                      , params: p
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
            : [TOKENS, [Ast::BINDING_INITS, [Ast::EXPR], [Ast::TYPE_EXPR]], boolean]
        {
            enter("Parser::parameters ", ts);

            switch (hd (ts)) {
            case Token::RightParen:
                let b1 = [];
                let i1 = [];
                let e1 = [];
                let t1 = [];
                var [ts1,nd1,hasRest] = [ts,[[b1,i1],e1,t1],false];
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

        function nonemptyParameters (ts: TOKENS, n, initRequired)
            : [TOKENS, [Ast::BINDING_INITS, [Ast::EXPR], [Ast::TYPE_EXPR]], boolean]
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
                    let [[b1,i1],e1,t1] = nd1;
                    var [ts2,nd2,hasRest] = nonemptyParameters (ts1, n+1, e1.length!=0);
                    let [[b2,i2],e2,t2] = nd2;
                    // FIXME when Array.concat works
                    for (let p in b2) b1.push(b2[p]);
                    for (let p in i2) i1.push(i2[p]);
                    for (let p in e2) e1.push(e2[p]);
                    for (let p in t2) t1.push(t2[p]);
                    var [ts1,nd1,hasRest] = [ts2,[[b1,i1],e1,t1],hasRest];
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

        function parameterInit (ts: TOKENS, n, initRequired)
            : [TOKENS, [Ast::BINDING_INITS, Ast::EXPR, Ast::TYPE_EXPR]]
        {
            enter("Parser::parameterInit ", ts);

            var [ts1,nd1] = parameter (ts,n);

            switch (hd (ts1)) {
            case Token::Assign:
                ts1 = eat (ts1, Token::Assign);
                var [ts2,nd2] = nonAssignmentExpression(ts1,AllowIn);
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
            var [b,i,temps] = desugarPattern (p, t, new Ast::GetParam (n), 0);
            // FIXME: what do we do with 'temps'
            b.push (new Ast::Binding (new Ast::ParamIdent (n), t)); // temp for desugaring
            exit("Parser::parameterInit ", ts2);
            return [ts2,[[b,i],nd2,t]];
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
            var [ts2,nd2] = typedPattern (ts1,AllowIn);

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
                    var [ts1,nd1] = nullableTypeExpression (ts1);
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

        function functionBody (ts: TOKENS, beta: BETA)
            : [TOKENS, Ast::BLOCK]
        {
            enter("Parser::functionBody ", ts);

            switch (hd (ts)) {

            case Token::LeftBrace:
                var [ts1,blck,fxtrs] = block (ts,Local);
                var ndx = nd1;
                break;
            default:
                var [ts1,nd1] = assignmentExpression (ts,beta);
                var blck = new Ast::Block ([],{fixtures:[],inits:[]},[new ReturnStmt (nd1)],null);
                var fxtrs = [];
                break;
            }

            exit("Parser::functionBody ", ts1);
            return [ts1,blck,fxtrs];
        }

        function classDefinition (ts: TOKENS, ns: Ast::NAMESPACE, isDynamic)
            : [TOKENS, Ast::STMTS, Ast::FIXTURES]
        {
            enter("Parser::classDefinition ", ts);

            ts = eat (ts, Token::Class);

            var [ts1,nd1] = identifier (ts);
            var [ts2,nd2] = typeSignature (ts1);
            var [ts3,nd3] = classInheritance (ts2);

            currentClassName = nd1;
            var [ts4,blck,fxtrs] = classBody (ts3);
            currentClassName = "";

            function getCtorFixture (fs) 
            {
                enter ("getCtorFixture ",fs.length);

                if (fs.length === 0)
                {
                    throw "constructor not found";
                }

                for (var i = 0; i < fs.length; i++)
                if (fs[i][1] is Ast::CtorFixture) 
                {
                    exit ("getCtorFixture ",fs[i]);
                    let a = fs.slice (0,i);
                    let b = fs.slice (i+1,fs.length);
                    for (n in b) a.push(b[n]);
                    return [fs[i][1],a];
                }

                exit ("getCtorFixture ",fs[0]);
                return getCtorFixture (fs.slice(1,fs.length));
            }

            var [ctorFxtr,ifxtrs] = getCtorFixture (fxtrs);

            var name = {ns:ns,id:nd1};
            var baseName = {ns: new Ast::PublicNamespace (""), id: "Object"}
            var interfaceNames = [];
            var ctor = ctorFxtr.Ast::ctor;
            var cfxtrs = [];
            var iinits = {fixtures:[],inits:[]};
            var ctype = Ast::anyType;
            var itype = Ast::anyType;
            var cls = new Ast::Cls (name,baseName,interfaceNames,ctor,cfxtrs,ifxtrs,iinits,ctype,itype);

            var ss4 = [new Ast::ClassBlock (name,blck)];
            var fx4 = [[new Ast::PropName(name),new Ast::ClassFixture (cls)]];

            exit("Parser::classDefinition ", ts4);

            return [ts4, ss4, fx4];
        }

        /*

        TypeSignature
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
            : [TOKENS, Ast::BLOCK, Ast::FIXTURES]
        {
            enter("Parser::classBody ", ts);

            var [ts1,blck,fxtrs] = block (ts,Class);

            exit("Parser::classBody ", ts1);

            return [ts1,blck,fxtrs];
        }

        // DIRECTIVES

        /*
          Directives(tau)
              empty
              DirectivesPrefix(tau) Directives(tau,full)

        */

        function directives (ts: TOKENS, tau: TAU)
            : [TOKENS, Ast::PRAGMAS, Ast::STMTS, Ast::FIXTURES, Ast::FIXTURES]
        {
            enter("Parser::directives ", ts);

            switch (hd (ts)) {
            case Token::RightBrace:
            case Token::EOS:
                var [ts1, pragmas1, stmts1, bfxtrs1, vfxtrs1] = [ts,[],[],[],[]];
                break;
            default:
                var [ts1, pragmas1, stmts1, bfxtrs1, vfxtrs1] = directivesPrefix (ts,tau);
                break;
            }

            exit("Parser::directives ", ts1);
            return [ts1,pragmas1,stmts1,bfxtrs1,vfxtrs1];
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
        */

        function directivesPrefix (ts: TOKENS, tau: TAU)
            : [TOKENS, Ast::PRAGMAS, Ast::STMTS, Ast::FIXTURES, Ast::FIXTURES]
        {
            enter("Parser::directives ", ts);

            switch (hd (ts)) {
            case Token::RightBrace:
            case Token::EOS:
                var [ts1,nd1] = [ts,[]];
                var [ts2,stmts2,bfxtrs2,vfxtrs2] = [ts1,[],[],[]];
                break;
            default:
                var [ts1,nd1] = [ts,[]]; //pragmas (ts);
                var [ts2,stmts2,bfxtrs2,vfxtrs2] = directivesPrefixPrime (ts,tau);
                break;
            }

            exit("Parser::directivesPrefix ", ts2);
            return [ts2,nd1,stmts2,bfxtrs2,vfxtrs2];
        }

        function directivesPrefixPrime (ts: TOKENS, tau: TAU)
            : [TOKENS, Ast::STMTS, Ast::FIXTURES, Ast::FIXTURES]
        {
            enter("Parser::directivesPrefixPrime ", ts);

            var ts1,nd1;

            switch (hd (ts)) {
            case Token::RightBrace:
            case Token::EOS:
                ts1 = ts;
                var stmts1 = [];
                var bfxtrs1 = [];
                var vfxtrs1 = [];
                break;
            default:
                [ts1,stmts1,bfxtrs1,vfxtrs1] = directive (ts,tau,full);
                var [ts2,stmts2,bfxtrs2,vfxtrs2] = directivesPrefixPrime (ts1,tau);
                // FIXME: poor man's array append
                print(stmts1);
                print(bfxtrs1);
                print(vfxtrs1);
                for (p in stmts2) stmts1.push(stmts2[p]);
                for (p in bfxtrs2) bfxtrs1.push(bfxtrs2[p]);
                for (p in vfxtrs2) vfxtrs1.push(vfxtrs2[p]);
                ts1 = ts2;
                break;
            }

            exit("Parser::directivesPrefixPrime ", ts1);
            return [ts1,stmts1,bfxtrs1,vfxtrs1];
        }

        function isCurrentClassName (ts: TOKENS) 
            : boolean {
            let text = Token::tokenText (ts[0]);
            if (text === currentClassName) 
            {
                return true;
            }
            else 
            {
                return false;
            }
        }

        function directive (ts: TOKENS, tau: TAU, omega: OMEGA)
            : [TOKENS, Ast::STMTS, Ast::FIXTURES]
        {
            enter("Parser::directive ", ts);

            var bfxtrs1 = [];  // block
            var vfxtrs1 = [];  // var
            var cfxtrs1 = [];  // class

            switch (hd(ts)) {
            case Token::Let:
                // could be const or function
                throw "let not implemented yet";
                break;
            case Token::Var:
            case Token::Const:
                var [ts1,stmts1,vfxtrs1]
                    = variableDefinition (ts, AllowIn
                                  , new Ast::PublicNamespace ("")
                                  , false, false);

                var tsx = semicolon (ts1,omega);
                break;
            case Token::Function:
                if (isCurrentClassName (tl (ts))) 
                {
                    var [ts1,stmts1,vfxtrs1] = constructorDefinition (ts, new Ast::PublicNamespace (""));
                }
                else 
                {
                    var [ts1,stmts1,vfxtrs1] = functionDefinition (ts, tau, new Ast::Var
                                  , new Ast::PublicNamespace ("")
                                  , false, false, false, false, false);
                }

                var tsx = semicolon (ts1,omega);

                break;
            case Token::Class:
                var [ts1,stmts1,vfxtrs1] = classDefinition (ts, new Ast::PublicNamespace (""), false);
                var bfxtrs1 = [];
                var tsx = ts1;
                break;
            default:
                var [ts2,nd2] = statement (ts,tau,omega);
                var stmts1 = [nd2];
                var tsx = ts2;
                var bfxtrs1 = [];
                var vfxtrs1 = [];
                break;
            }

            exit("Parser::directive ", tsx);
            return [tsx, stmts1, bfxtrs1, vfxtrs1];
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
//        function parseAttributes(first)
//        {
//            enter("parseAttributes",first)
//
//            while( !lookaheadSemicolon(full_mode) &&
//                   ( lookahead(public_token) ||
//                   lookahead(private_token) ||
//                   lookahead(internal_token)  ||
//                   lookahead(intrinsic_token) ||
//                   lookahead(protected_token) ||
//                   lookahead(dynamic_token) ||
//                   lookahead(final_token) ||
//                   lookahead(native_token) ||
//                   lookahead(override_token) ||
//                   lookahead(prototype_token) ||
//                   lookahead(static_token) ||
//                   lookahead(leftbracket_token) ||
//                   lookahead(packageidentifier_token) ||
//                   lookahead(identifier_token) ) )
//            {
//                first += parseAttribute()
//            }
//
//            var node = <Attributes>{first}</Attributes>
//
//            // todo: check for duplicates
//
//            exit("parseAttributes",node)
//            return node
//        }
//
//        function parseAttribute()
//        {
//            enter("parseAttribute")
//
//            var found = lookahead(public_token) ? match(public_token) :
//                        lookahead(private_token) ? match(private_token) :
//                        lookahead(internal_token) ? match(internal_token) :
//                        lookahead(intrinsic_token) ? match(intrinsic_token) :
//                        lookahead(protected_token) ? match(protected_token) :
//                        lookahead(dynamic_token) ? match(dynamic_token) :
//                        lookahead(final_token) ? match(final_token) :
//                        lookahead(native_token) ? match(native_token) :
//                        lookahead(override_token) ? match(override_token) :
//                        lookahead(prototype_token) ? match(prototype_token) :
//                        lookahead(static_token) ? match(static_token) : empty_token
//
//
//            if( found != empty_token )
//            {
//                if( lookahead(doublecolon_token) )  // todo: look for other tokens that indicate an expression
//                {
//                    throw "attribute names cannot start a labeled or expression statement"
//                }
//
//                var slot_context = slot_context_stack[slot_context_stack.length-1]
//                switch(found)
//                {
//                    case internal_token:
//                        if( slot_context == "function" )
//                        {
//                            throw "'internal' shall not be used in local definitions"
//                        }
//                        var node = <Namespace kind="internal" name={current_package}/>
//                        break
//                    case intrinsic_token:
//                        throw "'intrinsic' shall only be used by implementations"
//                        var node = <Namespace kind={scan.tokenText(found)}/>
//                        break
//                    case private_token:
//                        if( slot_context != "class" )
//                        {
//                            throw "'private' must only be used on class variable and method definitions"
//                        }
//                        var node = <Namespace kind="private" name={current_class}/>
//                        break
//                    case protected_token:
//                        if( slot_context != "class" )
//                        {
//                            throw "'protected' must only be used on class variable and method definitions"
//                        }
//                        var node = <Namespace kind="protected" name={current_class}/>
//                        break
//                    case public_token:
//                        if( slot_context == "function" )
//                        {
//                            throw "'public' shall not be used in local definitions"
//                        }
//                        if( inClassBody() )
//                        {
//                            var public_namespace_name = ""
//                        }
//                        else
//                        {
//                            var public_namespace_name = current_package
//                        }
//                        var node = <Namespace kind="public" name={public_namespace_name}/>
//                        break
//                    case dynamic_token:
//                        var node = <Dynamic/>
//                        break
//                    case final_token:
//                        var node = <Final/>
//                        break
//                    case native_token:
//                        var node = <Native/>
//                        break
//                    case override_token:
//                        var node = <Override/>
//                        break
//                    case prototype_token:
//                        var node = <Prototype/>
//                        break
//                    case static_token:
//                        var node = <Static/>
//                        break
//                    default:
//                        throw "invalid attribute kind"
//                }
//
//            }
//            else
//            if( lookahead(leftbracket_token) )
//            {
//                var node = parseArrayLiteral()  // not quite right but close enough for now
//            }
//            else
//            {
//                var node = parseSimpleTypeIdentifier()
//            }
//
//            exit("parseAttribute",node.toXMLString())
//            return node
//        }
//

        // PRAGMAS

//        public function parsePragmas()
//        {
//            enter("parsePragmas")
//
//            var node = <></>
//
//            while( !lookahead(eos_token) &&
//                (lookahead(use_token) || lookahead(import_token)) )
//            {
//                node += parsePragma()
//            }
//
//            exit("parsePragmas",node)
//            return node
//        }
//
//        public function parsePragma()
//        {
//            enter("parsePragma")
//
//            var node = <></>
//
//            if( lookahead(use_token) )
//            {
//                node += parseUsePragma()
//                matchSemicolon(full_mode)
//            }
//            else
//            if( lookahead(import_token) )
//            {
//                node += parseImportPragma()
//                matchSemicolon(full_mode)
//            }
//
//            exit("parsePragma",node)
//            return node
//        }
//
//        public function parseImportPragma()
//        {
//            enter("parseImportPragma")
//
//            match(import_token)
//
//            if( lookahead(identifier_token) )
//            {
//                var first = parseIdentifier()
//                if( lookahead(assign_token) )
//                {
//                    match(assign_token)
//                }
//                else
//                {
//                    throw "import name '"+first.@name+"' is not a known package identifier"
//                }
//                var second = parseImportName()
//                if( second.@def == "*" )
//                {
//                    throw "wildcard not allowed in aliasing import pragmas"
//                }
//                second.@alias=first.@name
//                var node = second
//            }
//            else
//            if( lookahead(packageidentifier_token) )
//            {
//                var second = parseImportName()
//                var node = second
//            }
//            else
//            {
//                throw "invalid import name"
//            }
//
//            var pkg_name = node.@pkg
//            var def_name = node.@def
//
//            if( def_name == "*" )
//            {
//                scopes[scopes.length-1].Imports.*+=node
//            }
//            else
//            {
//                scopes[scopes.length-1].Imports[def_name].*+=node
//            }
//
//            exit("parseImportPragma",node.toXMLString())
//            return node
//        }
//
//        var scopes = [<Scope/>]
//
//        function isImported(pkg_name,def_name)
//        {
//            var scope   = scopes[scopes.length-1]
//            for each( var def in scope.Imports[def_name] )
//            {
//                if( def.Import.@pkg == pkg_name )
//                {
//                    return true
//                }
//            }
//
//            for each( var def in scope.Imports.* )
//            {
//                if( def.@pkg == pkg_name )
//                {
//                    return true
//                }
//            }
//            return false
//        }
//
//        function parseImportName()
//        {
//            enter("parseImportName")
//
//            var pkg_part = scan.tokenText(match(packageidentifier_token))
//            match(dot_token)
//            if( lookahead(mult_token) )
//            {
//                match(mult_token)
//                var def_part = "*"
//            }
//            else
//            if( lookaheadReservedWord() )
//            {
//                var def_part = scan.tokenText(matchReservedWord())
//            }
//            else
//            {
//                var def_part = scan.tokenText(match(identifier_token))
//            }
//
//            var node = <Import pkg={pkg_part} def={def_part}/>
//
//            exit("parseImportName",node.toXMLString())
//            return node
//        }
//
//        public function parseUsePragma()
//        {
//            enter("parseUsePragma")
//
//            match(use_token)
//            var node = <></>
//            node += parsePragmaItem()
//            while( lookahead(comma_token) )
//            {
//                match(comma_token)
//                node += parsePragmaItem()
//            }
//
//            exit("parseUsePragma",node)
//            return node
//        }
//
//        function parsePragmaItem()
//        {
//            enter("parsePragmaItem")
//
//            if( lookaheadReservedWord() ||
//                lookahead(identifier_token) )
//            {
//                if( lookaheadReservedWord() )
//                {
//                    var first = matchReservedWord()
//                }
//                else
//                {
//                    var first = match(identifier_token)
//                }
//                var ident = scan.tokenText(first)
//
//                switch(ident)
//                {
//                    case 'decimal':
//                    case 'double':
//                    case 'int':
//                    case 'rounding':
//                    case 'standard':
//                    case 'strict':
//                    case 'uint':
//                        var node = <{ident}/>
//                        break
//                    case 'default':
//                        match(namespace_token)
//                        var node = <DefaultNamespace/>
//                        node.* = <Get kind="lexical">{parseSimpleTypeIdentifier()}</Get>
//                        break
//                    case 'namespace':
//                        var node = <UseNamespace/>
//                        node.* = <Get kind="lexical">{parseSimpleTypeIdentifier()}</Get>
//                        break
//                    default:
//                        throw "invalid pragma identifier:"+ident
//                        break
//                }
//            }
//
//            exit("parsePragmaItem",node)
//            return node
//        }
//

        // BLOCKS and PROGRAMS

        function block (ts:TOKENS, tau: TAU)
            : [TOKENS, Ast::BLOCK, Ast::FIXTURES]
        {
            enter("Parser::block ",ts);

            ts = eat (ts, Token::LeftBrace);
            var [ts1,pragmas,stmts,bfxtrs,vfxtrs] = directives (ts, tau);
            ts1 = eat (ts1, Token::RightBrace);

            exit ("Parser::block ", ts1);
            return [ts1, new Ast::Block (pragmas,{fixtures:bfxtrs,inits:[]},stmts),vfxtrs];
        }

        function program ()
            : [TOKENS, Ast::PROGRAM]
        {
            enter("Parser::program ","");

            let [ts,cs] = scan.tokenList (scan.start);
            this.coordList = cs;

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
            default_namespace = new Ast::PublicNamespace ("");
            current_class = "";

            var [ts2, pragmas, stmts, bfxtrs, vfxtrs] = directives (ts1, Global);

            switch (hd (ts2)) {
            case Token::EOS:
                break;
            default:
                throw "extra tokens after end of program: " + ts2;
            }

            exit ("Parser::program ", ts2);
            return [ts2, new Ast::Program (nd1,new Ast::Block (pragmas,{fixtures:bfxtrs,inits:[]},stmts),vfxtrs)];
        }
    }

    function test ()
    {
        var programs =
            [ "print('hi')"
            , readFile ("./tests/self/t.es")
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

                print(n, "> ", p, Ast::encodeProgram (nd1));
            }
            catch(x)
            {
                print(x)
            }
        }
    }

    test ()
}
}// end module
