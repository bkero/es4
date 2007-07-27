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
    use namespace Debug;

    type PATTERN =
          ( ObjectPattern
          , ArrayPattern
          , SimplePattern
          , IdentifierPattern );

    class ObjectPattern { }
    class ArrayPattern { }
    class SimplePattern { }
    class IdentifierPattern
    {
        const ident : Ast::IDENT;
        function IdentifierPattern (ident)
            : ident = ident { }
    }

    class Parser
    {
        static const AbbrevIfElse = 0;
        static const AbbrevDoWhile = AbbrevIfElse + 1;
        static const AbbrevFunction = AbbrevDoWhile + 1;
        static const Abbrev = AbbrevFunction + 1;
        static const Full = Abbrev + 1;

        type BETA = int;  // NoIn, AllowIn
        type TAU = int;   // Global, Class, Interface, Local
        type OMEGA = int; // Abbrev, Full
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

        function hd (ts) {
            //             print("hd ",ts[0]);
             return Token::tokenKind (ts[0]);
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

        function desugarPattern (p: PATTERN, t: Ast::TYPE_EXPR, e: Ast::EXPR)
            : [[Ast::BINDING],[Ast::INIT_STEP]]
        {
            var bs, ss;
            switch type (p) : PATTERN {
            case (p:IdentifierPattern) {
                let i = new Ast::PropIdent (p.ident);
                var bs = [new Ast::Binding (i,t)];
                if (e !== null) {
                    var ss = [new Ast::InitStep (i,e)];
                }
                else {
                    var ss = [];
                }
            }
            case (x: *) {
                throw "internal error: desugarPattern " + p;
            }
            }
            return [bs,ss];  // FIXME: RI allows [b,[]], which should be an error
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
            case Token::Identifier:
                let id = Token::tokenText (ts[0]);
                var [ts1,nd1] = [tl (ts), id];
                break;
            default:
                throw "invalid qualifier ";
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
                    /*
                case Token::LeftBracket:
                    var [ts2,nd2] = brackets (ts1);
                    var [ts3,nd3] = [ts1, new Ast::QualifiedExpression (nd1,nd2)];
                    break;
                    */
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

            exit ("Parser::priamryName ", ts2);
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

//        function parseXMLLiteral()
//        {
//            throw "XMLLiteral not implemented"
//        }
//
//        function parseXMLElement()
//        {
//        }
//
//        function parseXMLName(first)
//        {
//        }
//
//        function parseXMLAttributes(first)
//        {
//        }
//
//        function parseXMLAttribute(first)
//        {
//        }
//
//        function parseXMLElementContent(first)
//        {
//        }
//
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

//        function parseParenExpression()
//        {
//            enter("parseParenExpression")
//
//            enterSlashContext(regexpliteral_token)
//            match(leftparen_token);
//            var result = parseAssignmentExpression(allowIn_mode)
//            exitSlashContext(regexpliteral_token)
//            match(rightparen_token)
//
//            exit("parseParenExpression",result)
//            return result
//        }
//
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

//        function parseParenListExpression()
//        {
//            enter("parseParenListExpression")
//
//            enterSlashContext(regexpliteral_token)
//            match( leftparen_token );
//            var result = <ParenList>{parseListExpression(allowIn_mode)}</ParenList>
//            exitSlashContext(regexpliteral_token)
//            match( rightparen_token )
//
//            exit("parseParenListExpression",result)
//            return result
//        }
//
//        /*
//            ParenListOrExpressionQualifiedIdentifier
//                ParenListExpression
//                ParenListExpression :: PropertyIdentifier
//                ParenListExpression :: Brackets
//        */
//
//        function parseParenListOrExpressionQualifiedIdentifier()
//        {
//            enter("parseParenListOrExpressionQualifiedIdentifier")
//
//            var first = parseParenListExpression()
//            if( lookahead(doublecolon_token) )
//            {
//                match(doublecolon_token)
//                if( lookahead(leftbracket_token) )
//                {
//                    var second = parseBrackets()
//                    var result = <QualifiedExpression><Qualifier>{first}</Qualified><Expr>{second}</Expr></QualifiedExpression>
//                }
//                else
//                {
//                    var second = parsePropertyIdentifier()
//                    var result = <QualifiedIdentifier><Qualified>{first}</Qualified>{second}</QualifiedIdentifier>
//                }
//            }
//            else
//            {
//                var result = first
//            }
//
//            exit("parseParenListOrExpressionQualifiedIdentifier",result)
//            return result
//        }
//
//
//        */
//
//        function parseObjectLiteral()
//        {
//            enter("parseObjectLiteral")
//
//            enterSlashContext(regexpliteral_token)
//            match(leftbrace_token)
//            if( lookahead(rightbrace_token) )
//            {
//                var first = null
//            }
//            else
//            {
//                var first = parseFieldListPrime(<>{parseLiteralField()}</>)
//            }
//            exitSlashContext(regexpliteral_token)
//            match(rightbrace_token)
//            var result = <LiteralObject>{first}</LiteralObject>
//
//            exit("parseObjectLiteral",result)
//            return result
//        }
//
//        /*
//
//        */
//
//        function parseFieldListPrime(first)
//        {
//            enter("parseFieldListPrime",first)
//
//            if( lookahead(comma_token) )
//            {
//				if( isLet(first) )
//				{
//					throw "ambiguous syntax, use parens to clarify list association"
//				}
//                match(comma_token)
//                var second = parseLiteralField()
//                var result = parseFieldListPrime(<>{first}{second}</>)
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
//
//        LiteralField
//            FieldName  :  AssignmentExpressionallowIn
//
//        */
//
//        function parseLiteralField()
//        {
//            enter("parseLiteralField")
//
//            var first = parseFieldName()
//            match(colon_token)
//            var second = parseAssignmentExpression(allowIn_mode)
//            var result = <LiteralField>{first}{second}</LiteralField>
//
//            exit("parseLiteralField",result)
//            return result
//        }
//
//        /*
//
//        FieldName
//            NonAttributeQualifiedIdentifier
//            String
//            Number
//            ParenExpression
//            ReservedIdentifier
//            ContextuallyReservedIdentifier
//
//        */
//
//        function parseFieldName()
//        {
//            enter("parseFieldName")
//
//            if( lookahead(stringliteral_token) )
//            {
//                result = <LiteralString value={scan.tokenText(match(stringliteral_token))}/>
//            }
//            else if( lookahead(numberliteral_token) )
//            {
//                result = <LiteralNumber value={scan.tokenText(match(numberliteral_token))}/>
//            }
//            else if( lookahead(leftparen_token) )
//            {
//                var result = parseParenExpression();
//            }
//            else
//            if( lookahead( lookaheadReservedWord) )
//            {
//                var result = <Identifier>{scan.tokenText(matchReservedWord())}</Identifier>
//            }
//            else
//            {
//                var result = parseNonAttributeQualifiedIdentifier();
//            }
//
//            exit("parseFieldName",result)
//            return result
//        }
//
//        /*
//
//        ArrayLiteral
//            [  ElementList  ]
//
//        ElementList
//            empty
//            LiteralElement
//            ,  ElementList
//            LiteralElement  ,  ElementList
//
//        LiteralElement
//            AssignmentExpressionallowIn
//
//        */
//
//        function parseArrayLiteral()
//        {
//            enter("parseArrayLiteral")
//
//            enterSlashContext(regexpliteral_token)
//            match(leftbracket_token)
//            if( lookahead(rightbracket_token) )
//            {
//                var first = <></>
//            }
//            else
//            {
//                var temp = parseLiteralElement()
//                var first = parseElementListPrime(<>{temp}</>)
//            }
//            exitSlashContext(regexpliteral_token)
//            match(rightbracket_token)
//            var result = <LiteralArray>{first}</LiteralArray>
//
//            exit("parseArrayLiteral",result)
//            return result
//        }
//
//        function parseElementListPrime(first)
//        {
//            enter("parseElementListPrime",first)
//
//            while( lookahead(comma_token) )
//            {
//				if( isLet(first) )
//				{
//					throw "ambiguous syntax, use parens to clarify list association"
//				}
//                match(comma_token)
//                var second = parseLiteralElement()
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
//            exit("parseElementListPrime",result)
//            return result
//        }
//
//        function parseLiteralElement()
//        {
//            enter("parseLiteralElement")
//
//            if( lookahead(comma_token) )
//            {
//                var result = <EmptyElement/>
//            }
//            else
//            if( lookahead(rightbracket_token) )
//            {
//                var result = null
//            }
//            else
//            {
//                var result = parseAssignmentExpression(allowIn_mode)
//            }
//
//            exit("parseLiteralElement",result)
//            return result
//        }
//

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
                nd1 = new Ast::LexicalRef (nd1);
                break;
            }

            exit("Parser::primaryExpression ",ts1);
            return [ts1,nd1];
        }

//
//        /*
//
//        SuperExpression
//            super
//            super  Arguments
//
//        */
//
//        function parseSuperExpression()
//        {
//            enter("parseSuperExpression")
//
//            match(super_token)
//            var first = <SuperExpression/>
//            if( lookahead(leftparen_token) )
//            {
//                var result = parseArguments(first)
//            }
//            else
//            {
//                var result = first
//            }
//
//            exit("parseSuperExpression",result)
//            return result
//        }

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

//        function parsePropertyOperator(first)
//        {
//            enter("parsePropertyOperator",first)
//
//            if( lookahead(dot_token) )
//            {
//                match(dot_token)
//                if( lookahead(leftparen_token) )
//                {
//                    var second = parseParenListExpression()
//                    if( lookahead(doublecolon_token) )
//                    {
//                        match(doublecolon_token)
//                        if( lookahead(leftbracket_token) )
//                        {
//                            var third = parseBrackets()
//                            var result = <Get kind="bracket">{first}<QualifiedExpression><Qualifier>{second}</Qualifier>{third}</QualifiedExpression></Get>
//                        }
//                        else
//                        {
//                            var third = parsePropertyIdentifier()
//                            var result = <Get kind="dot">{first}<QualifiedIdentifier><Qualifier>{second}</Qualifier>{third}</QualifiedIdentifier></Get>
//                        }
//                    }
//                    else
//                    {
//                        var result = <FilterExpression>{first}{second}</FilterExpression>
//                    }
//                }
//                else
//                if( lookaheadReservedWord() )
//                {
//                    var second = <Identifier>{scan.tokenText(matchReservedWord())}</Identifier>
//                    var result = <Get kind="dot">{first}{second}</Get>
//                }
//                else
//                {
//                    var second = parseQualifiedIdentifier()
//                    var result = <Get kind="dot">{first}{second}</Get>
//                }
//            }
//            else
//            if( lookahead(doubledot_token) )
//            {
//                match(doubledot_token)
//                var second = parseQualifiedIdentifier()
//                var result = <DescendExpression>{first}{second}</DescendExpression>
//            }
//            else
//            if( lookahead(leftbracket_token) )
//            {
//                var second = parseBrackets()
//                var result = <Get kind="bracket">{first}{second}</Get>
//            }
//
//            exit("parsePropertyOperator",result)
//            return result
//        }
//
//        /*
//
//        Brackets
//            [  ]
//            [  ListExpressionallowIn  ]
//            [  ListExpressionallowIn  :  ]
//            [  ListExpressionallowIn  :  ListExpressionallowIn  ]
//            [  :  ListExpressionallowIn  ]
//
//        */
//
//        function parseBrackets()
//        {
//            enter("parseBrackets")
//
//            match(leftbracket_token)
//            if( lookahead(rightbracket_token) )
//            {
//                var first = null
//                var second = null
//            }
//            else
//            if( lookahead(colon_token) )
//            {
//                match(colon_token)
//                var first = null
//                var second = parseListExpression(allowIn_mode)
//            }
//            else
//            {
//                var first = parseListExpression(allowIn_mode)
//                if( lookahead(colon_token) )
//                {
//                    match(colon_token)
//                    if( lookahead(rightbracket_token) )
//                    {
//                        var second = null
//                    }
//                    else
//                    {
//                        var second = parseListExpression(allowIn_mode)
//                    }
//                }
//                else
//                {
//                }
//            }
//            match(rightbracket_token);
//            var result = <Brackets>{first}{second}</Brackets>
//
//            exit("parseBrackets",result)
//            return result
//        }
//
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

        CallExpressionPrime(beta)
            Arguments CallExpressionPrime(beta)
            [ Expression ] CallExpressionPrime(beta)
            . Identifier CallExpressionPrime(beta)
            empty

        */

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

            exit ("Parser::callExpressionPrime ", tsx);
            return [tsx, ndx];
        }

        // new X; new X (); new X () (); X (); X

        /*

        NewExpression
            MemberExpression
            new  NewExpression

        */

        function newExpression (ts: TOKENS, beta:BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::newExpression ", ts);

            switch (hd (ts)) {
            case Token::New:
                switch (hd (tl (ts))) {
                case Token::New:
                    let [ts1,nd1] = newExpression (tl (ts), beta);
                    var [tsx,ndx] = [tsx, new Ast::NewExpr (nd1,[])];
                    break;
                default:
                    var [tsx,ndx] = memberExpression (ts, beta);
                    break;
                }
                break;
            default:
                let [ts1,nd1] = memberExpression (ts,beta);
                switch (hd (ts1)) {
                case Token::LeftParen:
                    val [tsx,ndx] = callExpressionPrime (ts1,nd1,beta);
                    break;
                default:
                    var [tsx,ndx] = [ts1,ndx];
                    break;
                }
                break;
            }

            exit ("Parser::newExpression ", tsx);
            return [tsx, ndx];
        }

        /*

        LeftHandSideExpression
            NewExpression
            CallExpression

        Refactored:

        LeftHandSideExpression
            MemberExpression LeftHandSideExpressionPrime
            new NewExpression

        LeftHandSideExpressionPrime
            Arguments CallExpressionPrime
            empty

        */

        function leftHandSideExpression (ts: TOKENS, beta:BETA)
            : [TOKENS, Ast::EXPR]
        {
            enter("Parser::leftHandSideExpression ", ts);

            switch (hd (ts)) {
            case Token::New:
                var [tsx,ndx] = newExpression (ts,beta);
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
            }

            exit ("Parser::leftHandSideExpression ", tsx);
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
                var [tsx,ndx] = [tl (ts1), new Ast::UnaryExpr (Ast::preIncrOp,nd1)];
                break;
            case Token::MinusMinus:
                var [tsx,ndx] = [tl (ts1), new Ast::UnaryExpr (Ast::preDecrOp,nd1)];
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
                let [ts1,nd1] = postfixExpression (ts,beta);
                var [tsx,ndx] = [tsx,new Ast::UnaryExpr (Ast::deleteOp,ndx)];
                break;
            case Token::Void:
                let [ts1,nd1] = unaryExpression (ts,beta);
                var [tsx,ndx] = [tsx,new Ast::UnaryExpr (Ast::voidOp,ndx)];
                break;
            case Token::TypeOf:
                let [ts1,nd1] = unaryExpression (ts,beta);
                var [tsx,ndx] = [tsx,new Ast::UnaryExpr (Ast::typeOfOp,ndx)];
                break;
            case Token::PlusPlus:
                let [ts1,nd1] = postfixExpression (ts,beta);
                var [tsx,ndx] = [tsx,new Ast::UnaryExpr (Ast::preIncrOp,ndx)];
                break;
            case Token::MinusMinus:
                let [ts1,nd1] = postfixExpression (ts,beta);
                var [tsx,ndx] = [tsx,new Ast::UnaryExpr (Ast::preDecrOp,ndx)];
                break;
            case Token::Plus:
                let [ts1,nd1] = unaryExpression (ts,beta);
                var [tsx,ndx] = [tsx,new Ast::UnaryExpr (Ast::unaryPlusOp,ndx)];
                break;
            case Token::Minus:
                let [ts1,nd1] = unaryExpression (ts,beta);
                var [tsx,ndx] = [tsx,new Ast::UnaryExpr (Ast::unaryMinusOp,ndx)];
                break;
            case Token::BitwiseNot:
                let [ts1,nd1] = unaryExpression (ts,beta);
                var [tsx,ndx] = [tsx,new Ast::UnaryExpr (Ast::bitwiseNotOp,ndx)];
                break;
            case Token::Not:
                let [ts1,nd1] = unaryExpression (ts,beta);
                var [tsx,ndx] = [tsx,new Ast::UnaryExpr (Ast::logicalNotOp,ndx)];
                break;
            case Token::Type:
                let [ts1,nd1] = nullableTypeExpression (ts,beta);
                var [tsx,ndx] = [tsx,new Ast::TypeExpr (ndx)];
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

            [ts1,nd1] = unaryExpression (ts, beta);

            done:
            while (true) {
                switch (hd (ts1)) {
                case Token::Mult:
                    var op = Ast::multOp;
                    break;
                case Token::Div:
                    var op = Ast::divOp;
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

//        /*
//
//        YieldExpression
//            UnaryExpression
//            yield  UnaryExpression
//
//        */
//
//        function parseYieldExpression()
//        {
//            enter("parseYieldExpression")
//
//            if( lookahead(yield_token) )
//            {
//                enterSlashContext(regexpliteral_token)
//
//	            match(yield_token)
//    	        if( !(lookaheadSemicolon(full_mode) || lookahead(rightparen_token) || lookahead(rightbrace_token) || lookahead(comma_token)) )
//        	    {
//	            	var first = parseUnaryExpression()
//	    	        var result = <YieldExpression>{first}</YieldExpression>
//    	        }
//				else
//				{
//					var result = <YieldExpression/>
//				}
//
//                exitSlashContext(regexpliteral_token)
//            }
//            else
//            {
//                var result = parseUnaryExpression()
//            }
//
//            exit("parseYieldExpression",result)
//            return result
//        }

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

//
//        function parseConditionalExpression(mode)
//        {
//            enter("parseConditionalExpression",mode)
//
//            var result
//            var first
//
//            first = parseLogicalOrExpression(mode)
//
//            if( lookahead(questionmark_token) )
//            {
//                match(questionmark_token);
//                var second;
//                var third;
//                second = parseAssignmentExpression(mode);
//                match(colon_token);
//                third = parseAssignmentExpression(mode);
//                result = <ConditionalExpression>{first}{second}{third}</ConditionalExpression>
//            }
//            else
//            {
//                result = first
//            }
//
//            exit("parseConditionalExpression",result)
//            return result
//        }
//
//        /*
//
//
//
//        */
//
//        function parseNonAssignmentExpression(mode)
//        {
//            enter("parseNonAssignmentExpression",mode)
//
//            //var first = parseLogicalOrExpression(mode)
//            var first = parsePostfixExpression()
//
//            if( lookahead(questionmark_token) )
//            {
//                match(questionmark_token);
//                var second = parseNonAssignmentExpression(mode);
//                match(colon_token);
//                var third = parseNonAssignmentExpression(mode);
//                var result = <ConditionalExpression>{first}{second}{third}</ConditionalExpression>
//            }
//            else
//            {
//                var result = first
//            }
//
//            exit("parseNonAssignmentExpression",result)
//            return result
//        }
//
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

            exit ("Parser::assignmentExpression ", ts1);
            return [ts1,nd1];
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
                var [tsx,ndx] = [ts1, new IdentifierPattern (nd1.Ast::ident)];
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
            null
            undefined
            TypeExpression
            TypeExpression  ?
            TypeExpression  !

        */

        function nullableTypeExpression (ts: TOKENS)
            : [TOKENS, Ast::TYPE_EXPR]
        {
            enter("Parser::nullableTypeExpression ", ts);

            switch (hd (ts1)) {
            case Token::Null:
                var [ts1,nd1] = [tl (ts), new Ast::SpecialType (new Ast::NullType)];
                break;
            case Token::Undefined:
                var [ts1,nd1] = [tl (ts), new Ast::SpecialType (new Ast::UndefinedType)];
                break;
            default:
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
                break;
            }

            exit("Parser::nullableTypeExpression ", ts1);
            return [ts1,nd1];
        }

        /*

        TypeExpression
            FunctionType
            UnionType
            RecordType
            ArrayType
            PrimaryIdentifier

        */

        function typeExpression (ts: TOKENS)
            : [TOKENS, Ast::TYPE_EXPR]
        {
            enter("Parser::typeExpression ", ts);

            switch (hd (ts1)) {
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
                var [ts1,nd1] = primaryIdentifier (ts);
                nd1 = new Ast::TypeName (nd1);  // need type expr
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
            case Token::Return:
                var [ts1,nd1] = returnStatement (ts,omega);
                var [ts2,nd2] = [semicolon (ts1,omega),nd1];
                break;
            default:
                let [ts1,nd1] = expressionStatement (ts);
                var [ts2,nd2] = [semicolon (ts1,omega),nd1];
                break;
            }

            exit("Parser::statement ", ts2);
            return [ts2,nd2];
        }

        function newline (ts: TOKENS)
            : boolean
        {
            return false; // FIXME
        }

        function semicolon (ts: TOKENS, omega: OMEGA)
            : [TOKENS]
        {
            enter("Parser::semicolon ", ts);

            switch (omega) {
            case Full:
                switch (hd (ts)) {
                case Token::SemiColon:
                    var ts1 = tl (ts);
                    break;
                case Token::EOS:
                case Token::RightBrace:
                    var ts1 = ts;
                    break;
                default:
                    if (newline (ts)) { var ts1=ts } // semicolon inserted
                    else { throw "expecting semicolon" }
                }
            default:  // Abbrev, ShortIf
                switch (hd (ts)) {
                case Token::SemiColon:
                    var ts1 = tl (ts);
                    break;
                default:
                    var ts1 = ts;
                }
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
            var [ts2,nd2] = statement (ts1, omega); // FIXME: should be subStatement to include empty stmt
            switch (hd (ts2)) {
            case Token::Else:
                var [ts3,nd3] = statement (tl (ts2), omega);
                break;
            default:
                var [ts3,nd3] = [ts2,null];
                break;
            }

            exit("Parser::ifStatement ", ts3);
            return [ts3, new Ast::IfStmt (nd1,nd2,nd3)];
        }


//        function parseStatement(mode)
//        {
//            enter("parseStatement",mode)
//
//            if( lookahead(super_token) )
//            {
//                var node = parseSuperStatement()
//                matchSemicolon(mode)
//            }
//            else
//            if( lookahead(leftbrace_token) )
//            {
//                var node = parseBlockStatement()
//            }
//            else
//            if( lookahead(if_token) )
//            {
//                var node = parseIfStatement(mode)
//            }
//            else
//            if( lookahead(switch_token) )
//            {
//                var node = parseSwitchStatement()  //includes 'switch type'
//            }
//            else
//            if( lookahead(do_token) )
//            {
//                var node = parseDoStatement()
//                matchSemicolon(mode)
//            }
//            else
//            if( lookahead(while_token) )
//            {
//                var node = parseWhileStatement(mode)
//            }
//            else
//            if( lookahead(for_token) )
//            {
//                var node = parseForStatement(mode)
//            }
//            else
//            if( lookahead(let_token) )
//            {
//                match(let_token) // because other context do
//                var node = parseLetStatement(mode)
//            }
//            else
//            if( lookahead(with_token) )
//            {
//                var node = parseWithStatement(mode)
//            }
//            else
//            if( lookahead(continue_token) )
//            {
//                var node = parseContinueStatement()
//                matchSemicolon(mode)
//            }
//            else
//            if( lookahead(break_token) )
//            {
//                Var node = parseBreakStatement()
//                matchSemicolon(mode)
//            }
//            else
//            if( lookahead(return_token) )
//            {
//                var node = parseReturnStatement()
//                matchSemicolon(mode)
//            }
//            else
//            if( lookahead(throw_token) )
//            {
//                var node = parseThrowStatement()
//                matchSemicolon(mode)
//            }
//            else
//            if( lookahead(try_token) )
//            {
//                var node = parseTryStatement()
//            }
//            else
//            if( lookahead(default_token) )
//            {
//                var node = parseDefaultXMLNamespaceStatement()
//                matchSemicolon(mode)
//            }
//            else
//            {
//                var node = parseLabeledOrExpressionStatement(mode)
//                matchSemicolon(mode)
//            }
//
//            exit("parseStatement",node)
//            return node
//        }
//
//        /*
//        */
//
//        function parseSubstatement(mode)
//        {
//            enter("parseSubstatement")
//
//            var node = parseStatement(mode)
//
//            exit("parseSubstatement",node)
//            return node
//        }
//
//        function parseBlockStatement()
//        {
//            enter("parseSubstatement")
//
//            var prologue = <Prologue/>
//            var stmts = parseBlock(prologue)
//            //var slots = stmts.Slot  // hoist let slots
//            //delete stmts.Slot
//            var node = <BlockStatement>{prologue}{stmts}</BlockStatement>
//
//            exit("parseBlockStatement",node)
//            return node
//        }
//
//        /*
//
//        SuperExpression
//            super
//            super  Arguments
//
//        */
//
//        function parseSuperStatement()
//        {
//            enter("parseSuperStatement")
//
//            match(super_token)
//            var first = <SuperStatement/>
//            if( lookahead(leftparen_token) )
//            {
//                var result = parseArguments(first)
//            }
//            else
//            {
//                var result = first
//            }
//
//            exit("parseSuperStatement",result)
//            return result
//        }
//
//        function parseLabeledOrExpressionStatement(mode)
//        {
//            enter("parseLabeledOrExpressionStatement",mode)
//
//            var first = parseListExpression(allowIn_mode)
//            if( lookahead(colon_token) )
//            {
//                if( first.length() == 1 || first.Get.identifier != void 0 )
//                {
//                    first = first.Get.identifier
//                }
//                else
//                {
//                    throw "invalid label"
//                }
//                match(colon_token)
//                var second = parseSubstatement(mode)
//                var result = <LabeledStatement>{first}{second}</LabeledStatement>
//            }
//            else
//            {
//                var result = <ExpressionStatement>{first}</ExpressionStatement>
//                // leave matchSemicolon(mode) for caller
//            }
//
//            exit("parseLabeledOrExpressionStatement",result)
//            return result
//        }
//
//        function parseBlock(prologue)
//        {
//            exit("parseBlock")
//
//            match(leftbrace_token)
//            var node = parseDirectives(void 0,prologue)
//            match(rightbrace_token)
//
//            exit("parseBlock",node)
//            return node
//        }
//
//        function parseMetaData()
//        {
//        }
//
//        function parseIfStatement(mode)
//        {
//        }
//
//        function parseSwitchStatement()
//        {
//        }
//
//        function parseCaseStatement(mode)
//        {
//        }
//
//        function parseCaseLabel()
//        {
//        }
//
//        function parseCaseStatements()
//        {
//        }
//
//        function parseCaseStatementsPrefix(first)
//        {
//        }
//
//        function parseDoStatement()
//        {
//        }
//
//        function parseWhileStatement(mode)
//        {
//        }
//
//        function parseForStatement(mode)
//        {
//        }
//
//        function parseLetStatement(mode)
//        {
//            enter("parseLetStatement")
//
//            // already ate 'let'
//
//            var prologue = <Prologue/>
//            var block = <Block/>
//            match(leftparen_token)
//            if( lookahead(rightparen_token) )
//            {
//            }
//            else
//            {
//                block.* += parseVariableBinding(<Attributes><Let/></Attributes>,var_token,allowIn_mode,prologue)
//                while( lookahead(comma_token) )
//                {
//                    match(comma_token)
//                    block.* += parseVariableBinding(<Attributes><Let/></Attributes>,var_token,allowIn_mode,prologue)
//                }
//            }
//            match(rightparen_token)
//            var second = parseSubstatement(mode)
//            if( second.name() == "BlockStatement" )
//            {
//                prologue.* += second.Prologue.*
//                block.* += second.Block.*
//            }
//            else
//            {
//                block.* += second
//            }
//
//            var node = <BlockStatement kind="let">{prologue}{block}</BlockStatement>
//
//            exit("parseLetStatement",node)
//            return node
//        }
//
//        function parseWithStatement(mode)
//        {
//            throw "WithStatement not implemented"
//        }
//
//        function parseContinueStatement()
//        {
//            throw "ContinueStatement not implemented"
//        }
//
//        function parseBreakStatement()
//        {
//            throw "BreakStatement not implemented"
//        }
//
//        /*
//
//        Returnstatement
//            return
//            return [no line break] ListExpressionallowIn
//
//        */
//
//        function parseReturnStatement()
//        {
//            enter("parseReturnStatement")
//
//            match(return_token)
//
//            if( !inFunctionBody(true) )
//            {
//                throw "return statement is not allowed outside of function body"
//            }
//
//            var node = <Return/>
//
//            if( !lookaheadSemicolon(full_mode) )
//            {
//                node.* = parseListExpression(allowIn_mode)
//            }
//
//            exit("parseReturnStatement",node)
//            return node
//        }
//
//        function parseThrowStatement()
//        {
//            throw "ThrowStatement not implemented"
//        }
//
//        function parseTryStatement()
//        {
//            throw "TryStatement not implemented"
//        }
//
//        function parseCatchClauses()
//        {
//            throw "CatchClauses not implemented"
//        }
//
//        function parseCatchClause()
//        {
//            throw "CatchClause not implemented"
//        }
//
//        function parseFinallyClause()
//        {
//            throw "FinallyClause not implemented"
//        }
//
//        function parseDefaultXMLNamespaceStatement()
//        {
//            throw "DefaultXMLNamespaceStatement not implemented"
//        }
//
//        function parseAnnotatedDirective(mode)
//        {
//            throw "AnnotatedDirective not implemented"
//        }
//
//        function parseAnnotatedSubstatementsOrStatement(mode)
//        {
//            throw "not implemented"
//        }
//
//        function parseAnnotatableDirective(attrs,mode,prologue)
//        {
//            enter("parseAnnotatableDirective",attrs,mode)
//
//            if( lookahead(let_token) )
//            {
//                match(let_token)
//                attrs.* += <Let/>  // the let attribute
//                if( lookahead(function_token) )
//                {
//                    var node = parseFunctionDefinition(attrs,prologue)
//                }
//                else
//                {
//                    var node = parseVariableDefinition(attrs,allowIn_mode,prologue)
//                    matchSemicolon(mode)
//                }
//            }
//            else
//            if( lookahead(var_token) )
//            {
//                match(var_token) // eat 'var' before calling parseVar...
//                var node = parseVariableDefinition(attrs,allowIn_mode,prologue)
//            }
//            else
//            if( lookahead(const_token) )
//            {
//                var node = parseVariableDefinition(attrs,allowIn_mode,prologue)
//            }
//            else
//            if( lookahead(function_token) )
//            {
//                var node = parseFunctionDefinition(attrs,prologue)
//            }
//            else
//            if( lookahead(class_token) )
//            {
//                var node = parseClassDefinition(attrs,prologue)
//            }
//            else
//            if( lookahead(interface_token) )
//            {
//                var node = parseInterfaceDefinition(attrs,prologue)
//            }
//            else
//            if( lookahead(namespace_token) )
//            {
//                var node = parseNamespaceDefinition(attrs,prologue)
//            }
//            else
//            if( lookahead(type_token) )
//            {
//                var node = parseTypeDefinition(attrs,prologue)
//            }
//            else
//            {
//                throw "not implemented yet"
//            }
//
//            exit("parseAnnotatableDirective",node)
//            return node
//        }
//
//        function parseAnnotatableDirectiveOrLetStatement(attrs,mode,prologue)  // actually only need to handle let bindings and let statements
//        {
//            enter("parseAnnotatableDirectiveOrLetStatement",attrs,mode)
//
//            match(let_token)
//
//            if( lookahead(leftparen_token ) )  // Let statement
//            {
//                var node = parseLetStatement(mode)
//            }
//            else  // Let binding
//            {
//                attrs.* += <Let/>  // the let attribute
//                if( lookahead(function_token) )
//                {
//                    var node = parseFunctionDefinition(attrs,prologue)
//                }
//                else
//                {
//                    var node = parseVariableDefinition(attrs,allowIn_mode,prologue)
//                }
//            }
//
//            exit("parseAnnotatableDirectiveOrLetStatement",node)
//            return node
//        }
//
//        function parseIncludeDirective()
//        {
//        }
//
        /*

        VariableDefinition(beta)
            VariableDefinitionKind  VariableBindingList(beta)

        */

        function variableDefinition (ts: TOKENS, beta: BETA, ns, isPrototype, isStatic)
            : [TOKENS, Ast::DIRECTIVES]
        {
            enter("Parser::variableDefinition ", ts);

            let [ts1,nd1] = variableDefinitionKind (ts);
            let [ts2,nd2] = variableBindingList (ts1, beta);

            print("nd2[0].length=",nd2[0].length);
            exit("Parser::variableDefinition ", ts2);
            return [ ts2
                   , { pragmas: []
                     , defns: [new Ast::VariableDefn (ns,isStatic,isPrototype,nd1,nd2)]
                     , head: null
                     , stmts: []
                     , pos: null } ];
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
                switch (hd (ts)) {
                case Token::Const:
                    var [tsx,ndx] = [tl (ts), Ast::letConstTag];
                    break;
                case Token::Function:
                    throw "internal error: variableDefinitionKind after let";
                    break;
                default:
                    var [tsx,ndx] = [tl (ts), Ast::letTag];
                    break;
                }
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
            : [TOKENS, Ast::BINDING_INITS]
        {
            enter("Parser::variableBindingList ", ts);

            function variableBindingListPrime (ts: TOKENS )
                : [TOKENS, Ast::BINDING_INITS]
            {
                enter("Parser::variableBindingListPrime ", ts);
        
                switch (hd (ts)) {
                case Token::Comma:
                    var [ts1,nd1] = variableBinding (tl (ts), beta);
                    var [ts2,nd2] = variableBindingListPrime (ts1);

                    var [b1,i1] = nd1;  // FIXME: fold into patterns above when it works in the RI
                    var [b2,i2] = nd2;

                    for (var n in b2) b1.push (b2[n]);  // FIXME: use concat when it works in the RI
                    for (var n in i2) i1.push (i2[n]);

                    break;
                default:
                    var [ts2,nd2] = [ts,[[],[]]];
                    var [b2,i2] = nd2;
                    break;
                }

                exit ("Parser::variableBindingListPrime ", ts2);
                return [ts2,[b1,i1]];
            }

            var [ts1,nd1] = variableBinding (ts, beta);
            var [ts2,nd2] = variableBindingListPrime (ts1, beta);

            var [b1,i1] = nd1;  // FIXME: fold into patterns above when it works in the RI
            var [b2,i2] = nd2;

            for (var n in b2) b1.push (b2[n]);  // FIXME: use concat when it works in the RI
            for (var n in i2) i1.push (i2[n]);

            print("b1.length=",b1.length);
            exit ("Parser::variableBindingList ", ts2);
            return [ts2,[b1,i1]];
        }

        function variableBinding (ts: TOKENS, beta: BETA, ns, isPrototype, isStatic)
            : [TOKENS, Ast::BINDING_INITS]
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
                print('point a');
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

//        function parseVariableBindingList(attrs,kind,mode,prologue)
//        {
//            enter("parseVariableBindingList",attrs,kind,mode)
//
//            var node = <></>
//            node += parseVariableBinding(attrs,kind,mode,prologue)
//
//            while( lookahead( comma_token ) )
//            {
//                match( comma_token );
//                node += parseVariableBinding(attrs,kind,mode,prologue);
//            }
//
//            exit("parseVariableBindingList",node)
//            return node
//        }
//
//        /*
//
//        VariableBindingb
//            TypedIdentifierb VariableInitialisationb
//            DestructuringPattern VariableInitialisationb
//
//        VariableInitialisationb
//            empty
//            =  VariableInitialiserb
//
//        VariableInitialiserb
//            AssignmentExpressionb
//
//        */
//
//        function isNamespaceAttribute(attr)
//        {
//            enter("isNamespaceAttribute",attr.toXMLString())
//
//            var result =
//                    ( attr.name()=="Get" &&
//                      attr.Identifier != undefined ) ? true :
//                      attr.name()=="Namespace" ? true : false
//
//            exit("isNamespaceAttribute",result)
//            Return result
//        }
//
//        function inFunctionBody(recurse=false)
//        {
//            enter("inFunctionBody")
//
//            if( recurse )
//            {
//                var result = false
//                for each( var item in slot_context_stack )
//                {
//                    if( item=="function" )
//                    {
//                        result = true
//                        break
//                    }
//                }
//            }
//            else
//            {
//                var context = slot_context_stack[slot_context_stack.length-1]
//                var result = context=="function"
//            }
//
//            exit("inFunctionBody",result)
//            return result
//        }
//
//        function inClassBody()
//        {
//            enter("inClassBody")
//            var context = slot_context_stack[slot_context_stack.length-1]
//            var result = context=="class"
//            exit("inClassBody",result)
//            return result
//        }
//
//        function inInterfaceBody()
//        {
//            enter("inInterfaceBody")
//            var context = slot_context_stack[slot_context_stack.length-1]
//            var result = context=="interface"
//            exit("inInterfaceBody",result)
//            return result
//        }
//
//        function inClassOrInterfaceBody()
//        {
//            enter("inClassOrInterfaceBody")
//            var context = slot_context_stack[slot_context_stack.length-1]
//            var result = context=="class" || context=="interface"
//            exit("inClassOrInterfaceBody",result)
//            return result
//        }
//
//        function parseVariableBinding(attrs,kind,mode,prologue)
//        {
//            enter("parseVariableBinding",attrs,kind,mode)
//
//            if( lookahead(leftbrace_token) || lookahead(leftbracket_token) )
//            {
//                var first = parseDestructuringPattern()
//                match(assign_token)
//                var second = parseAssignmentExpression(mode)
//            }
//            else
//            {
//                var first  = parseTypedIdentifier(mode)
//                if( lookahead(assign_token) )
//                {
//                    match(assign_token)
//                    var second = parseAssignmentExpression(mode);
//                }
//                else
//                {
//                    var second
//                }
//                var node = makeBinding(attrs,kind,first,second,prologue)
//            }
//
//            exit("parseVariableBinding",node)
//            return node
//        }
//
//        /*
//
//        Make a slot
//
//        For some kinds of bindings we hoist the intialiser to the prologue along with
//        the slot (instance slots, function slots). The value of a function slot
//        initialiser is moved by the Definer to derive an ExpressionStatement inserted
//        at the beginning of the corresponding Block. The Definer also hoists some
//        slots (var,const,function) to the inner most enclosing variable object
//        (global,class,function)
//
//        */
//
//        function makeBinding(attrs,kind,typedid,value,prologue)
//        {
//            enter("makeBinding",attrs,kind,typedid,value)
//
//            // See if there is one namespace attribute
//
//            var ns = null
//            for each( var attr in attrs.* )
//            {
//                if( isNamespaceAttribute(attr) )
//                {
//                    if( ns === null )
//                    {
//                        ns = attr
//                    }
//                    else
//                    {
//                        throw "only one namespace attribute allowed"
//                    }
//                }
//            }
//
//            // Make a qualified identifier
//
//            if( ns != null )
//            {
//                var name =
//                    <QualifiedIdentifier>
//                        <Qualifier>{ns}</Qualifier>
//                        {typedid.Identifier}
//                    </QualifiedIdentifier>
//            }
//            else   // use the default namespace
//            {
//                var name =
//                    <QualifiedIdentifier>
//                        <Qualifier>
//                            {default_namespace}
//                        </Qualifier>
//                        {typedid.Identifier}
//                    </QualifiedIdentifier>
//            }
//
//            // Get the type if it has one
//
//            if( typedid.name() == "TypedIdentifier" )
//            {
//                var type = typedid.Type.*
//            }
//			else
//			if( kind == class_token )
//			{
//				var type = <Identifier name="Class"/>
//			}
//            else
//            {
//				var type = <Identifier name="Object"/>
//            }
//
//
//            // Make the slot and initialiser
//
//            if( kind == class_token ||
//                kind == interface_token ||
//                kind == function_token ||
//                kind == namespace_token ||
//                kind == type_token ||
//                inClassBody() && attrs.Let == void 0 && attrs.Static == void 0 )
//            {
//                var slot =
//                    <Slot kind={scan.tokenText(kind)}>
//                        <Name>{name}</Name>
//                        <Type>{type}</Type>
//                        <Init>{value}</Init>
//                    </Slot>
//
//                if( kind == function_token && inClassOrInterfaceBody() )
//                {
//                    slot.@method="true"
//                }
//                var init = <></>
//            }
//            else
//            {
//                var slot =
//                    <Slot kind={scan.tokenText(kind)}>
//                        <Name>{name}</Name>
//                        <Type>{type}</Type>
//                    </Slot>
//
//				if( value != void 0 )
//				{
//	                var init = <>
//                        <ExpressionStatement>
//                	        <Set kind="lexical">
//            	                {name}<To>{value}{typedid.Type}</To>
//        	                </Set>
//    	                </ExpressionStatement></>
//				}
//				else
//				{
//					var init = <></>
//				}
//            }
//
//            // Apply attributes to the slot
//
//            applyAttributesToSlot(attrs,slot)
//
//            // Return the results
//
//            var node = init
//
//            if( slot.@static == "true" )
//            {
//                prologue.Static.* += slot
//            }
//            else
//            if( inClassBody() && slot.@let != "true" )
//            {
//                prologue.Instance.* += slot
//            }
//            else
//            {
//                prologue.* += slot
//            }
//
//            exit("makeBinding",node,slot,prologue)
//            return node
//        }
//
//        var slot_context_stack = ["global"]
//
//        function applyAttributesToSlot(attrs,slot)
//        {
//            enter("applyAttributesToSlot",attrs.toXMLString(),slot)
//
//            var slot_context = slot_context_stack[slot_context_stack.length-1]
//            var slot_kind = slot.@kind
//
//            if( attrs.Let != void 0 )
//            {
//                slot.@let = true
//            }
//            if( attrs.Dynamic != void 0 )
//            {
//                if( slot_kind == "class" )
//                {
//                    slot.@dynamic = true
//                }
//                else
//                {
//                    throw "'dynamic' must only be used on class definitions"
//                }
//            }
//            if( attrs.Final != void 0 )
//            {
//                if( slot_kind == "class" ||
//                    (slot_context == "class" && slot_kind == "function" && attrs.Static == void 0 ) )
//                {
//                    slot.@final = true
//                }
//                else
//                {
//                    throw "'final' must only be used on class and non-static method definitions"
//                }
//            }
//            if( attrs.Native != void 0 )
//            {
//                if( slot_kind == "function" && slot_context != "function" )
//                {
//                    slot.@native = true
//                }
//                else
//                {
//                    throw "'native' must only be used on non-nested function definitions"
//                }
//            }
//            if( attrs.Override != void 0 )
//            {
//                if( slot_context == "class" && slot_kind == "function" && attrs.Static == void 0 )
//                {
//                    slot.@override = true
//                }
//                else
//                {
//                    throw "'override' must only be used on non-static method definitions"
//                }
//            }
//            if( attrs.Prototype != void 0 )
//            {
//                if( slot_context == "class" && attrs.Static == void 0 )
//                {
//                    slot.@prototype = true
//                }
//                else
//                {
//                    throw "'prototype' must only be used on non-static class variable and method definitions"
//                }
//            }
//            if( attrs.Static != void 0 )
//            {
//                if( slot_context == "class" )
//                {
//                    slot.@static = true
//                }
//                else
//                {
//                    throw "'static' must only be used on non-static class variable and method definitions"
//                }
//            }
//            if( attrs.get != void 0 )
//            {
//                if( slot.@kind == "function" )
//                {
//                    slot.@kind = "function get"
//                }
//                else
//                {
//                    throw "'get' must be used on function bindings only"
//                }
//            }
//            if( attrs.set != void 0 )
//            {
//                if( slot.@kind == "function" )
//                {
//                    slot.@kind = "function set"
//                }
//                else
//                {
//                    throw "'set' must be used on function bindings only"
//                }
//            }
//            if( attrs.call != void 0 )
//            {
//                if( slot.@kind == "function" )
//                {
//                    slot.@kind = "function call"
//                }
//                else
//                {
//                    throw "'call' must be used on function bindings only"
//                }
//            }
//            if( attrs.to != void 0 )
//            {
//                if( slot.@kind == "function" )
//                {
//                    slot.@kind = "function to"
//                }
//                else
//                {
//                    throw "'to' must be used on function bindings only"
//                }
//            }
//            if( attrs.operator != void 0 )
//            {
//                if( slot.@kind == "function" )
//                {
//                    slot.@kind = "function operator"
//                }
//                else
//                {
//                    throw "internal error"
//                }
//            }
//            if( attrs.parameter != void 0 )
//            {
//                if( slot.@kind == "var" || slot.@kind == "const" )
//                {
//                    slot.@is_param = "true"
//print("parameter slot found",slot)
//                }
//                else
//                {
//                    throw "internal error"
//                }
//            }
//
//            exit("applyAttributesToSlot",slot)
//            return
//        }
//
//        function parseTypedIdentifier(mode)
//        {
//            enter("parseTypedIdentifier",mode)
//
//            var first =    parseIdentifier()
//            if( lookahead(colon_token) )
//            {
//                match(colon_token)
//                if( lookahead(mult_token) )
//                {
//                    match(mult_token);
//                    var second = <Type><Identifier name="*"/></Type>  // same as no annotation
//                }
//                else
//                if( lookahead(multassign_token) )
//                {
//                    var nexttoken=assign_token; // morph into an assign token
//                    var second = <Type><Identifier name="*"/></Type>  // same as no annotation
//                }
//                else
//                {
//                    var second = <Type>{parseTypeExpression()}</Type>
//                }
//                var result = <TypedIdentifier>{first}{second}</TypedIdentifier>
//            }
//            else
//            {
//                var result = <TypedIdentifier>{first}<Type><Identifier name="*"/></Type></TypedIdentifier>
//            }
//
//            exit("parseTypedIdentifier",result)
//            return result
//        }
//
//        function parseSimpleVariableDefinition()
//        {
//        }
//
//        function parseUntypedVariableBindingList()
//        {
//        }
//
//        function parseUntypedVariableBinding()
//        {
//        }
//

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
            : [TOKENS, Ast::DIRECTIVES]
        {
            enter("Parser::functionDefinition ", ts);

            ts = eat (ts, Token::Function);

            var [ts1,nd1] = functionName (ts);
            var [ts2,nd2] = functionSignature (ts1);
            var [ts3,nd3] = functionBody (ts2, AllowIn);

            var {params:params,defaults:defaults,resultType:resultType,thisType:thisType,hasRest:hasRest} = nd2;
            var func = new Ast::Func (nd1,false,nd3,params,defaults,resultType);
            var defn = new Ast::FunctionDefn (kind,ns,isFinal,isOverride,isPrototype,isStatic,isAbstract,func);

            exit("Parser::functionDefinition ", ts3);

            return [ts3, { pragmas: []
                         , defns: [defn]
                         , head: null
                         , stmts: []
                         , pos: null }];
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

        */

        function functionSignature (ts: TOKENS)
            : [TOKENS, Ast::FUNC_SIG]
        {
            enter("Parser::functionSignature ", ts);

            function headFromBindingInits ([bindings,steps] /*: Ast::BINDING_INITS*/, ns )
                : Ast::HEAD {
                function fixturesFromBindings (bs: [Ast::BINDING])
                    : Ast::FIXTURES {
                    if(bs.length === 0) {
                        return [];
                    }
                    var b0 = bs[0];
                    var n0 = new Ast::PropName ({ns:ns,ident:ident});
                    var f0 = new Ast::ValFixture (null,false);
                    var fs = fixturesFromBindings (bs.slice(1,bs.length));
                    fs.unshift ([n0,f0]);
                    return fs;
                }

                function initsFromInitSteps (is: [Ast::INIT_STEP])
                    : Ast::INITS {
                    // FIXME
                    return [];
                }

                var fixtures = fixturesFromBindings (bindings);
                var inits = initsFromInitSteps (steps);
                return {fixtures:fixtures, inits:inits}
            }

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
                      , returnType: nd3
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
                var [ts1,nd1,hasRest] = nonemptyParameters (ts);
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
                    print("0");
                    let [k1,[b1,i1],e1,t1] = nd1;
                    print("1");
                    var [ts2,nd2,hasRest] = nonemptyParameters (ts1, n+1, e1!=null);
                    print("2");
                    let [[b2,i2],e2,t2] = nd2;
                    print("3");
                    // FIXME when Array.concat works
                    for (let p in b2) b1.push(b2[p]);
                    for (let p in i2) i1.push(i2[p]);
                    for (let p in e2) e1.push(e2[p]);
                    print("4");
                    for (let p in t2) t1.push(t2[p]);
                    print("5");
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
            switch (hd (ts)) {
            case Token::Assign:
                ts = eat (ts, Token::Assign);
                var [ts2,nd2] = nonAssignmentExpression(AllowIn);
                break;
            default:
                if (initRequired) {
                    throw "expecting default value expression";
                }
                var [ts2,nd2] = [ts1,null];
                break;
            }

            var [k,[p,t]] = nd1;
            var [b,i] = desugarPattern (p, t, new Ast::GetParam (n), 0);

            b.push (new Ast::Binding (new Ast::ParamIdent (n), t)); // temp for desugaring

            exit("Parser::parameterInit ", ts1);
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
                var [ts1,nd1] = block (ts,Local);
                var ndx = nd1;
                break;
            default:
                var [ts1,nd1] = assignmentExpression (ts,beta);
                var ndx = new Ast::Block ([],[],null,[new ReturnStmt (nd1)],null);
                break;
            }

            exit("Parser::functionBody ", ts1);

            return [ts1,ndx]
        }

//        /*
//
//        var current_class = null
//
//        /*
//
//        class A { function A(){} var x = 10; function m() {}; print("hello") }
//
//        class 'A'
//          prologue
//            slot 'iinit' function
//              prologue
//                slot 'construct' function ...
//                slot 'x' 10
//                slot 'm' function ...
//              block
//          block
//            print("hello")
//
//        bindings: class field initialisers are not hoisted
//        with their slots but instance var initialisers are
//
//        */
//
//        function parseClassDefinition(attrs,hoisted)
//        {
//            enter("parseClassDefinition",attrs)
//
//            match(class_token)
//            var name = parseClassName()
//
//            current_class = name
//            slot_context_stack.push("class")   // use to determine if inits are hoisted
//
//            var inherits = parseInheritance()
//            var stmt = parseBlockStatement()
//
//            // Move the static and instance slots out of the embedded block statement
//            var prologue = <Prologue>{stmt.Prologue.Static.*}<Instance>{stmt.Prologue.Instance.*}</Instance></Prologue>
//            delete stmt.Prologue.Static
//            delete stmt.Prologue.Instance
//
//            slot_context_stack.pop()
//
//            current_class = null
//
//            var value = <Class>{name}{inherits}{prologue}<Block>{stmt}</Block></Class>
//            delete value.Block.BlockStatement.Prologue.Static
//
//            var node = makeBinding(attrs,class_token,name,value,hoisted)
//
//            exit("parseClassDefinition",node)
//            return node
//        }
//
//        function parseClassName()
//        {
//
//            var first = parseIdentifier()
//
//            if( lookahead(leftdotangle_token) )
//            {
//                var second = parseTypeParameters()
//            }
//            else
//            {
//                var second = <></>
//            }
//
//            if( lookahead(not_token) )
//            {
//                match(not_token)
//                var node = <ClassName not_nullable="true">{first}{second}</ClassName>
//            }
//            else
//            {
//                var node = <ClassName>{first}{second}</ClassName>
//            }
//
//            return node
//        }
//
//        /*
//
//        Inheritance
//            empty
//            extends TypeName
//            implements TypeNameList
//            extends TypeName implements TypeNameList
//
//        */
//
//        function parseInheritance()
//        {
//            enter("parseInheritance")
//
//            var node = <Inheritance/>
//
//            if( lookahead(extends_token) )
//            {
//                match(extends_token)
//                var first = parseTypeName()
//                node.Extends.* = first
//                if( lookahead(implements_token) )
//                {
//                    match(implements_token)
//                    var second = parseTypeNameList()
//                    node.Implements.* = second
//                }
//            }
//            else
//            if( lookahead(implements_token) )
//            {
//                match(implements_token)
//                var second = parseTypeNameList()
//                node.Implements.* = second
//            }
//
//            exit("parseInheritance",node)
//            return node
//        }
//
//        function parseTypeName()
//        {
//            return parseTypeIdentifier()
//        }
//
//        function parseTypeNameList()
//        {
//            var node = <></>
//            node += parseTypeIdentifier()
//            while( lookahead(comma_token) )
//            {
//                match(comma_token)
//                node += parseTypeIdentifier()
//            }
//            return node
//        }
//
//
//        function parseInterfaceDefinition(attrs,hoisted)
//        {
//            enter("parseInterfaceDefinition",attrs)
//
//            match(interface_token)
//            var name = parseClassName()
//
//            current_class = name
//            slot_context_stack.push("interface")
//
//            var inherits = parseExtendsList()
//
//            var last_default_namespace = default_namespace
//            default_namespace = <Namespace kind="interface" name={name.Identifier.@name}/>
//
//            var stmt = parseBlockStatement()
//
//            default_namespace = last_default_namespace
//
//            slot_context_stack.pop()
//
//            current_class = null
//
//            var value = <Interface>{name}{inherits}{stmt.Prologue}{stmt.Block}</Interface>
//            var node = makeBinding(attrs,interface_token,name,value,hoisted)
//
//            exit("parseInterfaceDefinition",node)
//            return node
//        }
//
//        /*
//
//        ExtendsList
//            empty
//            extends TypeNameList
//
//        */
//
//        function parseExtendsList()
//        {
//            enter("parseExtendsList")
//
//            var node = <Inheritance/>
//
//            if( lookahead(extends_token) )
//            {
//                match(extends_token)
//                var first = parseTypeNameList()
//                node.Extends.* = first
//            }
//
//            exit("parseExtendsList",node)
//            return node
//        }
//
//
//        /*
//
//        TypeExpressionList
//            TypeExpression
//            TypeExpressionList  ,  TypeExpression
//
//        */
//
//        function parseTypeExpressionList()
//        {
//            enter("parseTypeExpressionList")
//
//            var list = <></>
//            list += parseTypeExpression()
//            while( lookahead( comma_token ) )
//            {
//                match( comma_token );
//                list += parseTypeExpression()
//            }
//            var result = list
//
//            exit("parseTypeExpressionList",result)
//            return result
//        }
//
//        function parseNamespaceDefinition(attrs,prologue)
//        {
//            enter("parseNamespaceDefinition",attrs)
//
//            match(namespace_token)
//            var first = parseTypedIdentifier(allowIn_mode)
//            if( lookahead(assign_token) )
//            {
//                match(assign_token)
//                if( lookahead(stringliteral_token) )
//                {
//                    var second = <LiteralString value={scan.tokenText(match(stringliteral_token))}/>
//                }
//                else
//                {
//                    var second = <Get kind="lexical">{parseSimpleTypeIdentifier()}</Get>
//                }
//            }
//            else
//            {
//                var second = <UniqueNamespaceName/>
//            }
//
//            if( inClassBody() )
//            {
//                attrs.* += <Static/>
//            }
//
//            var node = makeBinding(attrs,namespace_token,first,second,prologue)
//
//            exit("parseNamespaceDefinition",node)
//            return node
//        }
//
//        /*
//
//        */
//
//        function parseTypeDefinition(attrs,hoisted)
//        {
//            enter("parseTypeDefinition",attrs)
//
//            match(type_token)
//            var first = parseTypedIdentifier(allowIn_mode)
//            match(assign_token)
//            var second = parseTypeExpression()
//            var node = makeBinding(attrs,type_token,first,second,hoisted)
//
//            exit("parseTypeDefinition",node)
//            return node
//        }
//
//        /*
//
//        PackageDefinition
//            PackageAttributes package PackageNameOpt Block
//
//        PackageAttributes
//            private
//            empty
//
//        PackageNameOpt
//            empty
//            PackageName
//
//        PackageName [create a lexical PackageIdentifier with the sequence of characters that make a PackageName]
//            Identifier
//            PackageName  .  Identifier
//
//        */
//
//        var current_package
//
//        function parsePackageDefinition(attr)
//        {
//            enter("parsePackageDefinition")
//
//            enterSlashContext(div_token)
//            match(package_token)
//            var name = parsePackageName()
//            exitSlashContext(div_token)
//
//            current_package = name
//            default_namespace = <Namespace kind="internal" name={name}/>
//            var stmt = parseBlockStatement()
//            var block = stmt.Block
//            var prologue = stmt.Prologue
//            current_package = null
//            stmt.@name=name
//            stmt.@kind="package"
//            var node = stmt
//
////            prologue.insertChildBefore(prologue.*[0],
//            stmt.Prologue.* +=
//                    <OpenNamespaces ident="*">
//                        <Namespace kind="public" name={name}/>
//                        <Namespace kind="internal" name={name}/>
//                    </OpenNamespaces>
//
//
//            exit("parsePackageDefinition",node)
//            return node
//        }
//
//        function parsePackageName()
//        {
//            enter("parsePackageName")
//
//            var name = ""
//            if( lookahead(leftbrace_token) )
//            {
//            }
//            else
//            {
//                name += scan.tokenText(match(identifier_token))
//                while( lookahead(dot_token) )
//                {
//                    match(dot_token)
//                    name += "."
//                    name += scan.tokenText(match(identifier_token))
//                }
//
//                scan.addPackageName(name)
//            }
//
//            exit("parsePackageName",name)
//            return name
//        }
//
        // DIRECTIVES

        /*
          Directives(tau)
              empty
              DirectivesPrefix(tau) Directives(tau,full)

        */

        function directives (ts: TOKENS, tau: TAU)
            : [TOKENS, Ast::BLOCK]
        {
            enter("Parser::directives ", ts);

            switch (hd (ts)) {
            case Token::RightBrace:
            case Token::EOS:
                var {pragmas:pragmas, defns:defns, head:head, stmts:stmts, pos:pos} 
                  = {pragmas:[], defns:[], head:null, stmts:[], pos:null};
                var [ts1] = ts;
                break;
            default:
                var [ts1,nd1] = directivesPrefix (ts,tau);
                var {pragmas:pragmas, defns:defns, head:head, stmts:stmts, pos:pos} = nd1
                break;
            }

            exit("Parser::directives ", ts1);
            return [ts1, new Ast::Block (pragmas,defns,head,stmts,pos)];
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
            : [TOKENS, Ast::DIRECTIVES]
        {
            enter("Parser::directives ", ts);

            switch (hd (ts)) {
            case Token::RightBrace:
            case Token::EOS:
                var [ts2,nd2] = [ts, {pragmas:[], defns:[], head:null, stmts:[], pos:null}];
                break;
            default:
                //                var [ts1,nd1] = pragmas (ts);
                var [ts2,nd2] = directivesPrefixPrime (ts,tau);
                break;
            }

            exit("Parser::directivesPrefix ", ts2);
            return [ts2, nd2];
        }

        function directivesPrefixPrime (ts: TOKENS, tau: TAU)
            : [TOKENS, Ast::DIRECTIVES]
        {
            enter("Parser::directivesPrefixPrime ", ts);

            var ts1,nd1;

            switch (hd (ts)) {
            case Token::RightBrace:
            case Token::EOS:
                ts1 = ts;
                nd1 = {pragmas:[],defns:[],head:null,stmts:[],pos:null};
                break;
            default:
                [ts1,nd1] = directive (ts,tau,Full);
                var [ts2,nd2] = directivesPrefixPrime (ts1,tau);

                let {pragmas:pragmas1,defns:defns1,head:head1,stmts:stmts1,pos:pos1} = nd1;
                let {pragmas:pragmas2,defns:defns2,head:head2,stmts:stmts2,pos:pos2} = nd2;

                // FIXME: poor man's array append
                for (p in pragmas2) pragmas1.push(pragmas2[p]);
                for (p in defns2) defns1.push(defns2[p]);
                for (p in stmts2) stmts1.push(stmts2[p]);
                ts1 = ts2;
                nd1 = {pragmas:pragmas1,defns:defns1,head:head1,stmts:stmts1,pos:pos1};
                break;
            }

            exit("Parser::directivesPrefixPrime ", ts1);
            return [ts1,nd1];
        }

        function directive (ts: TOKENS, tau: TAU, omega: OMEGA)
            : [TOKENS, Ast::DIRECTIVES]
        {
            enter("Parser::directive ", ts);

            switch (hd(ts)) {
            case Token::Var:
            case Token::Let:
                let [ts1,nd1]
                    = variableDefinition (ts, AllowIn
                                  , new Ast::LiteralExpr (new Ast::LiteralNamespace (new Ast::PublicNamespace ("")))
                                  , false, false);

                print("nd1: ",nd1);
                var stmtsx = nd1.stmts;
                var defnsx = nd1.defns;
                var tsx = semicolon (ts1,omega);
                break;
            case Token::Function:
                let [ts1,nd1] = functionDefinition (ts, tau, new Ast::Var
                                  , new Ast::LiteralExpr (new Ast::LiteralNamespace (new Ast::PublicNamespace ("")))
                                  , false, false, false, false, false);
                var stmtsx = nd1.stmts;
                var defnsx = nd1.defns;
                var tsx = semicolon (ts1,omega);
                break;
            default:
                var [ts2,nd2] = statement (ts,omega);
                var stmtsx = [nd2];
                var defnsx = [];
                var tsx = ts2;
                break;
            }

            print ("defnsx: ", defnsx);

            var ndx = {pragmas:[],defns:defnsx,head:null,stmts:stmtsx,pos:null};

            exit("Parser::directive ", tsx);
            return [tsx, ndx];
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
            : [TOKENS, Ast::BLOCK]
        {
            enter("Parser::block ",ts);

            ts = eat (ts, Token::LeftBrace);
            var [ts1,nd1] = directives (ts, tau);
            var tsx = eat (ts1, Token::RightBrace);
            var ndx = nd1;

            exit ("Parser::block ", tsx);
            return [tsx, ndx];
        }

        function program ()
            : [TOKENS, Ast::PROGRAM]
        {
            enter("Parser::program ","");

            let ts = scan.tokenList (scan.start)
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

            var [ts2, nd2] = directives (ts1, Global);

            switch (hd (ts2)) {
            case Token::EOS:
                break;
            default:
                throw "extra tokens after end of program: " + ts2;
            }

            exit ("Parser::program ", ts2);
            return [ts2, new Ast::Program (nd1,nd2,null)];
        }
    }

    function test ()
    {
        var programs =
            [ "print('hi')"
              /*
            , "print('hello, world!')"
            , "x<y"
            , "x==y"
            , "m-n;n+m"
            , "10"
            , "p.q.r.x"
            , "f() ()"
            , "new A()"
            , "(new Fib(n-1)).val + (new Fib(n-2)).val"
            , "var x = 10, y = 20"
            , "var x = 10; var y"
            , "if (x) y; else z"
              */
            , "function f(x,y,z) { return 10 }"
              /*
            , "class A { function A() {} }"
            , "class Fib { function Fib (n) { } }"
            , readFile ("./tests/self/hello.es")
            , readFile ("./tests/self/fib.es")
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
                var [ts1,nd1] = parser.program();

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
