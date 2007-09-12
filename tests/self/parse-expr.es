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

        function primaryExpression(ts:TOKENS,beta:BETA)
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
                var [ts1,nd1] = newExpression (ts,beta,0);
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

            /// done:
            let done = false;
            while (true) {

                if (hd (ts1) === Token::BREAK) {
                    let tsx;
                    let csx;
                    [tsx,csx] = scan.tokenList (scan.div);
                    coordList = csx;
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
                    done = true;
                    break /// done;
                }
                if (done) break;

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

            /// done:
            let done = false;
            while (true) {
                switch (hd (ts1)) {
                case Token::Plus:
                    var op = Ast::plusOp;
                    break;
                case Token::Minus:
                    var op = Ast::minusOp;
                    break;
                default:
                    done = true;
                    break /// done;
                }
                if (done) break;

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
            
            let done = false;
            /// done:
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
                    done = true;
                    break /// done;
                }
                if (done) break;

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

            /// done:
            let done = false;
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
                        done = true;
                        break /// done;
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
                    done = true;
                    break /// done;
                }
                if (done) break;
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
            /// done:
            let done = false;
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
                    done = true;
                    break /// done;
                }
                if (done) break;

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
                    ts2 = eat (ts2,Token::Colon);
                    var [ts3,nd3] = nonAssignmentExpression (ts2,beta);
                    var [ts1,nd1] = [ts3, new Ast::TernaryExpr (nd1,nd2,nd3)];
                    break;
                default:
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
                    ts2 = eat (ts2,Token::Colon);
                    var [ts3,nd3] = assignmentExpression (ts2,beta);
                    var [ts1,nd1] = [ts3, new Ast::TernaryExpr (nd1,nd2,nd3)];
                    break;
                default:
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
                case (e: Ast::LexicalRef) {
                    var p = new SimplePattern (e);
                }
                case (e: Ast::ObjectRef) {
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

}
}
