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

}
}
