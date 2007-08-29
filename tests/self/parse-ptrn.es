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
    }
}
