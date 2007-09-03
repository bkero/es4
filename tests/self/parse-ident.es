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

use namespace intrinsic;
use namespace Release;

{
    use default namespace Parse;
    use namespace Lex;

    // Parse rountines

    {
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
                var [ts1,nd1] = [tl (ts), new Ast::InternalNamespace (currentPackageName)];
                break;
            case Token::Public:
                var [ts1,nd1] = [tl (ts), new Ast::PublicNamespace (currentPackageName)];
                break;
            case Token::Intrinsic:
                var [ts1,nd1] = [tl (ts), new Ast::IntrinsicNamespace];
                break;
            case Token::Private:
                var [ts1,nd1] = [tl (ts), new Ast::PrivateNamespace (currentClassName)];
                break;
            case Token::Protected:
                var [ts1,nd1] = [tl (ts), new Ast::ProtectedNamespace (currentClassName)];
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
                case (id:String) {
                    nd1 = new Ast::LexicalRef (new Ast::Identifier (nd1,cx.pragmas.openNamespaces))
                }
                case (ns:*) {
                    nd1 = new Ast::LiteralExpr (new Ast::LiteralNamespace (nd1));
                }
                }
                var [ts2,nd2] = qualifiedNameIdentifier (tl(ts1), nd1);
                break;
            default:
                switch type (nd1) {
                case (id:String) {
                    var [ts2,nd2] = [ts1,new Ast::Identifier (nd1,cx.pragmas.openNamespaces)];
                }
                case (ns:*) {
                    var [ts2,nd2] = [ts1,new Ast::ReservedNamespace (nd1)];
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
    }
}
