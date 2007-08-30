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

            switch (hd (ts)) {
            case Token::SemiColon:
            case Token::RightBrace:
                var [ts1,nd1] = [ts,null];
                break;
            default:
                if (newline(ts)) {
                    var [ts1,nd1] = [ts,null];
                }
                else {
                    var [ts1,nd1] = listExpression (ts,allowIn);
                }
                break;
            }

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
            case Token::Const:
            case Token::Let:
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
            let head = new Ast::Head (f,[i]);

            exit("Parser::catchClause ", ts2);
            return [ts2,new Ast::Catch (head,nd2)];
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

}
}
