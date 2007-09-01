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
use namespace Ast;
use namespace intrinsic;
public namespace Encode;

{
    use default namespace Encode;
    
    function indent (n:int)
        : string {
        let str = "\n";

        /*
        for ( ; n > 0; n-- ) {
            str = str + " ";
        }
        */

        return str;
    }

    function program (nd : PROGRAM, nesting : int = 0)
        : string {
        enter ("Encode::program ", nesting);
        var str = "";
        switch type (nd): PROGRAM {
        case (nd: Program) {
            var str =
                indent(nesting) + "{ 'ast_class': 'Program'"
              + indent(nesting) + ", 'packages': " + packages (nd.packages,nesting+", 'packages': ".length)
              + indent(nesting) + ", 'head': " + head (nd.head,nesting+", 'head': ".length)
              + indent(nesting) + ", 'block': " + block (nd.block,nesting+", 'block': ".length)
              + " }";
        }
        case (nd: *) {
            throw "error Encode::program " + nd;
        }
        }
        exit ("Encode::program");
        return str;
    }

    function packages (packages: [PACKAGE], nesting: int = 0)
        : string {
        var str : string = "[]";
        return str;
    }

    function fixtures (nd /*: FIXTURES*/, nesting: int = 0)
        : string {
        enter ("Encode::fixtures nd=",nd);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + fixtureBinding (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode::fixtures ",str);
        return str;
    }

    function fixtureBinding (nd /*: FIXTURE_BINDING*/, nesting: int = 0)
        : string {
        enter ("Encode::fixtureBinding ",nd);

            var str =
                "[ " + fixtureName (nd[0],nesting+"[ ".length)
              + indent(nesting) + ", " + fixture (nd[1],nesting+", ".length)
                + " ]";

        exit ("Encode::fixtureBinding ",str);
        return str;
    }

    function fixtureName (nd /*: FIXTURE_NAME*/, nesting: int = 0)
        : string {
        enter ("Encode::fixtureName ",nesting);

        var str = "";
        switch type (nd): FIXTURE_NAME {
        case (nd:Ast::PropName) {

            // print ("prop ",nd.name.id);

            var str =
                "{ 'ast_class': 'PropName'"
              + indent(nesting) + ", 'name': " + name (nd.name,nesting+", 'name': ".length)
              + " }";
        }
        case (nd:Ast::TempName) {
            var str =
                "{ 'ast_class': 'TempName'"
              + indent(nesting) + ", 'index': " + nd.index
              + " }";
        }
        case (nd: *) {
            var str = "**fixtureName, unhandled ast: " + nd + "]]";
        }
        }

        exit ("Encode::fixtureName ",str);
        return str;
    }

    function fixture (nd: FIXTURE, nesting: int = 0)
        : string {
        enter ("Encode::fixture ",nesting);

        var str = "";

        switch type (nd): FIXTURE {
        case (nd:Ast::ValFixture) {
            var str =
                "{ 'ast_class': 'ValFixture'"
              + indent(nesting) 
              + ", 'type': " 
              + typeExpr (nd.type,nesting+", 'type': ".length)
              + indent(nesting) + ", 'isReadOnly': " + nd.isReadOnly
              + " }";
        }
        case (nd:Ast::MethodFixture) {
            var str =
                "{ 'ast_class': 'MethodFixture'"
              + indent(nesting) + ", 'func': " + func (nd.func,nesting+", 'func': ".length)
              + indent(nesting) + ", 'type': " + typeExpr (nd.type,nesting+", 'type': ".length)
              + indent(nesting) + ", 'isReadOnly': " + nd.isReadOnly
              + indent(nesting) + ", 'isOverride': " + nd.isOverride
              + indent(nesting) + ", 'isFinal': " + nd.isFinal
              + " }";
        }
        case (nd:Ast::ClassFixture) {
            var str =
                "{ 'ast_class': 'ClassFixture'"
              + indent(nesting) + ", 'cls': " + cls (nd.cls,nesting+", 'cls': ".length)
              + " }";
        }
        case (nd:Ast::NamespaceFixture) {
            var str =
                "{ 'ast_class': 'NamespaceFixture'"
              + indent(nesting) + ", 'ns': " + namespace (nd.ns,nesting+", 'ns': ".length)
              + " }";
        }
        case (nd:Ast::TypeFixture) {
            var str =
                "{ 'ast_class': 'TypeFixture'"
              + indent(nesting) + ", 'type': " + typeExpr (nd.type,nesting+", 'type': ".length)
              + " }";
        }
        case (nd: *) {
            var str = "**fixture, unhandled ast: " + nd + "**";
        }
        }

        exit ("Encode::fixture ",str);
        return str;
    }

    function inits (nd /*: INITS*/, nesting: int = 0)
        : string {
        enter ("Encode::inits nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str
                + initBinding (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode:inits ",str);
        return str;
    }

    function initBinding (nd /*: INIT_BINDING*/, nesting: int = 0)
        : string {
        enter ("Encode::initBinding ",nesting);

            var str =
                "[ " + fixtureName (nd[0],nesting+"[ ".length)
              + indent(nesting) + ", " + expr (nd[1],nesting+", ".length)
              + " ]";

        exit ("Encode:initBinding ",str);
        return str;
    }

    function head (nd /*: HEAD*/, nesting: int = 0)
        : string {
        enter ("Encode::head ",nesting);

        var str = "{ 'fixtures': [ " 
                + fixtures (nd.fixtures,nesting+"{ 'fixtures': [ ".length) + " ]"
                + indent(nesting) + ", 'exprs': [ " + exprs (nd.exprs,nesting+", 'exprs': [ ".length)
                + " ] }";

        exit ("Encode::head");
        return str;
    }

    function block (nd: BLOCK, nesting: int = 0)
        : string {
        enter ("Encode::block ",nesting);
        var str;
        switch type (nd) : BLOCK {
        case (nd:Block) {
            var str =
                  "{ 'ast_class': 'Block'"
                + indent(nesting) + ", 'head': " + head (nd.head,nesting+", 'head': ".length)
                + indent(nesting) + ", 'stmts': [ " + stmts (nd.Ast::stmts,nesting+", 'stmts': [ ".length)
                + " ] }";
        }
        case (x: *) {
            throw "internalError: Encode::block";
        }
        }
        exit ("Encode::block");
        return str;
    }

    function stmts (nd /*: [STMT]*/, nesting: int = 0)
        : string {
        enter ("Encode::stmts ", nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + stmt (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode::stmts ",str);
        return str;
    }

    function stmt (nd : STMT, nesting: int = 0)
        : string {
        var str = "";
        enter ("Encode::stmt");
        // print ("  stmt");
        if (nd == null) {
            var str = "null";
        }
        else {
        switch type (nd): STMT {
        case (nd: ExprStmt) {
            var str =
                "{ 'ast_class': 'ExprStmt'"
              + indent(nesting)
              + ", 'expr': "
              + expr (nd.expr,nesting+", 'expr': ".length)
              + " }";
        }
        case (nd: ReturnStmt) {
            var str =
                "{ 'ast_class': 'ReturnStmt'"
              + indent(nesting)
              + ", 'expr': "
              + exprOpt (nd.expr,nesting+", 'expr': ".length)
              + " }";
        }
        case (nd: ThrowStmt) {
            var str =
                "{ 'ast_class': 'ThrowStmt'"
              + indent(nesting)
              + ", 'expr': "
              + expr (nd.expr,nesting+", 'expr': ".length)
              + " }";
        }
        case (nd: BreakStmt) {
            var str =
                "{ 'ast_class': 'BreakStmt'"
              + indent(nesting)
              + ", 'ident': "
              + identOpt (nd.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (nd: ContinueStmt) {
            var str =
                "{ 'ast_class': 'ContinueStmt'"
              + indent(nesting)
              + ", 'ident': "
              + identOpt (nd.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (nd: IfStmt) {
            var str =
                "{ 'ast_class': 'IfStmt'"
              + indent(nesting)
              + ", 'expr': "
              + expr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'then': "
              + stmt (nd.then,nesting+", 'then': ".length)
              + indent(nesting)
              + ", 'elseOpt': "
              + stmt (nd.elseOpt,nesting+", 'elseOpt': ".length)
              + " }";
        }
        case (nd: WhileStmt) {
            var str =
                "{ 'ast_class': 'WhileStmt'"
              + indent(nesting)
              + ", 'expr': "
              + expr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'stmt': "
              + stmt (nd.stmt,nesting+", 'stmt': ".length)
              + indent(nesting)
              + ", 'labels': "
              + "[]"  // for now: stmt (nd.stmt,nesting+", 'stmt': ".length)
              + " }";
        }
        case (nd: SwitchStmt) {
            var str =
                "{ 'ast_class': 'SwitchStmt'"
              + indent(nesting)
              + ", 'expr': "
              + expr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'cases': [ "
              + cases (nd.cases,nesting+", 'cases': ".length) + " ]"
              + indent(nesting)
              + ", 'labels': "
              + "[]"  // for now: stmt (nd.stmt,nesting+", 'stmt': ".length)
              + " }";
        }
        case (nd: SwitchTypeStmt) {
            var str =
                "{ 'ast_class': 'SwitchTypeStmt'"
              + indent(nesting)
              + ", 'expr': "
              + expr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'type': "
              + typeExpr (nd.type,nesting+", 'type': ".length)
              + indent(nesting)
              + ", 'cases': [ "
              + catches (nd.cases,nesting+", 'cases': ".length) + " ]"
              + " }";
        }
        case (nd: ForStmt) {
            var str =
                "{ 'ast_class': 'ForStmt'"
              + indent(nesting)
              + ", 'vars': "
              + head (nd.vars,nesting+", 'vars': ".length)
              + indent(nesting)
              + ", 'init': "
              + exprOpt (nd.init,nesting+", 'init': ".length)
              + indent(nesting)
              + ", 'cond': "
              + exprOpt (nd.cond,nesting+", 'cond': ".length)
              + indent(nesting)
              + ", 'incr': "
              + exprOpt (nd.incr,nesting+", 'incr': ".length)
              + indent(nesting)
              + ", 'stmt': "
              + stmt (nd.stmt,nesting+", 'stmt': ".length)
              + indent(nesting)
              + ", 'labels': "
              + "[]"  // for now: stmt (nd.stmt,nesting+", 'stmt': ".length)
              + " }";
        }
        case (nd: BlockStmt) {
            var str =
                "{ 'ast_class': 'BlockStmt'"
              + indent(nesting)
              + ", 'block': "
              + block (nd.block,nesting+", 'block': ".length)
              + " }";
        }
        case (nd: ClassBlock) {
            var str =
                "{ 'ast_class': 'ClassBlock'"
              + indent(nesting) + ", 'name': " + name (nd.name,nesting+", 'name': ".length)
              + indent(nesting) + ", 'block': " + block (nd.block,nesting+", 'block': ".length)
              + " }";
        }
        case (nd: TryStmt) {
            var str =
                "{ 'ast_class': 'TryStmt'"
              + indent(nesting)
              + ", 'block': "
              + block (nd.block,nesting+", 'block': ".length)
              + indent(nesting)
              + ", 'catches': [ "
              + catches (nd.catches,nesting+", 'catches': ".length) + " ]"
              + indent(nesting)
              + ", 'finallyBlock': "
              + "null"  // for now: blockOpt (nd.finallyBlock,nesting+", 'finallyBlock': ".length)
              + " }";
        }
        case (nd: EmptyStmt) {
            var str = "{ 'ast_class': 'EmptyStmt' }";
        }
        case (x: *) {
            throw "error stmt " + nd;
        }
        }
        }
        exit ("Encode:stmt");
        return str;
    }

    function catches (nd /*: CATCHES*/, nesting: int = 0)
        : string {
        enter ("Encode::catches ", nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + catchClause (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode::catches ",str);
        return str;
    }

    function catchClause (nd : CATCH, nesting: int = 0)
        : string {
        enter ("Encode::catchClause ");

            var str =
                "{ 'ast_class': 'Catch'"
              + indent(nesting)
              + ", 'param': "
              + head (nd.param,nesting+", 'param': ".length)
              + indent (nesting)
              + ", 'block': "
              + block (nd.block,nesting+", 'block': ".length)
              + " }";

        exit ("Encode::catchClause ",str);
        return str;
    }

    function cases (nd /*: CASES*/, nesting: int = 0)
        : string {
        enter ("Encode::cases ", nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + caseElement (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode::cases ",str);
        return str;
    }

    function caseElement (nd : CASE, nesting: int = 0)
        : string {
        enter ("Encode::caseElement ");

            var str =
                "{ 'ast_class': 'Case'"
              + indent(nesting)
              + ", 'expr': "
              + exprOpt (nd.expr,nesting+", 'expr': ".length)
              + indent (nesting)
              + ", 'stmts': [ "
              + stmts (nd.stmts,nesting+", 'stmts': ".length)
              + " ] }";

        exit ("Encode::caseElement ",str);
        return str;
    }

    function exprs (nd /*: [EXPR]*/, nesting: int = 0)
        : string {
        enter ("Encode::exprs nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str
                + expr (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode::exprs ",str);
        return str;
    }

    function exprOpt (nd : EXPR?, nesting: int = 0)
        : string {
        enter ("Encode::exprOpt");
        var str = "";
        if( nd === null ) {
            var str = "null";
        }
        else {
            var str = expr (nd,nesting);
        }
        exit ("Encode::exprOpt ",str);
        return str;
    }

    function identOpt (nd : IDENT?, nesting: int = 0)
        : string {
        enter ("Encode::identOpt");
        var str = "";
        if( nd === null ) {
            var str = "null";
        }
        else {
            var str = nd;
        }
        exit ("Encode::identOpt ",str);
        return str;
    }

    function expr (nd : EXPR, nesting: int = 0)
        : string {
        enter ("Encode::expr ",nd);
        var str = "";
        switch type (nd): EXPR {
        case (nd: LiteralExpr) {
            var str =
                "{ 'ast_class': 'LiteralExpr'"
              + indent(nesting)
              + ", 'literal': "
              + literal (nd.literal,nesting+", 'literal': ".length)
              + " }";
        }
        case (nd: ListExpr) {
            var str =
                "{ 'ast_class': 'ListExpr'"
              + indent(nesting)
              + ", 'exprs': [ "
              + exprs (nd.exprs,nesting+", 'exprs': [ ".length)
              + " ] }";
        }
        case (nd: CallExpr) {
            var str =
                "{ 'ast_class': 'CallExpr'"
              + indent(nesting)
              + ", 'expr': "
              + expr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'args': [ "
              + exprs (nd.args,nesting+", 'args': [ ".length)
              + " ] }";
        }
        case (nd: NewExpr) {
            enter ("newexpr");
            var str =
                "{ 'ast_class': 'NewExpr'"
              + indent(nesting)
              + ", 'expr': "
              + expr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'args': [ "
              + exprs (nd.args,nesting+", 'args': [ ".length)
              + " ] }";
            exit ("newexpr");
        }
        case (nd: LexicalRef) {
            var str =
                "{ 'ast_class': 'LexicalRef'"
              + indent(nesting)
              + ", 'ident': "
              + identExpr (nd.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (nd: ObjectRef) {
            var str =
                "{ 'ast_class': 'ObjectRef'"
              + indent(nesting)
              + ", 'base': "
              + expr (nd.base,nesting+", 'base': ".length)
              + indent(nesting)
              + ", 'ident': "
              + identExpr (nd.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (nd: SetExpr) {
            var str =
                "{ 'ast_class': 'SetExpr'"
              + indent(nesting)
              + ", 'op': "
              + assignOp (nd.op,nesting,", 'op': ".length)
              + indent(nesting)
              + ", 'le': "
              + expr (nd.le,nesting+", 'le': ".length)
              + indent(nesting)
              + ", 're': "
              + expr (nd.re,nesting+", 're': ".length)
              + " }";
        }
        case (nd: BinaryExpr) {
            var str =
                "{ 'ast_class': 'BinaryExpr'"
              + indent(nesting)
              + ", 'op': "
              + binOp (nd.op,nesting+", 'op': ".length)
              + indent(nesting)
              + ", 'e1': "
              + expr (nd.e1,nesting+", 'e1': ".length)
              + indent(nesting)
              + ", 'e2': "
              + expr (nd.e2,nesting+", 'e2': ".length)
              + " }";
        }
        case (nd: TernaryExpr) {
            var str =
                "{ 'ast_class': 'TernaryExpr'"
              + indent(nesting)
              + ", 'e1': "
              + expr (nd.e1,nesting+", 'e1': ".length)
              + indent(nesting)
              + ", 'e2': "
              + expr (nd.e2,nesting+", 'e2': ".length)
              + indent(nesting)
              + ", 'e3': "
              + expr (nd.e3,nesting+", 'e3': ".length)
              + " }";
        }
        case (nd: BinaryTypeExpr) {
            var str =
                "{ 'ast_class': 'BinaryTypeExpr'"
              + indent(nesting)
              + ", 'op': "
              + binTyOp (nd.op,nesting,", 'op': ".length)
              + indent(nesting)
              + ", 'e1': "
              + expr (nd.e1,nesting+", 'e1': ".length)
              + indent(nesting)
              + ", 'e2': "
              + typeExpr (nd.e2,nesting+", 'e2': ".length)
              + " }";
        }
        case (nd: UnaryExpr) {
            var str =
                "{ 'ast_class': 'UnaryExpr'"
              + indent(nesting)
              + ", 'op': "
              + unOp (nd.op,nesting,", 'op': ".length)
              + indent(nesting)
              + ", 'e1': "
              + expr (nd.e1,nesting+", 'e1': ".length)
              + " }";
        }
        case (nd: InitExpr) {
            var str =
                "{ 'ast_class': 'InitExpr'"
              + indent(nesting)
              + ", 'target': "
              + initTarget (nd.target,nesting+", 'target': ".length)
              + indent(nesting)
              + ", 'head': "
              + head (nd.head,nesting+", 'head': ".length)
              + indent(nesting)
              + ", 'inits': [ "
              + inits (nd.inits,nesting+", 'inits': [ ".length)
              + " ] }";
        }
        case (nd: LetExpr) {
            var str =
                "{ 'ast_class': 'LetExpr'"
              + indent(nesting)
              + ", 'head': "
              + head (nd.head,nesting+", 'head': ".length)
              + indent(nesting)
              + ", 'expr': "
              + expr (nd.expr,nesting+", 'expr': ".length)
              + " }";
        }
        case (nd: GetTemp) {
            var str =
                "{ 'ast_class': 'GetTemp'"
              + indent(nesting)
              + ", 'n': "
              + nd.n
              + " }";
        }
        case (nd: GetParam) {
            var str =
                "{ 'ast_class': 'GetParam'"
              + indent(nesting)
              + ", 'n': "
              + nd.n
              + " }";
        }
        case (nd: ThisExpr) {
            var str = "{ 'ast_class': 'ThisExpr'" + " }";
        }
        case (x: *) {
            throw "unknown node in expr "+nd+"**";
        }
        }
        exit ("Encode:expr ",str);
        return str;
    }

    function identExpr (nd : IDENT_EXPR, nesting: int = 0)
        : string {
        enter ("Encode::identExpr");
        var str = "";
        switch type (nd): EXPR {
        case (ie: Identifier) {
            var str =
                "{ 'ast_class': 'Identifier'"
              + indent(nesting)
              + ", 'ident': "
              + "'" + ie.ident + "'"
              + indent(nesting)
              + ", 'nss': [ "
              + namespacesList (ie.nss,nesting+", 'nss': [ ".length)
              + " ] }";
        }
        case (ie: ExpressionIdentifier) {
            var str =
                "{ 'ast_class': 'ExpressionIdentifier'"
              + indent(nesting)
              + ", 'expr': "
              + expr (ie.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'nss': [ "
              + namespacesList (ie.nss,nesting+", 'nss': [ ".length)
              + " ] }";
        }
        case (ie: QualifiedIdentifier) {
            var str =
                "{ 'ast_class': 'QualifiedIdentifier'"
              + indent(nesting) + ", 'qual': " + expr (nd.qual,nesting+", 'qual':".length)
              + indent(nesting) + ", 'ident': '" + ie.ident + "'"
              + " }";
        }
        case (ie: UnresolvedPath) {
            var str =
                "{ 'ast_class': 'UnresolvedPath'"
              + indent(nesting)
              + ", 'path': [ "
              + path (ie.path,nesting+", 'path': [ ".length)
              + " ]"
              + indent(nesting)
              + ", 'ident': "
              + identExpr (ie.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (ie: ReservedNamespace) {
            var str =
                "{ 'ast_class': 'ReservedNamespace'"
              + indent(nesting)
              + ", 'ns': "
              + namespace (ie.ns,nesting+", 'ns': ".length)
              + " }";
        }
        case (x: *) {
            throw "internalError: expr: "+nd;
        }
        }
        exit ("Encode:identExpr ",str);
        return str;
    }

    function typeExprs (nd /*: [TYPE_EXPR]*/, nesting: int = 0)
        : string {
        enter ("Encode::typeExprs nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + typeExpr (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode:typeExprs ",str);
        return str;
    }

    function typeExpr (nd : TYPE_EXPR, nesting: int = 0)
        : string {
        enter ("Encode::typeExpr ",nd);
        var str = "";
        switch type (nd): TYPE_EXPR {
        case (nd: TypeName) {
            var str =
                "{ 'ast_class': 'TypeName'"
              + indent(nesting)
              + ", 'ident': "
              + identExpr (nd.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (nd: SpecialType) {
            var str =
                "{ 'ast_class': 'SpecialType'"
              + indent(nesting)
              + ", 'kind': "
                + specialTypeKind (nd.kind,nesting+", 'kind': ".length)
              + " }";
        }
        case (nd: UnionType) {
            var str =
                "{ 'ast_class': 'UnionType'"
              + indent(nesting)
              + ", 'types': [ "
              + typeExprs (nd.types,nesting+", 'types': [ ".length)
              + " ] }";
        }
        case (nd: ObjectType) {
            var str =
                "{ 'ast_class': 'ObjectType'"
              + indent(nesting)
              + ", 'fields': [ "
              + fieldTypes (nd.fields,nesting+", 'fields': [ ".length)
              + " ] }";
        }
        case (nd: ArrayType) {
            var str =
                "{ 'ast_class': 'ArrayType'"
              + indent(nesting)
              + ", 'types': [ "
              + typeExprs (nd.types,nesting+", 'types': [ ".length)
              + " ] }";
        }
        case (nd: NullableType) {
            var str =
                "{ 'ast_class': 'NullableType'"
              + indent(nesting)
              + ", 'type': "
              + typeExpr (nd.type,nesting+", 'type': ".length)
              + indent(nesting)
              + ", 'isNullable': " + nd.isNullable
              + " }";
        }
        case (nd: ElementTypeRef) {
            var str =
                "{ 'ast_class': 'ElementTypeRef'"
              + indent(nesting)
              + ", 'base': "
              + typeExpr (nd.base,nesting+", 'base': ".length)
              + indent(nesting)
              + ", 'index': " + nd.index
              + " }";
        }
        case (nd: FieldTypeRef) {
            var str =
                "{ 'ast_class': 'FieldTypeRef'"
              + indent(nesting)
              + ", 'base': "
              + typeExpr (nd.base,nesting+", 'base': ".length)
              + indent(nesting)
              + ", 'ident': " 
              + identExpr(nd.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (nd: *) {
            var str = "** unknown type in typeExpr: " + nd;
        }
        }
        exit ("Encode:typeExpr ",str);
        return str;
    }

    function specialTypeKind (nd : SPECIAL_TYPE_KIND, nesting: int = 0)
        : string {
        enter ("Encode::specialTypeKind")
        var str = "";
        switch type (nd) {
        case (op: AnyType) {
            var str = "AnyType";
        }
        case (op: NullType) {
            var str = "NullType";
        }
        case (op: UndefinedType) {
            var str = "UndefinedType";
        }
        case (op: VoidType) {
            var str = "VoidType";
        }
        case (x: *) {
            throw "internalError: literal";
        }
        }
        var str = "{ 'ast_class': '" + str + "' }"
        exit ("Encode:specialTypeKind ",str);
        return str;
    }

    function fieldTypes (nd /*: [FIELD_TYPE]*/, nesting: int = 0)
        : string {
        enter ("Encode::fieldTypes nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str
                + fieldType (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode:fieldTypes ",str);
        return str;
    }

    function fieldType (nd : FIELD_TYPE, nesting: int = 0)
    {
        enter ("Encode::encode FieldType");

        var str =
                "{ 'ast_class': 'FieldType'"
              + indent(nesting) + ", 'ident': '" + nd.ident + "'"
              + indent(nesting) + ", 'type': " + typeExpr (nd.type,nesting+", 'type': ".length)
                + " }";

        exit ("Encode:fieldType");
        return str;
    }

    function literal (nd : LITERAL, nesting: int = 0)
        : string {
        enter ("Encode::literal")
        var str = "";
        switch type (nd): LITERAL {
        case (nd: LiteralString) {

            let val = "";
            let len = nd.strValue.length;
            for (var n=0; n<len; ++n) {
                let c = nd.strValue.charAt(n);
                if (c === "\n") c = "\\n";
                else if (c === "\r") c = "\\r";
                else if (c == '\"') c = '\\"';
                else if (c == "'") c = "\\'";
                else if (c == "\\") c = "\\\\";
                val = val + c;
            }

            var str =
                "{ 'ast_class': 'LiteralString'"
              + indent(nesting)
              + ", 'strValue': \""
              + val
              + "\" }";
        }
        case (nd: LiteralDecimal) {
            var str =
                "{ 'ast_class': 'LiteralDecimal'"
              + indent(nesting)
              + ", 'decimalValue': '"
              + nd.decimalValue
              + "' }";
        }
        case (nd: LiteralNamespace) {
            var str =
                "{ 'ast_class': 'LiteralNamespace'"
              + indent(nesting)
              + ", 'namespaceValue': "
              + namespace (nd.namespaceValue,nesting+", 'namespaceValue': ".length)
              + " }";
        }
        case (nd: LiteralBoolean) {
            var str =
                "{ 'ast_class': 'LiteralBoolean'"
              + indent(nesting)
              + ", 'booleanValue': "
              + nd.booleanValue
              + " }";
        }
        case (nd: LiteralNull) {
            var str =
                "{ 'ast_class': 'LiteralNull'"
              + " }";
        }
        case (nd: LiteralUndefined) {
            var str =
                "{ 'ast_class': 'LiteralUndefined'"
              + " }";
        }
        case (nd: LiteralArray) {
            var str =
                "{ 'ast_class': 'LiteralArray'"
              + indent(nesting)
              + ", 'exprs': [ "
              + exprs (nd.exprs,nesting+", 'exprs': [ ".length) + " ]"
              + indent(nesting)
              + ", 'type': "
              + typeExpr (nd.type,nesting+"] , 'type': ".length)
              + " }";
        }
        case (nd: LiteralObject) {
            var str =
                "{ 'ast_class': 'LiteralObject'"
              + indent(nesting)
              + ", 'fields': [ "
              + literalFields (nd.fields,nesting+", 'fields': [ ".length) + " ]"
              + indent(nesting)
              + ", 'type': "
              + typeExpr (nd.type,nesting+"] , 'type': ".length)
              + " }";
        }
        case (x: *) {
            throw "internalError: literal "+nd;
        }
        }
        exit ("Encode:literal ",str);
        return str;
    }

    function literalFields (nd : LITERAL_FIELDS, nesting: int = 0)
        : string {
        enter ("Encode::literalFields nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str
                + literalField (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode::literalFields ",str);
        return str;
    }

    function literalField (nd : LITERAL_FIELD, nesting: int = 0)
        : string {
        enter ("Encode::literalField ",nesting)
        var str = "";

        var str = "{ 'ast_class': 'LiteralField'"
                + indent(nesting) + ", 'kind': " + varKind (nd.kind,nesting+", 'kind': ".length)
                + indent(nesting) + ", 'ident': " + identExpr (nd.ident,nesting+", 'ident': ".length)
                + indent(nesting) + ", 'expr': " + expr (nd.expr,nesting+", 'expr': ".length)
                + " }";

        exit ("Encode:literalField ",str);
        return str;
    }

    function varKind (nd : VAR_DEFN_TAG, nesting: int = 0)
        : string {
        enter ("Encode::varKind ",nesting)

        switch type (nd) {
        case (nd: Const) {
            var str = "{ 'ast_class': 'Const' };"
        }
        case (nd: Var) {
            var str = "{ 'ast_class': 'Var' }";
        }
        case (nd: LetConst) {
            var str = "{ 'ast_class': 'LetConst' }";
        }
        case (nd: LetVar) {
            var str = "{ 'ast_class': 'LetVar' }";
        }
	}

        exit ("Encode:literalField ",str);
        return str;
    }

    function namespacesList (nd /*: [[NAMESPACE]]*/, nesting: int = 0)
        : string {
        enter ("Encode::namespacesList nd.length=",nd.length);

        var str;
        if (nd == null) {
            var str = "null";
        }
        else
        if (nd.length == 0) {
            var str = "";
        }
        else
        {
            var str = " [ "
                + namespaces (nd[0],nesting+" [ ".length)
                + " ]"
                + indent(nesting-2)
                + ", "
                + namespacesList (nd.slice (1,nd.length), nesting);
        }

        exit ("Encode:namespacesList ",str);
        return str;
    }

    function namespaces (nd /*: [NAMESPACE]*/, nesting: int = 0)
        : string {
        enter ("Encode::namespaces nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str
                + namespace (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode:namespaces ",str);
        return str;
    }

    function namespace (nd : NAMESPACE, nesting: int = 0)
        : string {
        enter ("Encode::namespace ",nesting)
        var str = "";
        switch type (nd): NAMESPACE {
        case (nd: PublicNamespace) {
            var str =
                "{ 'ast_class': 'PublicNamespace'"
              + indent(nesting) + ", 'name': '" + nd.name
              + "' }";
        }
        case (nd: PrivateNamespace) {
            var str =
                "{ 'ast_class': 'PrivateNamespace'"
              + indent(nesting) + ", 'name': '" + nd.name
              + "' }";
        }
        case (nd: ProtectedNamespace) {
            var str =
                "{ 'ast_class': 'ProtectedNamespace'"
              + indent(nesting) + ", 'name': '" + nd.name
              + "' }";
        }
        case (nd: InternalNamespace) {
            var str =
                "{ 'ast_class': 'InternalNamespace'"
              + indent(nesting) + ", 'name': '" + nd.name
              + "' }";
        }
        case (nd: AnonymousNamespace) {
            var str =
                "{ 'ast_class': 'AnonymousNamespace'"
              + indent(nesting) + ", 'name': '" + nd.name
              + "' }";
        }
        case (nd: IntrinsicNamespace) {
            var str =
                "{ 'ast_class': 'IntrinsicNamespace'"
              + " }";
        }
        case (x: *) {
            throw "internalError: namespace "+nd;
        }
        }
        exit ("Encode:namespace ",str);
        return str;
    }

    function name (nd /*: NAME*/, nesting: int = 0)
        : string {
        enter ("Encode::name ",nesting)

        var str =
                "{ 'ns': "+namespace (nd.ns,nesting+"{ 'ns': ".length)
              + indent(nesting) + ", 'id': '"+ nd.id+"' }";

        exit ("Encode:name ",str);
        return str;
    }

    function assignOp (nd : ASSIGNOP, nesting: int = 0)
        : string {
        enter ("Encode::assignOp")
        var str = "";
        switch type (nd): ASSIGNOP {
        case (op: Assign) {
            var str = "Assign";
        }
        case (op: AssignPlus) {
            var str = "AssignPlus";
        }
        case (op: AssignMinus) {
            var str = "AssignMinus";
        }
        case (op: AssignTimes) {
            var str = "AssignTimes";
        }
        case (op: AssignDivide) {
            var str = "AssignDivide";
        }
        case (op: AssignRemainder) {
            var str = "AssignRemainder";
        }
        case (op: AssignLeftShift) {
            var str = "AssignLeftShift";
        }
        case (op: AssignRightShift) {
            var str = "AssignRightShift";
        }
        case (op: AssignRightShiftUnsigned) {
            var str = "AssignRightShiftUnsigned";
        }
        case (op: AssignBitwiseAnd) {
            var str = "AssignBitwiseAnd";
        }
        case (op: AssignBitwiseOr) {
            var str = "AssignBitwiseOr";
        }
        case (op: AssignBitwiseXor) {
            var str = "AssignBitwiseXor";
        }
        case (op: AssignLogicalAnd) {
            var str = "AssignLogicalAnd";
        }
        case (op: AssignLogicalOr) {
            var str = "AssignLogicalOr";
        }
        case (x: *) {
            throw "internalError: literal";
        }
        }
        var str = "{ 'ast_class': '" + str + "' }"
        exit ("Encode:assignOp ",str);
        return str;
    }

    function binOp (nd : BINOP, nesting: int = 0)
        : string {
        enter ("Encode::binOp")
        var str = "";
        switch type (nd): BINOP {
        case (op: Plus) {
            var str = "Plus";
        }
        case (op: Minus) {
            var str = "Minus";
        }
        case (op: Times) {
            var str = "Times";
        }
        case (op: Divide) {
            var str = "Divide";
        }
        case (op: Remainder) {
            var str = "Remainder";
        }
        case (op: LeftShift) {
            var str = "LeftShift";
        }
        case (op: RightShift) {
            var str = "RightShift";
        }
        case (op: RightShiftUnsigned) {
            var str = "RightShiftUnsigned";
        }
        case (op: BitwiseAnd) {
            var str = "BitwiseAnd";
        }
        case (op: BitwiseOr) {
            var str = "BitwiseOr";
        }
        case (op: BitwiseXor) {
            var str = "BitwiseXor";
        }
        case (op: LogicalAnd) {
            var str = "LogicalAnd";
        }
        case (op: LogicalOr) {
            var str = "LogicalOr";
        }
        case (op: InstanceOf) {
            var str = "InstanceOf";
        }
        case (op: In) {
            var str = "In";
        }
        case (op: Equal) {
            var str = "Equal";
        }
        case (op: NotEqual) {
            var str = "NotEqual";
        }
        case (op: StrictEqual) {
            var str = "StrictEqual";
        }
        case (op: StrictNotEqual) {
            var str = "StrictNotEqual";
        }
        case (op: Less) {
            var str = "Less";
        }
        case (op: LessOrEqual) {
            var str = "LessOrEqual";
        }
        case (op: Greater) {
            var str = "Greater";
        }
        case (op: GreaterOrEqual) {
            var str = "GreaterOrEqual";
        }
        case (x: *) {
            throw "internalError: literal";
        }
        }
        var str = "{ 'ast_class': '" + str + "' }"
        exit ("Encode:binOp ",str);
        return str;
    }

    function binTyOp (nd : BINTYOP, nesting: int = 0)
        : string {
        enter ("Encode::binOp")
        var str = "";
        switch type (nd): BINOP {
        case (op: CastOp) {
            var str = "CastOp";
        }
        case (op: IsOp) {
            var str = "IsOp";
        }
        case (op: ToOp) {
            var str = "ToOp";
        }
        }
        var str = "{ 'ast_class': '" + str + "' }"
        exit ("Encode:binTyOp ",str);
        return str;
    }

    function unOp (nd : UNOP, nesting: int = 0)
        : string {
        enter ("Encode::unOp")
        var str = "";
        switch type (nd): UNOP {
        case (op: Delete) {
            var str = "Delete";
        }
        case (op: Void) {
            var str = "Void";
        }
        case (op: Typeof) {
            var str = "Typeof";
        }
        case (op: PreIncr) {
            var str = "PreIncr";
        }
        case (op: PreDecr) {
            var str = "PreDecr";
        }
        case (op: PostIncr) {
            var str = "PostIncr";
        }
        case (op: PostDecr) {
            var str = "PostDecr";
        }
        case (op: UnaryPlus) {
            var str = "UnaryPlus";
        }
        case (op: UnaryMinus) {
            var str = "UnaryMinus";
        }
        case (op: BitwiseNot) {
            var str = "BitwiseNot";
        }
        case (op: LogicalNot) {
            var str = "LogicalNot";
        }
        case (op: Type) {
            var str = "Type";
        }
        case (x: *) {
            throw "internalError: unOp";
        }
        }
        var str = "{ 'ast_class': '" + str + "' }"
        exit ("Encode:unOp ",str);
        return str;
    }

    function path (nd /*: [IDENT]*/, nesting: int = 0)
        : string {
        enter ("Encode::path nd.length=",nd.length);

        var str;
        if (nd.length == 0) {
            var str = "";
        }
        else
        {
            var str =
                  nd[0]
                + indent(nesting-2)
                + ", "
                + path (nd.slice (1,nd.length), nesting);
        }
        exit ("Encode:path ",str);
        return str;
    }

    function func (nd : FUNC, nesting: int = 0)
        : string {
        enter ("Encode::func");
        print ("func ",nd.name.ident);

        var str =
            "{ 'ast_class': 'Func'"
          + indent(nesting) + ", 'name': " + funcName (nd.name,nesting+", 'name': ".length)
          + indent(nesting) + ", 'isNative': " + nd.isNative
          + indent(nesting) + ", 'block': " + block (nd.block,nesting+", 'block': ".length)
          + indent(nesting) + ", 'params': " + head (nd.params,nesting+", 'params': ".length)
          + indent(nesting) + ", 'vars': " + head (nd.vars,nesting+", 'vars': ".length)
          + indent(nesting) + ", 'defaults': [" + exprs (nd.defaults,nesting+", 'defaults': ".length) + " ]"
          + indent(nesting) + ", 'type': " + typeExpr (nd.type,nesting+", 'type': ".length)
          + " }";

        exit ("Encode:func ",str);
        return str;
    }

    function cls (nd : CLS, nesting: int = 0)
        : string {

        enter ("Encode::cls ",nd);

        print ("cls ",nd.name.id);
        var str =
            "{ 'ast_class': 'Cls'"
          + indent(nesting) + ", 'name': " + name (nd.name,nesting+", 'name': ".length)
          + indent(nesting) + ", 'baseName': " + name (nd.baseName,nesting+", 'baseName': ".length)
          + indent(nesting) + ", 'interfaceNames': " + "[]" //names (nd.interfaceNames,nesting+", 'interfaceNames': ".length)
          + indent(nesting) + ", 'constructor': " + ctor (nd.constructor,nesting+", 'constructor': ".length)
          + indent(nesting) + ", 'classHead': " + head (nd.classHead,nesting+", 'classHead': ".length)
          + indent(nesting) + ", 'instanceHead': " + head (nd.instanceHead,nesting+", 'instanceHead': ".length)
          + indent(nesting) + ", 'classType': " + typeExpr (nd.classType,nesting+", 'classType': ".length)
          + indent(nesting) + ", 'instanceType': " + typeExpr (nd.instanceType,nesting+", 'instanceType': ".length)
          + " }";

        exit ("Encode:cls ",str);
        return str;
    }

    function ctor (nd : CTOR, nesting: int = 0)
        : string {
        enter ("Encode::ctor");

        var str =
            "{ 'ast_class': 'Ctor'"
          + indent(nesting) + ", 'settings': [ " + exprs (nd.settings,nesting+", 'settings': [ ".length) + "]"
          + indent(nesting) + ", 'superArgs': [ " + exprs (nd.superArgs,nesting+", 'superArgs': ".length) + " ]" 
          + indent(nesting) + ", 'func': " + func (nd.func,nesting+", 'func': ".length)
          + " }";

        exit ("Encode:ctor ",str);
        return str;
    }

    function funcName (nd /*: FUNC_NAME*/, nesting: int = 0)
        : string {
        enter ("Encode::funcName");

        var str =
            "{ 'kind': "
          + funcNameKind (nd.kind,nesting+"{ 'kind': ".length)
          + indent(nesting)
          + ", 'ident': '" + nd.ident + "'"
          + " }";

        exit ("Encode:funcName ",str);
        return str;
    }

    function funcNameKind (nd /*: FUNC_NAME_KIND*/, nesting: int = 0)
        : string {
        enter ("Encode::funcNameKind");

        switch type (nd) {
        case (nd:Ordinary) {
            var str = "Ordinary";
        }
        case (g:Get) {
            var str = "Get";
        }
        case (s:Set) {
            var str = "Set";
        }
        case (nd: *) {
            var str = "** funcNameKind " + nd + "**";
        }
        }

        var str = "{ 'ast_class': '" + str + "' }"
        exit ("Encode:funcNameKind ",str);
        return str;
    }

    function bindings (nd /*: [BINDING]*/, nesting: int = 0)
        : string {
        enter ("Encode::bindings nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str
                + binding (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode:bindings ",str);
        return str;
    }

    function binding (nd : BINDING, nesting: int = 0)
        : string {
        enter ("Encode::binding");

        var str =
            "{ 'ast_class': 'Binding'"
          + indent(nesting)
          + ", 'ident': "
          + bindingIdent (nd.ident,nesting+", 'ident': ".length)
          + indent(nesting)
          + ", 'type': "
          + typeExpr (nd.type,nesting+", 'type': ".length)
          + " }";

        exit ("Encode:binding ",str);
        return str;
    }

    function bindingIdent (nd : BINDING_IDENT, nesting: int = 0)
        : string {
        enter ("Encode::bindingIdent");

        var str = "";

        switch type (nd) {
        case (nd:TempIdent) {
            str = "{ 'ast_class': 'TempIdent'"
                + indent(nesting)
                + ", 'n': "
                + nd.n
                + " }";
        }
        case (nd:ParamIdent) {
            str = "{ 'ast_class': 'ParamIdent'"
                + indent(nesting)
                + ", 'n': "
                + nd.n
                + " }";
        }
        case (nd:PropIdent) {
            str = "{ 'ast_class': 'PropIdent'"
                + indent(nesting)
                + ", 'ident': '"
                + nd.ident
                + "' }";
        }
        }


        exit ("Encode:bindingIdent ",str);
        return str;
    }

    function initSteps (nd /*: [INIT_STEP]*/, nesting: int = 0)
        : string {
        enter ("Encode::initSteps nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + initStep (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode:initSteps ",str);
        return str;
    }

    function initStep (nd : INIT_STEP, nesting: int = 0)
        : string {
        enter ("Encode::initStep");

        var str;
        switch type (nd) {
        case (nd:InitStep) {
        var str =
            "{ 'ast_class': 'InitStep'"
          + indent(nesting)
          + ", 'ident': "
          + bindingIdent (nd.ident,nesting+", 'ident': ".length)
          + indent(nesting)
          + ", 'expr': "
          + expr (nd.expr,nesting+", 'expr': ".length)
          + " }";
        }
        case (nd:*) {
            throw "internal error: initStep";
        }
        }

        exit ("Encode:initStep ",str);
        return str;
    }

    function initTarget (nd : INIT_TARGET, nesting: int = 0)
        : string {
        enter ("Encode::initTarget")
        var str = "";
        switch type (nd) {
        case (op: VarInit) {
            var str = "VarInit";
        }
        case (op: LetInit) {
            var str = "LetInit";
        }
        case (op: PrototypeInit) {
            var str = "PrototypeInit";
        }
        case (op: InstanceInit) {
            var str = "InstanceInit";
        }
        case (x: *) {
            throw "internalError: literal";
        }
        }
        var str = "{ 'ast_class': '" + str + "' }"
        exit ("Encode:initTarget ",str);
        return str;
    }
}
