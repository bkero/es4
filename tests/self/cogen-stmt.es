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

package cogen
{
    import util.*;
    import abcfile.*;
    import assembler.*;

    use default namespace public;
    use namespace Ast;

    function cgStmt(ctx, s) {
        switch type (s) {
        case (s:EmptyStmt) { }
        case (s:ExprStmt) { cgExprStmt(ctx, s) }
        case (s:ClassBlock) { cgClassBlock(ctx, s) }
        case (s:ForInStmt) { cgForInStmt(ctx, s) }
        case (s:ThrowStmt) { cgThrowStmt(ctx, s) }
        case (s:ReturnStmt) { cgReturnStmt(ctx, s) }
        case (s:BreakStmt) { cgBreakStmt(ctx,s) }
        case (s:ContinueStmt) { cgContinueStmt(ctx,s) }
        case (s:BlockStmt) { cgBlockStmt(ctx,s) }
        case (s:LabeledStmt) { cgLabeledStmt(ctx,s) }
        case (s:LetStmt) { cgLetStmt(ctx,s) }
        case (s:WhileStmt) { cgWhileStmt(ctx, s) }
        case (s:DoWhileStmt) { cgDoWhileStmt(ctx, s) }
        case (s:ForStmt) { cgForStmt(ctx, s) }
        case (s:IfStmt) { cgIfStmt(ctx, s) }
        case (s:WithStmt) { cgWithStmt(ctx, s) }
        case (s:TryStmt) { cgTryStmt(ctx, s) }
        case (s:SwitchStmt) { cgSwitchStmt(ctx, s) }
        case (s:SwitchTypeStmt) { cgSwitchTypeStmt(ctx, s) }
        case (s:DXNStmt) { cgDxnStmt(ctx, s) }
        }
    }

    function cgExprStmt(ctx, s) {
        cgExpr(ctx, s.expr);
        if(!(s.expr is InitExpr))
            ctx.asm.I_pop();
    }

    function cgClassBlock(ctx, s) {
        cgBlock(ctx, s.block);
    }

    function cgBlockStmt(ctx, s) {
        cgBlock(ctx, s.block);
    }

    function cgLabeledStmt(ctx, {label: label, stmt: stmt}) {
        let L0 = asm.newLabel();
        cgStmt(pushBreak(ctx, [label], L0), stmt);
        asm.I_label(L0);
    }

    function cgIfStmt(ctx, {expr:test, then:consequent, elseOpt:alternate}) {
        let asm = ctx.asm;
        cgExpr(ctx, test);
        let L0 = asm.I_iffalse();
        cgStmt(ctx, consequent);
        if (alternate != null) {
            let L1 = asm.I_jump();
            asm.I_label(L0);
            cgStmt(ctx, alternate);
            asm.I_label(L1);
        }
        else
            asm.I_label(L0);
    }

    // Probable AST bug: should be no fixtures here, you can't define
    // vars in the WHILE head.
    function cgWhileStmt(ctx, {stmt: stmt, labels: labels, expr: expr}) {
        let asm    = ctx.asm;
        let Lbreak = asm.newLabel();
        let Lcont  = asm.I_jump();
        let Ltop   = asm.I_label();
        cgStmt(pushBreak(pushContinue(ctx, labels, Lcont), labels, Lbreak), stmt);
        asm.I_label(Lcont);
        cgExpr(ctx, expr);
        asm.I_iftrue(Ltop);
        asm.I_label(Lbreak);
    }

    // Probable AST bug: should be no fixtures here, you can't define
    // vars in the DO-WHILE head.
    function cgDoWhileStmt(ctx, {stmt: stmt, labels: labels, expr: expr}) {
        let asm    = ctx.asm;
        let Lbreak = asm.newLabel();
        let Lcont  = asm.newLabel();
        let Ltop   = asm.I_label();
        cgStmt(pushBreak(pushContinue(ctx, labels, Lcont), labels, Lbreak), stmt);
        asm.I_label(Lcont);
        cgExpr(ctx, expr);
        asm.I_iftrue(Ltop);
        asm.I_label(Lbreak);
    }

    function cgForStmt(ctx, {vars:vars,init:init,cond:cond,incr:incr,stmt:stmt,labels:labels}) {
        // FIXME: fixtures
        // FIXME: code shape?
        let asm = ctx.asm;
        let Lbreak = asm.newLabel();
        let Lcont = asm.newLabel();
        if (init != null)
            cgExpr(ctx, init);
        let Ltop = asm.I_label();
        if (cond != null) {
            cgExpr(ctx, cond);
            asm.I_iffalse(Lbreak);
        }
        cgStmt(pushBreak(pushContinue(ctx, labels, Lcont), labels, Lbreak), stmt);
        asm.I_label(Lcont);
        if (incr != null)
            cgExpr(ctx, incr);
        asm.I_jump(Ltop);
        asm.I_label(Lbreak);
    }

    function cgBreakStmt(ctx, {ident: ident}) {
        unstructuredControlFlow(ctx,
                                (function (node) node.tag == "break" && (ident == null || memberOf(ident, stk.labels))),
                                true,
                                "Internal error: definer should have checked that all referenced labels are defined");
    }

    function cgContinueStmt(ctx, {ident: ident}) {
        unstructuredControlFlow(ctx,
                                (function (node) node.tag == "continue" && (ident == null || memberOf(ident, stk.labels))),
                                true,
                                "Internal error: definer should have checked that all referenced labels are defined");
    }

    function cgThrowStmt(ctx, s) {
        cgExpr(ctx, s.expr);
        ctx.asm.I_throw();
    }

    function cgReturnStmt(ctx, s) {
        let asm = ctx.asm;
        let t = null;
        if (s.expr != null) {
            cgExpr(ctx, s.expr);
            t = asm.getTemp();
            asm.I_setlocal(t);
        }
        unstructuredControlFlow(ctx,
                                (function (node) node.tag == "function"),
                                false,
                                "Internal error: definer should have checked that top-level code does not return");
        if (s.expr == null)
            asm.I_returnvoid();
        else {
            asm.I_getlocal(t);
            asm.I_returnvalue();
            asm.killTemp(t);
        }
    }

    function cgSwitchStmt(ctx, {expr:expr, cases:cases, labels:labels}) {
        let asm = ctx.asm;
        cgExpr(ctx, expr);
        let t = asm.getTemp();
        asm.I_setlocal(t);
        let Ldefault = null;
        let Lnext = null;
        let Lbreak = asm.newLabel();
        let nctx = pushBreak(ctx, labels, Lbreak);
        for ( let i=0 ; i < cases.length ; i++ ) {
            let c = cases[i];
            if (Lnext !== null) {
                asm.I_label(Lnext);
                Lnext = null;
            }
            if (c.expr == null)
                Ldefault = asm.I_label();
            else {
                cgExpr(nctx, c.expr);
                asm.I_getlocal(t);
                asm.I_strictequals();
                Lnext = asm.I_iffalse();
            }
            let stmts = c.stmts;
            for ( let j=0 ; j < stmts.length ; j++ )
                cgStmt(nctx, stmts[j] );
        }
        if (Lnext !== null)
            asm.I_label(Lnext);
        if (Ldefault !== null)
            asm.I_jump(Ldefault);
        asm.I_label(Lbreak);
        asm.killTemp(t);
    }

    function cgWithStmt(ctx, {expr:expr}) {
        let asm = ctx.asm;
        // FIXME: save the scope object in a register and record this fact in the ctx inside
        // the body, so that catch/finally handlers inside the body can restore the scope
        // stack properly.
        //
        // FIXME: record the fact that "with" is in effect so that unstructured control flow can
        // pop the scope stack.
        cgExpr(ctx, expr);
        asm.I_pushwith();
        cgStmt(ctx, body);
        asm.I_popscope();
    }

}
