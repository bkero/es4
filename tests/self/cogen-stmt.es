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
        case (s:InitStmt) { cgInitStmt(ctx, s) }
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
        ctx.asm.I_pop();
    }

    function cgInitStmt(ctx, s) {
    }

    function cgClassBlock(ctx, s) {
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
        nonlocalControlFlow(ctx, "function");
        if (s.expr == null)
            asm.I_returnvoid();
        else {
            asm.I_getlocal(t);
            asm.I_returnvalue();
            asm.killTemp(t);
        }
    }

    function cgBreakStmt({asm:asm}, {label: label}) {
        asm.I_jump(nonlocalControlFlow(ctx, "break", label));
    }

    function cgContinueStmt({asm:asm}, {label: label}) {
        asm.I_jump(nonlocalControlFlow(ctx, "continue", label));
    }

    function cgBlockStmt(ctx, s) {
        // FIXME -- more here:
        // If it has local bindings, establish a local rib
        // Inits
        // etc
        let stmts = s.stmts;
        for ( let i=0 ; i < stmts.length ; i++ )
            cgStmt(ctx, stmts[i]);
    }

    function cgLabeledStmt(ctx, {label: label, stmt: stmt}) {
        let L0 = asm.newLabel();
        cgStmt(pushBreak(ctx, [label], L0), stmt);
        asm.I_label(L0);
    }

    function cgWhileStmt(ctx, {body: body, labels: labels, expr: expr}) {
        // FIXME: fixtures
        var asm = ctx.asm;
        var L2 = asm.newLabel();
        var L0 = asm.I_jump();
        var L1 = asm.I_label();
        cgStmt(pushBreak(pushContinue(ctx, labels, L0), labels, L2), body);
        asm.I_label(L0);
        cgExpr(ctx, expr);
        asm.I_iftrue(L1);
        asm.I_label(L2);
    }

    function cgForStmt(ctx, s) {
    }

    function cgIfStmt(ctx, s) {
        var asm = ctx.asm;
        cgExpr(ctx, s.expr);
        var L0 = asm.I_iffalse();
        cgStmt(ctx, s.consequent);
        var L1 = asm.I_jump();
        asm.I_label(L0);
        cgStmt(ctx, s.alternate);
        asm.I_label(L1);
    }
}
