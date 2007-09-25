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

//package cogen
{
    //import util.*;
    //import abcfile.*;
    //import assembler.*;
    use default namespace public;

    use default namespace Gen;
    use namespace Util;
    use namespace Abc;
    use namespace Asm;
    use namespace Emit;
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
        ctx.asm.I_pop();  // FIXME the last expr stmt of the program must save its value
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
        let L0 = asm.I_iffalse(undefined);
        cgStmt(ctx, consequent);
        if (alternate != null) {
            let L1 = asm.I_jump(undefined);
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
        let Lcont  = asm.I_jump(undefined);
        let Ltop   = asm.I_label(undefined);
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
        let Ltop   = asm.I_label(undefined);
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
        cgHead(ctx, vars);
        let Lbreak = asm.newLabel();
        let Lcont = asm.newLabel();
        if (init != null) {
            cgExpr(ctx, init);
            asm.I_pop();
        }
        let Ltop = asm.I_label(undefined);
        if (cond != null) {
            cgExpr(ctx, cond);
            asm.I_iffalse(Lbreak);
        }
        cgStmt(pushBreak(pushContinue(ctx, labels, Lcont), labels, Lbreak), stmt);
        asm.I_label(Lcont);
        if (incr != null)
        {
            cgExpr(ctx, incr);
            asm.I_pop();
        }
        asm.I_jump(Ltop);
        asm.I_label(Lbreak);
    }

    function cgBreakStmt(ctx, {ident: ident}) {
        function hit (node) {
            return node.tag == "break" && (ident == null || memberOf(ident, stk.labels))
        }
        unstructuredControlFlow(ctx,
                                hit,
                                true,
                                "Internal error: definer should have checked that all referenced labels are defined");
    }

    function cgContinueStmt(ctx, {ident: ident}) {
        function hit(node) {
             return node.tag == "continue" && (ident == null || memberOf(ident, stk.labels))
        }
        unstructuredControlFlow(ctx,
                                hit,
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
        function hit(node){
            return node.tag == "function" 
        }
        unstructuredControlFlow(ctx,
                                hit,
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
        let Lfall = null;
        let Lbreak = asm.newLabel();
        let nctx = pushBreak(ctx, labels, Lbreak);
        var hasBreak = false;
        for ( let i=0 ; i < cases.length ; i++ ) {
            let c = cases[i];

            if (c.expr == null) {
                assert (Ldefault==null);
                Ldefault = asm.I_label(undefined);    // label default pos
            }

            if (Lnext !== null) {
                asm.I_label(Lnext);          // label next pos
                Lnext = null;
            }

            if (c.expr != null) {
                cgExpr(nctx, c.expr);        // check for match
                asm.I_getlocal(t);
                asm.I_strictequals();
                Lnext = asm.I_iffalse(undefined);  // if no match jump to next label
            }

            if (Lfall !== null) {         // label fall through pos
                asm.I_label(Lfall);
                Lfall = null;
            }

            let stmts = c.stmts;
            for ( let j=0 ; j < stmts.length ; j++ ) {
                cgStmt(nctx, stmts[j] );
            }

            Lfall = asm.I_jump (undefined);         // fall through
        }
        if (Lnext !== null)
            asm.I_label(Lnext);
        if (Ldefault !== null)
            asm.I_jump(Ldefault);
        if (Lfall !== null)
            asm.I_label(Lfall);
        asm.I_label(Lbreak);
        asm.killTemp(t);
    }

    function cgSwitchTypeStmt(ctx, {expr:expr, type:type, cases:cases}) {
        let b = new Block(new Ast::Head([],[]), [new ThrowStmt(expr)]);

        cgTryStmt(ctx, {block:b, catches:cases, finallyBlock:null} );        
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
    
    function cgTryStmt(ctx, {block:block, catches:catches, finallyBlock:finallyBlock}) {
        let asm = ctx.asm;
        let code_start = asm.length;
        cgBlock(ctx, block);
        let code_end = asm.length;
        
        let Lend = asm.newLabel();
        asm.I_jump(Lend);

        for( let i = 0; i < catches.length; ++i ) {
            cgCatch(ctx, [code_start, code_end, Lend], catches[i]);
        }
        
        asm.I_label(Lend);
        
        
        //FIXME need to do finally
    }
    
    function cgCatch(ctx, [code_start, code_end, Lend], {param:param, block:block} ) {
        let {asm:asm, emitter:emitter, target:target} = ctx;
        
        if( param.fixtures.length != 1 )
            throw "Internal Error: catch should have 1 fixture";
        
        let [propname, fix] = param.fixtures[0];
        
        let param_name = emitter.fixtureNameToName(propname);
        let param_type = emitter.realTypeName(fix.type);
        
        let catch_idx = target.addException(new ABCException(code_start, code_end, asm.length, param_type, param_name));

        asm.startCatch();

        let t = asm.getTemp();
        asm.I_getlocal(0);
        asm.I_pushscope();
        restoreScopes(ctx);
        let catch_ctx = pushCatch(ctx,t);

        asm.I_newcatch(catch_idx);
        asm.I_dup();
        asm.I_setlocal(t);  // Store catch scope in register so it can be restored later
        asm.I_dup();
        asm.I_pushscope();
        
        // Store the exception object in the catch scope.
        asm.I_swap();
        asm.I_setproperty(param_name);

        // catch block body
        cgBlock(catch_ctx, block);
        
        asm.I_kill(t);
        
        asm.I_popscope();
        asm.I_jump(Lend);
    }

}
