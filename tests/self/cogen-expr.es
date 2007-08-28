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

/* FIXME: handling 'super'.
 *
 * super.m(...)
 *   calls the super class's "m" with "this" as the receiver:
 *   'callsuper' instruction
 *
 * super(...)
 *   calls the super class's constructor on the arguments:
 *   'constructsuper' instruction
 *   only legal in a constructor, the parser annotates the Ctor structure with the args,
 *   don't need to handle this as an expression
 *
 * super.x
 *   picks up the 'x' member from the super class:
 *   'getsuper' instruction
 *
 * super(o).m()
 *   calls the super class's "m" with "o" as the receiver (o must be of a reasonable type):
 *   'callsuper' instruction
 */
package cogen
{
    import util.*;
    import abcfile.*;
    import assembler.*;
    import emitter.*;

    use namespace Ast;
    use default namespace public;

    function cgExpr(ctx, e) {
        switch type (e) {
        case (e:TernaryExpr) { cgTernaryExpr(ctx, e) }
        case (e:BinaryExpr) { cgBinaryExpr(ctx, e) }
        case (e:BinaryTypeExpr) { cgBinaryTypeExpr(ctx, e) }
        case (e:UnaryExpr) { cgUnaryExpr(ctx, e) }
        case (e:TypeExpr) { cgTypeExpr(ctx, e) }
        case (e:ThisExpr) { cgThisExpr(ctx, e) }
        case (e:YieldExpr) { cgYieldExpr(ctx, e) }
        case (e:SuperExpr) { throw "Internal error: SuperExpr can't appear here" }
        case (e:LiteralExpr) { cgLiteralExpr(ctx, e) }
        case (e:CallExpr) { cgCallExpr(ctx, e) }
        case (e:ApplyTypeExpr) { cgApplyTypeExpr(ctx, e) }
        case (e:LetExpr) { cgLetExpr(ctx, e) }
        case (e:NewExpr) { cgNewExpr(ctx, e) }
        case (e:ObjectRef) { cgObjectRef(ctx, e) }
        case (e:LexicalRef) { cgLexicalRef(ctx, e) }
        case (e:SetExpr) { cgSetExpr(ctx, e) }
        case (e:ListExpr) { cgListExpr(ctx, e) }
        case (e:InitExpr) { cgInitExpr(ctx, e) }
        case (e:SliceExpr) { cgSliceExpr(ctx, e) }
        case (e:GetTemp) { cgGetTempExpr(ctx, e) }
        case (e:GetParam) { cgGetParamExpr(ctx, e) }
        case (e:*) { throw ("Internal error: Unimplemented expression type " + e) }
        }
    }

    function cgTernaryExpr(ctx, { e1: test, e2: consequent, e3: alternate }) {
        let asm = ctx.asm;
        cgExpr(ctx, test);
        let L0 = asm.I_iffalse();
        cgExpr(ctx, consequent);
        let L1 = asm.I_jump();
        asm.I_label(L0);
        cgExpr(ctx, alternate);
        asm.I_label(L1);
    }

    function cgBinaryExpr(ctx, e) {
        let asm = ctx.asm;
        if (e.op is LogicalAnd) {
            cgExpr(ctx, e.e1);
            asm.I_dup();
            let L0 = asm.I_iffalse();
            asm.I_pop();
            cgExpr(ctx, e.e2);
            asm.I_label(L0);
        }
        else if (e.op is LogicalOr) {
            cgExpr(ctx, e.e2);
            asm.I_dup();
            let L0 = asm.I_iftrue();
            asm.I_pop();
            cgExpr(ctx, e.e2);
            asm.I_label(L0);
        }
        else {
            cgExpr(ctx, e.e1);
            cgExpr(ctx, e.e2);
            switch type (e.op) {
            case (e:Plus) { asm.I_add() }
            case (e:Minus) { asm.I_subtract() }
            case (e:Times) { asm.I_multiply() }
            case (e:Divide) { asm.I_divide() }
            case (e:Remainder) { asm.I_modulo() }
            case (e:LeftShift) { asm.I_lshift() }
            case (e:RightShift) { asm.I_rshift() }
            case (e:RightShiftUnsigned) { asm.I_urshift() }
            case (e:BitwiseAnd) { asm.I_bitand() }
            case (e:BitwiseOr) { asm.I_bitor() }
            case (e:BitwiseXor) { asm.I_bitxor() }
            case (e:InstanceOf) { asm.I_instanceof() }
            case (e:In) { asm.I_in() }
            case (e:Equal) { asm.I_equals() }
            case (e:NotEqual) { asm.I_equals(); asm.I_not() }
            case (e:StrictEqual) { asm.I_strictequals() }
            case (e:StrictNotEqual) { asm.I_strictequals(); asm.I_not() }
            case (e:Less) { asm.I_lessthan() }
            case (e:LessOrEqual) { asm.I_lessequals() }
            case (e:Greater) { asm.I_greaterthan() }
            case (e:GreaterOrEqual) { asm.I_greaterequals() }
            case (e:*) { throw "Internal error: Unimplemented binary operator" }
            }
        }
    }

    function cgBinaryTypeExpr(ctx, e) {
        let asm = ctx.asm;
        cgExpr(ctx, e.e1);
        cgTypeExprHelper(ctx, e.e2);
        switch type (e.op) {
        case (op:CastOp) { asm.I_coerce() }
        case (op:IsOp) { asm.I_istypelate() }
        case (op:ToOp) {
            // If the type expression object has a property meta::convert then invoke that
            // method and return its result.  Otherwise, behave as cast.
            asm.I_dup();
            asm.I_getproperty(ctx.emitter.meta_convert_name);
            asm.I_pushundefined();
            asm.I_strictequals();
            var L1 = asm.I_iftrue();
            // not undefined
            asm.I_swap();
            asm.I_callproperty(ctx.emitter.meta_convert_name, 1);
            var L2 = asm.I_jump();
            asm.I_label(L1);
            // undefined
            asm.I_coerce();
            asm.I_label(L2);
        }
        case (op:*) { throw "Internal error: Unimplemented binary type operator" }
        }
    }

    function cgTypeExpr(ctx, e) {
        cgTypeExprHelper(ctx, e.ex);
    }

    function cgTypeExprHelper(ctx, ty) {
        let asm = ctx.asm;
        switch type (ty) {
        case (ty:TypeName) {
            //let name = cgIdentExpr(ctx, ty.ident);
            asm.I_findpropstrict(cgIdentExpr(ctx, ty.ident));
            asm.I_getproperty(cgIdentExpr(ctx, ty.ident));
        }
        case (ty:*) {
            /* FIXME */
            throw "Unimplemented: type expression type";
        }
        }
    }

    function cgUnaryExpr(ctx, e) {
        let {asm:asm, emitter:emitter} = ctx;

        let incdec = function incdec(pre, inc) {
            //let name;
            switch type (e.e1) {
            case (lr:LexicalRef) {
                //name = cgIdentExpr(ctx, lr.ident);
                asm.I_findpropstrict(cgIdentExpr(ctx, lr.ident));
            }
            case (or:ObjectRef) {
                //name = cgIdentExpr(ctx, or.ident);
                cgExpr(ctx, or.base);
            }
            case (x:*) { throw "Internal error: invalid lvalue" }
            }
            asm.I_dup();
            asm.I_getproperty(cgIdentExpr(ctx, e.e1.ident));
            let t = asm.getTemp();
            if (!pre) {
                asm.I_dup();
                asm.I_setlocal(t);
            }
            if (inc)
                asm.I_increment();
            else
                asm.I_decrement();
            if (pre) {
                asm.I_dup();
                asm.I_setlocal(t);
            }
            asm.I_setproperty(cgIdentExpr(ctx, e.e1.ident));
            asm.I_getlocal(t);
            asm.killTemp(t);
        }

        switch type (e.op) {
        case (op:Delete) {
            switch type (e.e1) {
            case (lr:LexicalRef) {
                //let name = cgIdentExpr(ctx, lr.ident);
                asm.I_findproperty(cgIdentExpr(ctx, lr.ident));
                asm.I_deleteproperty(cgIdentExpr(ctx, lr.ident));
            }
            case (or:ObjectRef) {
                //let name = cgIdentExpr(ctx, or.ident);
                cgExpr(ctx, or.base);
                asm.I_deleteproperty(cgIdentExpr(ctx, or.ident));
            }
            case (e1:*) {
                cgExpr(ctx, e1);
                asm.I_pop();
                asm.I_pushtrue();
            }
            }
        }
        case (op:Void) {
            cgExpr(ctx, e.e1);
            asm.I_pop();
            asm.I_pushundefined();
        }
        case (op:Typeof) {
            if (e.e1 is LexicalRef) {
                //let name = cgIdentExpr(ctx, e.e1.ident);
                ctx.asm.I_findproperty(cgIdentExpr(ctx, e.e1.ident));
                ctx.asm.I_getproperty(cgIdentExpr(ctx, e.e1.ident));
            }
            else {
                cgExpr(ctx, e.e1);
                // I_typeof is not compatible with ES4, so do something elaborate to work around that.
                asm.I_dup();
                asm.I_pushnull();
                asm.I_strictequals();
                let L0 = asm.I_iffalse();
                asm.I_pushstring(ctx.cp.stringUtf8("null"));
                let L1 = asm.I_jump();
                asm.I_label(L0);
                asm.I_typeof();
                asm.I_label(L1);
            }
        }
        case (op:PreIncr) { incdec(true, true) }
        case (op:PreDecr) { incdec(true, false) }
        case (op:PostIncr) { incdec(false, true) }
        case (op:PostDecr) { incdec(false, false) }
        case (op:UnaryPlus) {
            cgExpr(ctx, e.e1);
            asm.I_convert_d();
        }
        case (op:UnaryMinus) {
            cgExpr(ctx, e.e1);
            asm.I_negate();
        }
        case (op:BitwiseNot) {
            cgExpr(ctx, e.e1);
            asm.I_bitnot();
        }
        case (op:LogicalNot) {
            cgExpr(ctx, e.e1);
            asm.I_not();
        }
        case (op:*) { throw "Internal error: Unimplemented unary operation" }
        }
    }

    function cgThisExpr(ctx, e) {
        ctx.asm.I_getlocal(0);
    }

    function cgYieldExpr(ctx, e) {
        // FIXME
        throw "Unimplemented 'yield' expression";
    }

    function cgCallExpr(ctx, e) {
        let {asm:asm, emitter:emitter} = ctx;
        let name = null;
        switch type (e.expr) {
        case (or:ObjectRef) {
            name = or.ident; // = cgIdentExpr(ctx, or.ident);
            cgExpr(ctx, or.base);
        }
        case (lr:LexicalRef) {
            /* FIXME (obscure): not quite right, the function could be
             * bound by a "with" rib, and if so then that rib should
             * be the receiver here, not null.  There's no way to do
             * that on Tamarin without global conventions / parallel
             * with stacks, I think.
             */
            let n = cgIdentExpr(ctx, lr.ident);
            asm.I_findpropstrict(cgIdentExpr(ctx, lr.ident));
            asm.I_getproperty(cgIdentExpr(ctx, lr.ident));
            asm.I_pushnull();
        }
        case (x:*) {
            cgExpr(ctx, e.expr);
            asm.I_pushnull();
        }
        }
        let nargs = e.args.length;
        for ( let i=0 ; i < nargs ; i++ )
            cgExpr(ctx, e.args[i]);
        if (name != null)
            asm.I_callproperty(cgIdentExpr(ctx, name),nargs);
        else
            asm.I_call(nargs);
    }

    function cgApplyTypeExpr(ctx, e) {
        // FIXME
        throw "Unimplemented type application expression";
    }

    function cgLetExpr(ctx, e) {
        cgHead(ctx, e.head);
        cgExpr(ctx, e.expr);
    }

    function cgNewExpr(ctx, e) {
        cgExpr(ctx, e.expr);
        for ( let i=0 ; i < e.args.length ; i++ )
            cgExpr(ctx, e.args[i]);
        ctx.asm.I_construct(e.args.length);
    }

    function cgObjectRef(ctx, e) {
        cgExpr(ctx, e.base);
        ctx.asm.I_getproperty(cgIdentExpr(ctx, e.ident));
    }

    function cgLexicalRef(ctx, e) {
        let asm = ctx.asm;
        //let name = cgIdentExpr(ctx, e.ident);
        asm.I_findpropstrict(cgIdentExpr(ctx, e.ident));
        asm.I_getproperty(cgIdentExpr(ctx, e.ident));
    }

    function cgSetExpr(ctx, e) {
        let {asm:asm, emitter:emitter} = ctx;
        let name = null;

        // The switch leaves an object on the stack and sets "name"
        switch type (e.le) {
        case (lhs:ObjectRef) {
            cgExpr(ctx, lhs.base);
            name = lhs.ident;
        }
        case (lhs:LexicalRef) {
            //name = cgIdentExpr(ctx, lhs.ident);
            name = lhs.ident;
            if (e.op is Assign)
                asm.I_findproperty(cgIdentExpr(ctx, lhs.ident));
            else
                asm.I_findpropstrict(cgIdentExpr(ctx, lhs.ident));
        }
        case (lhs:*) { throw "Internal error: illegal ref type" }
        }

        if (e.op is AssignLogicalAnd) {
            asm.I_dup();
            asm.I_getproperty(cgIdentExpr(ctx, name));
            let L0 = asm.I_iffalse();
            asm.I_pop();
            cgExpr(ctx, e.re);
            asm.I_label(L0);
            asm.I_setproperty(cgIdentExpr(ctx, name));  // Always store it; the effect is observable
        }
        else if (e.op is AssignLogicalOr) {
            asm.I_dup();
            asm.I_getproperty(cgIdentExpr(ctx, name));
            let L0 = asm.I_iftrue();
            asm.I_pop();
            cgExpr(ctx, e.re);
            asm.I_label(L0);
            asm.I_setproperty(cgIdentExpr(ctx, name));  // Always store it; the effect is observable
        }
        else {
            cgExpr(ctx, e.re);
            let t = asm.getTemp();
            if (e.op is Assign) {
                asm.I_dup();
                asm.I_setlocal(t);
                asm.I_setproperty(cgIdentExpr(ctx, name));
            }
            else {
                asm.I_dup();
                asm.I_getproperty(cgIdentExpr(ctx, name));
                switch type (e.op) {
                case (op:AssignPlus) { asm.I_add() }
                case (op:AssignMinus) { asm.I_subtract() }
                case (op:AssignTimes) { asm.I_multiply() }
                case (op:AssignDivide) { asm.I_divide() }
                case (op:AssignRemainder) { asm.I_modulo() }
                case (op:AssignLeftShift) { asm.I_lshift() }
                case (op:AssignRightShift) { asm.I_rshift() }
                case (op:AssignRightShiftUnsigned) { asm.I_urshift() }
                case (op:AssignBitwiseAnd) { asm.I_bitand() }
                case (op:AssignBitwiseOr) { asm.I_bitor() }
                case (op:AssignBitwiseXor) { asm.I_bitxor() }
                case (op:*) { throw "Internal error: ASSIGNOP not supported" }
                }
                asm.I_dup();
                asm.I_setlocal(t);
                asm.I_setproperty(cgIdentExpr(ctx, name));
            }
            asm.I_getlocal(t);
            asm.killTemp(t);
        }
    }

    function cgListExpr(ctx, e) {
        let asm = ctx.asm;
        for ( let i=0, limit=e.exprs.length ; i < limit ; i++ ) {
            cgExpr(ctx, e.exprs[i]);
            if (i < limit-1)
                asm.I_pop();
        }
    }

    function cgInitExpr(ctx, e) {
        let asm = ctx.asm;
        let baseOnStk = false;
//        cgHead(ctx, e.head);
        switch type (e.target) {
            case (i:InstanceInit ) {
                // Load this on the stack
                asm.I_getlocal(0);
                baseOnStk = true;
            }
        }
        cgInits(ctx, e.inits, baseOnStk);
//	asm.I_pushundefined();
    }

    function cgLiteralExpr(ctx, e) {

        function cgArrayInitializer(ctx, {exprs:exprs}) {
            let asm = ctx.asm;
            for ( let i=0 ; i < exprs.length ; i++ ) {
                cgExpr(ctx, exprs[i]);
            }
            asm.I_newarray(exprs.length);
/*            asm.I_getglobalscope();
            asm.I_getproperty(ctx.emitter.Array_name);
            asm.I_construct(0);
            asm.I_dup();
            let t = asm.getTemp();
            asm.I_setlocal(t);
            for ( let i=0 ; i < exprs.length ; i++ ) {
                if (exprs[i] !== undefined) {
                    asm.I_getlocal(t);
                    asm.I_pushuint(cg.uint32(i));
                    cgExpr(ctx, exprs[i]);
                    asm.I_setproperty(genMultinameL());
                }
            }
            asm.I_getlocal(t);
            asm.killTemp(t);
*/        }

        function cgObjectInitializer(ctx, {fields:fields}) {
            let {asm:asm, emitter:emitter} = ctx;
            asm.I_findpropstrict(ctx.emitter.Object_name);
            asm.I_constructprop(ctx.emitter.Object_name, 0);
            let t = asm.getTemp();
            asm.I_setlocal(t);
            for ( let i=0 ; i < fields.length ; i++ ) {
                let f = fields[i];
                asm.I_getlocal(t);
                cgExpr(ctx, f.expr);
                asm.I_setproperty(cgIdentExpr(ctx, f.ident));
            }
            asm.I_getlocal(t);
            asm.killTemp(t);
        }

        function cgRegExpLiteral({asm:asm, cp:cp}, {src:src}) {
            // src is "/.../flags"
            // Slow...
            let p = src.lastIndexOf('/');
            asm.I_getglobalscope();
            asm.I_getproperty(ctx.emitter.RegExp_name);
            asm.I_pushstring(cp.stringUtf8(src.substring(1,p)));
            asm.I_pushstring(cp.stringUtf8(src.substring(p+1)));
            asm.I_construct(2);
        }

        let asm = ctx.asm;
        switch type (e.literal) {
        case (e:LiteralNull) { asm.I_pushnull() }
        case (e:LiteralUndefined) { asm.I_pushundefined() }
        case (e:LiteralInt) { asm.I_pushint(ctx.cp.int32(e.intValue)) }
        case (e:LiteralUInt) { asm.I_pushuint(ctx.cp.uint32(e.uintValue)) }
        case (e:LiteralDouble) { asm.I_pushdouble(ctx.cp.float64(e.doubleValue)) }
        case (e:LiteralDecimal) { asm.I_pushdouble(ctx.cp.float64(Number(e.decimalValue))) } // FIXME - the AVM2 can't handle decimal yet
        case (e:LiteralString) { asm.I_pushstring(ctx.cp.stringUtf8(e.strValue)) }
        case (e:LiteralBoolean) {
            if (e.booleanValue)
                asm.I_pushtrue();
            else
                asm.I_pushfalse();
        }
        case (e:LiteralFunction) { asm.I_newfunction(cgFunc(ctx, e.func)) }
        case (e:LiteralArray) { cgArrayInitializer(ctx, e) }
        case (e:LiteralObject) { cgObjectInitializer(ctx, e) }
        case (e:LiteralRegExp) { cgRegExpLiteral(ctx, e) }
        case (e:*) { throw "Unimplemented LiteralExpr" }
        }
    }

    function cgSliceExpr(ctx, e) {
        // FIXME
        throw "Unimplemented slice expression";
    }

    function cgGetTempExpr(ctx, e) {
        // FIXME
        let{asm:asm, emitter:emitter} = ctx;
        var qn = emitter.qname ({ns:Ast::noNS,id:"$t"+e.n});
        asm.I_findpropstrict(qn);
        asm.I_getproperty(qn);
    }

    function cgGetParamExpr(ctx, e) {
        let asm = ctx.asm;
        asm.I_getlocal(e.n + 1);  //account for 'this'
    }
    
    function cgIdentExpr(ctx, e) {
        let{asm:asm, emitter:emitter} = ctx;
        switch type(e) {
            case (id:Identifier) {
                return emitter.multiname(id);
            }
            case (ei:ExpressionIdentifier) {
                cgExpr(ctx, ei.expr);
                return emitter.multinameL(ei);
            }
            case (qi:QualifiedIdentifier) {
                cgExpr(ctx, qi.qual);
                return emitter.rtqname(qi);
            }
            case (x:*) { throw ("Unimplemented cgIdentExpr " + e) }
        }
    }
}
