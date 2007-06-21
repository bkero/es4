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
        case (e:SuperExpr) { cgSuperExpr(ctx, e) }
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
        case (e:*) { throw "Internal error: Unimplemented expression type" }
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
            case (e:Minus) { asm.I_minus() }
            case (e:Times) { asm.I_times() }
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

    function cgUnaryexpr(ctx, e) {
        let { asm: asm, emitter: emitter } = ctx;
        switch type (e.op) {
        case (op:Delete) {
            switch type (e.ex) {
            case (ex:LexicalRef) {
                let name = nameFromIdentExpr(ctx, e.ex.ident);
                ctx.asm.I_findproperty(name);
                ctx.asm.I_deleteproperty(name);
            }
            case (ex:ObjectRef) {
            }
            case (ex:*) {
                cgExpr(ctx, ex);
                asm.I_pop();
                asm.I_pushtrue();
            }
        }
        case (op:Void) {
            cgExpr(ctx, e.ex);
            asm.I_pop();
            asm.I_pushundefined();
        }
        case (op:Typeof) {
            if (e.ex is LexicalRef) {
                let name = nameFromIdentExpr(ctx, e.ex.ident);
                ctx.asm.I_findproperty(name);
                ctx.asm.I_getproperty(name);
            }
            else {
                // FIXME: What should be the behavior of typeOf on
                // existing but inaccessible properties on objects?
                cgExpr(ctx, e.ex);
                // FIXME: I_typeof is not compatible with ES4
                asm.I_typeof();
            }
        }
        case (op:PreIncr) {}
        case (op:PreDecr) {}
        case (op:PostIncr) {}
        case (op:PostDecr) {}
        case (op:UnaryPlus) {
            cgExpr(ctx, e.ex);
            asm.I_convert_d();
        }
        case (op:UnaryMinus) {
            cgExpr(ctx, e.ex);
            asm.I_negate();
        }
        case (op:BitwiseNot) {
            cgExpr(ctx, e.ex);
            asm.I_bitnot();
        }
        case (op:LogicalNot) {
            cgExpr(ctx, e.ex);
            asm.I_not();
        }
        case (op:Type) {
        }
        case (op:*) { throw "Internal error: Unimplemented unary operation" }
    }

    function cgThisExpr(ctx, e) {
        ctx.asm.I_getlocal(0);
    }

    function cgCallExpr(ctx, e) {
        cgExpr(ctx, e.func);
        ctx.asm.I_pushnull();  // receiver -- FIXME!  Just a placeholder.
        let nargs = e.args.length;
        for ( let i=0 ; i < nargs ; i++ )
            cgExpr(ctx, e.args[i]);
        ctx.asm.I_call(nargs);
    }

    function cgNewExpr(ctx, e) {
        cgExpr(ctx, e.ctor);
        for ( let i=0 ; i < e.args.length ; i++ )
            cgExpr(ctx, e.args[i]);
        ctx.asm.I_construct(args.length);
    }

    function cgLexicalRef(ctx, e) {
        let name = nameFromIdentExpr(ctx, e.ident);
        ctx.asm.I_findpropstrict(name);
        ctx.asm.I_getproperty(name);
    }

    function cgSetExpr(ctx, e) {
        let asm = ctx.asm;
        let name = null;

        // The switch leaves an object on the stack and sets "name"
        switch type (e.le) {
        case (lhs:ObjectRef) {
            cgExpr(ctx, lhs.base);
            name = nameFromIdent(ctx, lhs.ident);
        }
        case (lhs:LexicalRef) {
            name = nameFromIdentExpr(lhs);
            if (e.op is Assign)
                asm.I_findproperty(name);
            else
                asm.I_findpropstrict(name);
        }
        case (lhs:*) { throw "Internal error: illegal ref type" }
        }

        if (e.op is AssignLogicalAnd) {
            asm.I_dup();
            asm.I_getproperty(name);
            let L0 = asm.I_iffalse();
            asm.I_pop();
            cgExpr(ctx, e.re);
            asm.I_label(L0);
            asm.I_setproperty(name);  // Always store it; the effect is observable
        }
        else if (e.op is AssignLogicalOr) {
            asm.I_dup();
            asm.I_getproperty(name);
            let L0 = asm.I_iftrue();
            asm.I_pop();
            cgExpr(ctx, e.re);
            asm.I_label(L0);
            asm.I_setproperty(name);  // Always store it; the effect is observable
        }
        else {
            cgExpr(ctx, e.re);
            let t = asm.getTemp();
            if (e.op is Assign) {
                asm.I_dup();
                asm.I_setlocal(t);
                asm.I_setproperty(name);
            }
            else {
                asm.I_dup();
                asm.I_getproperty(name);
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
                asm.I_setproperty(name);
            }
            asm.I_getlocal(t);
            asm.killTemp(t);
        }
    }

    function cgLiteralExpr(ctx, e) {
        let asm = ctx.asm;
        switch type (e.literal) {
        case (e:LiteralNull) { ctx.asm.I_pushnull() }
        case (e:LiteralUndefined) { ctx.asm.I_pushundefined() }
        case (e:LiteralInt) { ctx.asm.I_pushint(ctx.cp.int32(e.intValue)) }
        case (e:LiteralUInt) { ctx.asm.I_pushuint(ctx.cp.uint32(e.uintValue)) }
        case (e:LiteralDouble) { ctx.asm.I_pushdouble(ctx.cp.float64(e.doubleValue)) }
        case (e: LiteralString) { ctx.asm.I_pushstring(ctx.cp.stringUtf8(e.strValue)) }
        case (e:LiteralBoolean) {
            if (e.booleanValue)
                ctx.asm.I_pushtrue();
            else
                ctx.asm.I_pushfalse();
        }
        case (e:LiteralArray) { cgArrayInitializer(ctx, e) }
        case (e:LiteralObject) { cgObjectInitializer(ctx, e) }
        case (e:LiteralFunction) { cgLambda(ctx, e) }
        case (e:LiteralRegExp) { cgRegExp(ctx, e) }
        case (e:*) { throw "Unimplemented LiteralExpr" }
        }
    }

    function ctListExpr(ctx, e) {
        let asm = ctx.asm;
        for ( let i=0, limit=e.exprs.length ; i < limit ; i++ ) {
            if (i != 0)
                asm.I_pop();
            cgexpr(ctx, e.exprs[i]);
        }
    }
}
