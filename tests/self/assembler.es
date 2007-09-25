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

namespace Asm;

//package assembler
{
    use default namespace Asm;
    use namespace Util;
    //use namespace Abc;
    
    //import util.*;
    //import bytestream.*;
    //import abcfile.*;

    /*******************************************************************
     * ABC constants
     */

    public const CONSTANT_Utf8               = 0x01;
    public const CONSTANT_Integer            = 0x03;
    public const CONSTANT_UInt               = 0x04;
    public const CONSTANT_PrivateNamespace   = 0x05;
    public const CONSTANT_Double             = 0x06;
    public const CONSTANT_QName              = 0x07; // ns::name, const ns, const name
    public const CONSTANT_Namespace          = 0x08;
    public const CONSTANT_Multiname          = 0x09; // [ns...]::name, const [ns...], const name
    public const CONSTANT_False              = 0x0A;
    public const CONSTANT_True               = 0x0B;
    public const CONSTANT_Null               = 0x0C;
    public const CONSTANT_QNameA             = 0x0D; // @ns::name, const ns, const name
    public const CONSTANT_MultinameA         = 0x0E; // @[ns...]::name, const [ns...], const name
    public const CONSTANT_RTQName            = 0x0F; // ns::name, var ns, const name
    public const CONSTANT_RTQNameA           = 0x10; // @ns::name, var ns, const name
    public const CONSTANT_RTQNameL           = 0x11; // ns::[name], var ns, var name
    public const CONSTANT_RTQNameLA          = 0x12; // @ns::[name], var ns, var name
    public const CONSTANT_NameL              = 0x13; // o[name], var name
    public const CONSTANT_NameLA             = 0x14; // @[name], var name
    public const CONSTANT_NamespaceSet       = 0x15;
    public const CONSTANT_PackageNamespace   = 0x16; // namespace for a package
    public const CONSTANT_PackageInternalNS  = 0x17;
    public const CONSTANT_ProtectedNamespace = 0x18;
    public const CONSTANT_ExplicitNamespace  = 0x19;
    public const CONSTANT_StaticProtectedNS  = 0x1A;
    public const CONSTANT_MultinameL         = 0x1B;
    public const CONSTANT_MultinameLA        = 0x1C;

    public const CONSTANT_ClassSealed        = 0x01;
    public const CONSTANT_ClassFinal         = 0x02;
    public const CONSTANT_ClassInterface     = 0x04;
    public const CONSTANT_ClassProtectedNs   = 0x08;

    public const TRAIT_Slot                  = 0;
    public const TRAIT_Method                = 1;
    public const TRAIT_Getter                = 2;
    public const TRAIT_Setter                = 3;
    public const TRAIT_Class                 = 4;
    public const TRAIT_Function              = 5;
    public const TRAIT_Const                 = 6;

    public const ATTR_Final                  = 0x01;
    public const ATTR_Override               = 0x02;
    public const ATTR_Metadata               = 0x04;

    public const SLOT_var                    = 0;
    public const SLOT_method                 = 1;
    public const SLOT_getter                 = 2;
    public const SLOT_setter                 = 3;
    public const SLOT_class                  = 4;
    public const SLOT_function               = 6;

    public const METHOD_Arguments            = 0x1;
    public const METHOD_Activation           = 0x2;
    public const METHOD_Needrest             = 0x4;
    public const METHOD_HasOptional          = 0x8;
    public const METHOD_IgnoreRest           = 0x10;
    public const METHOD_Native               = 0x20;
    public const METHOD_Setsdxns             = 0x40;
    public const METHOD_HasParamNames        = 0x80;


    /*********************************************************************************
     * AVM2 assembler for one code block.
     *
     * This is a lightweight class that is used to emit bytes for
     * instructions and data, to maintain stack and scope depths,
     * count local slots used, and to handle branch targets and
     * backpatching.  It has no code generation logic save for fairly
     * simple abstractions (eg, I_getlocal() maps to "getlocal_n" or
     * to the general "getlocal" instruction, depending on its
     * parameter value).
     *
     * FIXME:
     *  - There needs to be a way to set the scope stack depth to 0, to be used
     *    when generating code for exception handling
     *  - It would be nice if we could check that every join point has the same
     *    stack depth, this requires that the next linear instruction following
     *    an unconditional nonreturning control flow (return, throw, jump) is
     *    a label always, or is ignored for the purposes of computing the stack
     *    depth.
     *  - Ditto for the scope depth, really.
     */

    public class AVM2Assembler
    {
        const listify = false;
        const indent = "        ";

        function AVM2Assembler(constants, numberOfFormals, initScopeDepth) {
            this.constants = constants;
            this.nextTemp = numberOfFormals+1; // local 0 is always "this"
            this.current_scope_depth = initScopeDepth;
        }

        public function get maxStack() { return max_stack_depth }
        public function get maxLocal() { return nextTemp }
        public function get maxScope() { return max_scope_depth }
        public function get flags() { return (set_dxns ? METHOD_Setsdxns : 0) | (need_activation ? METHOD_Activation : 0) }

        /*private*/ function listL(n) {
            if (listify)
                print(n);
        }

        /*private*/ function list1(name) {
            if (listify)
                print(indent + name);
        }

        /*private*/ function list2(name, v) {
            if (listify)
                print(indent + name + " " + v);
        }

        /*private*/ function list3(name, v1, v2) {
            if (listify)
                print(indent + name + " " + v1 + " " + v2);
        }

        /*private*/ function list5(name, v1, v2, v3, v4) {
            if (listify)
                print(indent + name + " " + v1 + " " + v2 + " " + v3 + " " + v4);
        }
/*         function listn(name, ...rest) {
            if (listify)
                print(indent + name + " " + rest.join(" "));
        }
*/

        // Instructions that push one value, with a single opcode byte
        /*private*/ function pushOne(name, opcode) {
            stack(1);
            list1(name);
            code.uint8(opcode);
        }

        public function I_dup() { pushOne("dup", 0x2A) }
        public function I_getglobalscope() { pushOne("getglobalscope", 0x64) }
        public function I_getlocal_0() { pushOne("getlocal_0", 0xD0) }
        public function I_getlocal_1() { pushOne("getlocal_1", 0xD1) }
        public function I_getlocal_2() { pushOne("getlocal_2", 0xD2) }
        public function I_getlocal_3() { pushOne("getlocal_3", 0xD3) }
        public function I_newactivation() { need_activation=true; pushOne("newactivation", 0x57) }
        public function I_pushfalse() { pushOne("pushfalse", 0x27) }
        public function I_pushnan() { pushOne("pushnan", 0x28) }
        public function I_pushnull() { pushOne("pushnull", 0x20) }
        public function I_pushtrue() { pushOne("pushtrue", 0x26) }
        public function I_pushundefined() { pushOne("pushundefined", 0x21) }

        // Instructions that push one value, with an opcode byte followed by a u30 argument
        /*private*/ function pushOneU30(name, opcode, v) {
            stack(1);
            list2(name, v);
            code.uint8(opcode);
            code.uint30(v);
        }

        public function I_getglobalslot(index) { pushOneU30("getglobalslot", 0x6E, index) }
        public function I_getlex(index) { pushOneU30("getlex", 0x60, index) }
        public function I_getscopeobject(index) { pushOneU30("getscopeobject", 0x65, index) }
        public function I_newcatch(index) { pushOneU30("newcatch", 0x5A, index) }
        public function I_newfunction(index) { pushOneU30("newfunction", 0x40, index) }
        public function I_pushdouble(index) { pushOneU30("pushdouble", 0x2F, index) }
        public function I_pushint(index) { pushOneU30("pushint", 0x2D, index) }
        public function I_pushnamespace(index) { pushOneU30("pushnamespace", 0x31, index) }
        public function I_pushshort(v) { pushOneU30("pushshort", 0x25, v) }
        public function I_pushstring(index) { pushOneU30("pushstring", 0x2C, index) }
        public function I_pushuint(index) { pushOneU30("pushuint", 0x2E, index) }

        // start a catch block.  increments stack by 1 for the exception object
        public function startCatch() { stack(1) }
        
        // Instructions that pop one value, with a single opcode byte
        /*private*/ function dropOne(name, opcode) {
            stack(-1);
            list1(name);
            code.uint8(opcode);
        }

        public function I_add() { dropOne("add", 0xA0) }
        public function I_add_i() { dropOne("add_i", 0xC5) }
        public function I_astypelate() { dropOne("astypelate", 0x87) }
        public function I_bitand() { dropOne("bitand", 0xA8) }
        public function I_bitor() { dropOne("bitor", 0xA9) }
        public function I_bitxor() { dropOne("bitxor", 0xAA) }
        public function I_divide() { dropOne("divide", 0xA3) }
        public function I_dxnslate() { set_dxns=true; dropOne("dxnslate", 0x07) }
        public function I_equals() { dropOne("Equals", 0xAB) }
        public function I_greaterequals() { dropOne("greaterequals", 0xB0) }
        public function I_greaterthan() { dropOne("greaterthan", 0xAF) }
        public function I_hasnext() { dropOne("hasnext", 0x1F) }
        public function I_in() { dropOne("in", 0xB4) }
        public function I_instanceof() { dropOne("instanceof", 0xB1) }
        public function I_istypelate() { dropOne("istypelate", 0xB3) }
        public function I_lessequals() { dropOne("lessequals", 0xAE) }
        public function I_lessthan() { dropOne("lessthan", 0xAD) }
        public function I_lshift() { dropOne("lshift", 0xA5) }
        public function I_modulo() { dropOne("modulo", 0xA4) }
        public function I_multiply() { dropOne("multiply", 0xA2) }
        public function I_multiply_i() { dropOne("multiply_i", 0xC7) }
        public function I_nextname() { dropOne("nextname", 0x1E) }
        public function I_nextvalue() { dropOne("nextvalue", 0x23) }
        public function I_pop() { dropOne("pop", 0x29) }
        public function I_pushscope() { scope(1); dropOne("pushscope", 0x30) }
        public function I_pushwith() { scope(1); dropOne("pushwith", 0x1C) }
        public function I_returnvalue() { dropOne("returnvalue", 0x48) }
        public function I_rshift() { dropOne("rshift", 0xA6) }
        public function I_setlocal_0() { dropOne("setlocal_0", 0xD4) }
        public function I_setlocal_1() { dropOne("setlocal_1", 0xD5) }
        public function I_setlocal_2() { dropOne("setlocal_2", 0xD6) }
        public function I_setlocal_3() { dropOne("setlocal_3", 0xD7) }
        public function I_strictequals() { dropOne("strictequals", 0xAC) }
        public function I_subtract() { dropOne("subtract", 0xA1) }
        public function I_subtract_i() { dropOne("subtract_i", 0xC6) }
        public function I_throw() { dropOne("throw", 0x03) }
        public function I_urshift() { dropOne("urshift", 0xA7) }

        // Instructions that pop one value, with an opcode byte followed by an u30 argument
        /*private*/ function dropOneU30(name, opcode, v) {
            stack(-1);
            list2(name, v);
            code.uint8(opcode);
            code.uint30(v);
        }

        public function I_setglobalslot(index) { dropOneU30("setglobalslot", 0x6F, index) }

        // Instructions that do not change the stack height, with a single opcode byte
        /*private*/ function dropNone(name, opcode)
        {
            //stack(0);
            list1(name);
            code.uint8(opcode);
        }

        public function I_bitnot() { dropNone("bitnot", 0x97) }
        public function I_checkfilter() { dropNone("checkfilter", 0x78) }
        public function I_coerce_a() { dropNone("coerce_a", 0x82) }
        public function I_coerce_s() { dropNone("coerce_s", 0x85) }
        public function I_convert_b() { dropNone("convert_b", 0x76) }
        public function I_convert_d() { dropNone("convert_d", 0x75) }
        public function I_convert_i() { dropNone("convert_i", 0x73) }
        public function I_convert_o() { dropNone("convert_o", 0x77) }
        public function I_convert_s() { dropNone("convert_s", 0x70) }
        public function I_convert_u() { dropNone("convert_u", 0x74) }
        public function I_decrement() { dropNone("decrement", 0x93) }
        public function I_decrement_i() { dropNone("decrement_i", 0xC1) }
        public function I_esc_xattr() { dropNone("esc_xattr", 0x72) }
        public function I_esc_xelem() { dropNone("esc_xattr", 0x71) }
        public function I_increment() { dropNone("increment", 0x91) }
        public function I_increment_i() { dropNone("increment_i", 0xC0) }
        public function I_negate() { dropNone("negate", 0x90) }
        public function I_negate_i() { dropNone("negate_i", 0xC4) }
        public function I_nop() { dropNone("nop", 0x02) }
        public function I_not() { dropNone("not", 0x96) }
        public function I_popscope() { scope(-1); dropNone("popscope", 0x1D) }
        public function I_returnvoid() { dropNone("returnvoid", 0x47) }
        public function I_swap() { dropNone("swap", 0x2B) }
        public function I_typeof() { dropNone("typeof", 0x95) }

        // Instructions that do not change the stack height, with an opcode byte
        // followed by a u30 argument
        /*private*/ function dropNoneU30(name, opcode, x) {
            //stack(0)
            list2(name, x);
            code.uint8(opcode);
            code.uint30(x);
        }

        public function I_astype(index) { dropNoneU30("astype", 0x86, index) }
        public function I_coerce(index) { dropNoneU30("coerce", 0x80, index) }
        public function I_debugfile(index) { dropNoneU30("debugfile", 0xF1, index) }
        public function I_debugline(linenum) { dropNoneU30("debugline", 0xF0, linenum) }
        public function I_declocal(reg) { dropNoneU30("declocal", 0x94, reg) }
        public function I_declocal_i(reg) { dropNoneU30("declocal_i", 0xC3, reg) }
        public function I_dxns(index) { set_dxns=true; dropNoneU30("dxns", 0x06, index) }
        public function I_getslot(index) { dropNoneU30("getslot", 0x6C, index) }
        public function I_inclocal(reg) { dropNoneU30("inclocal", 0x92, reg) }
        public function I_inclocal_i(reg) { dropNoneU30("inclocal_i", 0xC2, reg) }
        public function I_istype(index) { dropNoneU30("istype", 0xB2, index) }
        public function I_kill(index) { dropNoneU30("kill", 0x08, index) }
        public function I_newclass(index) { dropNoneU30("newclass", 0x58, index) }

        public function I_getlocal(index) {
            switch (index) {
            case 0: I_getlocal_0(); break;
            case 1: I_getlocal_1(); break;
            case 2: I_getlocal_2(); break;
            case 3: I_getlocal_3(); break;
            default: pushOneU30("getlocal", 0x62, index);
            }
        }

        public function I_setlocal(index) {
            switch (index) {
            case 0: I_setlocal_0(); break;
            case 1: I_setlocal_1(); break;
            case 2: I_setlocal_2(); break;
            case 3: I_setlocal_3(); break;
            default: dropOneU30("setlocal", 0x63, index);
            }
        }

        // Local control flow instructions and I_label():
        //  - If called without an argument return a "label" that can later be
        //    passed to I_label() to give the label an actual value.
        //  - If called with an argument, the argument must have been returned
        //    from a control flow instruction or from I_label().  It represents
        //    a transfer target.
        //
        // A "label" is a data structure with these fields:
        //  - name (uint): a symbolic name for the label, to be used in listings
        //  - address (int): either -1 for "unknown" or the address of the label
        //  - stack (uint): the stack depth at label creation time; this is the
        //        stack depth at the target too [except for exception handling]
        //  - scope (uint): the scope stack depth at label creation time; this is the
        //        scope stack depth at the target too [except for exception handling]
        //
        // The method newLabel() can be called to return a label that
        // will later be defined by I_label and referenced by control
        // flow instructions, without creating a jump instruction at
        // the point where the label is created.  Typically this is
        // used to create branch targets for "break" and "continue".

        public function newLabel() {
            return { "name": nextLabel++, "address": -1, "stack": current_stack_depth, "scope": current_scope_depth };
        }

        /*private*/ function relativeOffset(base, L) {
            if (L.address != -1)
                code.int24(L.address - base);
            else {
                backpatches.push({ "loc": code.length, "base": base, "label": L });
                code.int24(0);
            }
        }

        /*private*/ function jmp(stk, name, opcode, L) {
            stack(stk);

            if (L === undefined)
                L = newLabel();

            list2(name, L.name);
            code.uint8(opcode);
            relativeOffset(code.length+3, L);

            return L;
        }

        public function I_label(L) {
            var here = code.length;
            var define = false;
            if (L === undefined) {
                define = true;
                L = newLabel();
            }
            else {
                assert( L.address == -1 );
                current_stack_depth = L.stack;
                current_scope_depth = L.scope;
            }
            L.address = here;
            listL(L.name + ":   -- " + L.stack + "/" + L.scope);
            if (define) {
                code.uint8(0x09);
                list1("label");
            }
            return L;
        }

        public function I_ifeq(L) { return jmp(-2, "ifeq", 0x13, L) }
        public function I_ifge(L) { return jmp(-2, "ifge", 0x18, L) }
        public function I_ifgt(L) { return jmp(-2, "ifgt", 0x17, L) }
        public function I_ifle(L) { return jmp(-2, "ifle", 0x16, L) }
        public function I_iflt(L) { return jmp(-2, "iflt", 0x15, L) }
        public function I_ifne(L) { return jmp(-2, "ifne", 0x14, L) }
        public function I_ifnge(L) { return jmp(-2, "ifnge", 0x0F, L) }
        public function I_ifngt(L) { return jmp(-2, "ifngt", 0x0E, L) }
        public function I_ifnle(L) { return jmp(-2, "ifnle", 0x0D, L) }
        public function I_ifnlt(L) { return jmp(-2, "ifnlt", 0x0C, L) }
        public function I_ifstricteq(L) { return jmp(-2, "ifstricteq", 0x19, L) }
        public function I_ifstrictne(L) { return jmp(-2, "ifstrictne", 0x1A, L) }

        public function I_iffalse(L) { return jmp(-1, "iffalse", 0x12, L) }
        public function I_iftrue(L) { return jmp(-1, "iftrue", 0x11, L) }

        public function I_jump(L) { return jmp(0, "jump", 0x10, L) }

        // Here, case_labels must be an array with a "length" property
        // that denotes the number of case labels in the array.
        // length cannot be 0.
        //
        // Either default_label is undefined and all the elements of
        // case_labels are also undefined, or default_label is a label
        // structure, and all the elements of case_labels between 0
        // and length-1 are label structures as well.
        //
        // In the former case, labels are created for the
        // default_label and for all the case_labels; the array is
        // updated; and the new default_label is returned.

        public function I_lookupswitch(default_label, case_labels) {
            assert( case_labels.push ); /*FIXME ES4: really "case_labels is Array" */
            assert( case_labels.length > 0 );

            stack(-1);

            if (default_label === undefined) {
                default_label = newLabel();
                for ( var i=0 ; i < case_labels.length ; i++ ) {
                    assert( case_labels[i] === undefined );
                    case_labels[i] = newLabel();
                }
            }

            function map_func(L) { return L.name };
            list3("lookupswitch", default_label.name, map(map_func, case_labels));
            var base = code.length;
            code.uint8(0x1B);
            relativeOffset(base, default_label);
            code.uint30(case_labels.length-1);
            for ( var i=0 ; i < case_labels.length ; i++ )
                relativeOffset(base, case_labels[i]);

            return default_label;
        }

        // Standard function calls
        /*private*/ function call(name, opcode, nargs) {
            stack(1-(nargs+2)); /* pop function/receiver/args; push result */
            list2(name, nargs);
            code.uint8(opcode);
            code.uint30(nargs);
        }

        /*private*/ function construct(name, opcode, nargs) {
            stack(1-(nargs+1)); /* pop function/receiver/args; push result */
            list2(name, nargs);
            code.uint8(opcode);
            code.uint30(nargs);
        }

        public function I_call(nargs) { call("call", 0x41, nargs) }
        public function I_construct(nargs) { construct("construct", 0x42, nargs) }

        public function I_constructsuper(nargs) {
            stack(nargs+1); /* pop receiver/args */
            list2("constructsuper", nargs);
            code.uint8(0x49);
            code.uint30(nargs);
        }

        /*private*/ function callIDX(name, opcode, index, nargs) {
            stack(1-(nargs+1)); /* pop receiver/args; push result */
            list3(name, index, nargs);
            code.uint8(opcode);
            code.uint30(index);
            code.uint30(nargs);
        }

        public function I_callmethod(index, nargs) { callIDX("callmethod", 0x43, index, nargs) }
        public function I_callstatic(index, nargs) { callIDX("callstatic", 0x44, index, nargs) }

        /*private*/ function callMN(name, opcode, index, nargs, isVoid) {
            /* pop receiver/NS?/Name?/args; push result? */
            var hasRTNS = constants.hasRTNS(index);
            var hasRTName = constants.hasRTName(index);
            stack((isVoid ? 0 : 1) - (1 + (hasRTNS ? 1 : 0) + (hasRTName ? 1 : 0) + nargs));
            list3(name + (hasRTNS ? "<NS>" : "") + (hasRTName ? "<Name>" : ""), index, nargs);
            code.uint8(opcode);
            code.uint30(index);
            code.uint30(nargs);
        }

        public function I_callsuper(index, nargs) { callMN("callsuper", 0x45, index, nargs, false) }
        public function I_callproperty(index, nargs) { callMN("callproperty", 0x46, index, nargs, false) }
        public function I_constructprop(index, nargs) { callMN("constructprop", 0x4A, index, nargs, false) }
        public function I_callproplex(index, nargs) { callMN("callproplex", 0x4C, index, nargs, false) }
        public function I_callsupervoid(index, nargs) { callMN("callsupervoid", 0x4E, index, nargs, true) }
        public function I_callpropvoid(index, nargs) { callMN("callpropvoid", 0x4F, index, nargs, true) }

        public function I_debug(debug_type, index, reg, extra=0) {
            //stack(0);
            list5("debug", debug_type, index, reg, extra);
            code.uint8(0xEF);
            code.uint8(debug_type);
            code.uint30(index);
            code.uint8(reg);
            code.uint30(extra);
        }

        /* Generic property operation when there may be a namespace or
           name on the stack.  The instruction pops and pushes some
           fixed amount and may pop one or two more items, depending
           on the kind of name that index references.
        */
        /*private*/ function propU30(name, pops, pushes, opcode, index) {
            var hasRTNS = constants.hasRTNS(index);
            var hasRTName = constants.hasRTName(index);
            stack(pushes - (pops + (hasRTNS ? 1 : 0) + (hasRTName ? 1 : 0)));
            list2(name + (hasRTNS ? "<NS>" : "") + (hasRTName ? "<Name>" : ""), index);
            code.uint8(opcode);
            code.uint30(index);
        }

        public function I_deleteproperty(index) { propU30("deleteproperty", 1, 1, 0x6A, index) }
        public function I_getdescendants(index) { propU30("getdescendants", 1, 1, 0x59, index) }
        public function I_getproperty(index) { propU30("getproperty", 1, 1, 0x66, index); }
        public function I_getsuper(index) { propU30("getsuper", 1, 1, 0x04, index); }
        public function I_findproperty(index) { propU30("findproperty", 0, 1, 0x5E, index) }
        public function I_findpropstrict(index) { propU30("findpropstrict", 0, 1, 0x5D, index) }
        public function I_initproperty(index) { propU30("initproperty", 2, 0, 0x68, index) }
        public function I_setproperty(index) { propU30("setproperty", 2, 0, 0x61, index) }
        public function I_setsuper(index) { propU30("setsuper", 2, 0, 0x05, index) }

        public function I_hasnext2(object_reg, index_reg) {
            stack(1);
            code.uint8(0x32);
            code.uint30(object_reg);
            code.uint30(index_reg);
        }

        public function I_newarray(nargs) {
            stack(1 - nargs);
            list2("newarray", nargs);
            code.uint8(0x56);
            code.uint30(nargs);
        }

        public function I_newobject(nargs) {
            stack(1 - (2 * nargs));
            list2("newobject", nargs);
            code.uint8(0x55);
            code.uint30(nargs);
        }

        public function I_pushbyte(b) {
            stack(1);
            list2("pushbyte", b);
            code.uint8(0x24);
            code.uint8(b);
        }

        public function I_setslot(index) {
            stack(-2);
            list2("setslot", index);
            code.uint8(0x6D);
            code.uint30(index);
        }

        public function getTemp() {
            if (freeTemps.length > 0)
                return freeTemps.pop();
            else
                return nextTemp++;
        }

        public function killTemp(t) {
            freeTemps.push(t);
            I_kill(t);
        }

        public function get length() {
            return code.length;
        }

        public function serialize(bs) {
            resolveBackpatches();
            code.serialize(bs);
        }

        /*private*/ function resolveBackpatches() {
            for ( var i=0 ; i < backpatches.length ; i++ ) {
                var bp = backpatches[i];
                if (bp.label.address == -1)
                    throw "Missing definition for label " + bp.label.name;
                var v = bp.label.address - bp.base;
                code.setInt24(bp.loc, v);
            }
            backpatches.length = 0;
        }

        /*private*/ function stack(n) {
            current_stack_depth = current_stack_depth + n;
            if (current_stack_depth > max_stack_depth) {
                max_stack_depth = current_stack_depth;
            }
        }

        /*private*/ function scope(n) {
            current_scope_depth = current_scope_depth + n;
            if (current_scope_depth > max_scope_depth)
                max_scope_depth = current_scope_depth;
        }

        /*private*/ var code = new ABCByteStream;
        /*private*/ var nextLabel = 1000;
        /*private*/ var backpatches = [];
        /*private*/ var current_scope_depth = 0;
        /*private*/ var max_scope_depth = 0;
        /*private*/ var current_stack_depth = 0;
        /*private*/ var max_stack_depth = 0;
        /*private*/ var nextTemp;
        /*private*/ var freeTemps = [];
        /*private*/ var constants;
        /*private*/ var set_dxns = false;
        /*private*/ var need_activation = false;
    }
}
