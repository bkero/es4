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

package es4 
{
    /*******************************************************************
     * ABC constants
     */

    const CONSTANT_Utf8               = 0x01;
    const CONSTANT_Integer            = 0x03;
    const CONSTANT_UInt               = 0x04;
    const CONSTANT_PrivateNamespace   = 0x05;
    const CONSTANT_Double             = 0x06;
    const CONSTANT_QName              = 0x07; // ns::name, const ns, const name
    const CONSTANT_Namespace          = 0x08;
    const CONSTANT_Multiname          = 0x09; // [ns...]::name, const [ns...], const name
    const CONSTANT_False              = 0x0A;
    const CONSTANT_True               = 0x0B;
    const CONSTANT_Null               = 0x0C;
    const CONSTANT_QNameA             = 0x0D; // @ns::name, const ns, const name
    const CONSTANT_MultinameA         = 0x0E; // @[ns...]::name, const [ns...], const name
    const CONSTANT_RTQName            = 0x0F; // ns::name, var ns, const name
    const CONSTANT_RTQNameA           = 0x10; // @ns::name, var ns, const name
    const CONSTANT_RTQNameL           = 0x11; // ns::[name], var ns, var name
    const CONSTANT_RTQNameLA          = 0x12; // @ns::[name], var ns, var name
    const CONSTANT_NameL              = 0x13; // o[name], var name
    const CONSTANT_NameLA             = 0x14; // @[name], var name
    const CONSTANT_NamespaceSet       = 0x15;
    const CONSTANT_PackageNamespace   = 0x16; // namespace for a package
    const CONSTANT_PackageInternalNS  = 0x17; 
    const CONSTANT_ProtectedNamespace = 0x18; 
    const CONSTANT_ExplicitNamespace  = 0x19;
    const CONSTANT_StaticProtectedNS  = 0x1A;
    const CONSTANT_MultinameL         = 0x1B;
    const CONSTANT_MultinameLA        = 0x1C;

    const CONSTANT_ClassSealed        = 0x01;
    const CONSTANT_ClassFinal         = 0x02;
    const CONSTANT_ClassInterface     = 0x04;
    const CONSTANT_ClassProtectedNs   = 0x08;

    const TRAIT_Slot                  = 0;
    const TRAIT_Method                = 1;
    const TRAIT_Getter                = 2;
    const TRAIT_Setter                = 3;
    const TRAIT_Class                 = 4;
    const TRAIT_Function              = 5;
    const TRAIT_Const                 = 6;

    const ATTR_Final                  = 0x01;
    const ATTR_Override               = 0x02;
    const ATTR_Metadata               = 0x04;

    const SLOT_var                    = 0;
    const SLOT_method                 = 1;
    const SLOT_getter                 = 2;
    const SLOT_setter                 = 3;
    const SLOT_class                  = 4;
    const SLOT_function               = 6;
    
    const METHOD_Arguments            = 0x1;
    const METHOD_Activation           = 0x2;
    const METHOD_Needrest             = 0x4;
    const METHOD_HasOptional          = 0x8;
    const METHOD_IgnoreRest           = 0x10;
    const METHOD_Native               = 0x20;
    const METHOD_Setsdxns             = 0x40;
    const METHOD_HasParamNames        = 0x80;


    /*********************************************************************************
     * Assembler for one code block.
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
    class ABCAssembler 
    {
        const listify = true;
        const indent = "        ";

        function ABCAssembler(constants, numberOfFormals) {
            this.constants = constants;
            this.nextTemp = numberOfFormals+1; // local 0 is always "this"
        }

        function get maxStack() { return max_stack_depth }
        function get maxLocal() { return nextTemp }
        function get maxScope() { return max_scope_depth }
        function get flags() { return (set_dxns ? METHOD_Setsdxns : 0) | (need_activation ? METHOD_Activation : 0) }
                                   
        private function listL(n) {
            if (listify)
                print(n);
        }

        private function list1(name) {
            if (listify)
                print(indent + name);
        }

        private function list2(name, v) {
            if (listify)
                print(indent + name + " " + v);
        }

        private function list3(name, v1, v2) {
            if (listify)
                print(indent + name + " " + v1 + " " + v2);
        }

        private function listn(name, ...rest) {
            if (listify)
                print(indent + name + " " + rest.join(" "));
        }


        // Instructions that push one value, with a single opcode byte
        private function pushOne(name, opcode) {
            stack(1);
            list1(name);
            code.uint8(opcode);
        }

        function I_dup() { pushOne("dup", 0x2A) }
        function I_getglobalscope() { pushOne("getglobalscope", 0x64) }
        function I_getlocal_0() { pushOne("getlocal_0", 0xD0) }
        function I_getlocal_1() { pushOne("getlocal_1", 0xD1) }
        function I_getlocal_2() { pushOne("getlocal_2", 0xD2) }
        function I_getlocal_3() { pushOne("getlocal_3", 0xD3) }
        function I_newactivation() { need_activation=true; pushOne("newactivation", 0x57) }
        function I_pushfalse() { pushOne("pushfalse", 0x27) }
        function I_pushnan() { pushOne("pushnan", 0x28) }
        function I_pushnull() { pushOne("pushnull", 0x20) }
        function I_pushtrue() { pushOne("pushtrue", 0x26) }
        function I_pushundefined() { pushOne("pushundefined", 0x21) }

        // Instructions that push one value, with an opcode byte followed by a u30 argument
        private function pushOneU30(name, opcode, v) {
            stack(1);
            list2(name, v);
            code.uint8(opcode);
            code.uint30(v);
        }

        function I_getglobalslot(index) { pushOneU30("getglobalslot", 0x6E, index) }
        function I_getlex(index) { pushOneU30("getlex", 0x60, index) }
        function I_getscopeobject(index) { pushOneU30("getscopeobject", 0x65, index) }
        function I_newcatch(index) { pushOneU30("newcatch", 0x5A, index) }
        function I_newfunction(index) { pushOneU30("newfunction", 0x40, index) }
        function I_pushdouble(index) { pushOneU30("pushdouble", 0x2F, index) }
        function I_pushint(index) { pushOneU30("pushint", 0x2D, index) }
        function I_pushnamespace(index) { pushOneU30("pushnamespace", 0x31, index) }
        function I_pushshort(v) { pushOneU30("pushshort", 0x25, v) }
        function I_pushstring(index) { pushOneU30("pushstring", 0x2C, index) }
        function I_pushuint(index) { pushOneU30("pushuint", 0x2E, index) }

        // Instructions that pop one value, with a single opcode byte
        private function dropOne(name, opcode) {
            stack(-1);
            list1(name);
            code.uint8(opcode);
        }

        function I_add() { dropOne("add", 0xA0) }
        function I_add_i() { dropOne("add_i", 0xC5) }
        function I_astypelate() { dropOne("astypelate", 0x87) }
        function I_bitand() { dropOne("bitand", 0xA8) }
        function I_bitor() { dropOne("bitor", 0xA9) }
        function I_bitxor() { dropOne("bitxor", 0xAA) }
        function I_divide() { dropOne("divide", 0xA3) }
        function I_dxnslate() { set_dxns=true; dropOne("dxnslate", 0x07) }
        function I_equals() { dropOne("Equals", 0xAB) }
        function I_greaterequals() { dropOne("greaterequals", 0xB0) }
        function I_greaterthan() { dropOne("greaterthan", 0xAF) }
        function I_hasnext() { dropOne("hasnext", 0x1F) }
        function I_in() { dropOne("in", 0xB4) }
        function I_instanceof() { dropOne("instanceof", 0xB1) }
        function I_istypelate() { dropOne("istypelate", 0xB3) }
        function I_lessequals() { dropOne("lessequals", 0xAE) } 
        function I_lessthan() { dropOne("lessthan", 0xAD) }
        function I_lshift() { dropOne("lshift", 0xA5) }
        function I_modulo() { dropOne("modulo", 0xA4) }
        function I_multiply() { dropOne("multiply", 0xA2) }
        function I_multiply_i() { dropOne("multiply_i", 0xC7) }
        function I_nextname() { dropOne("nextname", 0x1E) }
        function I_nextvalue() { dropOne("nextvalue", 0x23) }
        function I_pop() { dropOne("pop", 0x29) }
        function I_pushscope() { scope(1); dropOne("pushscope", 0x30) }
        function I_pushwith() { dropOne("pushwith", 0x1C) }
        function I_returnvalue() { dropOne("returnvalue", 0x48) }
        function I_rshift() { dropOne("rshift", 0xA6) }
        function I_setlocal_0() { dropOne("setlocal_0", 0xD4) }
        function I_setlocal_1() { dropOne("setlocal_1", 0xD5) }
        function I_setlocal_2() { dropOne("setlocal_2", 0xD6) }
        function I_setlocal_3() { dropOne("setlocal_3", 0xD7) }
        function I_strictequals() { dropOne("strictequals", 0xAC) }
        function I_subtract() { dropOne("subtract", 0xA1) }
        function I_subtract_i() { dropOne("subtract_i", 0xC6) }
        function I_throw() { dropOne("throw", 0x03) }
        function I_urshift() { dropOne("urshift", 0xA7) }

        // Instructions that pop one value, with an opcode byte followed by an u30 argument
        private function dropOneU30(name, opcode, v) {
            stack(-1);
            list2(name, v);
            code.uint8(opcode);
            code.uint30(v);
        }

        function I_setglobalslot(index) { dropOneU30("setglobalslot", 0x6F, index) }

        // Instructions that do not change the stack height, with a single opcode byte
        private function dropNone(name, opcode)
        {
            //stack(0);
            list1(name);
            code.uint8(opcode);
        }

        function I_bitnot() { dropNone("bitnot", 0x97) }
        function I_checkfilter() { dropNone("checkfilter", 0x78) }
        function I_coerce_a() { dropNone("coerce_a", 0x82) }
        function I_coerce_s() { dropNone("coerce_s", 0x85) }
        function I_convert_b() { dropNone("convert_b", 0x76) }
        function I_convert_d() { dropNone("convert_d", 0x75) }
        function I_convert_i() { dropNone("convert_i", 0x73) }
        function I_convert_o() { dropNone("convert_o", 0x77) }
        function I_convert_s() { dropNone("convert_s", 0x70) }
        function I_convert_u() { dropNone("convert_u", 0x74) }
        function I_decrement() { dropNone("decrement", 0x93) }
        function I_decrement_i() { dropNone("decrement_i", 0xC1) }
        function I_esc_xattr() { dropNone("esc_xattr", 0x72) }
        function I_esc_xelem() { dropNone("esc_xattr", 0x71) }
        function I_increment() { dropNone("increment", 0x91) }
        function I_increment_i() { dropNone("increment_i", 0xC0) }
        function I_negate() { dropNone("negate", 0x90) }
        function I_negate_i() { dropNone("negate_i", 0xC4) }
        function I_nop() { dropNone("nop", 0x02) }
        function I_not() { dropNone("not", 0x96) }
        function I_popscope() { scope(-1); dropNone("popscope", 0x1D) }
        function I_returnvoid() { dropNone("returnvoid", 0x47) }
        function I_swap() { dropNone("swap", 0x2B) }
        function I_typeof() { dropNone("typeof", 0x95) }

        // Instructions that do not change the stack height, with an opcode byte 
        // followed by a u30 argument
        private function dropNoneU30(name, opcode, x) {
            //stack(0)
            list2(name, x);
            code.uint8(opcode);
            code.uint30(x);
        }

        function I_astype(index) { dropNoneU30("astype", 0x86, index) }
        function I_coerce(index) { dropNoneU30("coerce", 0x80, index) }
        function I_debugfile(index) { dropNoneU30("debugfile", 0xF1, index) }
        function I_debugline(linenum) { dropNoneU30("debugline", 0xF0, linenum) }
        function I_declocal(reg) { dropNoneU30("declocal", 0x94, reg) }
        function I_declocal_i(reg) { dropNoneU30("declocal_i", 0xC3, reg) }
        function I_dxns(index) { set_dxns=true; dropNoneU30("dxns", 0x06, index) }
        function I_getslot(index) { dropNoneU30("getslot", 0x6C, index) }
        function I_inclocal(reg) { dropNoneU30("inclocal", 0x92, reg) }
        function I_inclocal_i(reg) { dropNoneU30("inclocal_i", 0xC2, reg) }
        function I_istype(index) { dropNoneU30("istype", 0xB2, index) }
        function I_kill(index) { dropNoneU30("kill", 0x08, index) }
        function I_newclass(index) { dropNoneU30("newclass", 0x58, index) }

        function I_getlocal(index) { 
            switch (index) {
            case 0: I_getlocal_0(); break;
            case 1: I_getlocal_1(); break;
            case 2: I_getlocal_2(); break;
            case 3: I_getlocal_3(); break;
            default: pushOneU30("getlocal", 0x62, index);
            }
        }

        function I_setlocal(index) { 
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

        private function newlabel() {
            return { "name": nextLabel++, "address": -1, "stack": current_stack_depth, "scope": current_scope_depth };
        }

        private function relativeOffset(base, L) {
            if (L.address != -1)
                code.int24(L.address - base);
            else {
                backpatches.push({ "loc": code.length, "base": base, "label": L });
                code.int24(0);
            }
        }

        private function jmp(stk, name, opcode, L) {
            stack(stk);

            if (L === undefined)
                L = newlabel();

            list2(name, L.name);
            code.uint8(opcode);
            relativeOffset(code.length+3, L);

            return L;
        }

        function I_label(L = undefined) { 
            var here = code.length;
            var define = false;
            if (L === undefined) {
                define = true;
                L = newlabel();
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

        function I_ifeq(L=undefined) { return jmp(-2, "ifeq", 0x13, L) }
        function I_ifge(L=undefined) { return jmp(-2, "ifge", 0x18, L) }
        function I_ifgt(L=undefined) { return jmp(-2, "ifgt", 0x17, L) }
        function I_ifle(L=undefined) { return jmp(-2, "ifle", 0x16, L) }
        function I_iflt(L=undefined) { return jmp(-2, "iflt", 0x15, L) }
        function I_ifne(L=undefined) { return jmp(-2, "ifne", 0x14, L) }
        function I_ifnge(L=undefined) { return jmp(-2, "ifnge", 0x0F, L) }
        function I_ifngt(L=undefined) { return jmp(-2, "ifngt", 0x0E, L) }
        function I_ifnle(L=undefined) { return jmp(-2, "ifnle", 0x0D, L) }
        function I_ifnlt(L=undefined) { return jmp(-2, "ifnlt", 0x0C, L) }
        function I_ifstricteq(L=undefined) { return jmp(-2, "ifstricteq", 0x19, L) }
        function I_ifstrictne(L=undefined) { return jmp(-2, "ifstrictne", 0x1A, L) }

        function I_iffalse(L=undefined) { return jmp(-1, "iffalse", 0x12, L) }
        function I_iftrue(L=undefined) { return jmp(-1, "iftrue", 0x11, L) }

        function I_jump(L=undefined) { return jmp(0, "jump", 0x10, L) }

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

        function I_lookupswitch(default_label, case_labels) {
            assert( case_labels is Array );
            assert( case_labels.length > 0 );

            stack(-1);

            if (default_label === undefined) {
                default_label = newlabel();
                for ( var i=0 ; i < case_labels.length ; i++ ) {
                    assert( case_labels[i] === undefined );
                    case_labels[i] = newlabel();
                }
            }
                
            list3("lookupswitch", default_label.name, map(function (L) { return L.name }, case_labels));
            var base = code.length;
            code.uint8(0x1B);
            relativeOffset(base, default_label);
            code.uint30(case_labels.length-1);
            for ( var i=0 ; i < case_labels.length ; i++ ) 
                relativeOffset(base, case_labels[i]);

            return default_label;
        }

        // Standard function calls
        private function call(name, opcode, nargs) {
            stack(1-(nargs+2)); /* pop function/receiver/args; push result */
            list2(name, nargs);
            code.uint8(opcode);
            code.uint30(nargs);
        }

        function I_call(nargs) { call("call", 0x41, nargs) }
        function I_construct(nargs) { call("construct", 0x42, nargs) }

        function I_constructsuper(nargs) {
            stack(nargs+1); /* pop receiver/args */
            list2("constructsuper", nargs);
            code.uint8(0x49);
            code.uint30(nargs);
        }

        private function callIDX(name, opcode, index, nargs) {
            stack(1-(nargs+1)); /* pop receiver/args; push result */
            list3(name, index, nargs);
            code.uint8(opcode);
            code.uint30(index);
            code.uint30(nargs);
        }

        function I_callmethod(index, nargs) { callIDX("callmethod", 0x43, index, nargs) }
        function I_callstatic(index, nargs) { callIDX("callstatic", 0x44, index, nargs) }

        private function callMN(name, opcode, index, nargs, isVoid=false) {
            /* pop receiver/NS?/Name?/args; push result? */
            var hasRTNS = constants.hasRTNS(index);
            var hasRTName = constants.hasRTName(index);
            stack((isVoid ? 0 : 1) - (1 + (hasRTNS ? 1 : 0) + (hasRTName ? 1 : 0) + nargs));
            list3(name + (hasRTNS ? "<NS>" : "") + (hasRTName ? "<Name>" : ""), index, nargs);
            code.uint8(opcode);
            code.uint30(index);
            code.uint30(nargs);
        }

        function I_callsuper(index, nargs) { callMN("callsuper", 0x45, index, nargs) }
        function I_callproperty(index, nargs) { callMN("callproperty", 0x46, index, nargs) }
        function I_constructprop(index, nargs) { callMN("constructprop", 0x4A, index, nargs) }
        function I_callproplex(index, nargs) { callMN("callproplex", 0x4C, index, nargs) }
        function I_callsupervoid(index, nargs) { callMN("callsupervoid", 0x4E, index, nargs, true) }
        function I_callpropvoid(index, nargs) { callMN("callpropvoid", 0x4F, index, nargs, true) }

        function I_debug(debug_type, index, reg, extra=0) {
            //stack(0);
            listn("debug", debug_type, index, reg, extra);
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
        private function propU30(name, pops, pushes, opcode, index) {
            var hasRTNS = constants.hasRTNS(index);
            var hasRTName = constants.hasRTName(index);
            stack(pushes - (pops + (hasRTNS ? 1 : 0) + (hasRTName ? 1 : 0)));
            list2(name + (hasRTNS ? "<NS>" : "") + (hasRTName ? "<Name>" : ""), index);
            code.uint8(opcode);
            code.uint30(index);
        }

        function I_deleteproperty(index) { propU30("deleteproperty", 1, 1, 0x6A, index) }
        function I_getdescendants(index) { propU30("getdescendants", 1, 1, 0x59, index) }
        function I_getproperty(index) { propU30("getproperty", 1, 1, 0x66, index); }
        function I_getsuper(index) { propU30("getsuper", 1, 1, 0x04, index); }
        function I_findproperty(index) { propU30("findproperty", 0, 1, 0x5E, index) }
        function I_findpropstrict(index) { propU30("findpropstict", 0, 1, 0x5D, index) }
        function I_initproperty(index) { propU30("initproperty", 2, 0, 0x68, index) }
        function I_setproperty(index) { propU30("setproperty", 2, 0, 0x61, index) }
        function I_setsuper(index) { propU30("setsuper", 2, 0, 0x05, index) }

        function I_hasnext2(object_reg, index_reg) {
            stack(1);
            code.uint8(0x32);
            code.uint30(object_reg);
            code.uint30(index_reg);
        }

        function I_newarray(nargs) {
            stack(1 - nargs);
            list2("newarray", nargs);
            code.uint8(0x56);
            code.uint30(nargs);
        }

        function I_newobject(nargs) {
            stack(1 - (2 * nargs));
            list2("newobject", nargs);
            code.uint8(0x55);
            code.uint30(nargs);
        }

        function I_pushbyte(b) {
            stack(1);
            list2("pushbyte", b);
            code.uint8(0x24);
            code.uint8(b);
        }

        function I_setslot(index) {
            stack(-2);
            list2("setslot", index);
            code.uint8(0x6D);
            code.uint30(index);
        }

        function getTemp() {
            if (freeTemps.length > 0)
                return freeTemps.pop();
            else
                return nextTemp++;
        }

        function killTemp(t) {
            freeTemps.push(t);
            I_kill(t);
        }

        function get length() {
            return code.length;
        }

        function serialize(bs) {
            resolveBackpatches();
            code.serialize(bs);
        }

        private function resolveBackpatches() {
            for ( var i=0 ; i < backpatches.length ; i++ ) {
                var bp = backpatches[i];
                if (bp.label.address == -1)
                    throw "Missing definition for label " + bp.label.name;
                var v = bp.label.address - bp.base;
                code.setInt24(bp.loc, v);
            }
            backpatches.length = 0;
        }

        private function stack(n) {
            current_stack_depth += n;
            if (current_stack_depth > max_stack_depth)
                max_stack_depth = current_stack_depth;
        }

        private function scope(n) {
            current_scope_depth += n;
            if (current_scope_depth > max_scope_depth)
                max_scope_depth = current_scope_depth;
        }

        private var code = new ABCByteStream;
        private var nextLabel = 1000;
        private var backpatches = [];
        private var current_scope_depth = 0;
        private var max_scope_depth = 0;
        private var current_stack_depth = 0;
        private var max_stack_depth = 0;
        private var nextTemp;
        private var freeTemps = [];
        private var constants;
        private var set_dxns = false;
        private var need_activation = false;
    }
}
