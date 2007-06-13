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

    const TRAIT_Slot = 0;
    const TRAIT_Method = 1;
    const TRAIT_Getter = 2;
    const TRAIT_Setter = 3;
    const TRAIT_Class = 4;
    const TRAIT_Function = 5;
    const TRAIT_Const = 6;

    const ATTR_Final = 0x01;
    const ATTR_Override = 0x02;
    const ATTR_Metadata = 0x04;

    const SLOT_var      = 0;
    const SLOT_method   = 1;
    const SLOT_getter   = 2;
    const SLOT_setter   = 3;
    const SLOT_class    = 4;
    const SLOT_function = 6;
    
    const METHOD_Arguments     = 0x1;
    const METHOD_Activation    = 0x2;
    const METHOD_Needrest      = 0x4;
    const METHOD_HasOptional   = 0x8;
    const METHOD_IgnoreRest    = 0x10;
    const METHOD_Native        = 0x20;
    const METHOD_Setsdxns      = 0x40;
    const METHOD_HasParamNames = 0x80;

    const CONSTANT_ClassSealed = 0x01;
    const CONSTANT_ClassFinal  = 0x02;
    const CONSTANT_ClassInterface = 0x04;
    const CONSTANT_ClassProtectedNs = 0x08;


    /*********************************************************************************
     * Assembler for one code block.
     *
     * This is a lightweight class that is used to emit bytes for
     * instructions and data, and to maintain stack and scope depths,
     * but which has no code generation logic save for simple
     * abstractions (eg, GetLocal maps to GetLocalN or to the general
     * GetLocal instruction, depending on its parameter value).
     */
    final class ABCAssembler 
    {
        function ABCAssembler(constants, numberOfFormals) {
            this.constants = constants;
            this.nextTemp = numberOfFormals;
        }

        // Performance: turn this into list1(), list2(), etc, as many as we need (not many)
        private function list(name, ...rest) {
            Debug.log_mode::log("  "+code.length+":" + name, code_out);
        }

        private function nullOp(name, opcode) {
            //stack(0);
            list(name);
            code.uint8(opcode);
        }

        private function constantOp(name, opcode, index) {
            stack(1);
            list(name, index);
            code.uint8(opcode);
            code.int32(index);
        }

        function IBkpt() { nullOp("bkpt", OP_bkpt) }
        function INop() { nullOp("nop", OP_nop) }
        function IThrow() { nullOp("throw", OP_throw) }

    
        function IPushString(str) { constantOp("PushString " + str, OP_pushstring, ConstantUtf8(str)) }
        function IPushNumber(num:Number) { constantOp("PushNumber", OP_pushdouble, ConstantDouble(num)) }
        function IPushInt(num:int) { constantOp("PushInt", OP_pushint, ConstantInt(num)) }
        function INewFunction(method_index) { constantOp("NewFunction", OP_newfunction, method_index) }

        private function stackOp(name, opcode, movement) {
            stack(movement);
            list(name);
            code.uint8(opcode);
        }

        private function pushOne(name, opcode) {
            stack(1);
            list(name);
            code.uint8(opcode);
        }

        function I_dup() { pushOne("dup", 0x2A) }
        function I_getglobalscope() { pushOne("getglobalscope", 0x64) }
        function I_getlocal_0() { pushOne("getlocal_0", 0xD0) }
        function I_getlocal_1() { pushOne("getlocal_1", 0xD1) }
        function I_getlocal_2() { pushOne("getlocal_2", 0xD2) }
        function I_getlocal_3() { pushOne("getlocal_3", 0xD3) }

        private function pushOneU30(name, opcode, v) {
            stack(1);
            list(name);
            code.uint8(opcode);
            code.uint30(v);
        }

        function I_getglobalslot(index) { pushOneU30("getglobalslot", 0x6E, index) }
        function I_getlex(index) { pushOneU30("getlex", 0x60, index) }
        function I_getlocal(index) { pushOneU30("getlocal", 0x62, index) }
        function I_getscopeobject(index) { pushOneU30("getscopeobject", 0x65, index) }

        function ILoadThis() { pushOne("LoadThis", OP_getlocal0) }
        function IPushNull() { pushOne("PushNull", OP_pushnull) }
        function IPushTrue() { pushOne("PushTrue", OP_pushtrue) }
        function IPushFalse() { pushOne("PushFalse", OP_pushfalse) }
        function IPushUndefined() { pushOne("PushUndefined", OP_pushundefined) }

        function IPop() { stackOp("Pop", OP_pop, -1) }
        function ISwap() { stackOp("Swap", OP_swap, 0) }

        function I_hasnext2(object_reg, index_reg) {
            stack(1);
            code.uint8(0x32);
            code.uint30(object_reg);  // FIXME: maybe -- spec unclear
            code.uint30(index_reg);   // FIXME: maybe -- spec unclear
        }

        /* Opcode without arguments, drop one stack slot */
        private function dropOne(name, opcode) {
            stack(-1);
            list(name);
            code.uint8(opcode);
        }

        function I_add() { dropOne("add", 0xA0) }
        function I_add_i() { dropOne("add_i", 0xC5) }
        function I_astypelate() { dropOne("astypelate", 0x87) }
        function I_bitand() { dropOne("bitand", 0xA8) }
        function I_bitor() { dropOne("bitor", 0xA9) }
        function I_bitxor() { dropOne("bitxor", 0xAA) }
        function I_divide() { dropOne("divide", 0xA3) }
        function I_dxnslate() { dropOne("dxnslate", 0x07) }
        function I_equals() { dropOne("Equals", 0xAB) }
        function I_greaterequals() { dropOne("greaterequals", 0xAF) }  // FIXME: wrong for these
        function I_greaterthan() { dropOne("greaterthan", 0xAF) }      //   to have same opcode!
        function I_hasnext() { dropOne("hasnext", 0x1F) }
        function I_in() { dropOne("in", 0xB4) }
        function I_instanceof() { dropOne("instanceof", 0xB1) }
        function I_istypelate() { dropOne("istypelate", 0xB3) }

        function I_subtract()     { dropOne("subtract", OP_subtract) }
        function IMultiply()      { dropOne("Multiply", OP_multiply) }
        function IModulo()        { dropOne("Modulo", OP_modulo) }
        function ILShift()        { dropOne("LShift", OP_lshift) }
        function IRShift()        { dropOne("RShift", OP_rshift) }
        function IURShift()       { dropOne("URShift", OP_urshift) }
        function IStrictEquals()  { dropOne("StrictEquals", OP_strictequals) }
        function ILessThan()      { dropOne("LessThan", OP_lessthan) }
        function ILessEquals()    { dropOne("LessEquals", OP_lessequals) }
 
        private function dropNone(name, opcode)
        {
            //stack(0);
            list(name);
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

        function INot()       { dropNone("Not", OP_not) }
        function ITypeOf()    { dropNone("TypeOf", OP_typeof) }
        function INegate()    { dropNone("Negate", OP_negate) }
        function IConvert_B() { dropNone("Convert_B", OP_convert_b) }
        function IIncrement() { dropNone("Increment", OP_increment) }
        
        private function dropNoneU30(name, opcode, x) {
            //stack(0)
            list(name, x);
            code.uint8(opcode);
            code.uint30(x);
        }

        function I_astype(index) { dropNoneU30("astype", 0x86, index) }
        function I_coerce(index) { dropNoneU30("coerce", 0x80, index) }
        function I_debugfile(index) { dropNoneU30("debugfile", 0xF1, index) }
        function I_debugline(linenum) { dropNoneU30("debugline", 0xF0, linenum) }
        function I_declocal(reg) { dropNoneU30("declocal", 0x94, reg) }
        function I_declocal_i(reg) { dropNoneU30("declocal_i", 0xC3, reg) }
        function I_dxns(index) { dropNoneU30("dxns", 0x06, index) }
        function I_getslot(index) { dropNoneU30("getslot", 0x6C, index) }
        function I_inclocal(reg) { dropNoneU30("inclocal", 0x92, reg) }
        function I_inclocal_i(reg) { dropNoneU30("inclocal_i", 0xC2, reg) }
        function I_istype(index) { dropNoneU30("istype", 0xB2, reg) }

        private function ifCond2(name, opcode, offset) {
            stack(-2);
            list(name, offset);
            code.uint8(opcode);
            code.int24(offset);
        }

        function I_ifeq(offset) { ifCond2("ifeq", 0x13, offset) }
        function I_ifge(offset) { ifCond2("ifge", 0x18, offset) }
        function I_ifgt(offset) { ifCond2("ifgt", 0x17, offset) }
        function I_ifle(offset) { ifCond2("ifle", 0x16, offset) }
        function I_iflt(offset) { ifCond2("iflt", 0x15, offset) }
        function I_ifne(offset) { ifCond2("ifne", 0x14, offset) }
        function I_ifnge(offset) { ifCond2("ifnge", 0x0F, offset) }
        function I_ifngt(offset) { ifCond2("ifngt", 0x0E, offset) }
        function I_ifnle(offset) { ifCond2("ifnle", 0x0D, offset) }
        function I_ifnlt(offset) { ifCond2("ifnlt", 0x0C, offset) }
        function I_ifstricteq(offset) { ifCond2("ifstricteq", 0x19, offset) }
        function I_ifstrictne(offset) { ifCond2("ifstrictne", 0x1A, offset) }

        private function ifCond1(name, opcode, offset) {
            stack(-1);
            list(name, offset);
            code.uint8(opcode);
            code.int24(offset);
        }

        function I_iffalse(offset) { ifCond1("iffalse", 0x12, offset) }
        function I_iftrue(offset) { ifCond1("iftrue", 0x11, offset) }


        function IJump(lnum) {
            stack(0);
            list("Jump", lnum);
            code.uint8(OP_jump);
            code.int24(0);
        }

        function IReturnValue() {
            stack(-1);
            list("ReturnValue");
            code.uint8(OP_returnvalue);
        }
        
        function IReturnVoid() { 
            nullOp("ReturnVoid", OP_returnvoid);
        }
        
        function INewActivation() {
            stack(1);
            list("NewActivation");
            code.uint8(OP_newactivation);
        }

        function IGetScopeObject(n) {
            stack(1);
            list("GetScopeObject", n);
            code.uint8(OP_getscopeobject);
            code.int32(n);
        }
    
        private function call(name, opcode, nargs) {
            stack(1-(nargs+2)); /* pop function/receiver/args; push result */
            list(name, nargs);
            code.uint8(opcode);
            code.uint30(nargs);
        }

        function I_call(nargs) { call("call", 0x41, nargs) }
        function I_construct(nargs) { call("construct", 0x42, nargs) }

        function I_constructsuper(nargs) {
            stack(nargs+1); /* pop receiver/args */
            list("constructsuper", nargs);
            code.uint8(0x49);
            code.uint30(nargs);
        }

        private function callIDX(name, opcode, index, nargs) {
            stack(1-(nargs+1)); /* pop receiver/args; push result */
            list(name, index, nargs);
            code.uint8(opcode);
            code.uint30(index);
            code.uint30(nargs);
        }

        function I_callmethod(index, nargs) { callIDX("callmethod", 0x43, index, nargs) }
        function I_callstatic(index, nargs) { callIDX("callstatic", 0x44, index, nargs) }

        private function callMN(name, opcode, index, nargs, hasRTNS, hasRTName, isVoid=false) {
            /* pop receiver/NS?/Name?/args; push result? */
            stack((isVoid ? 0 : 1) - (1 + (hasRTNS ? 1 : 0) + (hasRTName ? 1 : 0) + nargs));
            list(name + (hasRTNS ? "<NS>" : "") + (hasRName ? "<Name>" : ""), index, nargs);
            code.uint8(opcode);
            code.uint30(index);
            code.uint30(nargs);
        }

        function I_callsuper(index, nargs, hasRTNS, hasRTName) { 
            callMN("callsuper", 0x45, index, nargs, hasRTNS, hasRTName);
        }
        function I_callproperty(index, nargs, hasRTNS, hasRTName) { 
            callMN("callproperty", 0x46, index, nargs, hasRTNS, hasRTName);
        }
        function I_constructprop(index, nargs, hasRTNS, hasRTName) { 
            callMN("constructprop", 0x4A, index, nargs, hasRTNS, hasRTName);
        }
        function I_callproplex(index, nargs, hasRTNS, hasRTName) { 
            callMN("callproplex", 0x4C, index, nargs, hasRTNS, hasRTName);
        }
        function I_callsupervoid(index, nargs, hasRTNS, hasRTName) { 
            callMN("callsupervoid", 0x4E, index, nargs, hasRTNS, hasRTName, true);
        }
        function I_callpropvoid(index, nargs, hasRTNS, hasRTName) { 
            callMN("callpropvoid", 0x4F, index, nargs, hasRTNS, hasRTName, true);
        }

        function I_debug(debug_type, index, reg, extra=0) {
            //stack(0);
            list("debug", debug_type, index, reg, extra);
            code.uint8(0xEF);
            code.uint8(debug_type);
            code.uint30(index);
            code.uint8(reg);
            code.uint30(extra);
        }

        private function propOpU30(name, opcode, v, hasRTNS, hasRTName) {
            /* pop object/NS?/Name?; push result */
            stack(1 - (1 + (hasRTNS ? 1 : 0) + (hasRTName ? 1 : 0)));
            list(name + (hasRTNS ? "<NS>" : "") + (hasRName ? "<Name>" : ""), v);
            code.uint8(opcode);
            code.uint30(v);
        }

        function I_deleteproperty(index, hasRTNS, hasRTName) { 
            propOpU30("deleteproperty", 0x6A, index, hasRTNS, hasRTName);
        }
        function I_getdescendants(index, hasRTNS, hasRTName) { 
            propOpU30("getdescendants", 0x59, index, hasRTNS, hasRTName);
        }
        function I_getproperty(index, hasRTNS, hasRTName) { 
            propOpU30("getproperty", 0x66, index, hasRTNS, hasRTName);
        }
        function I_getsuper(index, hasRTNS, hasRTName) { 
            propOpU30("getsuper", 0x04, index, hasRTNS, hasRTName);
        }

        function I_findproperty(hasRTNS, hasRTName) { 
            /* pop NS?/Name?; push result */
            stack(1 - ((hasRTNS ? 1 : 0) + (hasRTName ? 1 : 0)));
            list("findproperty" + (hasRTNS ? "<NS>" : "") + (hasRName ? "<Name>" : ""));
            code.uint8(0x5E);
        }

        function I_findpropstrict(index, hasRTNS, hasRTName) { 
            /* pop NS?/Name?; push result */
            stack(1 - ((hasRTNS ? 1 : 0) + (hasRTName ? 1 : 0)));
            list("findpropstrict" + (hasRTNS ? "<NS>" : "") + (hasRName ? "<Name>" : ""), index);
            code.uint8(0x5D);
            code.uint30(index);
        }

        function I_initproperty(index, hasRTNS, hasRTName) {
            /* pop object/NS?/Name?/value */
            stack(- (2 + (hasRTNS ? 1 : 0) + (hasRTName ? 1 : 0)));
            list("initproperty" + (hasRTNS ? "<NS>" : "") + (hasRName ? "<Name>" : ""), index);
            code.uint8(0x68);
            code.uint30(index);
        }

        function IPushWith() {
            stack(-1);
            list("PushWith");
            code.uint8(OP_pushwith);
            scope_depth++;
        }
        
        function IPushScope() {
            stack(-1);
            Debug.log_mode::log("  "+code.length+":PushScope", code_out)
            makeByte(code,OP_pushscope)
            scope_depth++
        }
        
        function IPopScope() {
            stack(0);
            Debug.log_mode::log("  "+code.length+":PopScope", code_out)
            makeByte(code,OP_popscope)
            scope_depth--
        }
        
        function INewArray(count) {
            stack (1-count);
            Debug.log_mode::log("  "+code.length+":NewArray "+count, code_out);
            makeByte(code, OP_newarray);
            makeInt32(code, count);
        }

        function IKill(n)
        {
            stack(0);
            Debug.log_mode::log("  "+code.length+":Kill " + n, code_out)
            makeByte(code,OP_kill)
            makeInt32(code,n)
        }

        function getTemp() {
            if (freeTemps.length > 0)
                return freeTemps.pop();
            else
                return nextTemp++;
        }

        function killTemp(t) {
            freeTemps.push(t);
            Kill(t);
        }

        private function stack(size:int):void {
            current_stack_depth += size;
            if (current_stack_depth > max_stack_depth)
                max_stack_depth = current_stack_depth;
        }

        private var code = new ABCByteStream;
        private var scope_depth = 1;
        private var current_stack_depth = 0;
        private var max_stack_depth = 0;
        private var nextTemp;
        private var freeTemps = [];
        private var constants;
    }

    public function testABCAssembler() {
        var cp = new ABCConstantPool();
        var as = new ABCAssembler(cp,0);

        as.I_returnvoid();
    }
}
