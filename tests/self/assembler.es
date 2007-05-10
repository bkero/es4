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
    const CONSTANT_Qname              = 0x07; // ns::name, const ns, const name
    const CONSTANT_Namespace          = 0x08;
    const CONSTANT_Multiname          = 0x09; // [ns...]::name, const [ns...], const name
    const CONSTANT_False              = 0x0A;
    const CONSTANT_True               = 0x0B;
    const CONSTANT_Null               = 0x0C;
    const CONSTANT_QnameA             = 0x0D; // @ns::name, const ns, const name
    const CONSTANT_MultinameA         = 0x0E; // @[ns...]::name, const [ns...], const name
    const CONSTANT_RTQname            = 0x0F; // ns::name, var ns, const name
    const CONSTANT_RTQnameA           = 0x10; // @ns::name, var ns, const name
    const CONSTANT_RTQnameL           = 0x11; // ns::[name], var ns, var name
    const CONSTANT_RTQnameLA          = 0x12; // @ns::[name], var ns, var name
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
    
    const OP_bkpt               = 0x01;
    const OP_nop                = 0x02;
    const OP_throw              = 0x03;
    const OP_getsuper           = 0x04;
    const OP_setsuper           = 0x05;
    const OP_dxns               = 0x06;
    const OP_dxnslate           = 0x07;
    const OP_kill               = 0x08;
    const OP_label              = 0x09;
    const OP_ifnlt              = 0x0C;
    const OP_ifnle              = 0x0D;
    const OP_ifngt              = 0x0E;
    const OP_ifnge              = 0x0F;
    const OP_jump               = 0x10;
    const OP_iftrue             = 0x11;
    const OP_iffalse            = 0x12;
    const OP_ifeq               = 0x13;
    const OP_ifne               = 0x14;
    const OP_iflt               = 0x15;
    const OP_ifle               = 0x16;
    const OP_ifgt               = 0x17;
    const OP_ifge               = 0x18;
    const OP_ifstricteq         = 0x19;
    const OP_ifstrictne         = 0x1A;
    const OP_lookupswitch       = 0x1B;
    const OP_pushwith           = 0x1C;
    const OP_popscope           = 0x1D;
    const OP_nextname           = 0x1E;
    const OP_hasnext            = 0x1F;
    const OP_pushnull           = 0x20;
    const OP_pushundefined      = 0x21;
    const OP_nextvalue          = 0x23;
    const OP_pushbyte           = 0x24;
    const OP_pushshort          = 0x25;
    const OP_pushtrue           = 0x26;
    const OP_pushfalse          = 0x27;
    const OP_pushnan            = 0x28;
    const OP_pop                = 0x29;
    const OP_dup                = 0x2A;
    const OP_swap               = 0x2B;
    const OP_pushstring         = 0x2C;
    const OP_pushint            = 0x2D;
    const OP_pushuint           = 0x2E;
    const OP_pushdouble         = 0x2F;
    const OP_pushscope          = 0x30;
    const OP_pushnamespace      = 0x31;
    const OP_newfunction        = 0x40;
    const OP_call               = 0x41;
    const OP_construct          = 0x42;
    const OP_callmethod         = 0x43;
    const OP_callstatic         = 0x44;
    const OP_callsuper          = 0x45;
    const OP_callproperty       = 0x46;
    const OP_returnvoid         = 0x47;
    const OP_returnvalue        = 0x48;
    const OP_constructsuper     = 0x49;
    const OP_constructprop      = 0x4A;
    const OP_callsuperid        = 0x4B;
    const OP_callproplex        = 0x4C;
    const OP_callinterface      = 0x4D;
    const OP_callsupervoid      = 0x4E;
    const OP_callpropvoid       = 0x4F;
    const OP_newobject          = 0x55;
    const OP_newarray           = 0x56;
    const OP_newactivation      = 0x57;
    const OP_newclass           = 0x58;
    const OP_getdescendants     = 0x59;
    const OP_findpropstrict     = 0x5D;
    const OP_findproperty       = 0x5E;
    const OP_finddef            = 0x5F;
    const OP_getlex             = 0x60;
    const OP_setproperty        = 0x61;
    const OP_getlocal           = 0x62;
    const OP_setlocal           = 0x63;
    const OP_getglobalscope     = 0x64;
    const OP_getscopeobject     = 0x65;
    const OP_getproperty        = 0x66;
    const OP_getpropertylate    = 0x67;
    const OP_initproperty       = 0x68;
    const OP_deleteproperty     = 0x6A;
    const OP_deletepropertylate = 0x6B;
    const OP_getslot            = 0x6C;
    const OP_setslot            = 0x6D;
    const OP_getglobalslot      = 0x6E;
    const OP_setglobalslot      = 0x6F;
    const OP_convert_s          = 0x70;
    const OP_esc_xelem          = 0x71;
    const OP_esc_xattr          = 0x72;
    const OP_convert_i          = 0x73;
    const OP_convert_u          = 0x74;
    const OP_convert_d          = 0x75;
    const OP_convert_b          = 0x76;
    const OP_convert_o          = 0x77;
    const OP_coerce             = 0x80;
    const OP_coerce_b           = 0x81;
    const OP_coerce_a           = 0x82;
    const OP_coerce_i           = 0x83;
    const OP_coerce_d           = 0x84;
    const OP_coerce_s           = 0x85;
    const OP_astype             = 0x86;
    const OP_astypelate         = 0x87;
    const OP_coerce_u           = 0x88;
    const OP_coerce_o           = 0x89;
    const OP_negate             = 0x90;
    const OP_increment          = 0x91;
    const OP_inclocal           = 0x92;
    const OP_decrement          = 0x93;
    const OP_declocal           = 0x94;
    const OP_typeof             = 0x95;
    const OP_not                = 0x96;
    const OP_bitnot             = 0x97;
    const OP_concat             = 0x9A;
    const OP_add_d              = 0x9B;
    const OP_add                = 0xA0;
    const OP_subtract           = 0xA1;
    const OP_multiply           = 0xA2;
    const OP_divide             = 0xA3;
    const OP_modulo             = 0xA4;
    const OP_lshift             = 0xA5;
    const OP_rshift             = 0xA6;
    const OP_urshift            = 0xA7;
    const OP_bitand             = 0xA8;
    const OP_bitor              = 0xA9;
    const OP_bitxor             = 0xAA;
    const OP_equals             = 0xAB;
    const OP_strictequals       = 0xAC;
    const OP_lessthan           = 0xAD;
    const OP_lessequals         = 0xAE;
    const OP_greaterthan        = 0xAF;
    const OP_greaterequals      = 0xB0;
    const OP_instanceof         = 0xB1;
    const OP_istype             = 0xB2;
    const OP_istypelate         = 0xB3;
    const OP_in                 = 0xB4;
    const OP_increment_i        = 0xC0;
    const OP_decrement_i        = 0xC1;
    const OP_inclocal_i         = 0xC2;
    const OP_declocal_i         = 0xC3;
    const OP_negate_i           = 0xC4;
    const OP_add_i              = 0xC5;
    const OP_subtract_i         = 0xC6;
    const OP_multiply_i         = 0xC7;
    const OP_getlocal0          = 0xD0;
    const OP_getlocal1          = 0xD1;
    const OP_getlocal2          = 0xD2;
    const OP_getlocal3          = 0xD3;
    const OP_setlocal0          = 0xD4;
    const OP_setlocal1          = 0xD5;
    const OP_setlocal2          = 0xD6;
    const OP_setlocal3          = 0xD7;
    const OP_abs_jump           = 0xEE;
    const OP_debug              = 0xEF;
    const OP_debugline          = 0xF0;
    const OP_debugfile          = 0xF1;
    const OP_bkptline           = 0xF2;
    const OP_timestamp          = 0xF3;
    const OP_verifypass         = 0xF5;
    const OP_alloc              = 0xF6;
    const OP_mark               = 0xF7;
    const OP_wb                 = 0xF8;
    const OP_prologue           = 0xF9;
    const OP_sendenter          = 0xFA;
    const OP_doubletoatom       = 0xFB;
    const OP_sweep              = 0xFC;
    const OP_codegenop          = 0xFD;
    const OP_verifyop           = 0xFE;
    const OP_decode             = 0xFF;
    

    /*********************************************************************************
     * Assembler for one code block.
     *
     * This is a lightweight class that is used to emit bytes for
     * instructions and data, and to maintain stack and scope depths,
     * but which has no code generation logic save for simple
     * abstractions (eg, GetLocal maps to GetLocalN or to the general
     * GetLocal instruction, depending on its parameter value).
     */
    class ABCAssembler 
    {
        function ABCAssembler(constants, numberOfFormals) {
            this.constants = constants;
            this.code.endian = "littleEndian";
            this.nextTemp = numberOfFormals;
        }

        private function nullOp(name, opcode) {
            stack(0);
            Debug.log_mode::log("  "+code.length+":" + name, code_out);
            code.uint8(opcode);
        }

        private function immediateOp(name, opcode) {
            stack(1);
            Debug.log_mode::log("  "+code.length+":" + name, code_out);
            code.uint8(opcode);
        }

        private function constantOp(name, opcode, index) {
            stack(1);
            Debug.log_mode::log("  "+code.length+":" + name, code_out);
            code.uint8(opcode);
            code.int32(index);
        }

        function i_bkpt() { nullOp("bkpt", OP_bkpt) }
        function i_nop() { nullOp("nop", OP_nop) }
        function i_throw() { nullOp("throw", OP_throw) }


        function LoadThis() { immediateOp("LoadThis", OP_getlocal0) }
        function PushNull() { immediateOp("PushNull", OP_pushnull) }
        function PushTrue() { immediateOp("PushTrue", OP_pushtrue) }
        function PushFalse() { immediateOp("PushFalse", OP_pushfalse) }
        function PushUndefined() { immediateOp("PushUndefined", OP_pushundefined) }
        function GetGlobalScope() { immediateOp("GetGlobalScope", OP_getglobalscope) }
    
        function PushString(str) { constantOp("PushString " + str, OP_pushstring, ConstantUtf8(str)) }
        function PushNumber(num:Number) { constantOp("PushNumber", OP_pushdouble, ConstantDouble(num)) }
        function PushInt(num:int) { constantOp("PushInt", OP_pushint, ConstantInt(num)) }
        function NewFunction(method_index) { constantOp("NewFunction", OP_newfunction, method_index) }

        private function stackOp(name, opcode, movement) {
            stack(movement);
            Debug.log_mode::log("  "+code.length+":" + name, code_out);
            makeByte(code,opcode);
        }
        
        function Pop() { stackOp("Pop", OP_pop, -1) }
        function Dup() { stackOp("Dup", OP_dup, 1) }
        function Swap() { stackOp("Swap", OP_swap, 0) }

        private function binOp(name, opcode) {
            stack(-1);
            Debug.log_mode::log("  "+code.length+":" + name + " ", code_out);
            makeByte(code, opcode);
        }

        function Add()           { binOp("Add", OP_add) }
        function Subtract()      { binOp("Subtract", OP_subtract) }
        function Multiply()      { binOp("Multiply", OP_multiply) }
        function Divide()        { binOp("Divide", OP_divide) }
        function Modulo()        { binOp("Modulo", OP_modulo) }
        function LShift()        { binOp("LShift", OP_lshift) }
        function RShift()        { binOp("RShift", OP_rshift) }
        function URShift()       { binOp("URShift", OP_urshift) }
        function BitAnd()        { binOp("BitAnd", OP_bitand) }
        function BitOr()         { binOp("BitOr", OP_bitor) }
        function BitXor()        { binOp("BitXor", OP_bitxor) }
        function Equals()        { binOp("Equals", OP_equals) }
        function StrictEquals()  { binOp("StrictEquals", OP_strictequals) }
        function LessThan()      { binOp("LessThan", OP_lessthan) }
        function LessEquals()    { binOp("LessEquals", OP_lessequals) }
        function GreaterThan()   { binOp("GreaterThan", OP_greaterthan) }
        function GreaterEquals() { binOp("GreaterEquals", OP_greaterequals) }
        function InstanceOf()    { binOp("InstanceOf", OP_instanceof) }
        function IsType()        { binOp("IsType", OP_istype) }
        function In()            { binOp("In", OP_in) }
        
        private function unOp(name, opcode)
        {
            stack(0);
            Debug.log_mode::log("  "+code.length+":" + name + " ", code_out);
            makeByte(code, opcode);
        }

        function BitNot()    { unOp("BitNot", OP_bitnot) }
        function Not()       { unOp("Not", OP_not) }
        function TypeOf()    { unOp("TypeOf", OP_typeof) }
        function Negate()    { unOp("Negate", OP_negate) }
        function Convert_B() { unOp("Convert_B", OP_convert_b) }
        function Increment() { unOp("Increment", OP_increment) }
        function Decrement() { unOp("Decrement", OP_decrement) }
        
        function Jump(lnum) {
            stack(0);
            Debug.log_mode::log("  "+code.length+":Jump L" + lnum, code_out);
            code.uint8(OP_jump);
            code.int24(0);
        }

        function ReturnValue() {
            stack(-1);
            Debug.log_mode::log("  "+code.length+":ReturnValue", code_out);
            code.uint8(code,OP_returnvalue)
        }
        
        function ReturnVoid() { nullOp("ReturnVoid", OP_returnvoid) }
        
        function NewActivation() {
            stack(1);
            Debug.log_mode::log("  "+code.length+":NewActivation", code_out)
            makeByte(code,OP_newactivation)
        }

        function GetScopeObject(n) {
            stack(1);
            Debug.log_mode::log("  "+code.length+":GetScopeObject "+n, code_out)
            makeByte(code,OP_getscopeobject)
            makeInt32(code,n)
        }
    
        function Call(size:uint) {
            stack(1-(size+1)); // one more for "this"
            Debug.log_mode::log("  "+code.length+":Call", code_out)
            makeByte(code,OP_call)
            makeInt32(code,size)
        }

        function PushWith() {
            stack(-1);
            Debug.log_mode::log("  "+code.length+":PushWithe", code_out)
            makeByte(code,OP_pushwith)
            scope_depth++
        }
        
        function PushScope() {
            stack(-1);
            Debug.log_mode::log("  "+code.length+":PushScope", code_out)
            makeByte(code,OP_pushscope)
            scope_depth++
        }
        
        function PopScope() {
            stack(0);
            Debug.log_mode::log("  "+code.length+":PopScope", code_out)
            makeByte(code,OP_popscope)
            scope_depth--
        }
        
        function NewArray(count) {
            stack (1-count);
            Debug.log_mode::log("  "+code.length+":NewArray "+count, code_out);
            makeByte(code, OP_newarray);
            makeInt32(code, count);
        }

        function Kill(n)
        {
            stack(0);
            Debug.log_mode::log("  "+code.length+":Kill " + n, code_out)
            makeByte(code,OP_kill)
            makeInt32(code,n)
        }

        function GetLocal(n) {
            stack(1);
            Debug.log_mode::log("  "+code.length+":GetLocal "+n, code_out)
            if( n <= 3 )
                makeByte(code,OP_getlocal0+n)
            else {
                makeByte(code,OP_getlocal)
                makeInt32(code,n)
            }
        }
        
        function SetLocal(n) {
            stack(-1);
            Debug.log_mode::log("  "+code.length+":SetLocal "+n, code_out)
            if( n <= 3 )
                makeByte(code,OP_setlocal0+n)
            else {
                makeByte(code,OP_setlocal)
                makeInt32(code,n)
            }
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

        private var code = new ByteArray;
        private var scope_depth = 1;
        private var current_stack_depth = 0;
        private var max_stack_depth = 0;
        private var nextTemp;
        private var freeTemps = [];
        private var constants;
    }

}
