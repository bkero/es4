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

/* Unit tests for the assembler (and underlying components) */

package es4 
{
    public function testABCAssembler() {
        //testCoverage();
        //testHelloWorld();
        testLoop();
    }

    function testLoop() {
        var file = new ABCFile();
        var cp = new ABCConstantPool();

        file.addConstants(cp);
        
        var print_name = cp.QName(cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8("")), 
                                  cp.stringUtf8("print"));

        // Local 1 has a counter
        var asm = new ABCAssembler(cp,0);
        asm.I_getlocal_0();
        asm.I_pushscope();
        asm.I_pushbyte(10);
        asm.I_setlocal_1();
        var L0 = asm.I_label();
        asm.I_getlocal_1();
        asm.I_pushbyte(0);
        var L1 = asm.I_ifeq();
        asm.I_findpropstrict(print_name, false, false);
        asm.I_getlocal_1();
        asm.I_callpropvoid(print_name, 1, false, false);
        asm.I_declocal_i(1);
        asm.I_jump(L0);
        asm.I_deflabel(L1);
        asm.I_returnvoid();

        var meth = file.addMethod(new ABCMethodInfo(0, [], 0, 0));
        var body = new ABCMethodBodyInfo(meth);
        body.setMaxStack(2);       // FIXME: Should not have to compute this, assembler should provide it
        body.setLocalCount(2);     // FIXME: compute this?  Must be at least one more than the number of params
        body.setInitScopeDepth(0); // FIXME: useful default?
        body.setMaxScopeDepth(1);  // FIXME: should not have to compute this?
        body.setCode(asm);
        file.addMethodBody(body);

        file.addScript(new ABCScriptInfo(meth));

        loadAndRunABCFile(file);

    }

    function testHelloWorld() {
        var file = new ABCFile();
        var cp = new ABCConstantPool();

        file.addConstants(cp);

        // AVM BUG: passing 0 for the namespace causes the AVM to
        // crash, though the doc says it's legal.
        //
        // AVM BUG? passing 0 for the empty string causes the program
        // to fail, though the doc at least implies it's legal.

        var print_name = cp.QName(cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8("")), 
                                  cp.stringUtf8("print"));

        var asm = new ABCAssembler(cp,0);
        asm.I_getlocal_0();
        asm.I_pushscope();
        asm.I_findpropstrict(print_name, false, false);
        asm.I_pushstring(cp.stringUtf8("Hello, world!"));
        asm.I_callpropvoid(print_name, 1, false, false);
        asm.I_returnvoid();

        var meth = file.addMethod(new ABCMethodInfo(0, [], 0, 0));
        var body = new ABCMethodBodyInfo(meth);
        body.setMaxStack(2);       // FIXME: Should not have to compute this, assembler should provide it
        body.setLocalCount(1);     // FIXME: compute this?  Must be at least one more than the number of params
        body.setInitScopeDepth(0); // FIXME: useful default?
        body.setMaxScopeDepth(1);  // FIXME: should not have to compute this?
        body.setCode(asm);
        file.addMethodBody(body);

        file.addScript(new ABCScriptInfo(meth));

        loadAndRunABCFile(file);
    }

    function testCoverage() {
        var cp = new ABCConstantPool();
        var asm = new ABCAssembler(cp,0);

        asm.I_dup();
        asm.I_getglobalscope();
        asm.I_getlocal_0();
        asm.I_getlocal_1();
        asm.I_getlocal_2();
        asm.I_getlocal_3();
        asm.I_newactivation();
        asm.I_pushfalse();
        asm.I_pushnan();
        asm.I_pushnull();
        asm.I_pushtrue();
        asm.I_pushundefined();
        asm.I_getglobalslot(0);
        asm.I_getlex(0);
        asm.I_getlocal(0);
        asm.I_getscopeobject(0);
        asm.I_newcatch(0);
        asm.I_newfunction(0);
        asm.I_pushdouble(0);
        asm.I_pushint(0);
        asm.I_pushnamespace(0);
        asm.I_pushshort(0);
        asm.I_pushstring(0);
        asm.I_pushuint(0);
        asm.I_add();
        asm.I_add_i();
        asm.I_astypelate();
        asm.I_bitand();
        asm.I_bitor();
        asm.I_bitxor();
        asm.I_divide();
        asm.I_dxnslate();
        asm.I_equals();
        asm.I_greaterequals();
        asm.I_greaterthan();
        asm.I_hasnext();
        asm.I_in();
        asm.I_instanceof();
        asm.I_istypelate();
        asm.I_lessequals();
        asm.I_lessthan();
        asm.I_lshift();
        asm.I_modulo();
        asm.I_multiply();
        asm.I_multiply_i();
        asm.I_nextname();
        asm.I_nextvalue();
        asm.I_pop();
        asm.I_pushscope();
        asm.I_pushwith();
        asm.I_returnvalue();
        asm.I_rshift();
        asm.I_setlocal_0();
        asm.I_setlocal_1();
        asm.I_setlocal_2();
        asm.I_setlocal_3();
        asm.I_strictequals();
        asm.I_subtract();
        asm.I_subtract_i();
        asm.I_throw();
        asm.I_urshift();
        asm.I_setlocal(0);
        asm.I_setglobalslot(0);
        asm.I_bitnot();
        asm.I_checkfilter();
        asm.I_coerce_a();
        asm.I_coerce_s();
        asm.I_convert_b();
        asm.I_convert_d();
        asm.I_convert_i();
        asm.I_convert_o();
        asm.I_convert_s();
        asm.I_convert_u();
        asm.I_decrement();
        asm.I_decrement_i();
        asm.I_esc_xattr();
        asm.I_esc_xelem();
        asm.I_increment();
        asm.I_increment_i();
        asm.I_kill();
        asm.I_label();
        asm.I_negate();
        asm.I_negate_i();
        asm.I_nop();
        asm.I_not();
        asm.I_popscope();
        asm.I_returnvoid();
        asm.I_swap();
        asm.I_typeof();
        asm.I_astype(0);
        asm.I_coerce(0);
        asm.I_debugfile(0);
        asm.I_debugline(0);
        asm.I_declocal(0);
        asm.I_declocal_i(0);
        asm.I_dxns(0);
        asm.I_getslot(0);
        asm.I_inclocal(0);
        asm.I_inclocal_i(0);
        asm.I_istype(0);
        asm.I_newclass(0);
        asm.I_ifeq(0);
        asm.I_ifge(0);
        asm.I_ifgt(0);
        asm.I_ifle(0);
        asm.I_iflt(0);
        asm.I_ifne(0);
        asm.I_ifnge(0);
        asm.I_ifngt(0);
        asm.I_ifnle(0);
        asm.I_ifnlt(0);
        asm.I_ifstricteq(0);
        asm.I_ifstrictne(0);
        asm.I_iffalse(0);
        asm.I_iftrue(0);
        asm.I_jump(0);
        asm.I_call(0);
        asm.I_construct(0);
        asm.I_constructsuper(0);
        asm.I_callmethod(0, 0);
        asm.I_callstatic(0, 0);
        asm.I_callsuper(0, 0, false, false);
        asm.I_callproperty(0, 0, false, true);
        asm.I_constructprop(0, 0, true, false);
        asm.I_callproplex(0, 0, true, true);
        asm.I_callsupervoid(0, 0, true, false);
        asm.I_callpropvoid(0, 0, false, true);
        asm.I_debug(0, 0, 0, 0);
        asm.I_deleteproperty(0, true, false);
        asm.I_getdescendants(0, false, true);
        asm.I_getproperty(0, false, false);
        asm.I_getsuper(0, true, true);
        asm.I_findproperty(true, true);
        asm.I_findpropstrict(0, false, false);
        asm.I_initproperty(0, true, false);
        asm.I_setproperty(0, false, true);
        asm.I_setsuper(0, true, true);
        asm.I_hasnext2(0, 0);
        asm.I_lookupswitch(0, [1,2,3]);
        asm.I_newarray(0);
        asm.I_newobject(0);
        asm.I_pushbyte(0);
        asm.I_setslot(0);
    }
}
