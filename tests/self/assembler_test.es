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

package assembler
{
    import util.*;
    import bytestream.*;
    import abcfile.*;

    public function runTests() {
        testCoverage();
    }

    function testCoverage() {
        var cp = new ABCConstantPool();
        var asm = new AVM2Assembler(cp,0);

        var print_name = cp.QName(cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8("")),
                                  cp.stringUtf8("print"));

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
        asm.I_kill(0);
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
        asm.I_ifeq();
        asm.I_ifge();
        asm.I_ifgt();
        asm.I_ifle();
        asm.I_iflt();
        asm.I_ifne();
        asm.I_ifnge();
        asm.I_ifngt();
        asm.I_ifnle();
        asm.I_ifnlt();
        asm.I_ifstricteq();
        asm.I_ifstrictne();
        asm.I_iffalse();
        asm.I_iftrue();
        asm.I_jump();
        asm.I_call(0);
        asm.I_construct(0);
        asm.I_constructsuper(0);
        asm.I_callmethod(print_name, 0);
        asm.I_callstatic(print_name, 0);
        asm.I_callsuper(print_name, 0);
        asm.I_callproperty(print_name, 0);
        asm.I_constructprop(print_name, 0);
        asm.I_callproplex(print_name, 0);
        asm.I_callsupervoid(print_name, 0);
        asm.I_callpropvoid(print_name, 0);
        asm.I_debug(0, 0, 0, 0);
        asm.I_deleteproperty(print_name);
        asm.I_getdescendants(print_name);
        asm.I_getproperty(print_name);
        asm.I_getsuper(print_name);
        asm.I_findproperty(print_name);
        asm.I_findpropstrict(print_name);
        asm.I_initproperty(print_name);
        asm.I_setproperty(print_name);
        asm.I_setsuper(print_name);
        asm.I_hasnext2(0, 0);
        asm.I_lookupswitch(undefined, [,,,]);
        asm.I_newarray(0);
        asm.I_newobject(0);
        asm.I_pushbyte(0);
        asm.I_setslot(0);
    }

    /* Test case:
         var a = 4;
         print((let (a=1) a) + (let (a=2) a) + a)
       Should print 7
    */
    public function testLet() {
        var file = new ABCFile();
        var cp = new ABCConstantPool();

        file.addConstants(cp);

        var ns = cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8(""));
        var a_name = cp.QName(ns, cp.stringUtf8("a"));
        var print_name = cp.QName(ns, cp.stringUtf8("print"));

        var asm = new AVM2Assembler(cp,0);

        var method = new ABCMethodInfo(0, [], 0);
        var meth = file.addMethod(method);
        var body = new ABCMethodBodyInfo(meth);

        asm.I_getlocal_0();
        asm.I_pushscope();
        asm.I_getscopeobject(0);
        asm.I_pushundefined();
        asm.I_setproperty(a_name);
        var L1 = asm.I_jump();

        var L0 = asm.I_label();
        asm.I_returnvoid();
        var exn = body.addException(new ABCException(0, 0, L0.address, 0, a_name));

        asm.I_label(L1);
        asm.I_findpropstrict(a_name);
        asm.I_pushbyte(4);
        asm.I_setproperty(a_name);

        asm.I_newcatch(exn);
        asm.I_pushscope();
        asm.I_findpropstrict(a_name);
        asm.I_pushbyte(1);
        asm.I_setproperty(a_name);
        asm.I_findpropstrict(a_name);
        asm.I_getproperty(a_name);
        asm.I_popscope();

        asm.I_newcatch(exn);
        asm.I_pushscope();
        asm.I_findpropstrict(a_name);
        asm.I_pushbyte(2);
        asm.I_setproperty(a_name);
        asm.I_findpropstrict(a_name);
        asm.I_getproperty(a_name);
        asm.I_popscope();

        asm.I_add();

        asm.I_findpropstrict(a_name);
        asm.I_getproperty(a_name);

        asm.I_add();

        asm.I_findpropstrict(print_name);
        asm.I_swap();
        asm.I_callpropvoid(print_name, 1);
        asm.I_returnvoid();

        method.setFlags(asm.flags|METHOD_Activation);
        body.setMaxStack(asm.maxStack);
        body.setLocalCount(asm.maxLocal);
        body.setMaxScopeDepth(asm.maxScope);
        body.setCode(asm);
        file.addMethodBody(body);

        file.addScript(new ABCScriptInfo(meth));

        dumpABCFile(file, "let-test.es");
    }

    /*
      class Fib {
          function Fib(n) {
             if (n < 2)
                 val = n;
             else
                 val = (new Fib(n-1)).val + (new Fib(n-2)).val
          }
          var val;
      }
      print((new Fib(10)).val)
    */
    public function testFib() {
        var file = new ABCFile();
        var cp = new ABCConstantPool();

        file.addConstants(cp);

        var ns = cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8(""));
        var n_name = cp.QName(ns, cp.stringUtf8("n"));
        var print_name = cp.QName(ns, cp.stringUtf8("print"));
        var val_name = cp.QName(ns, cp.stringUtf8("val"));
        var Fib_name = cp.QName(ns, cp.stringUtf8("Fib"));
        var Object_name = cp.QName(ns, cp.stringUtf8("Object"));

        function mkClassInit() {
            var formals = [];
            var asm = new AVM2Assembler(cp,formals.length);
            asm.I_returnvoid();

            var meth = file.addMethod(new ABCMethodInfo(cp.stringUtf8("Fib$cinit"), formals, 0, asm.flags));
            var body = new ABCMethodBodyInfo(meth);
            body.setMaxStack(asm.maxStack);
            body.setLocalCount(asm.maxLocal);
            body.setMaxScopeDepth(asm.maxScope);
            body.setCode(asm);
            file.addMethodBody(body);

            return meth;
        }

        function mkInstanceInit() {
            var formals = [0];
            var asm = new AVM2Assembler(cp,formals.length);
            asm.I_getlocal(0);
            asm.I_pushscope();

            asm.I_getlocal(0);
            asm.I_constructsuper(0);

            asm.I_getlocal(1);  // n
            asm.I_pushbyte(2);
            var L0 = asm.I_iflt();
            asm.I_findpropstrict(Fib_name);
            asm.I_getlocal(1);
            asm.I_pushbyte(1);
            asm.I_subtract();
            asm.I_constructprop(Fib_name, 1);
            asm.I_getproperty(val_name);
            asm.I_findpropstrict(Fib_name);
            asm.I_getlocal(1);
            asm.I_pushbyte(2);
            asm.I_subtract();
            asm.I_constructprop(Fib_name, 1);
            asm.I_getproperty(val_name);
            asm.I_add();
            var L1 = asm.I_jump();
            asm.I_label(L0);
            asm.I_getlocal(1);
            asm.I_label(L1);
            asm.I_findpropstrict(val_name);
            asm.I_swap();
            asm.I_setproperty(val_name);
            asm.I_returnvoid();

            var meth = file.addMethod(new ABCMethodInfo(cp.stringUtf8("Fib$iinit"), formals, 0, asm.flags));
            var body = new ABCMethodBodyInfo(meth);
            body.setMaxStack(asm.maxStack);
            body.setLocalCount(asm.maxLocal);
            body.setMaxScopeDepth(asm.maxScope);
            body.setCode(asm);
            file.addMethodBody(body);

            return meth;
        }

        var asm = new AVM2Assembler(cp,0);
        asm.I_getlocal_0();
        asm.I_pushscope();

        // Create the 'Fib' class
        var cls = new ABCClassInfo;
        cls.setCInit(mkClassInit());
        var clsidx = file.addClass(cls);

        var inst= new ABCInstanceInfo(Fib_name, Object_name, 0, 0, []);
        inst.setIInit(mkInstanceInit());
        inst.addTrait(new ABCSlotTrait(val_name, 0));
        file.addInstance(inst);

        asm.I_findpropstrict(Object_name);
        asm.I_getproperty(Object_name);
        asm.I_dup();
        asm.I_pushscope();
        asm.I_newclass(clsidx);
        asm.I_popscope();
        asm.I_getglobalscope();
        asm.I_swap();
        asm.I_initproperty(Fib_name);

        // Main program
        var reg = asm.getTemp();
        asm.I_findpropstrict(Fib_name);
        asm.I_pushbyte(10);
        asm.I_constructprop(Fib_name, 1);
        asm.I_getproperty(val_name);
        asm.I_setlocal(reg);
        asm.I_findpropstrict(print_name);
        asm.I_getlocal(reg);
        asm.I_callpropvoid(print_name, 1);
        asm.I_returnvoid();

        var meth = file.addMethod(new ABCMethodInfo(cp.stringUtf8("$main"), [], 0, asm.flags));
        var body = new ABCMethodBodyInfo(meth);
        body.setMaxStack(asm.maxStack);
        body.setLocalCount(asm.maxLocal);
        body.setMaxScopeDepth(asm.maxScope);
        body.setCode(asm);
        file.addMethodBody(body);

        var script = new ABCScriptInfo(meth);
        script.addTrait(new ABCOtherTrait(Fib_name, 0, TRAIT_Class, 0, clsidx));
        file.addScript(script);

        dumpABCFile(file, "fib-test.es");
    }

    /* function f(n, m) { return n+m }
       print(f(3,4));
    */
    public function testFn() {
        var file = new ABCFile();
        var cp = new ABCConstantPool();

        file.addConstants(cp);

        var ns = cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8(""));
        var n_name = cp.QName(ns, cp.stringUtf8("n"));
        var m_name = cp.QName(ns, cp.stringUtf8("m"));
        var print_name = cp.QName(ns, cp.stringUtf8("print"));
        var f_name = cp.QName(ns, cp.stringUtf8("f"));

        function mkFunction() {
            var formals = [0, 0];
            var asm = new AVM2Assembler(cp,formals.length);
            asm.I_getlocal(0);
            asm.I_pushscope();

            // Create and populate rib
            asm.I_newactivation();
            asm.I_dup();
            asm.I_pushscope();
            asm.I_dup();
            asm.I_getlocal(1);
            asm.I_setproperty(m_name);
            asm.I_getlocal(2);
            asm.I_setproperty(n_name);

            asm.I_findpropstrict(n_name);
            asm.I_getproperty(n_name);
            asm.I_findpropstrict(m_name);
            asm.I_getproperty(m_name);
            asm.I_add();
            asm.I_returnvalue();

            var meth = file.addMethod(new ABCMethodInfo(f_name, formals, 0, asm.flags));
            var body = new ABCMethodBodyInfo(meth);
            body.setMaxStack(asm.maxStack);
            body.setLocalCount(asm.maxLocal);
            body.setMaxScopeDepth(asm.maxScope);
            body.setCode(asm);
            print("There");
            body.addTrait(new ABCSlotTrait(m_name, 0));
            body.addTrait(new ABCSlotTrait(n_name, 0));
            print("Here");
            file.addMethodBody(body);

            return meth;
        }

        var asm = new AVM2Assembler(cp,0);
        asm.I_getlocal_0();
        asm.I_pushscope();

        var fn = mkFunction();
        asm.I_findproperty(f_name); // arguably getglobalscope?
        asm.I_newfunction(fn);
        asm.I_initproperty(f_name);

        // Main program
        var reg = asm.getTemp();
        asm.I_findpropstrict(print_name);
        asm.I_getproperty(print_name);
        asm.I_pushnull();
        asm.I_findpropstrict(f_name);
        asm.I_getproperty(f_name);
        asm.I_pushnull();
        asm.I_pushbyte(3);
        asm.I_pushbyte(4);
        asm.I_call(2);
        asm.I_call(1);
        asm.I_pop();
        asm.I_returnvoid();

        var meth = file.addMethod(new ABCMethodInfo(cp.stringUtf8("$main"), [], 0, asm.flags));
        var body = new ABCMethodBodyInfo(meth);
        body.setMaxStack(asm.maxStack);
        body.setLocalCount(asm.maxLocal);
        body.setMaxScopeDepth(asm.maxScope);
        body.setCode(asm);
        file.addMethodBody(body);

        var script = new ABCScriptInfo(meth);
        // AVM BUG: the AVM barfs on this, reports "unsupported traits kind"
        // Doesn't look necessary anyway.
        //        script.addTrait(new ABCOtherTrait(f_name, 0, TRAIT_Function, 0, fn));
        file.addScript(script);

        dumpABCFile(file, "fn-test.es");
    }

    public function testSwitch1() {
        var file = new ABCFile();
        var cp = new ABCConstantPool();

        file.addConstants(cp);

        var print_name = cp.QName(cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8("")),
                                  cp.stringUtf8("print"));

        var asm = new AVM2Assembler(cp,0);
        asm.I_getlocal_0();
        asm.I_pushscope();

        // First case: all the labels are defined before the lookupswitch
        var Ls = asm.I_jump();
        var L0 = asm.I_label();
        asm.I_findpropstrict(print_name);
        asm.I_pushstring(cp.stringUtf8("zero!"));
        asm.I_callpropvoid(print_name, 1);
        var Lx = asm.I_jump();
        var L1 = asm.I_label();
        asm.I_findpropstrict(print_name);
        asm.I_pushstring(cp.stringUtf8("one!"));
        asm.I_callpropvoid(print_name, 1);
        asm.I_jump(Lx);
        var L2 = asm.I_label();
        asm.I_findpropstrict(print_name);
        asm.I_pushstring(cp.stringUtf8("default!"));
        asm.I_callpropvoid(print_name, 1);
        asm.I_jump(Lx);
        asm.I_label(Ls);
        asm.I_pushbyte(1);
        asm.I_lookupswitch(L2,[L0,L1]);
        asm.I_label(Lx);
        asm.I_returnvoid();

        var meth = file.addMethod(new ABCMethodInfo(0, [], 0, asm.flags));
        var body = new ABCMethodBodyInfo(meth);
        body.setMaxStack(asm.maxStack);
        body.setLocalCount(asm.maxLocal);
        body.setMaxScopeDepth(asm.maxScope);
        body.setCode(asm);
        file.addMethodBody(body);

        file.addScript(new ABCScriptInfo(meth));

        dumpABCFile(file, "switch1-test.es");
    }

    public function testSwitch2() {
        var file = new ABCFile();
        var cp = new ABCConstantPool();

        file.addConstants(cp);

        var print_name = cp.QName(cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8("")),
                                  cp.stringUtf8("print"));

        var asm = new AVM2Assembler(cp,0);
        asm.I_getlocal_0();
        asm.I_pushscope();

        // Second case: the labels are defined after the lookupswitch
        asm.I_pushbyte(1);
        var Lx = asm.I_jump();
        asm.I_label(Lx);
        var labels = [,,];
        var L2 = asm.I_lookupswitch(undefined,labels);
        var L0 = labels[0];
        var L1 = labels[1];

        asm.I_label(L0);
        asm.I_findpropstrict(print_name);
        asm.I_pushstring(cp.stringUtf8("zero!"));
        asm.I_callpropvoid(print_name, 1);
        var Lx = asm.I_jump();
        asm.I_label(L1);
        asm.I_findpropstrict(print_name);
        asm.I_pushstring(cp.stringUtf8("one!"));
        asm.I_callpropvoid(print_name, 1);
        asm.I_jump(Lx);
        asm.I_label(L2);
        asm.I_findpropstrict(print_name);
        asm.I_pushstring(cp.stringUtf8("default!"));
        asm.I_callpropvoid(print_name, 1);
        asm.I_label(Lx);
        asm.I_returnvoid();

        var meth = file.addMethod(new ABCMethodInfo(0, [], 0, asm.flags));
        var body = new ABCMethodBodyInfo(meth);
        body.setMaxStack(asm.maxStack);
        body.setLocalCount(asm.maxLocal);
        body.setMaxScopeDepth(asm.maxScope);
        body.setCode(asm);
        file.addMethodBody(body);

        file.addScript(new ABCScriptInfo(meth));

        dumpABCFile(file, "switch2-test.es");
    }

    public function testLoop() {
        var file = new ABCFile();
        var cp = new ABCConstantPool();

        file.addConstants(cp);

        var print_name = cp.QName(cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8("")),
                                  cp.stringUtf8("print"));

        // Local 1 has a counter
        var asm = new AVM2Assembler(cp,0);
        asm.I_getlocal_0();
        asm.I_pushscope();
        var reg = asm.getTemp();
        asm.I_pushbyte(10);
        asm.I_setlocal(reg);
        var L0 = asm.I_label();
        asm.I_getlocal(reg);
        asm.I_pushbyte(0);
        var L1 = asm.I_ifeq();
        asm.I_findpropstrict(print_name);
        asm.I_getlocal(reg);
        asm.I_callpropvoid(print_name, 1);
        asm.I_declocal_i(reg);
        asm.I_jump(L0);
        asm.I_label(L1);
        asm.killTemp(reg);
        asm.I_returnvoid();

        var meth = file.addMethod(new ABCMethodInfo(0, [], 0, asm.flags));
        var body = new ABCMethodBodyInfo(meth);
        body.setMaxStack(asm.maxStack);
        body.setLocalCount(asm.maxLocal);
        body.setMaxScopeDepth(asm.maxScope);
        body.setCode(asm);
        file.addMethodBody(body);

        file.addScript(new ABCScriptInfo(meth));

        dumpABCFile(file, "loop-test.es");
    }

    public function testHelloWorld() {
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

        var asm = new AVM2Assembler(cp,0);
        asm.I_getlocal_0();
        asm.I_pushscope();
        asm.I_findpropstrict(print_name);
        asm.I_pushstring(cp.stringUtf8("Hello, world!"));
        asm.I_callpropvoid(print_name, 1);
        asm.I_returnvoid();

        var meth = file.addMethod(new ABCMethodInfo(0, [], 0, asm.flags));
        var body = new ABCMethodBodyInfo(meth);
        body.setMaxStack(asm.maxStack);
        body.setLocalCount(asm.maxLocal);
        body.setMaxScopeDepth(asm.maxScope);
        body.setCode(asm);
        file.addMethodBody(body);

        file.addScript(new ABCScriptInfo(meth));

        dumpABCFile(file, "hello-test.es");
    }

}
