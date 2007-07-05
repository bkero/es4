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

package emitter
{
    import util.*;
    import abcfile.*;
    import assembler.*;

    public function runTests() {
        testHello();
        testFib();
    }

    function testHello() {
        var e    = new ABCEmitter();
        var cp   = e.constants;
        var ini  = e.newScript().init;

        var nm = cp.QName(cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8("")),
                          cp.stringUtf8("print"));

        ini.I_findpropstrict(nm);
        ini.I_pushstring(cp.stringUtf8("Hello, world!"));
        ini.I_callpropvoid(nm, 1);
        ini.I_returnvoid();

        loadAndRunABCFile(e.finalize());
    }

    function testFib() {
        var e    = new ABCEmitter();
        var cp   = e.constants;

        var ns = cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8(""));
        var n_name = cp.QName(ns, cp.stringUtf8("n"));
        var print_name = cp.QName(ns, cp.stringUtf8("print"));
        var val_name = cp.QName(ns, cp.stringUtf8("val"));
        var Fib_name = cp.QName(ns, cp.stringUtf8("Fib1")); // avoid clashes with other Fib classes in the tests
        var Object_name = cp.QName(ns, cp.stringUtf8("Object"));

        function mkInstanceInit(asm) {
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
        }

        var s = e.newScript("$main");

        var c = s.newClass(Fib_name, Object_name);
        c.newInstanceTrait(new ABCSlotTrait(val_name, 0));

        c.newCInit("Fib$cinit");
        mkInstanceInit(c.newIInit([0], "Fib"));

        // Main program
        var ini = s.init;
        var reg = ini.getTemp();
        ini.I_findpropstrict(Fib_name);
        ini.I_pushbyte(10);
        ini.I_constructprop(Fib_name, 1);
        ini.I_getproperty(val_name);
        ini.I_setlocal(reg);
        ini.I_findpropstrict(print_name);
        ini.I_getlocal(reg);
        ini.I_callpropvoid(print_name, 1);
        ini.I_returnvoid();

        loadAndRunABCFile(e.finalize());
    }
}
