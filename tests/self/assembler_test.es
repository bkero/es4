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

namespace Asm;
{
    use default namespace Asm;
    
    use namespace Util;
    use namespace Abc;
    
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

        dumpABCFile(file, "hello-test.abc");
    }
    
    testHelloWorld();

}
