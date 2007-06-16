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
    class ABCEmitter
    {
        var abcfile, constants;
        private var scripts = [];

        function ABCEmitter() {
            abcfile = new ABCFile;
            constants = new ABCConstantPool;
            abcfile.addConstants(constants);
        }

        function newScript(): Script {
            var s = new Script(this);
            scripts.push(s);
            return s;
        }

        function finalize() {
            forEach(function (s) { s.finalize() }, scripts);
            return abcfile;
        }
    }

    class Script
    {
        var emitter;
        private var init_method;

        function Script(emitter) {
            this.emitter = emitter;
        }

        function get init(): Method {
            if (!init_method)
                init_method = new Method(emitter, []);
            return init_method;
        }

        function finalize() {
            var id = init.finalize();
            emitter.abcfile.addScript(new ABCScriptInfo(id));
        }
    }

    class Method extends ABCAssembler
    {
        var emitter, formals;

        function Method(emitter, formals) {
            super(emitter.constants, formals.length);
            this.formals = formals;
            this.emitter = emitter;

            // Standard prologue?
            I_getlocal_0();
            I_pushscope();
        }

        function finalize() {
            // Standard epilogue?

            var meth = emitter.abcfile.addMethod(new ABCMethodInfo(0, formals, 0, super.flags));
            var body = new ABCMethodBodyInfo(meth);
            body.setMaxStack(super.maxStack);
            body.setLocalCount(super.maxLocal);
            body.setMaxScopeDepth(super.maxScope);
            body.setCode(this);
            emitter.abcfile.addMethodBody(body);

            return meth;
        }
    }
}
