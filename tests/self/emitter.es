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

/* Rough sketch:

   The emitter provides abstractions for common code generation
   patterns, and some arbitrary amount of utilities to help in code
   generation.  The client starts by creating an emitter, then
   creating scripts on that emitter and classes and other traits on
   those scripts, methods on the classes, and so on.  Boilerplate code
   is inserted for you, so code is generated for class creation when
   you create a class on a script.

   These sketches are particularly rough right now because the needs
   of the code generator (verify.es) are not known precisely yet.  But
   I expect that there will be quite a bit of code in here, and it
   will encapsulate a lot of useful knowledge about how things are
   done on the AVM.

   One thing I'm apparently unresolved on here is when to create structures
   in the ABCFile; right now there's a mix of late and early.  Since the
   abcfile is not laid out until it is finalized this matters little, except
   for the sake of clarity.

   Sometimes this OO setup does not seem natural, other times it simplifies...
*/

package emitter
{
    import util.*;
    import abcfile.*;
    import assembler.*;

    public class ABCEmitter
    {
        public var file, constants;
        private var scripts = [];

        function ABCEmitter() {
            file = new ABCFile;
            constants = new ABCConstantPool;
            file.addConstants(constants);
        }

        public function newScript(): Script {
            var s = new Script(this);
            scripts.push(s);
            return s;
        }

        public function finalize() {
            forEach(function (s) { s.finalize() }, scripts);
            return file;
        }
    }

    public class Script
    {
        public var e, init, traits=[];

        function Script(e:ABCEmitter) {
            this.e = e;
            this.init = new Method(e,[]);
        }

        public function newClass(name, basename) {
            return new Class(this, name, basename);
        }

        // Here we probably want: newVar, newConst, newFunction...
        public function addTrait(t) {
            return traits.push(t);
        }

        public function finalize() {
            var id = init.finalize();
            e.file.addScript(new ABCScriptInfo(id));
        }
    }

    /*
    public class Class
    {
        public var s, name, basename;

        function Class(script, name, basename) {
            this.s = script;
            this.name = name;
            this.basename = basename;

            var asm = script.init;
            // Create the class
            asm.I_findpropstrict(Object_name);
            asm.I_getproperty(Object_name);
            asm.I_dup();
            asm.I_pushscope();
            asm.I_newclass(clsidx);
            asm.I_popscope();
            asm.I_getglobalscope();
            asm.I_swap();
            asm.I_initproperty(Fib_name);
        }

        public function newCInit(name) {
            return new Method(e, [], name);
        }

        public function newIInit(formals, name=null) {
            var iinit = new Method(s.e, formals, name);
            iinit.I_getlocal(0);
            iinit.I_constructsuper(0);
            return iinit;
        }

        public function addTrait(t) {
            cls.addTrait(t);
        }

        function finalize() {
            clsidx = file.addClass();
            s.addTrait(new ABCOtherTrait(name, 0, TRAIT_Class, 0, clsidx));
        }
    }
    */

    public class Method // extends AVM2Assembler
    {
        public var e, formals, name, asm;

        function Method(e:ABCEmitter, formals:Array, name=null) {
            asm = new AVM2Assembler(e.constants, formals);
            //super(e.constants, formals.length);
            this.formals = formals;
            this.e = e;
            this.name = name;

            // Standard prologue -- but is this always right?
            asm.I_getlocal_0();
            asm.I_pushscope();
        }

        public function finalize() {
            // Standard epilogue for lazy clients.
            asm.I_returnvoid();

            var meth = e.file.addMethod(new ABCMethodInfo(0, formals, 0, asm.flags));
            var body = new ABCMethodBodyInfo(meth);
            body.setMaxStack(asm.maxStack);
            body.setLocalCount(asm.maxLocal);
            body.setMaxScopeDepth(asm.maxScope);
            body.setCode(asm);
            e.file.addMethodBody(body);

            return meth;
        }
    }
}
