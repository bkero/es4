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

/* The structure ctx has the following fields:

      asm: emitter.Method
      lexical: Object 
      ...

   Think of it as a "multi-stack": it's a data structure that
   encapsulates several stacks, and the data structure is handled by
   extension, not update.

   But it uses the prototype chain extensively, so there is no
   structural type to describe it.

   It will tend to be shallow.

   In *practice* it may be faster to have instances with all the
   fields and copy the fields in when a new structure is
   created... depends on how big the structure becomes.
*/

package cogen
{
    import assembler.*;
    import emitter.*;

    /* Returns an array of bytes as delivered by ABCByteStream.getBytes,
     * or throws an exception (maybe).
     */
    public function generateCode(tree) {
        var e = new ABCEmitter;
        //var c = new Cogen(e, tree);
        c.gen(tree);
        return e.finalize().getBytes();
    }

    /*
    function CTX() {}

    function bindAsm(ctx, asm) {
        CTX.prototype = ctx;
        var v = new CTX();
        v.asm = asm;
        return v;
    }

    function bindWith(ctx, c) { ... }
    function bindBreak(ctx, c) { ... }
    function bindContinue(ctx, c) { ... }
    function bindCatch(ctx, c) { ... }

    var baseCTX = {
        bindAsm: bindAsm,
        bindWith: bindWith,
        bindBreak: bindBreak,
        bindContinue: bindContinue,
        bindCatch: bindCatch
    };

    function cgProgram(e, p) {
        var program = e.newScript();
        var newctx = ctx.bindAsm(baseCTX, ctx.program.getInit());
        // for each fixture, define it in newctx
        // ...
        cgStmt(newctx, p.body);
    }
    */

    public function nameFromIdent(ctx, id) {
        let cp = ctx.cp;
        return cp.QName(cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8("")), 
                        cp.stringUtf8(id));
    }

    public function nameFromIdentExpr(ctx, e) {
        use namespace Ast;
        switch type (e) {
        case (id:Identifier) { return nameFromIdent(ctx, id.ident) }
        case (x:*) { throw "Unimplemented: nameFromIdentExpr" }
        }
    }

    // Handles scopes and finally handlers and returns a label, if appropriate, to
    // branch to.  "what" is one of "return", "break", "continue", ...
    public function nonlocalControlFlow(ctx, what, ...rest) {
        // FIXME 
    }
}
