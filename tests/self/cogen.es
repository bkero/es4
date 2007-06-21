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

package cogen
{
    import assembler.*;
    import emitter.*;

    use namespace Ast;

    /* Returns an ABCFile structure */
    public function cg(tree) {
        var e = new ABCEmitter;
        var s = e.newScript();
        CTX.prototype = { "emitter": e, "script":s, "cp": e.constants };
        cgProgram(new CTX(s.init.asm), tree);
        return e.finalize();
    }

    /* A context is a structure with the fields
     * 
     *    emitter
     *    script
     *    asm
     *    cp
     *
     * All of these are invariant and kept in the prototype except for
     * 'asm', and some fields to come.
    */

    function CTX(asm) {
        this.asm = asm;
    }

    function cgProgram(ctx, prog) {
        cgBlock(ctx, prog.block);
    }

    function cgBlock(ctx, b) {
        // FIXME -- more here:
        // If it has local bindings, establish a local rib
        // Inits
        // etc
        let stmts = b.stmts;
        for ( let i=0 ; i < stmts.length ; i++ )
            cgStmt(ctx, stmts[i]);
    }

    /*
    function cgFunctionBody(ctx, f:FUNC) {
        // FIXME
        // allocate a rib
        // store params in that rib
        //
        // there must be a slot-trait for each param and local
        // variable (bound by 'var', 'const', 'function', though not
        // by 'let') in the methodbody so that the rib can be created
        // correctly by the AVM
        asm.I_newactivation();
        asm.I_pushscope();
        // for p in formals, add a slot-trait for p
        let newctx = pushFunction(ctx, <some ABCMethodBody structure>);
    }
    */

    // Handles scopes and finally handlers and returns a label, if appropriate, to
    // branch to.  "what" is one of "function", "break", "continue"
    function nonlocalControlFlow(ctx, what, ...rest) {
        // FIXME 
    }

    // The following return extended contexts
    function pushBreak(ctx, labels, target) {
        // FIXME
    }

    function pushContinue(ctx, labels, target) {
        // FIXME
    }

    function pushFunction(ctx /*more*/) {
        // FIXME
    }

    function pushWith(ctx /*more*/) {
        // FIXME
    }

    function pushLet(ctx /*more*/) {
    }

    function pushCatch(ctx /*more*/) {
        // FIXME
    }

    function pushFinally(ctx /*more*/) {
        // FIXME
    }
}
