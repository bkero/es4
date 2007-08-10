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
    import util.*;
    import abcfile.*;
    import assembler.*;
    import emitter.*;

    use namespace Ast;

    /* Returns an ABCFile structure */
    public function cg(tree: PROGRAM) {
        var e = new ABCEmitter;
        var s = e.newScript();
        CTX.prototype = { "emitter": e, "script": s, "cp": e.constants };
        cgProgram(new CTX(s.init.asm, null, s), tree);
        return e.finalize();
    }

    /* A context is a structure with the fields
     *
     *    emitter  -- the unique emitter
     *    script   -- the only script we care about in that emitter
     *    cp       -- the emitter's constant pool
     *    asm      -- the current function's assembler
     *    stk      -- the current function's binding stack (labels, ribs)
     *    target   -- the current trait target
     *
     * All of these are invariant and kept in the prototype except for
     * 'asm', 'stk', and some fields to come.
     *
     * FIXME, there are probably at least two targets: one for LET, another
     * for VAR/CONST/FUNCTION.
     */

    function CTX(asm, stk, target) {
        this.asm = asm;
        this.stk = stk;
        this.target = target;
    }

    function push(ctx, node) {
        node.link = ctx.stk;
        return new CTX(ctx.asm, node, ctx.target);
    }

    function cgProgram(ctx, prog) {
        if (prog.head.fixtures != null)
            cgFixtures(ctx, prog.head.fixtures);
        cgBlock(ctx, prog.block);
    }

    function cgFixtures(ctx, fixtures) {
        let { target:target, asm:asm, emitter:emitter } = ctx;
        for ( let i=0 ; i < fixtures.length ; i++ ) {
            let [fxname, fx] = fixtures[i];
            let name = emitter.fixtureNameToName(fxname);

            switch type (fx) {
            case (vf:ValFixture) {
                target.addTrait(new ABCSlotTrait(name, 0, 0, emitter.fixtureTypeToType(vf)));
            }
            case (mf:MethodFixture) {
                target.addTrait(new ABCOtherTrait(name, 0, TRAIT_Method, 0, cgFunc(ctx, mf.func)));
            }
            case (x:*) { throw "Internal error: unhandled fixture type" }
            }
        }
    }

    function cgBlock(ctx, b) {
        // FIXME -- more here
        let stmts = b.stmts;
        for ( let i=0 ; i < stmts.length ; i++ )
            cgStmt(ctx, stmts[i]);
    }

    function cgDefn(ctx, d) {
        let { asm:asm, emitter:emitter } = ctx;
        switch type (d) {
        case (fd:FunctionDefn) {
            assert( fd.func.name.kind is Ordinary );
            let name = emitter.nameFromIdent(fd.func.name.ident);
            //asm.I_findpropstrict(name); // name is fixture, thus always defined
            //asm.I_newfunction(cgFunc(ctx, fd.func));
            //asm.I_initproperty(name);
        }
        case (vd: VariableDefn) {
            // nothing to do, right?
        }
        case (x:*) { throw "Internal error: unimplemented defn" }
        }
    }

    function extractNamedFixtures(fixtures)
    {
        let named = [];
        for(let i = 0; i < fixtures.length; ++i)
        {
            let [name,fixture] = fixtures[i];
            switch type (name) {
                case (pn:PropName) {
                    named.push([name,fixture]);
                }
                case (tn:TempName) {
                    // do nothing
                }
            }
        }
        return named;
    }
    
    /* Create a method trait in the ABCFile
     * Generate code for the function
     * Return the function index
     */
    function cgFunc({emitter:emitter, script:script}, f:FUNC) {
        function extractType([name,fixture])
            emitter.fixtureTypeToType(fixture);
        
        function extractDefaults(expr)
            emitter.defaultExpr(expr);
            
        let named_fixtures = extractNamedFixtures(f.params.fixtures);
        
        let formals_type = map(extractType, named_fixtures);
        let method = script.newFunction(formals_type);
        let asm = method.asm;
        
        let defaults = map(extractDefaults, f.defaults);
        
        if( defaults.length > 0 )
        {
            method.setDefaults(defaults);
        }

        /* Create a new rib and populate it with the values of all the
         * formals.  Add slot traits for all the formals so that the
         * rib have all the necessary names.  Later code generation
         * will add properties for all local (hoisted) VAR, CONST, and
         * FUNCTION bindings, and they will be added to the rib too,
         * but not initialized here.  (That may have to change, for
         * FUNCTION bindings at least.)
         *
         * FIXME: if a local VAR shadows a formal, there's more
         * elaborate behavior here, and the compiler must perform some
         * analysis and avoid the shadowed formal here.
         *
         * God only knows about the arguments object...
         */
        asm.I_newactivation();
        asm.I_pushscope();
        
        let ctx = new CTX(asm, {tag: "function"}, method);

        cgHead(ctx, f.params);
        /* Generate code for the body.  If there is no return statement in the
         * code then the default behavior of the emitter is to add a returnvoid
         * at the end, so there's nothing to worry about here.
         */
        cgBlock(ctx, f.block);
        return method.finalize();
    }
    
    function cgHead(ctx, head) {
        let {asm:asm, emitter:emitter, target:target} = ctx;
        
        function extractName([name,fixture])
            ctx.emitter.fixtureNameToName(name); //FIXME: shouldn't need ctx.
        
        function extractType([name,fixture])
            ctx.emitter.fixtureTypeToType(fixture); //FIXME: shouldn't need ctx.
        
        let named_fixtures = extractNamedFixtures(head.fixtures);

        let formals = map(extractName, named_fixtures);
        let formals_type = map(extractType, named_fixtures);
        for ( let i=0 ; i < formals.length ; i++ ) {
            target.addTrait(new ABCSlotTrait(formals[i], 0, 0, formals_type[i]));
        }

        // do inits
        cgInits(ctx, head.inits);    
        
    }
    
    function cgInits(ctx, inits, isExpr=false){
        let {asm:asm, emitter:emitter} = ctx;

        let t = -1;
        
        for( let i=0; i < inits.length; ++i ) {
            let [name, init] = inits[i];

            let name_index = emitter.fixtureNameToName(name);

            asm.I_findproperty(name_index);
            cgExpr(ctx, init);
            
            if( isExpr && i == inits.length-1 )
            {
                t = asm.getTemp();
                asm.I_dup();
                asm.I_setlocal(t);
            }
            
            asm.I_setproperty(name_index);
        }

        if( t != -1 )
        {
            asm.I_getlocal(t);
            asm.killTemp(t);
        }
    }
    

    // Handles scopes and finally handlers and returns a label, if appropriate, to
    // branch to.  "tag" is one of "function", "break", "continue"

    function unstructuredControlFlow({stk:stk, asm:asm}, hit, jump, msg) {
        while (stk != null) {
            if (hit(stk)) {
                if (jump)
                    asm.I_jump(stk.target);
                return;
            }
            else {
                // FIXME
                // if there's a FINALLY, visit it here
            }
            stk = stk.link;
        }
        throw msg;
    }


    // The following return extended contexts
    function pushBreak(ctx, labels, target)
        push(ctx, { tag:"break", labels:labels, target:target });

    function pushContinue(ctx, labels, target)
        push(ctx, { tag:"continue", labels:labels, target:target });

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
