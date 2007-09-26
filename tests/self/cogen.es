/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */
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

var CTX_shared;
class CTX {
    var asm, stk, target;
    var emitter, script, cp;

    function CTX (asm, stk, target) {
        this.asm = asm;
        this.stk = stk;
        this.target = target;

        // tamarin hack
        this.emitter = CTX_shared.emitter;
        this.script = CTX_shared.script;
        this.cp = CTX_shared.cp;
    }
}



namespace Gen;
//package cogen
{
    //import util.*;
    //import abcfile.*;
    //import assembler.*;
    //import emitter.*;
    use default namespace Gen;
    use namespace Util;
    use namespace Abc;
    use namespace Asm;
    use namespace Emit;
    use namespace Ast;

    /* Returns an ABCFile structure */
    function cg(tree: PROGRAM) {
    /// function cg(tree: PROGRAM) {
        var e = new ABCEmitter;
        var s = e.newScript();
        // CTX.prototype = { "emitter": e, "script": s, "cp": e.constants };  // tamarin doesn't like initing prototype here
        CTX_shared = { "emitter": e, "script": s, "cp": e.constants };
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


    function push(ctx, node) {
        node.link = ctx.stk;
        return new CTX(ctx.asm, node, ctx.target);
    }

    function cgProgram(ctx, prog) {
        if (prog.head.fixtures != null)
            cgFixtures(ctx, prog.head.fixtures);
        cgBlock(ctx, prog.block);
    }

    function hasTrait(traits, name, kind) {
        for(var i = 0, l =traits.length; i < l; i++) {
            let t = traits[i];
            if(t.name==name && ((t.kind&15)==kind))
                return true;
        }
        return false;
    }
    
    function cgFixtures(ctx, fixtures) {
        let { target:target, asm:asm, emitter:emitter } = ctx;
        let methidx, trait_kind, clsidx;
        for ( let i=0 ; i < fixtures.length ; i++ ) {
            let [fxname, fx] = fixtures[i];
            let name = emitter.fixtureNameToName(fxname);

            /// switch type (fx) {
            /// case (fx:ValFixture) {
            if (fx is ValFixture) {
                if( !hasTrait(target.traits, name, TRAIT_Slot) )
                    target.addTrait(new ABCSlotTrait(name, 0, false, 0, emitter.typeFromTypeExpr(fx.type), 0, 0)); 
					// FIXME when we have more general support for type annos
            }
            /// case (fx:MethodFixture) {
            else if (fx is MethodFixture) {
                var initScopeDepth = (ctx.stk!=null && ctx.stk.tag=="instance")?2:0;
                methidx = cgFunc(ctx, fx.func, initScopeDepth);
                /// switch type (target) {
                /// case (m:Method) {
                if (target is Method) {
                    target.addTrait(new ABCSlotTrait(name, 0, false, 0, 0, 0, 0)); 
                    asm.I_findpropstrict(name);
                    asm.I_newfunction(methidx);
                    asm.I_setproperty(name);
                }
                /// case (x:*) {
                else {
                    // target.addTrait(new ABCOtherTrait(name, 0, TRAIT_Method, 0, methidx));
                    trait_kind = TRAIT_Method;
                    /// switch type(fx.func.name.kind) {
                    /// case (g:Get) {
                    if (fx.func.name.kind is Get) {
                        print("Getter, target: " + target);
                        trait_kind = TRAIT_Getter;
                    }
                    /// case (s:Set) {
                    else if (fx.func.name.kind is Set) {
                        print("Setter, target: " +target);
                        trait_kind = TRAIT_Setter;
                    }
                    /// }
                    target.addTrait(new ABCOtherTrait(name, 0, trait_kind, 0, methidx));
                }
                /// }
            }
            /// case (fx:ClassFixture) {
            else if (fx is ClassFixture) {
                clsidx = cgClass(ctx, fx.cls);
                target.addTrait(new ABCOtherTrait(name, 0, TRAIT_Class, 0, clsidx));
            }
            /// case (fx:NamespaceFixture) {
            else if (fx is NamespaceFixture) {
                target.addTrait(new ABCSlotTrait(name, 0, true, 0, emitter.qname({ns:new PublicNamespace(""), id:"Namespace"},false), emitter.namespace(fx.ns), CONSTANT_Namespace));
            }
            /// case (fx:TypeFixture) {
            else if (fx is TypeFixture) {
                print ("warning: ignoring type fixture");
            }
            /// case (fx:*) { 
            else {
                throw "Internal error: unhandled fixture type" 
            }
            /// }
        }
    }

    function cgBlock(ctx, b) {
        // FIXME -- more here
        cgHead(ctx, b.head);
        let stmts = b.stmts;
        for ( let i=0 ; i < stmts.length ; i++ )
            cgStmt(ctx, stmts[i]);
    }

/*
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
*/

    function extractNamedFixtures(fixtures)
    {
        let named = [];
        let fix_length = fixtures ? fixtures.length : 0;
        for(let i = 0; i < fix_length; ++i)
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
    
    function cgClass(ctx, c) {
        
        let {asm:asm, emitter:emitter, script:script} = ctx;
        
        let classname = emitter.qname(c.name,false);
        let basename = c.baseName != null ? emitter.qname(c.baseName,false) : 0;
        
        let cls = script.newClass(classname, basename);
/*        
        let c_ctx = new CTX(asm, {tag:"class"}, cls);
        cgHead(c_ctx, c.classHead);
*/      
        let inst = cls.getInstance();
        
        // Context for the instance
        let i_ctx = new CTX(asm, {tag:"instance"}, inst);
        
        // do instance slots
        cgFixtures(i_ctx, c.instanceHead.fixtures);  // FIXME instanceHead and instanceInits should be unified
        
        inst.setIInit(cgCtor(i_ctx, c.constructor, {fixtures:[],exprs:c.instanceHead.exprs}));
        
        var clsidx = cls.finalize();
        var Object_name = emitter.qname({ns:new PublicNamespace(""), id:"Object"},false);

        asm.I_findpropstrict(Object_name);
        asm.I_getproperty(Object_name);
        asm.I_dup();
        asm.I_pushscope();
        asm.I_newclass(clsidx);
        asm.I_popscope();
        asm.I_getglobalscope();
        asm.I_swap();
        asm.I_initproperty(classname);

        return clsidx;
    }
    
    /*  
     *  Generate code for a ctor.
     */
    function cgCtor(ctx, c, instanceInits) {
        let formals_type = extractFormalTypes(ctx, c.func);
        let method = new Method(ctx.script.e, formals_type, 2, "$construct", false);
        let asm = method.asm;

        let defaults = extractDefaultValues(ctx, c.func);
        if( defaults.length > 0 )
        {
            method.setDefaults(defaults);
        }
        
        let t = asm.getTemp();
        let ctor_ctx = new CTX(asm, {tag:"function", scope_reg:t}, method);
       
        asm.I_getlocal(0);
        asm.I_dup();
        // Should this be instanceInits.inits only?
        asm.I_pushscope();  // This isn't quite right...
        for( let i = 0; i < instanceInits.length; i++ ) {
            cgExpr(ctor_ctx, instanceInits[i]);
            asm.I_pop();
        }
        cgHead(ctor_ctx, instanceInits);
        asm.I_popscope();
        //cgHead(ctor_ctx, instanceInits.inits, true);

        // Push 'this' onto scope stack
        //asm.I_getlocal(0);
        //asm.I_pushscope();
        // Create the activation object, and initialize params
        asm.I_newactivation();
        asm.I_dup();
        asm.I_setlocal(t);
        asm.I_dup();
        asm.I_pushwith();
        cgHead(ctor_ctx, c.func.params);

        for ( let i=0 ; i < c.settings.length ; i++ ) {
            cgExpr(ctor_ctx, c.settings[i]);
            asm.I_pop();
        }

        // Eval super args, and call super ctor
        asm.I_getlocal(0);
        let nargs = c.superArgs.length;
        for ( let i=0 ; i < nargs ; i++ )
            cgExpr(ctx, c.superArgs[i]);
        asm.I_constructsuper(nargs);
        
        asm.I_popscope();
        asm.I_getlocal(0);
        asm.I_pushscope();  //'this'
        asm.I_pushscope();  //'activation'
        
        cgHead(ctor_ctx, c.func.vars);

        cgBlock(ctor_ctx, c.func.block);
        
        asm.I_kill(t);
        return method.finalize();
    }

    function extractFormalTypes({emitter:emitter, script:script}, f:Func) {
        function extractType([name,fixture])
            emitter.fixtureTypeToType(fixture);
        
        let named_fixtures = extractNamedFixtures(f.params.fixtures);
        
        return map(extractType, named_fixtures);
    }
        
    function extractDefaultValues({emitter:emitter, script:script}, f:Func) {
        function extractDefaults(expr)
            emitter.defaultExpr(expr);

        return map(extractDefaults, f.defaults);
    }
    
    /* Create a method trait in the ABCFile
     * Generate code for the function
     * Return the function index
     */
    function cgFunc({emitter:emitter, script:script}, f:FUNC, initScopeDepth) {
        let formals_type = extractFormalTypes({emitter:emitter, script:script}, f);
        let method = script.newFunction(formals_type,initScopeDepth);
        let asm = method.asm;

        let defaults = extractDefaultValues({emitter:emitter, script:script}, f);
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
        let t = asm.getTemp();
        asm.I_newactivation();
        asm.I_dup();
        asm.I_setlocal(t);
        asm.I_pushscope();
        
        let ctx = new CTX(asm, {tag: "function", scope_reg:t, has_scope:true}, method);

        cgHead(ctx, f.params);

        cgHead(ctx, f.vars);
        
        /* Generate code for the body.  If there is no return statement in the
         * code then the default behavior of the emitter is to add a returnvoid
         * at the end, so there's nothing to worry about here.
         */
        cgBlock(ctx, f.block);
        asm.I_kill(t);
        return method.finalize();
    }
    
    function cgHead(ctx, head) {
        let {asm:asm, emitter:emitter, target:target} = ctx;
        
        function extractName([name,fixture])
            ctx.emitter.fixtureNameToName(name); //FIXME: shouldn't need ctx.
        
        function extractType([name,fixture])
            ctx.emitter.fixtureTypeToType(fixture); //FIXME: shouldn't need ctx.
        
        let named_fixtures = extractNamedFixtures(head.fixtures);
/*
        let formals = map(extractName, named_fixtures);
        let formals_type = map(extractType, named_fixtures);
        for ( let i=0 ; i < formals.length ; i++ ) {
            if(!hasTrait(target.traits, formals[i], TRAIT_Slot) )
                target.addTrait(new ABCSlotTrait(formals[i], 0, false, 0, formals_type[i]));
        }
*/
        cgFixtures(ctx, named_fixtures);
        for ( let i=0 ; i < head.exprs.length ; i++ ) {
            cgExpr(ctx, head.exprs[i]);
            asm.I_pop();
        }
    }
    
    function cgInits(ctx, inits, baseOnStk){
        let {asm:asm, emitter:emitter} = ctx;

        let t = -1;
        let inits_length = inits?inits.length:0;
        for( let i=0; i < inits_length; ++i ) {
            let [name, init] = inits[i];

            let name_index = emitter.fixtureNameToName(name);

            if( baseOnStk ) {
                if(i < inits_length-1)
                    asm.I_dup();
            }
            else
                asm.I_findproperty(name_index);
            
            cgExpr(ctx, init);
            asm.I_setproperty(name_index);
        }
        if( inits_length == 0 && baseOnStk )
        {
            asm.I_pop();
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
                if(stk.has_scope) {
                    asm.I_popscope();
                }
                // FIXME
                // if there's a FINALLY, visit it here
            }
            stk = stk.link;
        }
        throw msg;
    }

    function restoreScopes({stk:stk, asm:asm}) {
        while (stk != null) {
            if(stk.has_scope) {
                asm.I_getlocal(stk.scope_reg);
                asm.I_pushscope();
            }
            if( stk.tag != "function" ) {
                stk = stk.link;
            }
            else {
                stk = null;
            }
        }
    }

    // The following return extended contexts
    function pushBreak(ctx, labels, target)
        push(ctx, { tag:"break", labels:labels, target:target, has_scope:false });

    function pushContinue(ctx, labels, target)
        push(ctx, { tag:"continue", labels:labels, target:target, has_scope:false });

    function pushFunction(ctx /*more*/) {
        // FIXME
    }

    function pushWith(ctx /*more*/) {
        // FIXME
    }

    function pushLet(ctx /*more*/) {
    }

    function pushCatch(ctx, scope_reg )
        push(ctx, {tag:"catch", has_scope:true, scope_reg:scope_reg});
        // FIXME anything else?

    function pushFinally(ctx /*more*/) {
        // FIXME
    }
}
