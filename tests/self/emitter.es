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

namespace Emit;
//package emitter
{
    use default namespace Emit;
    use namespace Util;
    use namespace Abc;
    use namespace Asm;
    //import util.*;
    //import abcfile.*;
    //import assembler.*;

    use namespace Ast;

    public class ABCEmitter
    {
        public var file, constants;
        /*private*/ var scripts = [];

        function ABCEmitter() {
            file = new ABCFile;
            constants = new ABCConstantPool;
            file.addConstants(constants);
            Object_name = nameFromIdent("Object");
            Array_name = nameFromIdent("Array");
            RegExp_name = nameFromIdent("RegExp");
        }

        public function newScript(): Script {
            var s = new Script(this);
            scripts.push(s);
            return s;
        }

        public function finalize() {
            function f(s) { s.finalize() }
            forEach(f, scripts);
            return file;
        }

        public var Object_name;
        public var Array_name;
        public var RegExp_name;
        public var meta_construct_name;

        public function namespace( ns:NAMESPACE ) {
            switch type ( ns ) {
                case (int_ns:IntrinsicNamespace) {
                    return constants.namespace(CONSTANT_Namespace, constants.stringUtf8("intrinsic"));  // FIXME
                }
                case (on:OperatorNamespace) {
                    throw ("Unimplemented namespace OperatorNamespace");
                }
                case (pn:PrivateNamespace) {
                    return constants.namespace(CONSTANT_PrivateNamespace, constants.stringUtf8(pn.name));
                }
                case (pn:ProtectedNamespace) {
                    return constants.namespace(CONSTANT_ProtectedNamespace, constants.stringUtf8(pn.name));
                }
                case (pn:PublicNamespace) {
                    return constants.namespace(CONSTANT_Namespace, constants.stringUtf8(pn.name));
                }
                case (int_ns:InternalNamespace) {
                    return constants.namespace(CONSTANT_PackageInternalNS, constants.stringUtf8(pn.name));
                }
                case (un:UserNamespace) {
                    /// return constants.namespace(CONSTANT_ExplicitNamespace, constants.stringUtf8(pn.name));
                    return constants.namespace(CONSTANT_Namespace, constants.stringUtf8(un.name));
                }
                case (an:AnonymousNamespace) {
                    /// return constants.namespace(CONSTANT_PackageInternalNS, constants.stringUtf8(an.name));
                    return constants.namespace(CONSTANT_Namespace, constants.stringUtf8(an.name));
                }
                case (imp_ns:ImportNamespace) {
                    throw ("Unimplemented namespace ImportNamespace");
                }
                case (x:*) {
                    throw ("Unimplemented namespace " + ns);
                }
            }
        }
        function flattenNamespaceSet(nss /*:[[NAMESPACE]]*/) {
            var new_nss = [];
            for( let i = 0; i <nss.length; i++ ) {
                let temp = nss[i];
                for( let q = 0; q < temp.length; q++) {
                    new_nss.push(namespace(temp[q]));
                } 
            } 
            return new_nss;
        }
        public function multiname({ nss:nss, ident:ident }, is_attr) {
            return constants.Multiname(constants.namespaceset(flattenNamespaceSet(nss)), constants.stringUtf8(ident), is_attr);
        }
        public function qname({ns:ns, id:id}, is_attr ) {
            return constants.QName(namespace(ns), constants.stringUtf8(id), is_attr);
        }
        public function nameFromIdent(id) {
            return constants.QName(constants.namespace(CONSTANT_PackageNamespace, constants.stringUtf8("")),
                                   constants.stringUtf8(id),false);
        }

        public function multinameL({nss:nss}, is_attr) {
            return constants.MultinameL(constants.namespaceset(flattenNamespaceSet(nss)), is_attr);
        }

        public function nameFromIdentExpr(e) {
            use namespace Ast;
            switch type (e) {
            case (id:Identifier) { return multiname(id,false) }
            case (qi:QualifiedIdentifier) { 
                switch type(qi.qual) {
                    case( lr:LexicalRef ) {
                        // Hack to deal with namespaces for now...
                        // later we will have to implement a namespace lookup to resolve qualified typenames
                        return qname({ns:new AnonymousNamespace(lr.ident.ident), id:qi.ident}, false)
                    }
                    case( e:* ) {
                        throw ("Unimplemented: nameFromIdentExpr " + e);
                    }
                }
                return multiname(id,false) 
            }
            case (x:*) { throw ("Unimplemented: nameFromIdentExpr " + e) }
            }
        }
        
        public function rtqname({ident:ident}, is_attr) {
            return constants.RTQName(constants.stringUtf8(ident), is_attr);
        }

        public function typeFromTypeExpr(t) {
            use namespace Ast;
            // not dealing with types for now
            switch type (t) {
                case (tn:TypeName) {
                    switch type( tn.ident ){
                        case(i:Identifier) {
                            let name = i.ident;
                            if( name=="String" || name=="Number" ||
                                name=="Boolean" || name=="int" ||
                                name=="uint" || name=="Object" ||
                                name=="Array" || name=="Class" ||
                                name=="Function") {
                                return nameFromIdent(name);
                            }
                            else if( name=="string" ) {
                                return nameFromIdent("String");
                            }
                            else if( name=="boolean" ) {
                                return nameFromIdent("Boolean");
                            }
		            else {
                                print ("warning: unknown type name " + t + ", using Object");
				return nameFromIdent("Object");
			    }
                        }
                    }
                }
                case (x:*) { 
                    print ("warning: Unimplemented: typeFromTypeExpr " + t + ", using *");
                }
            }
            return 0;
            
        }

        // Use this only for places that need a QName, only works with basic class names
        // as Tamarin doesn't support 
        public function realTypeName(t) {
            use namespace Ast;
            // not dealing with types for now
            switch type (t) {
                case (tn:TypeName) {
                    return nameFromIdentExpr(tn.ident);
                }
                case (st:SpecialType) {
                    return 0;
                }
                case (x:*) { 
                    throw ("Unimplemented: realTypeName " + t + ", using *")
                }
            }
            return 0;
            
        }

        public function fixtureNameToName(fn) {
            switch type (fn) {
            case (pn:PropName) {
                return qname(pn.name, false);
            }
            case (tn:TempName) {
		return qname ({ns:Ast::noNS,id:"$t"+tn.index},false);  // FIXME allocate and access actual temps
            }
            case (x:*) { throw "Internal error: not a valid fixture name" }
            }
        }
        
        public function fixtureTypeToType(fix) {
            switch type (fix) {
                case (vf:ValFixture) {
                    return vf.type != null ? typeFromTypeExpr(vf.type) : 0 ;
                }
                case (mf:MethodFixture) {
                    return 0;
                }
                case(x:*) { throw "Unimplemented: fixtureTypeToType " + x }
            }
        }
        
        public function defaultLiteralExpr(lit)
        {
            switch type (lit) {
                case(ln:LiteralNull) {
                    return {val:CONSTANT_Null, kind:CONSTANT_Null}
                }
                case(lu:LiteralUndefined) {
                    return {val:0, kind:0}
                }
                case(ld:LiteralDouble) {
                    let val = constants.float64(ld.doubleValue);
                    return {val:val, kind:CONSTANT_Double};
                }
                case(ld:LiteralDecimal) {
                    let val = constants.float64(ld.decimalValue);
                    return {val:val, kind:CONSTANT_Double};
                }
                case(li:LiteralInt) {
                    let val = constants.int32(li.intValue);
                    return {val:val, kind:CONSTANT_Integer};
                }
                case(lu:LiteralUInt) {
                    let val = constants.uint32(lu.uintValue);
                    return {val:val, kind:CONSTANT_UInteger};
                }
                case(lb:LiteralBoolean) {
                    let val = (lb.booleanValue ? CONSTANT_True : CONSTANT_False);
                    return {val:val, kind:val};
                }
                case(ls:LiteralString) {
                    let val = constants.stringUtf8(ls.strValue);
                    return {val:val, kind:CONSTANT_Utf8};
                }
                case(ln:LiteralNamespace) {
                    let val = constants.namespace(ln.namespaceValue);
                    return  {val:val, kind:CONSTANT_Namespace};
                }
                case(x:*) {
                    throw ("le Default expression must be a constant value" + x)
                }
            }
        }
        public function defaultExpr(expr) {
            switch type (expr) {
                case(le:LiteralExpr) {
                    return defaultLiteralExpr(le.literal);
                }
                case(lr:LexicalRef) {
                    switch type ( lr.ident ) {
                        case (i:Identifier) {
                            if( i.ident == "undefined" ) {
                                // Handle defualt expr of (... arg = undefined ...)
                                return defaultLiteralExpr(new LiteralUndefined());
                            }
                        }
                    } 
                }
            }
            throw ("Default expression must be a constant value" + expr);
        }
    }

    public class Script
    {
        public var e, init, traits=[];

        function Script(e:ABCEmitter) {
            this.e = e;
            this.init = new Method(e,[], 0, "", null);
        }

        public function newClass(name, basename) {
            return new Class(this, name, basename);
        }

        /* All functions are in some sense global because the
           methodinfo and methodbody are both global. */
        public function newFunction(formals,initScopeDepth) {
            return new Method(e, formals, initScopeDepth, null, null);
        }

        public function addException(e) {
            return init.addException(e);
        }
        // Here we probably want: newVar, newConst, ... instead?
        public function addTrait(t) {
            return traits.push(t);
        }

        public function finalize() {
            var id = init.finalize();
            var si = new ABCScriptInfo(id);
            for ( var i=0 ; i < traits.length ; i++ )
                si.addTrait(traits[i]);
            e.file.addScript(si);
        }
    }
    
    public class Class
    {
        public var s, name, basename, traits=[], instance=null, cinit;

        function Class(script, name, basename) {
            this.s = script;
            this.name = name;
            this.basename = basename;

            var asm = script.init;
            // Create the class
 /*           asm.I_findpropstrict(Object_name);
            asm.I_getproperty(Object_name);
            asm.I_dup();
            asm.I_pushscope();
            asm.I_newclass(clsidx);
            asm.I_popscope();
            asm.I_getglobalscope();
            asm.I_swap();
            asm.I_initproperty(Fib_name);
*/
        }

        public function getCInit() {
            if(cinit == null )
                cinit = new Method(s.e, [], 0, "$cinit", null);
            return cinit;
        }

/*
        public function newIInit(formals, name) {
            var iinit = new Method(s.e, formals, 2, name, null);
            iinit.I_getlocal(0);
            iinit.I_constructsuper(0);
            return iinit;
        }
*/
        public function getInstance() {
            if( this.instance == null )
                this.instance = new Instance(s, name, basename);
            
            return this.instance;
        }
        
        public function addTrait(t) {
            traits.push(t);
        }

        public function finalize() {
            var instidx = instance.finalize();
            
            var clsinfo = new ABCClassInfo();
            clsinfo.setCInit(getCInit().finalize());
            for(let i = 0; i < traits.length; ++i)
                clsinfo.addTrait(traits[i]);
            
            var clsidx = s.e.file.addClass(clsinfo);
            
            assert(clsidx == instidx);

            //s.addTrait(new ABCOtherTrait(name, 0, TRAIT_Class, 0, clsidx));
            return clsidx;
        }
    }
    
    
    public class Instance {
        // FIXME: interfaces
        
        public var s, name, basename, traits = [], iinit;
        
        function Instance(s:Script, name, basename)  : 
            s=s, 
            name=name, 
            basename=basename {
            
        }
        
        public function setIInit(method) {
            iinit = method
        }
        public function addTrait(t) {
            traits.push(t);
        }
        
        public function finalize() {
            var instinfo = new ABCInstanceInfo(name, basename, 0, 0, []);
            
            instinfo.setIInit(iinit);
            
            for(let i = 0; i < traits.length; i++)
                instinfo.addTrait(traits[i]);
            
            return s.e.file.addInstance(instinfo);
        }
    }

    public class Method // extends AVM2Assembler
    {
        public var e, formals, name, asm, traits = [], finalized=false, defaults = null, exceptions=[];
	var initScopeDepth;
        function Method(e:ABCEmitter, formals:Array, initScopeDepth, name, standardPrologue) {
            asm = new AVM2Assembler(e.constants, formals.length, initScopeDepth);
            //super(e.constants, formals.length);
            this.formals = formals;
            this.initScopeDepth = initScopeDepth
            this.e = e;
            this.name = name;

            // Standard prologue -- but is this always right?
            // ctors don't need this - have a more complicated prologue
            if(standardPrologue)
            {
                asm.I_getlocal_0();
                asm.I_pushscope();
            }
        }

        public function addTrait(t) {
            return traits.push(t);
        }

        public function setDefaults(d) {
            defaults = d;
        }

        public function addException(e) {
            return exceptions.push(e)-1;
        }
        
        public function finalize() {
            if (finalized)
                return;
            finalized = true;

            // Standard epilogue for lazy clients.
            asm.I_returnvoid();

            var meth = e.file.addMethod(new ABCMethodInfo(0, formals, 0, asm.flags, defaults,null));
            var body = new ABCMethodBodyInfo(meth);
            body.setMaxStack(asm.maxStack);
            body.setLocalCount(asm.maxLocal);
            body.setInitScopeDepth(this.initScopeDepth);
            body.setMaxScopeDepth(asm.maxScope);
            body.setCode(asm);
            for ( var i=0 ; i < traits.length ; i++ )
                body.addTrait(traits[i]);
            
            for ( var i=0 ; i < exceptions.length; i++ )
                body.addException(exceptions[i]);
            
            e.file.addMethodBody(body);

            return meth;
        }
    }
}
