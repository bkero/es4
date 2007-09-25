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

namespace Abc;
//package abcfile
{
    use default namespace Abc;
    use namespace Asm;
    //import util.*;
    //import assembler.*;
    //import bytestream.*;

    /* ABCFile container & helper class.
     *
     * Every argument to an addWhatever() method is retained by
     * reference.  When getBytes() is finally called, each object is
     * serialized.  The order of serialization is the order they will
     * have in the ABCFile, and the order among items of the same type
     * is the order in which they were added.
     *
     * Performance ought to be good; nothing is serialized more than
     * once and no data are copied except during serialization.
     */
    public class ABCFile
    {
        public const major_version = 46;
        public const minor_version = 16;

        public function getBytes(): * /* same type as ABCByteStream.getBytes() */ {
            function emitArray(a, len) {
                if (len)
                    bytes.uint30(a.length);
                for ( var i=0 ; i < a.length ; i++ )
                    a[i].serialize(bytes);
            }

            var bytes = new ABCByteStream;

            Util::assert(constants);
            Util::assert(scripts.length != 0);
            Util::assert(methods.length != 0);
            Util::assert(bodies.length != 0);
            Util::assert(classes.length == instances.length);

            // print ("emitting magic");
            bytes.uint16(minor_version);
            bytes.uint16(major_version);
            // print ("emitting constants");
            constants.serialize(bytes);
            // print ("emitting methods");
            emitArray(methods,true);
            // print ("emitting metadatas");
            emitArray(metadatas,true);
            // print ("emitting instances");
            emitArray(instances,true);
            // print ("emitting classes");
            emitArray(classes, false);
            // print ("emitting scripts");
            emitArray(scripts,true);
            // print ("emitting bodies");
            emitArray(bodies,true);
            return bytes.getBytes();
        }

        public function addConstants(cpool: ABCConstantPool): void {
            constants = cpool;
        }

        public function addMethod(m: ABCMethodInfo)/*: uint*/ {
            return methods.push(m)-1;
        }

        public function addMetadata(m: ABCMetadataInfo)/*: uint*/ {
            return metadatas.push(m)-1;
        }

        public function addClassAndInstance(cls, inst)/*: uint*/ {
            var x = addClass(cls);
            var y = addInstance(inst);
            Util::assert( x == y );
            return x;
        }

        public function addInstance(i: ABCInstanceInfo)/*: uint*/ {
            return instances.push(i)-1;
        }

        public function addClass(c: ABCClassInfo)/*: uint*/ {
            return classes.push(c)-1;
        }

        public function addScript(s: ABCScriptInfo)/*: uint*/ {
            return scripts.push(s)-1;
        }

        public function addMethodBody(b: ABCMethodBodyInfo)/*: uint*/ {
            return bodies.push(b)-1;
        }

        /*private*/ const methods = [];
        /*private*/ const metadatas = [];
        /*private*/ const instances = [];
        /*private*/ const classes = [];
        /*private*/ const scripts = [];
        /*private*/ const bodies = [];
        /*private*/ var constants;
    }

    /* FIXME: we should be using hash tables here, not linear searching. */
    public class ABCConstantPool
    {
        function ABCConstantPool() {
            // All pools start at 1.
            int_pool.length = 1;
            uint_pool.length = 1;
            double_pool.length = 1;
            utf8_pool.length = 1;
            namespace_pool.length = 1;
            namespaceset_pool.length = 1;
            multiname_pool.length = 1;
        }

        /**/ function findOrAdd(x, pool, cmp, emit) {
            var i;
            for ( i=1 ; i < pool.length ; i++ )
                if (cmp(pool[i], x))
                    return i;

            emit(x);
            pool.push(x);
            return i;
        }

        /*private*/ function cmp(a, b) { return a === b }

        public function int32(n/*:int*/)/*:uint*/ {
            function temp_func (x) { int_bytes.int32(x) };
            return findOrAdd( n, int_pool, cmp, temp_func );
        }

        public function uint32(n/*:uint*/)/*:uint*/ {
            function temp_func(x) { uint_bytes.uint32(x) }
            return findOrAdd( n, uint_pool, cmp, temp_func );
        }

        public function float64(n/*FIXME ES4: double*/)/*:uint*/ {
            function temp_func(x) { double_bytes.float64(x) } 
            return findOrAdd( n, double_pool, cmp, temp_func);
        }

        public function stringUtf8(s/*FIXME ES4: string*/)/*:uint*/ {
            function temp_func(x) { utf8_bytes.uint30(x.length); utf8_bytes.utf8(x) }
            return findOrAdd( s,
                              utf8_pool,
                              cmp,
                              temp_func )
        }

        /*private*/ function cmpname(a, b) {
            return a.kind == b.kind && a.ns == b.ns && a.name == b.name;
        }

        public function namespace(kind/*:uint*/, name/*:uint*/) {
            function temp_func(x) {
              namespace_bytes.uint8(x.kind);
              namespace_bytes.uint30(x.name); }
            return findOrAdd( { "kind": kind, "name": name },
                              namespace_pool,
                              cmpname,
                              temp_func );
        }

        /*private*/ function cmparray(a, b) {
            var i;
            if (a.length != b.length)
                return false;
            for ( i=0 ; i < a.length ; i++ )
                if (a[i] != b[i])
                    return false;
            return true;
        }

        public function namespaceset(namespaces:Array) {
            function temp_func (x) {
              namespaceset_bytes.uint30(x.length);
              for ( var i=0 ; i < x.length ; i++ )
                  namespaceset_bytes.uint30(x[i]);
            }
            return findOrAdd( Util::copyArray(namespaces),
                              namespaceset_pool,
                              cmparray,
                              temp_func );
        }

        public function QName(ns/*: uint*/, name/*: uint*/, is_attr: Boolean /*FIXME ES4: boolean*/) {
            function temp_func(x) {
              multiname_bytes.uint8(x.kind);
              multiname_bytes.uint30(x.ns);
              multiname_bytes.uint30(x.name); 
            }
            return findOrAdd( { "kind": is_attr ? CONSTANT_QNameA : CONSTANT_QName, "ns": ns, "name": name },
                              multiname_pool,
                              cmpname,
                              temp_func );
        }

        public function RTQName(name/*: uint*/, is_attr: Boolean /*FIXME ES4: boolean*/) {
            function temp_func(x) {
              multiname_bytes.uint8(x.kind);
              multiname_bytes.uint30(x.name); 
            }
            return findOrAdd( { "kind": is_attr ? CONSTANT_RTQNameA : CONSTANT_RTQName, "name": name },
                              multiname_pool,
                              cmpname,
                              temp_func );
        }

        public function RTQNameL(is_attr: Boolean /*FIXME ES4: boolean*/) {
            function temp_func (x) { multiname_bytes.uint8(x.kind) } 
            return findOrAdd( { "kind": is_attr ? CONSTANT_RTQNameLA : CONSTANT_RTQNameL },
                              multiname_pool,
                              cmpname,
                              temp_func);
        }

        public function Multiname(nsset/*: uint*/, name/*: uint*/, is_attr: Boolean /*FIXME ES4: boolean*/ ) {
            function temp_func(x) {
                  multiname_bytes.uint8(x.kind);
                  multiname_bytes.uint30(x.name);
                  multiname_bytes.uint30(x.ns); 
            } 
            return findOrAdd( { "kind": is_attr ? CONSTANT_MultinameA : CONSTANT_Multiname, "name": name, "ns":nsset },
                              multiname_pool,
                              cmpname,
                              temp_func);
        }

        public function MultinameL(nsset/*: uint*/, is_attr: Boolean /*FIXME ES4: boolean*/) {
            function temp_func (x) {
              multiname_bytes.uint8(x.kind);
              multiname_bytes.uint30(x.ns); 
            }
            return findOrAdd( { "kind": is_attr ? CONSTANT_MultinameLA : CONSTANT_MultinameL, "ns":nsset },
                              multiname_pool,
                              cmpname,
                              temp_func );
        }

        public function hasRTNS(index) {
            var kind = multiname_pool[index].kind;
            var result;
            switch (kind) {
            case CONSTANT_RTQName:
            case CONSTANT_RTQNameA:
            case CONSTANT_RTQNameL:
            case CONSTANT_RTQNameLA:
                result = true;
            default:
                result = false;
            }
            return result;
        }

        public function hasRTName(index) {
            var kind = multiname_pool[index].kind;
            var result;
            switch (multiname_pool[index].kind) {
            case CONSTANT_RTQNameL:
            case CONSTANT_RTQNameLA:
            case CONSTANT_MultinameL:
            case CONSTANT_MultinameLA:
                result = true;
            default:
                result = false;
            }
            return result;
        }

        public function serialize(bs) {
            bs.uint30(int_pool.length);
            bs.byteStream(int_bytes);

            bs.uint30(uint_pool.length);
            bs.byteStream(uint_bytes);

            bs.uint30(double_pool.length);
            bs.byteStream(double_bytes);

            bs.uint30(utf8_pool.length);
            bs.byteStream(utf8_bytes);

            bs.uint30(namespace_pool.length);
            bs.byteStream(namespace_bytes);

            bs.uint30(namespaceset_pool.length);
            bs.byteStream(namespaceset_bytes);

            bs.uint30(multiname_pool.length);
            bs.byteStream(multiname_bytes);

            return bs;
        }

        /*private*/ const int_pool = new Array;
        /*private*/ const uint_pool = new Array;
        /*private*/ const double_pool = new Array;
        /*private*/ const utf8_pool = new Array;
        /*private*/ const namespace_pool = new Array;
        /*private*/ const namespaceset_pool = new Array;
        /*private*/ const multiname_pool = new Array;

        /*private*/ const int_bytes = new ABCByteStream;
        /*private*/ const uint_bytes = new ABCByteStream;
        /*private*/ const double_bytes = new ABCByteStream;
        /*private*/ const utf8_bytes = new ABCByteStream;
        /*private*/ const namespace_bytes = new ABCByteStream;
        /*private*/ const namespaceset_bytes = new ABCByteStream;
        /*private*/ const multiname_bytes = new ABCByteStream;
    }

    public class ABCMethodInfo
    {
        /* \param name         string index
         * \param param_types  array of multiname indices.  May not be null.
         * \param return_type  multiname index.
         * \param flags        bitwise or of NEED_ARGUMENTS, NEED_ACTIVATION, HAS_REST, SET_DXNS
         * \param options      [{val:uint, kind:uint}], if present.
         * \param param_names  array of param_info structures, if present.
         */
        function ABCMethodInfo(name/*:uint*/, param_types:Array, return_type/*:uint*/, flags/*:uint*/,
                               options:Array, param_names:Array) {
            this.name = name;
            this.param_types = param_types;
            this.return_type = return_type;
            this.flags = flags;
            this.options = options;
            this.param_names = param_names;
        }

        public function setFlags(flags) {
            this.flags = flags;
        }

        public function serialize(bs) {
            var i;
            bs.uint30(param_types.length);
            bs.uint30(return_type);
            for ( i=0 ; i < param_types.length ; i++ ) {
                bs.uint30(param_types[i]);
            }
            bs.uint30(name);
            if (options != null) {
                flags = flags | METHOD_HasOptional;
            }
            if (param_names != null) {
                flags = flags | METHOD_HasParamNames;
            }
            bs.uint8(flags);
            if (options != null) {
                bs.uint30(options.length);
                for ( i=0 ; i < options.length ; i++ ) {
                    bs.uint30(options[i].val);
                    bs.uint8(options[i].kind);
                }
            }
            if (param_names != null) {
                Util::assert( param_names.length == param_types.length );
                for ( i=0 ; i < param_names.length ; i++ )
                    bs.uint30(param_names[i]);
            }
        }

        /*private*/ var name, param_types, return_type, flags, options, param_names;
    }

    public class ABCMetadataInfo
    {
        function ABCMetadataInfo( name/*: uint*/, items: Array ) {
            Util::assert( name != 0 );
            this.name = name;
            this.items = items;
        }

        public function serialize(bs) {
            bs.uint30(name);
            bs.uint30(items.length);
            for ( var i=0 ; i < items.length ; i++ ) {
                bs.uint30(items[i].key);
                bs.uint30(items[i].value);
            }
        }

        /*private*/ var name, items;
    }

    public class ABCInstanceInfo
    {
        function ABCInstanceInfo(name, super_name, flags, protectedNS, interfaces) {
            this.name = name;
            this.super_name = super_name;
            this.flags = flags;
            this.protectedNS = protectedNS;
            this.interfaces = interfaces;
            this.traits = [];
        }

        public function setIInit(x) {
            iinit = x;
        }

        public function addTrait(t) {
            return traits.push(t)-1;
        }

        public function serialize(bs) {
            var i;

            Util::assert( iinit != undefined );

            bs.uint30(name);
            bs.uint30(super_name);
            bs.uint8(flags);
            if (flags & CONSTANT_ClassProtectedNs)
                bs.uint30(protectedNS);
            bs.uint30(interfaces.length);
            for ( i=0 ; i < interfaces.length ; i++ ) {
                Util::assert( interfaces[i] != 0 );
                bs.uint30(interfaces[i]);
            }
            bs.uint30(iinit);
            bs.uint30(traits.length);
            for ( i=0 ; i < traits.length ; i++ )
                traits[i].serialize(bs);
        }

        /*private*/ var name, super_name, flags, protectedNS, interfaces, iinit, traits;
    }

    public class ABCTrait
    {
        /* FIXME #101: super not implemented; subclasses must do implementation themselves;
           the constructor must not be defined here (for the sake of AS3).  */
        /*
        function ABCTrait(name, kind) {
            this.name = name;
            this.kind = kind;
        }
        */

        public function addMetadata(n) {
            return metadata.push(n)-1;
        }
/*
        public function inner_serialize(bs) {
            throw "ABSTRACT";
        }
*/
        public function serialize(bs) {
            if (metadata.length > 0)
                kind = kind | ATTR_Metadata;
            bs.uint30(name);
            bs.uint30(kind);
            inner_serialize(bs);
            if (metadata.length > 0) {
                bs.uint30(metadata.length);
                for ( var i=0 ; i < metadata.length ; i++ )
                    bs.uint30(metadata[i]);
            }
        }

        /*FIXME #101: super not implemented, so subclasses must initialize
          these directly.  They should be private, and there should be
          an initializer for metadata initializing it to an empty array. */
        /*FIXME #102: They should be protected for the workaround, but
          protected is not implemented either. */
        public var name, kind, metadata;
    }

    public class ABCSlotTrait extends ABCTrait
    {
        function ABCSlotTrait(name, attrs, is_const, slot_id, type_name, vindex, vkind) {
            /*FIXME #101: super not implemented*/
            //super(name, (attrs << 4) | TRAIT_Slot);
            this.name = name;
            this.kind = (attrs << 4) | (is_const ? TRAIT_Const : TRAIT_Slot);
            this.metadata = [];
            //End of fixme
            this.slot_id = slot_id;
            this.type_name = type_name;
            this.vindex = vindex;
            this.vkind = vkind;
        }

        // esc doesn't support override yet
        /*override*/ public function inner_serialize(bs) {
            bs.uint30(slot_id);
            bs.uint30(type_name);
            bs.uint30(vindex);
            if (vindex != 0)
                bs.uint8(vkind);
        }

        /*private*/ var slot_id, type_name, vindex, vkind;
    }

    public class ABCOtherTrait extends ABCTrait
    {
        /* TAG is one of the TRAIT_* values, except TRAIT_Slot */
        function ABCOtherTrait(name, attrs, tag, id, val) {
            /*FIXME #101: super not implemented*/
            //super(name, (attrs << 4) | tag);
            this.name = name;
            this.kind = (attrs << 4) | tag;
            this.metadata = [];
            //End of fixme
            this.id = id;
            this.val = val;
        }

        // esc doesn't support override yet
        /*override*/ public function inner_serialize(bs) {
            bs.uint30(id);
            bs.uint30(val);
        }

        /*private*/ var id, val;
    }

    public class ABCClassInfo
    {
        public function setCInit(cinit) {
            this.cinit = cinit;
        }

        public function addTrait(t) {
            return traits.push(t)-1;
        }

        public function serialize(bs) {
            Util::assert( cinit != undefined );
            bs.uint30(cinit);
            bs.uint30(traits.length);
            for ( var i=0 ; i < traits.length ; i++ )
                traits[i].serialize(bs);
        }

        /*private*/ var cinit, traits = [];
    }

    public class ABCScriptInfo
    {
        function ABCScriptInfo(init) {
            this.init = init;
        }

        public function setInit(init) {
            this.init = init;
        }

        public function addTrait(t) {
            return traits.push(t)-1;
        }

        public function serialize(bs) {
            Util::assert( init != undefined );
            bs.uint30(init);
            bs.uint30(traits.length);
            for ( var i=0 ; i < traits.length ; i++ )
                traits[i].serialize(bs);
        }

        /*private*/ var init, traits = [];
    }

    public class ABCMethodBodyInfo
    {
        function ABCMethodBodyInfo(method) {
            this.method = method;
        }
        public function setMaxStack(ms) { max_stack = ms }
        public function setLocalCount(lc) { local_count = lc }
        public function setInitScopeDepth(sd) { init_scope_depth = sd }
        public function setMaxScopeDepth(msd) { max_scope_depth = msd }
        public function setCode(insns) { code = insns }

        public function addException(exn) {
            return exceptions.push(exn)-1;
        }

        public function addTrait(t) {
            return traits.push(t)-1;
        }

        public function serialize(bs) {
            Util::assert( max_stack != undefined && local_count != undefined );
            Util::assert( init_scope_depth != undefined && max_scope_depth != undefined );
            Util::assert( code != undefined );

            bs.uint30(method);
            bs.uint30(max_stack);
            bs.uint30(local_count);
            bs.uint30(init_scope_depth);
            bs.uint30(max_scope_depth);
            bs.uint30(code.length);
            code.serialize(bs);
            bs.uint30(exceptions.length);
            for ( var i=0 ; i < exceptions.length ; i++ )
                exceptions[i].serialize(bs);
            bs.uint30(traits.length);
            for ( var i=0 ; i < traits.length ; i++ )
                traits[i].serialize(bs);
        }

        /*private*/ var init_scope_depth = 0, exceptions = [], traits = [];
        /*private*/ var method, max_stack, local_count, max_scope_depth, code;
    }

    public class ABCException
    {
        function ABCException(first_pc, last_pc, target_pc, exc_type, var_name) {
            this.first_pc = first_pc;
            this.last_pc = last_pc;
            this.target_pc = target_pc;
            this.exc_type = exc_type;
            this.var_name = var_name;
        }

        public function serialize(bs) {
            bs.uint30(first_pc);
            bs.uint30(last_pc);
            bs.uint30(target_pc);
            bs.uint30(exc_type);
            bs.uint30(var_name);
        }

        /*private*/ var first_pc, last_pc, target_pc, exc_type, var_name;
    }
}
