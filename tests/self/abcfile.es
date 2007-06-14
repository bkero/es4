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
    class ABCFile 
    {
        const major_version = 46;
        const minor_version = 16;

        function getBytes(): * /* same type as ABCByteStream.getBytes() */ {
            function emitArray(a, len=true) {
                if (len) 
                    bytes.uint30(a.length);
                for ( var i=0 ; i < a.length ; i++ )
                    a[i].serialize(bytes);
            }

            var bytes = new ABCByteStream;
            
            assert(constants);
            assert(scripts.length != 0);
            assert(methods.length != 0);
            assert(bodies.length != 0);
            assert(classes.length == instances.length);

            bytes.uint16(minor_version);
            bytes.uint16(major_version);
            constants.serialize(bytes);
            emitArray(methods);
            emitArray(metadatas);
            emitArray(instances, false);
            emitArray(classes);
            emitArray(scripts);
            emitArray(bodies);

            return bytes.getBytes();
        }

        function addConstants(cpool: ABCConstantPool): void {
            constants = cpool;
        }

        function addMethod(m: ABCMethodInfo): uint {
            methods.push(m);
            return methods.length-1;
        }

        function addMetadata(m: ABCMetadataInfo): uint {
            metadatas.push(m);
            return metadatas.length-1;
        }

        function addInstance(i: ABCInstanceInfo): uint {
            instances.push(i);
            return instances.length-1;
        }

        function addClass(c: ABCClassInfo): uint {
            classes.push(c);
            return classes.length-1;
        }

        function addScript(s: ABCScriptInfo): uint {
            scripts.push(s);
            return scripts.length-1;
        }

        function addMethodBody(b: ABCMethodBodyInfo): uint {
            bodies.push(b);
            return bodies.length-1;
        }

        private const methods = [];
        private const metadatas = [];
        private const instances = [];
        private const classes = [];
        private const scripts = [];
        private const bodies = [];
        private var   constants;
    }

    /* FIXME: we should be using hash tables here, not linear searching. */
    class ABCConstantPool
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

        private static function findOrAdd(x, pool, cmp, emit) {
            var i;

            for ( i=1 ; i < pool.length ; i++ )
                if (cmp(pool[i], x))
                    return i;
            
            emit(x);
            pool[i] = x;
            return i;
        }
        
        private static function cmp(a, b) { return a === b }

        function int32(n:int):uint {
            return findOrAdd( n, int_pool, cmp, function (x) { int_bytes.int32(x) } );
        }
        
        function uint32(n:uint):uint {
            return findOrAdd( n, uint_pool, cmp, function (x) { uint_bytes.uint32(x) } );
        }
        
        function float64(n:Number/*FIXME ES4: double*/):uint {
            return findOrAdd( n, double_pool, cmp, function (x) { double_bytes.float64(x) } );
        }
        
        function stringUtf8(s:String/*FIXME ES4: string*/):uint {
            return findOrAdd( s, 
                              utf8_pool, 
                              cmp,
                              function (x) { utf8_bytes.uint30(x.length); utf8_bytes.utf8(x) } )
        }

        private static function cmpname(a, b) {
            return a.kind == b.kind && a.ns == b.ns && a.name == b.name;
        }

        function namespace(kind:uint, name:uint) { 
            return findOrAdd( { "kind": kind, "name": name }, 
                              namespace_pool, 
                              cmpname, 
                              function (x) { 
                                  namespace_bytes.uint8(x.kind); 
                                  namespace_bytes.uint30(x.name); } );
        }

        private static function cmparray(a, b) {
            var i;
            if (a.length != b.length) 
                return false;
            for ( i=0 ; i < a.length ; i++ )
                if (a[i] != b[i])
                    return false;
            return true;
        }

        function namespaceset(namespaces:Array) {
            return findOrAdd( copyArray(namespaces), 
                              namespaceset_pool,
                              cmparray,
                              (function (x) {
                                  namespaceset_bytes.uint30(x.length);
                                  for ( var i=0 ; i < x.length ; i++ ) 
                                      namespaceset_bytes.uint30(x[i]);
                              }) );
        }

        function QName(ns: uint, name: uint, is_attr: Boolean=false /*FIXME ES4: boolean*/) {
            return findOrAdd( { "kind": is_attr ? CONSTANT_QNameA : CONSTANT_QName, "ns": ns, "name": name },
                              multiname_pool,
                              cmpname,
                              function (x) {
                                  multiname_bytes.uint8(x.kind);
                                  multiname_bytes.uint30(x.ns);
                                  multiname_bytes.uint30(x.name); } );
        }

        function RTQName(name: uint, is_attr: Boolean=false /*FIXME ES4: boolean*/) {
            return findOrAdd( { "kind": is_attr ? CONSTANT_RTQNameA : CONSTANT_RTQName, "name": name },
                              multiname_pool,
                              cmpname, 
                              function (x) {
                                  multiname_bytes.uint8(x.kind);
                                  multiname_bytes.uint30(x.name); } );
        }

        function RTQNameL(is_attr: Boolean=false /*FIXME ES4: boolean*/) {
            return findOrAdd( { "kind": is_attr ? CONSTANT_RTQNameLA : CONSTANT_RTQNameL },
                              multiname_pool,
                              cmpname,
                              function (x) { multiname_bytes.uint8(x.kind) } );
        }

        function Multiname(nsset: uint, name: uint, is_attr: Boolean=false /*FIXME ES4: boolean*/ ) {
            return findOrAdd( { "kind": is_attr ? CONSTANT_MultinameA : CONSTANT_Multiname, "name": name, "ns":nsset },
                              multiname_pool,
                              cmpname,
                              function (x) {
                                  multiname_bytes.uint8(x.kind);
                                  multiname_bytes.uint30(x.name);
                                  multiname_bytes.uint30(x.ns); } );
        }

        function MultinameL(nsset: uint, is_attr: Boolean=false /*FIXME ES4: boolean*/) {
            return findOrAdd( { "kind": is_attr ? CONSTANT_MultinameLA : CONSTANT_MultinameL, "ns":nsset },
                              multiname_pool,
                              cmpname,
                              function (x) {
                                  multiname_bytes.uint8(x.kind);
                                  multiname_bytes.uint30(x.ns); } );
        }

        function hasRTNS(index) {
            switch (multiname_pool[index].kind) {
            case CONSTANT_RTQName:
            case CONSTANT_RTQNameA:
            case CONSTANT_RTQNameL:
            case CONSTANT_RTQNameLA:
                return true;
            default:
                return false;
            }
        }

        function hasRTName(index) {
            switch (multiname_pool[index].kind) {
            case CONSTANT_RTQNameL:
            case CONSTANT_RTQNameLA:
            case CONSTANT_MultinameL:
            case CONSTANT_MultinameLA:
                return true;
            default:
                return false;
            }
        }

        function serialize(bs) {
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

        private const int_pool = new Array;
        private const uint_pool = new Array;
        private const double_pool = new Array;
        private const utf8_pool = new Array;
        private const namespace_pool = new Array;
        private const namespaceset_pool = new Array;
        private const multiname_pool = new Array;

        private const int_bytes = new ABCByteStream;
        private const uint_bytes = new ABCByteStream;
        private const double_bytes = new ABCByteStream;
        private const utf8_bytes = new ABCByteStream;
        private const namespace_bytes = new ABCByteStream;
        private const namespaceset_bytes = new ABCByteStream;
        private const multiname_bytes = new ABCByteStream;
    }

    class ABCMethodInfo 
    {
        /* \param name         string index
         * \param param_types  array of multiname indices.  May not be null.
         * \param return_type  multiname index.
         * \param flags        bitwise or of NEED_ARGUMENTS, NEED_ACTIVATION, HAS_REST, SET_DXNS
         * \param options      [{val:uint, kind:uint}], if present.
         * \param param_names  array of param_info structures, if present.
         */
        function ABCMethodInfo(name:uint, param_types:Array, return_type:uint, flags:uint, 
                               options:Array=null, param_names:Array=null) {
            this.name = name;
            this.param_types = param_types;
            this.return_type = return_type;
            this.flags = flags;
            this.options = options;
            this.param_names = param_names;
        }

        function serialize(bs) {
            var i;
            bs.uint30(param_types.length);
            bs.uint30(return_type);
            for ( i=0 ; i < param_types.length ; i++ )
                bs.uint30(param_types[i]);
            bs.uint30(name);
            if (options != null)
                flags |= METHOD_HasOptional;
            if (param_names != null)
                flags |= METHOD_HasParamNames;
            bs.uint8(flags);
            if (options != null) {
                bs.uint30(options.length);
                for ( i=0 ; i < options.length ; i++ ) {
                    bs.uint30(options[i].val);
                    bs.uint8(options[i].kind);
                }
            }
            if (param_names != null) {
                assert( param_names.length == param_types.length );
                for ( i=0 ; i < param_names.length ; i++ )
                    bs.uint30(param_names[i]);
            }
        }

        private var name, param_types, return_type, flags, options, param_names;
    }

    class ABCMetadataInfo 
    {
        function ABCMetadataInfo( name: uint, items: Array ) {
            assert( name != 0 );
            this.name = name;
            this.items = items;
        }

        function serialize(bs) {
            bs.uint30(name);
            bs.uint30(items.length);
            for ( var i=0 ; i < items.length ; i++ ) {
                bs.uint30(items.key);
                bs.uint30(items.value);
            }
        }

        private var name, items;
    }

    class ABCInstanceInfo
    {
        function ABCInstanceInfo(name, super_name, flags, protectedNS, interfaces) {
            this.name = name;
            this.super_name = name;
            this.flags = flags;
            this.protectedNS = protectedNS;
            this.interfaces = interfaces;
            this.traits = [];
        }

        function setIInit(x) {
            iinit = x;
        }

        function addTrait(t) {
            traits.push(t);
            return traits.length-1;
        }

        function serialize(bs) {
            var i;

            assert( iinit != undefined );

            bs.uint30(name);
            bs.uint30(super_name);
            bs.uint8(flags);
            if (flags & CONSTANT_ClassProtectedNs)
                bs.uint30(protectedNS);
            bs.uint30(interfaces.length);
            for ( i=0 ; i < interfaces.length ; i++ ) {
                assert( interfaces[i] != 0 );
                bs.uint30(interfaces[i]);
            }
            bs.uint30(iinit);
            bs.uint30(traits.length);
            for ( i=0 ; i < traits.length ; i++ )
                traits[i].serialize(bs);
        }

        private var name, super_name, flags, protectedNS, interfaces, iinit, traits;
    }

    class ABCTrait 
    {
        function ABCTrait(name, kind) {
            this.name = name;
            this.kind = kind;
        }

        function addMetadata(n) {
            metadata.push(n);
            return metadata.length-1;
        }

        function inner_serialize(bs) {
            throw "ABSTRACT";
        }

        function serialize(bs) {
            if (metadata.length > 0)
                kind |= ATTR_Metadata;
            bs.uint30(name);
            bs.uint30(kind);
            this.inner_serialize(bs);
            if (metadata.length > 0) {
                bs.uint30(metadata.length);
                for ( var i=0 ; i < metadata.length ; i++ ) 
                    bs.uint30(metadata[i]);
            }
        }

        private var name, kind, metadata = [];
    }

    class ABCSlotTrait extends ABCTrait
    {
        function ABCSlotTrait(name, attrs, slot_id=0, type_name=0, vindex=0, vkind=0) {
            super(name, (attrs << 4) | TRAIT_Slot);
            this.slot_id = slot_id;
            this.type_name = type_name;
            this.vindex = vindex;
            this.vkind = vkind;
        }

        override function inner_serialize(bs) {
            bs.uint30(slot_id);
            bs.uint30(type_name);
            bs.uint30(vindex);
            if (vindex != 0)
                bs.uint8(vkind);
        }

        private var slot_id, type_name, vindex, vkind;
    }

    class ABCOtherTrait extends ABCTrait
    {
        /* TAG is one of the TRAIT_* values, except TRAIT_Slot */
        function ABCOtherTrait(name, attrs, tag, id, val) {
            super(name, (attrs << 4) | tag);
            this.id = id;
            this.val = val;
        }

        override function inner_serialize(bs) {
            bs.uint30(id);
            bs.uint30(val);
        }

        private var id, val;
    }

    class ABCClassInfo
    {
        function setCInit(cinit) {
            this.cinit = cinit;
        }

        function addTrait(t) {
            traits.push(t);
            return traits.length-1;
        }

        function serialize(bs) {
            assert( cinit != undefined );
            bs.uint30(cinit);
            bs.uint30(traits.length);
            for ( var i=0 ; i < traits.length ; i++ ) 
                traits[i].serialize(bs);
        }

        private var cinit, traits = [];
    }

    class ABCScriptInfo
    {
        function ABCScriptInfo(init=undefined) {
            this.init = init;
        }

        function setInit(init) {
            this.init = init;
        }

        function addTrait(t) {
            traits.push(t);
            return traits.length-1;
        }

        function serialize(bs) {
            assert( init != undefined );
            bs.uint30(init);
            bs.uint30(traits.length);
            for ( var i=0 ; i < traits.length ; i++ ) 
                traits[i].serialize(bs);
        }

        private var init, traits = [];
    }

    class ABCMethodBodyInfo
    {
        function ABCMethodBodyInfo(method) {
            this.method = method;
        }
        function setMaxStack(ms) { max_stack = ms }
        function setLocalCount(lc) { local_count = lc }
        function setInitScopeDepth(sd) { init_scope_depth = sd }
        function setMaxScopeDepth(msd) { max_scope_depth = msd }
        function setCode(insns) { code = insns }

        function addException(exn) {
            exceptions.push(exn);
            return exceptions.length-1;
        }

        function addTrait(t) {
            traits.push(t);
            return traits.length-1;
        }

        function serialize(bs) {
            assert( max_stack != undefined && local_count != undefined );
            assert( init_scope_depth != undefined && max_scope_depth != undefined );
            assert( code != undefined );

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

        private var init_scope_depth = 0, exceptions = [], traits = [];
        private var method, max_stack, local_count, max_scope_depth, code;
    }

    class ABCException 
    {
        function ABCException(first_pc, last_pc, target_pc, exc_type=0, var_name=0) {
            this.first_pc = first_pc;
            this.last_pc = last_pc;
            this.target_pc = target_pc;
            this.exc_type = exc_type;
            this.var_name = var_name;
        }

        function serialize(bs) {
            bs.uint30(first_pc);
            bs.uint30(last_pc);
            bs.uint30(target_pc);
            bs.uint30(exc_type);
            bs.uint30(var_name);
        }

        private var first_pc, last_pc, target_pc, exc_type, var_name;
    }

    function testABCConstantPool() {
        print("--------------------------------------------");
        print("Testing ABCConstantPool");
        print("");
            
        var cp = new ABCConstantPool;

        // Sharing working OK?
        var a = cp.int32(37);
        var b = cp.int32(37);
        assert( a == b );

        var a = cp.uint32(37);
        var b = cp.uint32(37);
        assert( a == b );

        var a = cp.float64(1.0);
        var b = cp.float64(1.0);
        assert( a == b );

        var a = cp.stringUtf8("foo");
        var b = cp.stringUtf8("foo");
        var s = a;
        assert( a == b );
        var k = cp.stringUtf8("x");

        var a = cp.namespace(CONSTANT_PackageInternalNS, k);
        var b = cp.namespace(CONSTANT_PackageInternalNS, k);
        assert( a == b );
        cp.namespace(CONSTANT_ProtectedNamespace, s);
        var c = cp.namespace(CONSTANT_ProtectedNamespace, k);

        var nsa = a;
        var nsc = c;

        var nss = cp.namespaceset([nsa,nsc]);

        cp.QName(nsa, s);
        cp.QName(nsa, s, true);

        cp.RTQName(s);
        cp.RTQName(s, true);

        cp.RTQNameL();
        cp.RTQName(true);

        cp.Multiname(nss, k);
        cp.Multiname(nss, k, true);

        cp.MultinameL(nss);
        cp.MultinameL(nss, true);

        var bytes = new ABCByteStream;
        cp.serialize(bytes);
        dumpByteStream( bytes );
    }

    public function testABCFile() {
        testABCConstantPool();

        var f = new ABCFile;
        var cp = new ABCConstantPool;
        f.addConstants(cp);
        var m = f.addMethod(new ABCMethodInfo(cp.stringUtf8("foo"), 
                                               [cp.Multiname(cp.namespaceset([cp.namespace(CONSTANT_ProtectedNamespace, 
                                                                                            cp.stringUtf8("bar"))]),
                                                             cp.stringUtf8("baz"))],
                                               0,
                                               0,
                                               [],
                                               [cp.stringUtf8("x")]));

        f.addMetadata(new ABCMetadataInfo(cp.stringUtf8("meta"), 
                                          [{key: cp.stringUtf8("fnord"), value: cp.stringUtf8("foo")}]));
        var cl = new ABCClassInfo();
        var cli = f.addClass(cl);
        cl.setCInit(0);

        var ii = new ABCInstanceInfo(cp.stringUtf8("foo"),
                                     0,
                                     0,
                                     0,
                                     []);
        f.addInstance(ii);
        ii.setIInit(0);
        ii.addTrait(new ABCSlotTrait(cp.stringUtf8("x"), 0));
        ii.addTrait(new ABCOtherTrait(cp.stringUtf8("y"), 0, TRAIT_Class, 0, cli));

        var sc = new ABCScriptInfo;
        f.addScript(sc);
        sc.setInit(0);

        var mb = new ABCMethodBodyInfo(0);
        f.addMethodBody(mb);
        mb.setMaxStack(0);
        mb.setLocalCount(0);
        mb.setInitScopeDepth(0);
        mb.setMaxScopeDepth(0);
        mb.setCode({ "serialize": function (bs) {}, "length": 0 });
        
        f.getBytes();
    }
}
