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
    /****************************************************************
     * Constant pool manager.
     *
     * Reuse constants whenever possible.
     *
     * FIXME: we should be using hash tables here, not linear searching.
     * Not hard to fix.
     */
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

    public function testABCConstantPool() {
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
}
