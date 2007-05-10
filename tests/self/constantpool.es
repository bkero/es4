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
        function int32(n:int):uint {
            var i;

            for ( i=0 ; i < int_pool.length ; i++ )
                if (int_pool[i] === n)
                    return i;
            
            int_bytes.int32(n);
            int_pool[i] = n;
            return i;
        }
        
        function uint32(n:uint):uint {
            var i;

            for ( i=0 ; i < uint_pool.length ; i++ )
                if (uint_pool[i] === n)
                    return i;
            
            uint_bytes.int32(n);
            uint_pool[i] = n;
            return i;
        }
        
        function float64(n:double):uint {
            var i;

            for ( i=0 ; i < double_pool.length ; i++ )
                if (double_pool[i] === n)
                    return i;
            
            double_bytes.float64(n);
            double_pool[i] = n;
            return i;
        }
        
        function stringUtf8(s:string):uint {
            Debug.enter("ConstantUtf8",str.length,str)
            
            var bytes = new ByteArray
            bytes.endian = "littleEndian";
            makeInt32(bytes,str.length)
            bytes.writeUTFBytes(str)
            var index = addBytesToPool(bytes,utf8_pool)
            
            Debug.exit("ConstantUtf8",index)
            Debug.log_mode::log("ConstantUtf8 "+str+" -> "+index,utf8_pool_out)
            return index
        }
        
        function qName(name_index,ns_index,is_attr) {
            var bytes = new ByteArray
            bytes.endian = "littleEndian";
            makeByte(bytes,is_attr?CONSTANT_QnameA:CONSTANT_Qname)
            makeInt32(bytes,ns_index)
            makeInt32(bytes,name_index)
            var index = addBytesToPool(bytes,multiname_pool)
            Debug.log_mode::log("ConstantQualifiedName "+name_index+" "+ns_index+" "+is_attr+" -> "+index,multiname_pool_out)
            return index
        }

        function ConstantNamespace(uri_index:uint,kind)
        { 
            var kind_str = kind==CONSTANT_PackageNamespace ? "public" :         // package public
                        kind==CONSTANT_PackageInternalNS ? "internal" :             // package internal
                        kind==CONSTANT_ProtectedNamespace ? "protected" :
                        kind==CONSTANT_StaticProtectedNS ? "static protected" :
                        kind==CONSTANT_Namespace ? "user" :
                        kind==CONSTANT_PrivateNamespace ? "private" : "**error**"


            Debug.enter("ConstantNamespace",uri_index,kind_str)  
            var bytes = new ByteArray         
            bytes.endian = "littleEndian";
            makeByte(bytes,kind)
            makeInt32(bytes,uri_index)
            var index = addBytesToPool(bytes,namespace_pool)
            Debug.exit("ConstantNamespace",index)  
            Debug.log_mode::log("ConstantNamespace "+uri_index+" "+kind_str+" -> "+index,namespace_pool_out)
            return index
        }

        function ConstantNamespaceSet(namespaces)
        {            
            var bytes = new ByteArray         
            bytes.endian = "littleEndian";
            var count = namespaces.length

            makeInt32(bytes,count)
            var nsset_out = " "
            for( var i = 0; i < count; i++ )
            {
                var name = namespaces[i].@name
                var kind = namespaces[i].@kind=="internal"?CONSTANT_PackageInternalNS:
                                                "public"?CONSTANT_PackageNamespace:
                                                    CONSTANT_Namespace
                var utf8_index = ConstantUtf8(name)
                var ns_index = ConstantNamespace(utf8_index,kind)
                nsset_out += ns_index + " "
                makeInt32(bytes,ns_index)
            }
            var index = addBytesToPool(bytes,namespaceset_pool)
            Debug.log_mode::log("ConstantNamespaceSet ["+nsset_out+"] -> "+index,namespaceset_pool_out)
            return index
        }

        function ConstantMultiname(name_index,nsset_index,is_attr)
        {            
            var bytes = new ByteArray
            bytes.endian = "littleEndian";
            makeByte(bytes,is_attr?CONSTANT_MultinameA:CONSTANT_Multiname)
            makeInt32(bytes,name_index)
            makeInt32(bytes,nsset_index)
            var index = addBytesToPool(bytes,multiname_pool)
            Debug.log_mode::log("ConstantMultiname "+name_index+" "+nsset_index+" -> "+index,multiname_pool_out)
            return index
        }

        function ConstantMultinameL(nsset_index,is_attr)
        {            
            var bytes = new ByteArray
            bytes.endian = "littleEndian";
            makeByte(bytes,is_attr?CONSTANT_MultinameLA:CONSTANT_MultinameL)
            makeInt32(bytes,nsset_index)
            var index = addBytesToPool(bytes,multiname_pool)
            Debug.log_mode::log("ConstantMultinameL "+nsset_index+" -> "+index, multiname_pool_out)
            return index
        }

        function writeToByteStream(bs) {
            // FIXME: write the fully formatted constant pool to the stream bs
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

}
