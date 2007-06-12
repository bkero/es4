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
    use namespace release;

    function compareByteArrays(ba1:ByteArray, ba2:ByteArray) {
        if( ba1.length !== ba2.length ) 
            return false;
        
        for( var i = ba1.length-1; i >= 0; i-- )
            if( ba1[i] !== ba2[i] ) 
                return false;
    
        return true;
    }

    class ABCEmitter
    {
        // IF kinds

        public static const IF_false = 0;
        public static const IF_true = IF_false + 1;
        public static const IF_lt = IF_true + 1;
        public static const IF_lti = IF_lt + 1;
        public static const IF_le = IF_lti + 1;
        public static const IF_gt = IF_le + 1;
        public static const IF_ge = IF_gt + 1;
        public static const IF_gei = IF_ge + 1;
        public static const IF_eq = IF_gei + 1;
        public static const IF_ne = IF_eq + 1;
        public static const IF_stricteq = IF_ne + 1;
        public static const IF_strictne = IF_stricteq + 1;
        public static const IF_nlt = IF_strictne + 1;
        public static const IF_nle = IF_nlt + 1;
        public static const IF_ngt = IF_nle + 1;
        public static const IF_nge = IF_ngt + 1;

        var if_addrs = new Array;   // used for fixup
        var else_addrs = new Array;
        var lblnum = 0;

        // ABC parts
        
        var minor_version = 16;
        var major_version = 46;
        var int_pool = new Array;
        var uint_pool = new Array;
        var double_pool = new Array;
        var utf8_pool = new Array;
        var namespace_pool = new Array;
        var namespaceset_pool = new Array;
        var multiname_pool = new Array;
        var method_infos = new Array;
        var metadata_infos = new Array;
        var instance_infos = new Array;
        var class_infos = new Array;
        var script_infos = new Array;
        var method_bodys = new Array;

        function methodCount() {
            return method_infos.length;
        }

        function ABCEmitter(minor=16,major=46) {
            if( major != 46 ) 
                throw "major version " + major + " not supported!";

            minor_version = minor;
            major_version = major;
        }
        
        function addBytesToPool(bytes,pool) {
            var count = pool.length;
            for( var i = 0 ; i < count && !compareByteArrays(bytes, pool[i]) ; ++i )
                ;
            pool[i] = bytes;
            return i+1;
        }
        
        var info_out = ["---------","I N F O S","---------"]
        var body_out = ["-----------","B O D I E S","-----------"]
        var code_out = []
        
/*

MethodInfo {
    U30 param_count
    U30 ret_type                      // CONSTANT_Multiname, 0=Object
    U30 param_types[param_count]      // CONSTANT_Multiname, 0=Object
    U30 name_index                    // 0=no name.
    // 1=need_arguments, 2=need_activation, 4=need_rest 8=has_optional 16=ignore_rest, 32=explicit, 64=setsdxns, 128=has_paramnames
    U8 flags                          
    U30 optional_count                // if has_optional
    ValueKind[optional_count]         // if has_optional
    U30 param_names[param_count]      // if has_paramnames
}

*/
        function MethodInfo(param_count,type,types,name_index,flags,optional_count,optional_kinds)
        {
            Debug.enter("MethodInfo",param_count,type,name_index,flags,optional_count,optional_kinds)

            var method_info = method_infos.length //getMethodInfo(name)
            var bytes = new ByteArray
            bytes.endian = "littleEndian";

            makeInt32(bytes,param_count);
            makeInt32(bytes,type);
            for (var i=0; i < param_count; i++)
            {
                makeInt32(bytes,types[i]);
            }
            makeInt32(bytes,name_index);
            makeByte(bytes,flags);
            if( false /*flags & HAS_OPTIONAL*/ )
            {
                makeInt32(bytes,optional_count);
                for (var i=0; i < optional_count; i++)
                {
                    makeInt32(bytes,optional_kinds[i]);
                }
            }
            
            Debug.log_mode::log("MethodInfo "+param_count+" "+type+" "+types+" name="+name_index+" "+flags+" "+optional_count+" "+optional_kinds+" -> "+method_info,info_out)

            method_infos.push(bytes)
            Debug.exit("MethodInfo")
            return method_info
        }

        function dumpBytes(bytes)
        {
            bytes.position = 0;
            var str = "";
            while( bytes.bytesAvailable ) { str += " "+bytes.readByte() }
            print(str);
        }
        
        function MethodBody(info_index,max_stack,max_locals,scope_depth,max_scope,code,exceptions,slot_infos)
        {
            Debug.enter("MethodBody",info_index,max_stack,max_locals,scope_depth,max_scope)
            var bytes = new ByteArray
            bytes.endian = "littleEndian";

            makeInt32(bytes,info_index)
            makeInt32(bytes,max_stack)
            makeInt32(bytes,max_locals)
            makeInt32(bytes,scope_depth)
            makeInt32(bytes,max_scope)
            makeBytes(bytes,code)
            makeBytes(bytes,exceptions)
            emitInfos(bytes,slot_infos)

            method_bodys.push(bytes)
            Debug.log_mode::log("MethodBody "+info_index+" "+max_stack+" "+max_locals+" "+scope_depth+" "+max_scope+" length="+code.length+" slots="+slot_infos.length+" size="+bytes.length,body_out)
            Debug.exit("MethodBody")
            return method_bodys.length
        }

        function ScriptInfo(init_index,slot_infos)
        {
            Debug.log_mode::log("ScriptInfo init_index="+init_index+" slots="+slot_infos.length,info_out)
            var bytes = new ByteArray
            bytes.endian = "littleEndian";

            makeInt32(bytes,init_index)
            emitInfos(bytes,slot_infos)

            script_infos.push(bytes)
            return script_infos.length
        }

        // Emitter methods

        function emitVersion(bytes,minor,major)
        {
            Debug.enter("emitVersion",minor,major)
            makeInt16(bytes,minor)
            makeInt16(bytes,major)
            Debug.exit("emitVersion",bytes.length)
        }
        
        function emitConstantPool(bytes,pool)
        {
            Debug.enter("emitConstantPool",bytes.length,pool.length)
            var count = pool.length
            makeInt32(bytes,count==0?0:count+1)
            for( var i = 0; i < count; ++i )
            {
                bytes.writeBytes(pool[i])
            }
            Debug.exit("emitConstantPool",bytes.length)
        }
        
        function emitInfos(bytes,infos)
        {
            Debug.enter("emitInfos",infos.length)
            var count = infos.length
            makeInt32(bytes,count)
            for( var i = 0; i < count; ++i )
            {
                bytes.writeBytes(infos[i])
            }
            Debug.exit("emitInfos",bytes.length)
        }
        
        function emitClassInfos(bytes,instance_infos,class_infos)
        {
            Debug.enter("emitClassInfos")
            var count = instance_infos.length
            makeInt32(bytes,count)
            for( var i = 0; i < count; ++i )
            {
                bytes.writeBytes(instance_infos[i])
            }
            for( var i = 0; i < count; ++i )
            {
                bytes.writeBytes(class_infos[i])
            }
            Debug.exit("emitClassInfos",bytes.length)
        }

/*
 AbcFile {
   U16 minor_version                  // = 16
   U16 major_version                  // = 46
   U30 constant_int_pool_count
   ConstantInteger[constant_int_pool_count] // Cpool entries for integers
   U30 constant_uint_pool_count
   ConstantUInteger[constant_uint_pool_count] // Cpool entries for uints
   U30 constant_double_pool_count
   ConstantDouble[constant_double_pool_count] // Cpool entries for doubles
   U30 constant_string_pool_count
   ConstantString[constant_string_pool_count] // Cpool entries for strings
   U30 constant_namespace_pool_count
   ConstantNamespace[constant_namespace_pool_count] // Cpool entries for namespaces
   U30 constant_namespace_set_pool_count
   ConstantNamespaceSet[constant_namespace_set_pool_count] //Cpool entries for namespace sets
   U30 constant_multiname_pool_count
   ConstantMultiname[constant_multiname_pool_count] //Cpool entries for Multinames, Qnames, RTQnames, and RTQnamesLate
   U30 methods_count
   MethodInfo[methods_count]
   U30 metadata_count
   MetadataInfo[metadata_count]
   U30 class_count
   InstanceInfo[class_count]
   ClassInfo[class_count]
   U30 script_count
   ScriptInfo[script_count]         // ScriptInfo[script_count-1] is main entry point
   U30 bodies_count
   MethodBody[bodies_count]
}
*/

        public function emit() {
            Debug.enter("emit")
            
            var bytes = new ByteArray
            bytes.endian = "littleEndian";
            
            emitVersion(bytes,minor_version,major_version)
            emitConstantPool(bytes,int_pool)
            emitConstantPool(bytes,uint_pool)
            emitConstantPool(bytes,double_pool)
            emitConstantPool(bytes,utf8_pool)
            emitConstantPool(bytes,namespace_pool)
            emitConstantPool(bytes,namespaceset_pool)
            emitConstantPool(bytes,multiname_pool)
            emitInfos(bytes,method_infos)
            emitInfos(bytes,metadata_infos)
            emitClassInfos(bytes,instance_infos,class_infos)
            emitInfos(bytes,script_infos)
            emitInfos(bytes,method_bodys)

            Debug.log_mode::dump(int_pool_out)
            Debug.log_mode::dump(uint_pool_out)
            Debug.log_mode::dump(double_pool_out)
            Debug.log_mode::dump(utf8_pool_out)
            Debug.log_mode::dump(namespace_pool_out)
            Debug.log_mode::dump(namespaceset_pool_out)
            Debug.log_mode::dump(multiname_pool_out)
            Debug.log_mode::dump(info_logs)
            Debug.log_mode::dump(body_out)
            Debug.log_mode::dump(code_logs)

            Debug.exit("emit",bytes.length)
            
            return bytes
        }
        
        var code
        var code_blocks = []
        var code_logs = ["-------","C O D E","-------"]
        var info_logs = ["---------","I N F O S","---------"]
        var pending_code_logs = []
        var pending_info_logs = []
        var initial_scope_depth_stack = []
        
        /* RES Kludge - probably do something more complicated */

        var temp_pool_stack = [];
        var tempPool;

        function newTempPool( first:int ) {
            return {max_locals: first, available: new Array()};
        }

        function getLocalTemp() {
            if (tempPool.available.length > 0)
                return tempPool.available.pop();
            return tempPool.max_locals++;
        }

        function killLocalTemp(addr) {
            // put out a Kill instruction?
            tempPool.available.push(addr);
            FreeTemp(addr);
        }
            
        var local_count_stack = []
        
        var max_method_stack:int;
        var cur_method_stack:int;

        function stack(size:int):void
        {
            cur_method_stack += size;
            if (cur_method_stack > max_method_stack)
            {
                max_method_stack = cur_method_stack;
            }
        }

        var stackDepthStack:Array = new Array();
        var scopeDepthStack:Array = new Array();

        function saveStackDepth():void {
            stackDepthStack.push(cur_method_stack);
        }

        function restoreStackDepth():void {
            cur_method_stack = stackDepthStack.pop();
        }

        var stackInfoStack:Array = new Array();
            
        
        function StartMethod(node, name) 
        {
            Debug.enter("StartMethod")

            stackInfoStack.push({cur: cur_method_stack, max: max_method_stack});
            cur_method_stack = max_method_stack = 0;
            this.pending_code_logs.push(code_out)   // save current code log
            this.code_out = []                      // and start a new one
            this.pending_info_logs.push(info_out)   // save current info log
            this.info_out = []                      // and start a new one

            Debug.log_mode::log("StartMethod "+name,code_out)
            
            this.code_blocks.push(code)     // save the current code block
            this.code = new ByteArray       // create a new one
            this.code.endian = "littleEndian";
            initial_scope_depth_stack.push(scope_depth)
            temp_pool_stack.push(tempPool);
            var firstTemp =  1; 
            if (node.localName() == "Function") {
                var params:int = node.@paramCount;
                firstTemp = params + 1;
            }
            tempPool = newTempPool(firstTemp);

            Debug.exit("StartMethod")
        }
        
        function FinishMethod(node, name,slot_infos, need_activation) 
        {        
            Debug.enter("FinishMethod",name)
            Debug.log_mode::log("FinishMethod "+name,code_out)

            var param_count = 0
            if (node.localName() == "Function") {
                var params:int = node.@paramCount;
                param_count = params;
            }
            var type_index = 0
            var types = new Array
            var name_index = ConstantUtf8(name)
            var flags = (need_activation)? METHOD_Activation: 0;
            var optional_count = 0
            var optional_kinds = new Array
            
            var info_index = MethodInfo(param_count,type_index,types,name_index,flags,optional_count,optional_kinds)
            
            var max_stack = max_method_stack;
            var max_scope_depth = scope_depth
            var exceptions = new ByteArray
            exceptions.endian = "littleEndian";
            var initial_scope_depth = initial_scope_depth_stack.pop()

            MethodBody(info_index,max_stack,tempPool.max_locals,initial_scope_depth,max_scope_depth,this.code,exceptions,slot_infos)

            this.code = this.code_blocks.pop() // restore the previously active code block
            tempPool = temp_pool_stack.pop();

            this.code_logs.push(this.code_out)              // save the finish code log
            this.code_out = this.pending_code_logs.pop()    // resume the outer code log, pushing inner one deeper
            this.info_logs.push(this.info_out)
            this.info_out = this.pending_info_logs.pop()
            
            scope_depth = initial_scope_depth
            var stackDepths = stackInfoStack.pop();
            cur_method_stack = stackDepths.cur;
            max_method_stack = stackDepths.max;

            Debug.exit("FinishMethod",info_index)
            return info_index
        }

        function getNamespaceKind(kind_str : String)
        {
            var result = 
                    kind_str == "internal" ? CONSTANT_PackageInternalNS :
                    kind_str == "public" ? CONSTANT_PackageNamespace :
                    kind_str == "user" ? CONSTANT_Namespace :
                    kind_str == "private" ? CONSTANT_PrivateNamespace : CONSTANT_Namespace
            return result
        }

        function SlotInfo(slot_infos,kind,name,namespace) 
        {        
            Debug.enter("SlotInfo",name,kind,namespace)

            var slot_info = slot_infos.length //getMethodInfo(name)
            var bytes = new ByteArray
            bytes.endian = "littleEndian";

            var identifier_index = ConstantUtf8(name)
            var namespace_index = ConstantNamespace(ConstantUtf8(namespace.@name),getNamespaceKind(namespace.@kind))
            var name_index = ConstantQualifiedName(identifier_index,namespace_index,false)  // is_attr = false  

            makeInt32(bytes,name_index)
            makeByte(bytes,kind)

            var kind_str = kind==SLOT_var?"var":
                            kind==SLOT_function?"function":
                            "unimplemented slot kind"
            Debug.log_mode::log("SlotInfo "+kind_str+" "+name+" {"+dumpNamespaces([namespace])+"}",info_out)

            switch( kind )
            {
                case SLOT_var:
                    makeInt32(bytes,0)  // 0 = autoassign
                    makeInt32(bytes,0)  // type *
                    makeInt32(bytes,0)  // no default value
                    break
                case SLOT_method:
                case SLOT_getter:
                case SLOT_setter:
                case SLOT_class:
                case SLOT_function:
                    throw "slot kind not implemented"
                    break
            }

            slot_infos.push(bytes)
            var result = slot_info

            Debug.exit("SlotInfo")
            return result 
        }
                
        function getIP():int {
            return code.position;
        }
                
        function StartClass() 
        {
        }
        
        function FinishClass() 
        {
        }
        
        function StartProgram() 
        {
            this.pending_info_logs.push(info_out)   // save current code log
            this.info_out = []                      // and start a new one

            Debug.log_mode::log("StartProgram",code_out)
        }
        
        function FinishProgram(init_index,slot_infos) 
        {
            Debug.enter("FinishProgram",init_index)
            Debug.log_mode::log("FinishProgram",code_out)

            ScriptInfo(init_index,slot_infos)

            this.info_logs.push(this.info_out)
            this.info_out = this.pending_info_logs.pop()

            Debug.exit("FinishProgram")
        }
        
        // Abstract machine methods

        // If statement processsing
        
        function ifUnOp(name, opcode, offset = 0) {
            stack(-1);
            Debug.log_mode::log("  "+code.length+":" + name + " L" + lblnum, code_out);
            makeByte(code, opcode);
            makeInt24(code, offset);
        }
            
        function ifBinOp(name, opcode, offset = 0) {
            stack(-2);
            Debug.log_mode::log("  "+code.length+":" + name + " L" + lblnum, code_out);
            makeByte(code, opcode);
            makeInt24(code, offset);
        }

        public function If(kind) {
            switch (kind) {
            case IF_false:    ifUnOp("IfFalse", OP_iffalse); break;
            case IF_true:     ifUnOp("IfTrue", OP_iftrue); break;
            case IF_nlt:      ifBinOp("IfNlt", OP_ifnlt); break;
            case IF_nle:      ifBinOp("IfNle", OP_ifnle); break;
            case IF_ngt:      ifBinOp("IfNgt", OP_ifngt); break;
            case IF_nge:      ifBinOp("IfNge", OP_ifnge); break;
            case IF_lt:       ifBinOp("IfLt", OP_iflt); break;
            case IF_le:       ifBinOp("IfLe", OP_ifle); break;
            case IF_gt:       ifBinOp("IfGt", OP_ifgt); break;
            case IF_ge:       ifBinOp("IfGe", OP_ifge); break;
            case IF_eq:       ifBinOp("IfEq", OP_ifeq); break;
            case IF_ne:       ifBinOp("IfNe", OP_ifne); break;
            case IF_stricteq: ifBinOp("IfStrictEq", OP_ifstricteq); break;
            case IF_strictne: ifBinOp("IfStrictNe", OP_ifstrictne); break;
            default:
                throw new Error("invalid if kind " + kind);
                break;
            }
            if_addrs.push({addr: code.position - 3, lbl:lblnum++});
            saveStackDepth(); // to be correct at else branch
        }

        public function PatchIf(target:int) {
            restoreStackDepth();
            var if_addr = if_addrs.pop();
            Debug.log_mode::log("  "+code.length+": L" + if_addr.lbl + ":", code_out);
            var offset:int = target - if_addr.addr + 1 - 4;
            var savePos = code.position;
            code.position = if_addr.addr;
            makeInt24(code, offset);
            code.position = savePos;
        }

        public function Else() {
            Jump(lblnum);
            else_addrs.push({addr:code.position - 3, lbl:lblnum++});
        }

        public function PatchElse(target:int) {
            var else_addr = else_addrs.pop();
            Debug.log_mode::log("  "+code.length+": L" + else_addr.lbl + ":", code_out);
            var offset:int = target - else_addr.addr + 1 - 4;
            var savePos = code.position;
            code.position = else_addr.addr;
            makeInt24(code, offset);
            code.position = savePos;
        }
            

        // Unconditional jump

        function dumpNamespaces(nsset)
        {
            Debug.enter("dumpNamespaces",nsset.length)

            var result = ""
            if( nsset != void )
            for each( var ns in nsset )
            {
                if( result != "" )
                {
                    result += ","
                }
                result += ns.@kind +':"'+ns.@name+'"'
            }

            Debug.exit("dumpNamespaces",result)

            return result
        }

        function FindProperty(name,namespaces,is_qualified,is_attr,is_strict, is_late)
        {
            Debug.enter("FindProperty",name,namespaces,is_strict,is_qualified,is_attr)
            Debug.log_mode::log("  "+code.length+":FindProperty "+name+" {"+dumpNamespaces(namespaces)+"} is_qualified="+is_qualified+" is_strict="+is_strict, code_out)

            var index,name_index,ns_index
            
            if( name == "*" )
            {
                name_index = 0
            }
            else
            {
                name_index = ConstantUtf8(name)
            }

            /* We're not handling RTQname or RTQnameL style properties for now.
               When we do, the stack effects will be different */
            if (!is_late)
                stack(1);

            if (is_late) {
                ns_index = ConstantNamespaceSet(namespaces)
                index = ConstantMultinameL(ns_index,is_attr)
            }
            else if( is_qualified && namespaces.length == 1 )
            {
                var ns_name = namespaces[0].@name
                var ns_kind = namespaces[0].@kind=="internal"?CONSTANT_PackageInternalNS:CONSTANT_Namespace
                var ns_utf8_index = ConstantUtf8(ns_name)
                var ns_index = ConstantNamespace(ns_utf8_index,ns_kind)
                index = ConstantQualifiedName(name_index,ns_index,is_attr)
            }
            else
            {
                ns_index = ConstantNamespaceSet(namespaces)
                index = ConstantMultiname(name_index,ns_index,is_attr)
            }
            
            if( is_strict )
            {
                makeByte(code,OP_findpropstrict);
                makeInt32(code,index);
            }
            else
            {
                makeByte(code,OP_findproperty);
                makeInt32(code,index);
            }
            Debug.exit("FindProperty")
        }
        
        function CallProperty(name,namespaces,size,is_qualified,is_super,is_attr,is_lex)
        {
            Debug.enter("CallProperty",name,dumpNamespaces(namespaces),size,is_qualified,is_super,is_attr,is_lex)
            Debug.log_mode::log("  "+code.length+":CallProperty "+name+" {"+dumpNamespaces(namespaces)+"} "+size,code_out)

            var index,name_index,ns_index
            
            /* Currently doesn't implement RTQname or any L, such things load the
               function object and use Call instead.  If we start running them through
               here, we need to rethink stack effect */

            stack(-size); // pop obj + size args, push result
            
            if( name == "*" )
            {
                name_index = null
            }
            else
            {
                name_index = ConstantUtf8(name)
            }

            if( is_qualified && namespaces.length == 1 )
            {
                var ns_name = namespaces[0].@name
                var ns_kind = namespaces[0].@kind=="public"?CONSTANT_PackageNamespace:CONSTANT_PackageInternalNS
                var ns_utf8_index = ConstantUtf8(ns_name)
                var ns_index = ConstantNamespace(ns_utf8_index,ns_kind)
                index = ConstantQualifiedName(name_index,ns_index,is_attr)
            }
            else
            {
                ns_index = ConstantNamespaceSet(namespaces)
                index = ConstantMultiname(name_index,ns_index,is_attr)
            }
            
            if( is_super )
            {
                makeByte(code,OP_callsuper);
                makeInt32(code,index);
                makeInt32(code,size);
            }
            else
            if( is_lex )
            {
                makeByte(code,OP_callproplex);
                makeInt32(code,index);
                makeInt32(code,size);
            }
            else
            {
                makeByte(code,OP_callproperty);
                makeInt32(code,index);
                makeInt32(code,size);
            }
        }
        
        function SetProperty(name,namespaces,is_qualified,is_super,is_attr,is_constinit, is_late)
        {
            Debug.enter("SetProperty",name,namespaces,is_qualified,is_super,is_attr,is_constinit);
            Debug.log_mode::log("  "+code.length+":SetProperty "+name+" {"+dumpNamespaces(namespaces)+"} is_qualified="+is_qualified, code_out);
            
            var index,name_index,ns_index;
            stack((is_late)?-3:-2);
            
            if( name == "*" )
            {
                name_index = 0
            }
            else
            {
                name_index = ConstantUtf8(name)
            }

            if (is_late) {
                ns_index = ConstantNamespaceSet(namespaces)
                index = ConstantMultinameL(ns_index,is_attr)
            }
            else if( is_qualified && namespaces.length == 1 )
            {
                var ns_name = namespaces[0].@name
                var ns_kind = namespaces[0].@kind=="public"?CONSTANT_PackageNamespace:CONSTANT_PackageInternalNS
                var ns_utf8_index = ConstantUtf8(ns_name)
                var ns_index = ConstantNamespace(ns_utf8_index,ns_kind)
                index = ConstantQualifiedName(name_index,ns_index,is_attr)
            }
            else
            {
                ns_index = ConstantNamespaceSet(namespaces)
                index = ConstantMultiname(name_index,ns_index,is_attr)
            }
            
            if( is_super )
            {
                makeByte(code,OP_setsuper);
                makeInt32(code,index);
            }
            if( is_constinit )
            {
                makeByte(code,OP_initproperty);
                makeInt32(code,index);
            }
            else
            {
                makeByte(code,OP_setproperty);
                makeInt32(code,index);
            }
        }
        
        function GetProperty(name,namespaces,is_qualified,is_super,is_attr, is_late)
        {
            Debug.enter("GetProperty",name,namespaces,is_qualified,is_super,is_attr);
            Debug.log_mode::log("  "+code.length+":GetProperty "+name+" {"+dumpNamespaces(namespaces)+"}", code_out);
            
            var index,name_index,ns_index;

            if (is_late)
                stack(-1);
            
            if( name == "*" )
            {
                name_index = 0
            }
            else
            {
                name_index = ConstantUtf8(name)
            }

            if (is_late) {
                ns_index = ConstantNamespaceSet(namespaces)
                index = ConstantMultinameL(ns_index,is_attr)
            }
            else if( is_qualified && namespaces.length == 1 )
            {
                var ns_name = namespaces[0].@name
                var ns_kind = namespaces[0].@kind=="public"?CONSTANT_PackageNamespace:CONSTANT_PackageInternalNS
                var ns_utf8_index = ConstantUtf8(ns_name)
                var ns_index = ConstantNamespace(ns_utf8_index,ns_kind)
                index = ConstantQualifiedName(name_index,ns_index,is_attr)
            }
            else
            {
                ns_index = ConstantNamespaceSet(namespaces)
                index = ConstantMultiname(name_index,ns_index,is_attr)
            }
            
            if( is_super )
            {
                makeByte(code,OP_getsuper)
                makeInt32(code,index)
            }
            else
            {
                makeByte(code,OP_getproperty)
                makeInt32(code,index)
            }
        }
        
        function DeleteProperty(name,namespaces,is_qualified,is_super,is_attr, is_late)
        {
            Debug.enter("DeleteProperty",name,namespaces,is_qualified,is_super,is_attr);
            Debug.log_mode::log("  "+code.length+":DeleteProperty "+name+" {"+dumpNamespaces(namespaces)+"}", code_out);

            var index,name_index,ns_index;

            if (is_late)
                stack(-1);
            
            if( name == "*" )
            {
                name_index = 0
            }
            else
            {
                name_index = ConstantUtf8(name)
            }

            if (is_late) {
                ns_index = ConstantNamespaceSet(namespaces)
                index = ConstantMultinameL(ns_index,is_attr)
            }
            else if( is_qualified && namespaces.length == 1 )
            {
                var ns_name = namespaces[0].@name
                var ns_kind = namespaces[0].@kind=="public"?CONSTANT_PackageNamespace:CONSTANT_PackageInternalNS
                var ns_utf8_index = ConstantUtf8(ns_name)
                var ns_index = ConstantNamespace(ns_utf8_index,ns_kind)
                index = ConstantQualifiedName(name_index,ns_index,is_attr)
            }
            else
            {
                ns_index = ConstantNamespaceSet(namespaces)
                index = ConstantMultiname(name_index,ns_index,is_attr)
            }
            
            if( is_super )
            {
            }
            else
            {
                makeByte(code,OP_deleteproperty)
                makeInt32(code,index)
            }
        }
        
        function CheckType(name)
        {
            Debug.log_mode::log("  "+code.length+":CheckType "+name,code_out)

            var fullname = name.toString();
            if ("*" == fullname)
            {
                makeByte(code,OP_coerce_a);
            }
            else 
            if ("Object" == fullname)
            {
                makeByte(code,OP_coerce_o);
            }
            else 
            if ("String" == fullname)
            {
                makeByte(code,OP_coerce_s);
            }
            else 
            if ("Boolean" == fullname)
            {
                makeByte(code,OP_coerce_b);
            }
            else 
            if ("Number" == fullname)
            {
                makeByte(code,OP_coerce_d);
            }
            else 
            if ("int" == fullname)
            {
                makeByte(code,OP_coerce_i)
            }
            else if ("uint" == fullname)
            {
                makeByte(code,OP_coerce_u)
            }
            else
            {
/*
                int type_index = AddClassName(name);
                Coerce(*ab->code,class_index);
                makeByte(code,OP_coerce)
                makeInt32(code,type_index)
*/
            }

        }
    }

    import avmplus.Domain
    
    public function testABCEmitter() 
    {
//print("testABCEmitter")
        var emitter = new ABCEmitter()
        emitter.StartProgram()
        emitter.StartMethod()
        emitter.LoadThis()
        emitter.PushScope()
        emitter.FindProperty("print",[""],true,false,false)
        emitter.PushString("hello, world")
        emitter.CallProperty("print",[""],1,false,false,false,false)
        emitter.Pop()
        emitter.FindProperty("print",[""],true,false,false)
        emitter.PushString("goodbye, jeff")
        emitter.CallProperty("print",[""],1,false,false,false,false)
        emitter.Pop()
        emitter.LoadRegister(1)
        emitter.ReturnValue()
        emitter.FreeTemp(1)
        var init_index = emitter.FinishMethod("")
        emitter.FinishProgram(init_index)
        var abc = emitter.emit()
        //ByteArray.writeFile("temp.abc",abc)
        
        //print("temp.abc, "+abc.length+" bytes written")
        
        var dom = Domain.currentDomain
        dom.loadBytes(abc)
    }
    
    
}
