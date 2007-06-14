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
    class Script
    {
        private var emitter, init_method;

        function Script(emitter) {
            this.emitter = emitter;
        }

        function get init(): Method {
            if (init_method == null)
                init_method = new Method(emitter, []);
            return init_method;
        }

        function finalize() {
            init.finalize();
            emitter.file.addScript(new ABCScriptInfo(init.methodID));
        }
    }

    class Method extends ABCAssembler
    {
        private var emitter, formals;

        function Method(emitter, formals) {
            super(emitter.constants, formals.length);
            this.formals = formals;
            this.emitter = emitter;

            // Standard prologue
            I_getlocal_0();
            I_pushscope();
        }


        function finalize() {
            // Standard epilogue??

            var body = new ABCMethodBodyInfo(meth);
            body.setMaxStack(maxStack);
            body.setLocalCount(maxLocal);
            body.setMaxScopeDepth(maxScope);
            body.setCode(this);
        }
    }


    class ABCEmitter
    {
        var abcfile, constants;

        function ABCEmitter() {
            abcfile = new ABCFile;
            constants = new ABCConstantPool;
            abcfile.addConstants(constants);
        }

        function get abcfile() {
        }

        function get packageNamespace() {
            return constants.namespace(CONSTANT_PackageNamespace, cp.stringUtf8(""));
        }

        function QName(ns_idx, str) {
            return constants.namespace(ns_idx, cp.stringUtf8(str));
        }

        function newScript(): Script {
            return new Script(this);
        }

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
        
        function startProgram() 
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

        public function Else() {
            Jump(lblnum);
            else_addrs.push({addr:code.position - 3, lbl:lblnum++});
        }

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
