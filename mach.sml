(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 *
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 *
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 *
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * End of Terms and Conditions
 *
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 *)
(* "Virtual machine" for executing ES4 code. *)

structure Mach = struct

(* Local tracing machinery *)

val doTrace = ref false
val traceStack = ref false
fun log ss = LogErr.log ("[mach] " :: ss)
fun trace ss = if (!doTrace) then log ss else ()
fun error ss = LogErr.machError ss
fun error0 ss = LogErr.machError ss

structure StrListKey = struct type ord_key = string list val compare = List.collate String.compare end
structure StrListMap = SplayMapFn (StrListKey);

structure NsKey = struct type ord_key = Ast.NAMESPACE val compare = NameKey.cmpNS end
structure NsMap = SplayMapFn (NsKey);

structure NmKey = struct type ord_key = Ast.NAME val compare = NameKey.compare end
structure NmMap = SplayMapFn (NmKey);

structure StrKey = struct type ord_key = Ustring.STRING val compare = NameKey.cmp end
structure StrMap = SplayMapFn (StrKey);

structure Real64Key = struct type ord_key = Real64.real val compare = Real64.compare end
structure Real64Map = SplayMapFn (Real64Key);
          
fun nameEq (a:Ast.NAME) (b:Ast.NAME) = ((#id a) = (#id b) andalso (#ns a) = (#ns b))

val cachesz = 1024
                                       
type ATTRS = { dontDelete: bool,
               dontEnum: bool,
               readOnly: bool,
               isFixed: bool }     
             
datatype VAL = Object of OBJ
             | Wrapped of (VAL * Ast.TYPE_EXPR)
             | Null
             | Undef

     and OBJ =
         Obj of { ident: OBJ_IDENT,
                  tag: VAL_TAG,
                  props: PROP_BINDINGS,
                  proto: VAL ref,
                  magic: (MAGIC option) ref }

     and VAL_TAG =
         ObjectTag of Ast.FIELD_TYPE list
       | ArrayTag of Ast.TYPE_EXPR list
       | FunctionTag of Ast.FUNC_TYPE
       | ClassTag of Ast.INSTANCE_TYPE
       | NoTag (*
                * NoTag objects are made for scopes and
                * temporaries passed as arguments during
                * builtin construction.
                *)

     and OBJ_CACHE = 
         ObjCache of 
         {
          doubleCache: (OBJ Real64Map.map) ref,
          intCache: (OBJ Real64Map.map) ref,
          uintCache: (OBJ Real64Map.map) ref,
          nsCache: (OBJ NsMap.map) ref,
          nmCache: (OBJ NmMap.map) ref,
          strCache: (OBJ StrMap.map) ref
         }

     and PROFILER =
         Profiler of
         {
          profileMap: (int StrListMap.map) ref, (* = ref StrListMap.empty *)
          doProfile: (int option) ref (*  = ref NONE *)
         }

     and SPECIAL_OBJS = 
         SpecialObjs of 
         { 
          classClass : (OBJ option) ref,
          interfaceClass : (OBJ option) ref,
          namespaceClass : (OBJ option) ref,

          objectClass : (OBJ option) ref,
          arrayClass : (OBJ option) ref,
          functionClass : (OBJ option) ref,

          stringClass : (OBJ option) ref,
          stringWrapperClass : (OBJ option) ref,

          numberClass : (OBJ option) ref,
          intClass : (OBJ option) ref,
          uintClass : (OBJ option) ref,
          doubleClass : (OBJ option) ref,
          decimalClass : (OBJ option) ref,

          booleanClass : (OBJ option) ref,
          booleanWrapperClass : (OBJ option) ref,

          booleanTrue : (OBJ option) ref,
          booleanFalse : (OBJ option) ref,
          doubleNaN : (OBJ option) ref
         }

     and FRAME = 
         Frame of { name: string, args: VAL list }

(*
 * Magic is visible only to the interpreter;
 * it is not visible to users.
 *)

     and MAGIC =
         Boolean of bool
       | Double of Real64.real
       | Decimal of Decimal.DEC
       | String of Ustring.STRING
       | Namespace of Ast.NAMESPACE
       | Class of CLS_CLOSURE
       | Interface of IFACE_CLOSURE
       | Function of FUN_CLOSURE
       | Type of Ast.TYPE_EXPR
       | NativeFunction of NATIVE_FUNCTION

     and SCOPE =
         Scope of { object: OBJ,
                    parent: SCOPE option,
                    temps: TEMPS,
                    decimal: DECIMAL_CONTEXT,
                    kind: SCOPE_KIND }

     and SCOPE_KIND =
         WithScope
       | GlobalScope
       | InstanceScope
       | ActivationScope
       | BlockScope
       | TypeArgScope

     and TEMP_STATE = UninitTemp
                    | ValTemp of VAL

     and PROP_STATE = TypeVarProp
                    | TypeProp
                    | UninitProp
                    | ValProp of VAL

                    (* One might imagine that namespaces, methods and
                     * the 'arguments' object in a function can all be stored
                     * as instances of the classes Namespace, Function and
                     * Array, respectively. This works in some contexts, but leads
                     * to feedback loops when constructing the classes Function,
                     * Namespace and Array themselves. So instead of eagerly instantiating
                     * such values, we allocate them as the following 3 "lazy"
                     * property states. If anyone performs a "get" on these property
                     * states, a Namespace, Function or Array object (respectively) is
                     * constructed. We then ensure that the Namespace, Function and Array
                     * constructors, settings and initializers (in the builtins)
                     * do *not* cause any "get" operations on properties in these states.
                     *
                     * FIXME: The 'arguments' object can't be an array.
                     *)
                    | NamespaceProp of Ast.NAMESPACE
                    | MethodProp of FUN_CLOSURE
                    | ValListProp of VAL list

                    | NativeFunctionProp of NATIVE_FUNCTION
                    | VirtualValProp of
                      { getter: FUN_CLOSURE option,
                        setter: FUN_CLOSURE option }

     and AUX = 
         Aux of 
         { 
          (* 
           * Auxiliary machine/eval data structures, not exactly
           * spec-normative, but important! Embedded in REGS.
           *)
          booting: bool ref,
          specials: SPECIAL_OBJS,
          stack: FRAME list ref,
          objCache: OBJ_CACHE, 
          profiler: PROFILER 
         }

withtype FUN_CLOSURE =
         { func: Ast.FUNC,
           this: OBJ option,
           env: SCOPE }

     and CLS_CLOSURE =
         { cls: Ast.CLS,
           env: SCOPE }

     and IFACE_CLOSURE =
         { iface: Ast.IFACE,
           env: SCOPE }

     and DECIMAL_CONTEXT = 
         { precision: int,
           mode: DecimalParams.ROUNDING_MODE }

     and REGS = 
         { 
          scope: SCOPE,
          this: OBJ,
          thisFun: OBJ option,
          global: OBJ,
          prog: Fixture.PROGRAM,          
          aux: AUX
         }

     and NATIVE_FUNCTION =
         { func: ({ scope: SCOPE, 
                    this: OBJ, 
                    thisFun: OBJ option,
                    global: OBJ, 
                    prog: Fixture.PROGRAM, 
                    aux: AUX } (* REGS *)
                  -> VAL list -> VAL),
           length: int }

     and OBJ_IDENT =
         int

(* Important to model "fixedness" separately from
 * "dontDelete-ness" because fixedness affects
 * which phase of name lookup the name is found during.
 *)

     and TEMPS = (Ast.TYPE_EXPR * TEMP_STATE) list ref

     and PROP = { ty: Ast.TYPE_EXPR,
                  state: PROP_STATE,
                  attrs: ATTRS }

     and PROP_BINDINGS = { max_seq: int,
			               bindings: { seq: int,
				                       prop: (* PROP *)
				                                 { ty: Ast.TYPE_EXPR,   
					                               state: PROP_STATE,
					                               attrs: ATTRS } } NameMap.map } ref 
			 
			 
(* Exceptions for control transfer. *)

exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> VAL)
exception ThrowException of VAL
exception ReturnException of VAL
exception StopIterationException

fun isObject (v:VAL) : bool =
    case v of
        Object _ => true
      | _ => false


fun isDouble (v:VAL) : bool =
    case v of
        Object (Obj ob) =>
        (case !(#magic ob) of
             SOME (Double _) => true
           | _ => false)
      | _ => false


fun isDecimal (v:VAL) : bool =
    case v of
        Object (Obj ob) =>
        (case !(#magic ob) of
             SOME (Decimal _) => true
           | _ => false)
      | _ => false


fun isString (v:VAL)
    : bool =
    case v of
        Object (Obj ob) =>
        (case !(#magic ob) of
             SOME (String _) => true
           | _ => false)
      | _ => false


fun isBoolean (v:VAL) : bool =
    case v of
        Object (Obj ob) =>
        (case !(#magic ob) of
             SOME (Boolean _) => true
           | _ => false)
      | _ => false


fun isNamespace (v:VAL) : bool =
    case v of
        Object (Obj ob) =>
        (case !(#magic ob) of
             SOME (Namespace _) => true
           | _ => false)
      | _ => false


fun isClass (v:VAL) : bool =
    case v of
        Object (Obj ob) =>
        (case !(#magic ob) of
             SOME (Class _) => true
           | _ => false)
      | _ => false


fun isInterface (v:VAL) : bool =
    case v of
        Object (Obj ob) =>
        (case !(#magic ob) of
             SOME (Interface _) => true
           | _ => false)
      | _ => false


fun isFunction (v:VAL) : bool =
    case v of
        Object (Obj ob) =>
        (case !(#magic ob) of
             SOME (Function _) => true
           | _ => false)
      | _ => false


fun isType (v:VAL) : bool =
    case v of
        Object (Obj ob) =>
        (case !(#magic ob) of
             SOME (Type _) => true
           | _ => false)
      | _ => false


fun isNativeFunction (v:VAL) : bool =
    case v of
        Object (Obj ob) =>
        (case !(#magic ob) of
             SOME (NativeFunction _) => true
           | _ => false)
      | _ => false


fun isNumeric (v:VAL) : bool =
    case v of
        Object (Obj ob) =>
        (case !(#magic ob) of
             SOME (Double _) => true
           | SOME (Decimal _) => true
           | _ => false)
      | _ => false

fun isNull (v:VAL) : bool =
    case v of
        Null => true
      | _ => false

fun isUndef (v:VAL) : bool =
    case v of
        Undef => true
      | _ => false

(*
 * The "machine type" of a value here is an ES3-ism. It exists only for
 * compatibility, and has nothing to do with the ES4 type system.
 *
 * The important part is that in ES3 algorithms, a machine value has
 * exactly *one* of these types. No overlap!
 *)

datatype MACHTY = TYNULL | TYUNDEF | TYNUMBER | TYSTRING | TYBOOLEAN | TYOBJECT

fun es3Type (v:VAL) : MACHTY =
    if isNull v then TYNULL
    else if isUndef v then TYUNDEF
    else if isNumeric v then TYNUMBER
    else if isString v then TYSTRING
    else if isBoolean v then TYBOOLEAN
    else TYOBJECT

fun isSameType (va:VAL) (vb:VAL) : bool =
    es3Type va = es3Type vb

(* Binding operations. *)

fun newPropBindings _ : PROP_BINDINGS =
    ref { max_seq = 0, bindings = NameMap.empty }

fun addProp (b:PROP_BINDINGS)
            (n:Ast.NAME)
            (x:PROP)
    : unit =
    let
	val { max_seq, bindings } = !b	
	val s = max_seq + 1
	val binding = { seq = s, prop = x }
	val bindings = NameMap.insert (bindings, n, binding)
    in
	b := { max_seq = s, bindings = bindings }
    end

fun delProp (b:PROP_BINDINGS)
            (n:Ast.NAME)
    : unit =
    let
	val { max_seq, bindings } = !b	
	val (bindings, _) = NameMap.remove (bindings, n)
    in
	b := { max_seq = max_seq, bindings = bindings }
    end

fun findProp (b:PROP_BINDINGS)
             (n:Ast.NAME)
    : PROP option =
    let
	val { bindings, ... } = !b
    in
	case NameMap.find (bindings, n) of
	    NONE => NONE
	  | SOME { prop, ... } => SOME prop
    end

fun matchProps (fixedProps:bool)
               (b:PROP_BINDINGS)
               (searchId:Ast.IDENT)
               (nss:Ast.NAMESPACE list)
    : Ast.NAME list =
    let
        fun tryNS ns =
            let
                val name = {id=searchId, ns=ns}
            in
                case findProp b name of
                    NONE => NONE
                  | SOME _ => SOME name
            end
    in
        List.mapPartial tryNS nss
    end

fun getProp (b:PROP_BINDINGS)
            (n:Ast.NAME)
    : PROP =
    case findProp b n of
        SOME p => p
      | NONE =>
        (*
         * If not found, then cons up a temporary property
         * with value undefined. Any property not found
         * errors would have been caught by evalRefExpr
         *)
        {ty=Ast.SpecialType Ast.Undefined,
         state=ValProp Undef,
         attrs={dontDelete=false,  (* unused attrs *)
                dontEnum=false,
                readOnly=false,
                isFixed=false}}


fun hasProp (b:PROP_BINDINGS)
            (n:Ast.NAME)
    : bool =
    case findProp b n of
        NONE => false
      | SOME _ => true


fun hasMagic (ob:OBJ) =
    case ob of
        Obj { magic, ... } =>
        case !magic of
            SOME _ => true
          | NONE => false

fun setPropDontEnum (props:PROP_BINDINGS)
                    (n:Ast.NAME)
                    (dontEnum:bool)
    : unit =
    case findProp props n of
        SOME prop =>
        let
            val attrs = (#attrs prop)
            val newProp = { ty = (#ty prop),
                            state = (#state prop),
                            attrs = { dontDelete = (#dontDelete attrs),
                                      dontEnum = dontEnum,
                                      readOnly = (#readOnly attrs),
                                      isFixed = (#isFixed attrs) } }
        in
            delProp props n;
            addProp props n newProp
        end
      | NONE => ()


(* Safe: will overflow when it runs out of identities. *)
val currIdent = ref 0
fun nextIdent _ =    
    (currIdent := (((!currIdent) + 1) 
                   handle Overflow => error ["overflowed maximum object ID"]);
     !currIdent)

fun newObj (t:VAL_TAG)
           (p:VAL)
           (m:MAGIC option)
    : OBJ =
    Obj { ident = nextIdent (),
          tag = t,
          props = newPropBindings (),
          proto = ref p,
          magic = ref m }

fun newObjNoTag _
    : OBJ =
    newObj NoTag Null NONE

fun getProto (ob:OBJ)
    : VAL =
    let
         val Obj {proto, ...} = ob
    in
        !proto
    end

fun setProto (ob:OBJ) (p:VAL)
    : OBJ =
    let
         val Obj {proto, ...} = ob
    in
        proto := p;
        ob
    end

fun getTemp (temps:TEMPS)
            (n:int)
    : VAL =
    let        
        val _ = trace ["getTemp ",Int.toString n]                
    in
        if n >= length (!temps)
        then LogErr.machError ["getting out-of-bounds temporary"]
        else 
            case List.nth ((!temps), n) of
                (_, UninitTemp) => LogErr.machError ["getting uninitialized temporary ",Int.toString n]
              | (_, ValTemp v) => v
    end

fun defTemp (temps:TEMPS)
            (n:int)
            (v:VAL)
    : unit =
    let
        val _ = trace ["defTemp ",Int.toString n]
        fun replaceNth k [] = LogErr.machError ["temporary-definition error"]
          | replaceNth k (x::xs) =
            if k = 0
            then (case x of
                      (t, UninitTemp) =>
                      ((* FIXME: put typecheck here *)
                       (t, ValTemp v) :: xs)
                    | (t, _) =>
                       (t, ValTemp v) :: xs)
                (* ISSUE: we allow redef of temps: LogErr.machError ["re-defining temporary"]) *)
            else x :: (replaceNth (k-1) xs)
    in
        if n >= (length (!temps))
        then LogErr.machError ["defining out-of-bounds temporary"]
        else temps := replaceNth n (!temps)
    end


fun isIntegral d = 
    let
        val truncated = Real64.realTrunc d
    in
        if Real64.isFinite d
        then Real64.==(truncated, d)
        else false
    end
         
fun isInRange (low:Real64.real) 
              (high:Real64.real) 
              (d:Real64.real) 
  : bool = 
    low <= d andalso d <= high
                     
fun fitsInUInt (d:Real64.real) : bool 
  = isIntegral d andalso isInRange 0.0 4294967295.0 d

fun fitsInInt (d:Real64.real) : bool 
  = isIntegral d andalso isInRange (~2147483647.0) 2147483647.0 d


(* 
 * Some stringification helpers on low-level values.
 *)

fun magicToUstring (magic:MAGIC)
    : Ustring.STRING =
    case magic of
        Double n => NumberToString n
      | Decimal d => Ustring.fromString (Decimal.toString d)
      | String s => s
      | Boolean true => Ustring.true_
      | Boolean false => Ustring.false_
      | Namespace ns => Ustring.fromString (LogErr.namespace ns)
      | Class _ => Ustring.fromString "[class Class]"
      | Interface _ => Ustring.fromString "[interface Interface]"
      | Function _ => Ustring.fromString "[function Function]"
      | Type _ => Ustring.fromString "[type Type]"
      | NativeFunction _ => Ustring.fromString "[function Function]"


(*
 * ES-262-3 9.8.1: ToString applied to the Number (double) type.
 *)

and NumberToString (r:Real64.real)
    : Ustring.STRING =
    if Real64.isNan r
    then Ustring.NaN_
    else
        if Real64.==(0.0, r) orelse Real64.==(~0.0, r)
        then Ustring.zero
        else
            if Real64.<(r, 0.0)
            then Ustring.append [Ustring.dash, NumberToString (Real64.~(r))]
            else
                if Real64.==(Real64.posInf, r)
                then Ustring.Infinity_
                else
                    let
                        (*
                         * Unfortunately SML/NJ has a pretty deficient selection of the numerical
                         * primitives; about the best we can get from it is a high-precision SCI
                         * conversion that we then parse. This is significantly more fun than
                         * writing your own dtoa.
                         *)
                        val x = Real64.fmt (StringCvt.SCI (SOME 30)) r

                        val (mantissaSS,expSS) = Substring.splitr (fn c => not (c = #"E")) (Substring.full x)
                        val mantissaSS = Substring.dropr (fn c => (c = #"E") orelse (c = #"0")) mantissaSS
                        val (preDot,postDot) = Substring.position "." mantissaSS
                        val postDot = Substring.triml 1 postDot

                        val exp = valOf (Int.fromString (Substring.string expSS))
                        val digits = (Substring.explode preDot) @ (Substring.explode postDot)
                        val k = length digits
                        val n = exp + 1

                        fun zeroes z = List.tabulate (z, (fn _ => #"0"))
                        fun expstr _ = (#"e" ::
                                        (if (n-1) < 0 then #"-" else #"+") ::
                                        (String.explode (Int.toString (Int.abs (n-1)))))
                    in
                        Ustring.fromString
                        (String.implode
                         (if k <= n andalso n <= 21
                          then digits @ (zeroes (n-k))
                          else
                              if 0 < n andalso n <= 21
                              then (List.take (digits, n)) @ [#"."] @ (List.drop (digits, n))
                              else
                                  if ~6 < n andalso n <= 0
                                  then [#"0", #"."] @ (zeroes (~n)) @ digits
                                  else
                                      if k = 1
                                      then digits @ (expstr())
                                      else (hd digits) :: #"." :: ((tl digits) @ expstr())))
                    end


fun inspect (v:VAL)
            (d:Int32.int)
    : unit =
    let
        val pad = "          "
        fun p 0 s = List.app TextIO.print s
          | p n s = (TextIO.print pad; p (n-1) s)

        fun nl _ = TextIO.print "\n";

        fun att {dontDelete,dontEnum,readOnly,isFixed} =
            if not dontDelete
               andalso not dontEnum
               andalso not readOnly
               andalso not isFixed
            then ""
            else
                (" ("
                 ^ (if dontDelete then "DD," else "")
                 ^ (if dontEnum then "DE," else "")
                 ^ (if readOnly then "RO," else "")
                 ^ (if isFixed then "FX" else "")
                 ^ ") ")

        fun id (Obj ob) = Int.toString (#ident ob)

        fun typ t = LogErr.ty t

        fun magType t = 
            case t of 
                Class { cls = Ast.Cls { instanceType, classType, ... }, ... } => 
                (" : instanceType=" ^ (typ instanceType) ^ ", classType=" ^ (typ classType))
              | Interface { iface = Ast.Iface { instanceType, ... }, ... } => 
                (" : instanceType=" ^ (typ instanceType))
              | Function { func = Ast.Func { ty=ty0, ... }, ... } => 
                (" : " ^ (typ ty0))
              | Type t => (" = " ^ (typ t))
              | _ => ""
                
        fun tag (Obj ob) =
            case (#tag ob) of
                (* FIXME: elaborate printing of structural tags. *)
                ObjectTag _ => "<Obj>"
              | ArrayTag _ => "<Arr>"
              | FunctionTag t => "<Fn " ^ (typ (Ast.FunctionType t)) ^ ">"
              | ClassTag t => "<Class " ^ (typ (Ast.InstanceType t)) ^ ">"
              | NoTag => "<NoTag>"

        (* FIXME: elaborate printing of type expressions. *)
        fun mag m = case m of
                        String s => ("\"" ^ (Ustring.toAscii s) ^ "\"")
                      | m => Ustring.toAscii (magicToUstring m) ^ (magType m)

        fun printVal indent _ Undef = TextIO.print "undefined\n"
          | printVal indent _ Null = TextIO.print "null\n"
          | printVal indent n (Wrapped (v, t)) = 
            (TextIO.print ("wrapped " ^ (typ t) ^ ":\n");
             printVal (indent+1) n v)

          | printVal indent 0 (Object (Obj ob)) =
            (TextIO.print (case !(#magic ob) of
                               NONE => tag (Obj ob)
                             | SOME m => mag m);
             TextIO.print "\n")

          | printVal indent n (Object obj) =
            let
                fun subVal i v = printVal (i+1) (n-1) v
                fun prop np =
                    let
			val (n,binding) = np
			val {prop={ty=ty0, state, attrs}, seq} = binding
                        val indent = indent + 1
                        val stateStr =
                            case state of
                                TypeVarProp => "[typeVar]"
                              | TypeProp => "[type]"
                              | UninitProp => "[uninit]"
                              | ValProp v => "[val]"
                              | VirtualValProp _ => "[virtual val]"
                              | MethodProp _ => "[method]"
                              | NativeFunctionProp _ => "[native function]"
                              | NamespaceProp _ => "[namespace]"
                              | ValListProp _ => "[val list]"
                    in
                        p indent ["   prop = ", LogErr.name n, ": ", typ ty0, att attrs,  " = "];
                        case state of
                            ValProp v => subVal indent v
                          | _ => TextIO.print (stateStr ^ "\n")
                    end
                val Obj { magic, props, proto, ... } = obj
		val { bindings, ... } = !props
            in
                TextIO.print "Obj {\n";
                (case !magic of
                     SOME m => (p indent ["  magic = ", (mag m)]; nl())
                   | NONE => ());
                p indent ["    tag = ", (tag obj)]; nl();
                p indent ["  ident = ", (id obj)]; nl();
                p indent ["  proto = "]; subVal indent (!proto);
                p indent ["  props = ["]; nl();
                NameMap.appi prop bindings;
                p indent ["          ] }"]; nl()
            end
    in
        printVal 0 d v
    end


fun magStr (SOME mag) = Ustring.toAscii (magicToUstring mag)
  | magStr NONE = "<none>"
                  
fun setMagic (ob:OBJ) (m:MAGIC option)
    : OBJ =
    let
         val Obj {magic, ident, ...} = ob                      
    in
        trace ["changing magic on obj #", Int.toString ident, 
               " from ", magStr (!magic), " to ", magStr m];
        magic := m;        
        ob
    end


fun newObject (t:VAL_TAG)
              (p:VAL)
              (m:MAGIC option)
    : VAL =
    let
        val obj = newObj t p m
        val Obj { magic, ident, ... } = obj
    in
        trace ["setting initial magic on obj #", Int.toString ident, 
               " to ", magStr (!magic)];
        Object (obj)
    end
    

(*
 * To get from any object to its CLS, you work out the
 * "nominal base" of the object's tag. You can then find
 * a fixed prop in the global object that has a "Class"
 * magic value pointing to the CLS.
 *)

fun nominalBaseOfTag (t:VAL_TAG)
    : Ast.NAME =
    case t of
        ObjectTag _ => Name.nons_Object
      | ArrayTag _ => Name.nons_Array
      | FunctionTag _ => Name.nons_Function
      | ClassTag ity => (#name ity)
      | NoTag => error ["searching for nominal base of no-tag object"]

fun getObjMagic (ob:OBJ)
    : (MAGIC option) =
    case ob of
        Obj ob' => !(#magic ob')

fun getMagic (v:VAL)
    : (MAGIC option) =
    case v of
        Object (Obj ob) => !(#magic ob)
      | _ => NONE

fun needMagic (v:VAL)
    : (MAGIC) =
    case v of
        Object (Obj ob) => valOf (!(#magic ob))
      | _ => (inspect v 1; 
              error ["require object with magic"])

fun needClass (v:VAL)
    : (CLS_CLOSURE) =
    case needMagic v of
        Class cls => cls
      | _ => (inspect v 1; 
              error ["require class object"])

fun needInterface (v:VAL)
    : (IFACE_CLOSURE) =
    case needMagic v of
        Interface iface => iface
      | _ => (inspect v 1; 
              error ["require interface object"])

fun needFunction (v:VAL)
    : (FUN_CLOSURE) =
    case needMagic v of
        Function f => f
      | _ => (inspect v 1; 
              error ["require function object"])

fun needType (v:VAL)
    : (Ast.TYPE_EXPR) =
    case needMagic v of
        Type t => t
      | _ => (inspect v 1; 
              error ["require type object"])


(* Call stack and debugging stuff *)

(* An approximation of an invocation argument list, for debugging. *)
fun approx (arg:VAL)
    : string =
    case arg of
        Null => "null"
      | Undef => "undefined"
      | Wrapped (v, t) => "wrapped(" ^ (approx v) ^ ")"
      | Object ob =>
        if hasMagic ob
        then
            let
                val str = Ustring.toAscii (magicToUstring (needMagic arg))
            in
                if isString arg
                then "\"" ^ str ^ "\""
                else str
            end
        else
            "obj"

fun stackOf (regs:REGS) 
    : (FRAME list) =
    let 
        val { aux = Aux { stack, ...}, ... } = regs
    in
        !stack
    end


fun stackString (stack:FRAME list) =
    let
        fun fmtFrame (Frame { name, args }) =
            name ^ "(" ^ (LogErr.join ", " (map approx args)) ^ ")"
    in
        "[" ^ (LogErr.join " | " (map fmtFrame (List.rev (stack)))) ^ "]"
    end


fun resetProfile (regs:REGS) : unit =
    let
        val { aux = 
              Aux { profiler = 
                    Profiler { profileMap, ... }, 
                    ...}, 
              ... } = regs
    in
        profileMap := StrListMap.empty
    end

fun resetStack (regs:REGS) : unit =
    let
        val { aux = 
              Aux { stack, ...}, 
              ... } = regs
    in
        stack := []
    end

fun push (regs:REGS) 
         (name:string) 
         (args:VAL list) 
    : unit =
    let 
        val { aux = 
              Aux { stack, 
                    profiler =
                    Profiler { doProfile,
                               profileMap }, 
                    ... }, 
              ... } = regs
        val newStack = (Frame { name = name, args = args }) :: (!stack)
    in
        stack := newStack;
        if !traceStack
        then LogErr.log ("[stack] -> " :: [stackString (!stack)])
        else ();
        case !doProfile of
            NONE => ()
          | SOME n =>
            let
                val n = Int.min (n, length newStack)
                val frameNames = map (fn Frame { name, ...} => name) (List.take (newStack, n))
                val count = case StrListMap.find ((!profileMap), frameNames) of
                                NONE => 1
                              | SOME k => (k+1)
            in
                profileMap := StrListMap.insert ((!profileMap), frameNames, count)
            end
    end

fun reportProfile (regs:REGS)
    : unit = 
    let 
        val { aux = 
              Aux { profiler =
                    Profiler { doProfile,
                               profileMap }, 
                    ... },
              ... } = regs
    in
        case !doProfile of
            NONE => ()
          | SOME _ =>
            let
                val items = StrListMap.listItemsi (!profileMap)
                val itemArr = Array.fromList items
                fun sort ((a,acount), (b,bcount)) = Int.compare (acount,bcount)
                fun emitEntry (names, count) =
                    let
                        val n = LogErr.join " | " (List.rev names)
                    in
                        LogErr.log ["[prof] ", (Int.toString count), " : ", n]
                    end
            in
                ArrayQSort.sort sort itemArr;
                Array.app emitEntry itemArr
            end
 
    end
fun pop (regs:REGS) 
    : unit =
    let 
        val { aux = Aux { stack, ...}, ... } = regs
        val newStack = tl (!stack)
    in
        if !traceStack
        then LogErr.log ("[stack] <- " :: [stackString (!stack)])
        else ();
        stack := newStack
    end

fun isBooting (regs:REGS) 
    : bool =
    let 
        val { aux = Aux { booting, ...}, ... } = regs
    in
        !booting
    end

fun setBooting (regs:REGS) 
               (isBooting:bool)
    : unit =
    let 
        val { aux = Aux { booting, ...}, ... } = regs
    in
        booting := isBooting
    end

fun getSpecials (regs:REGS) =
    let 
        val { aux = Aux { specials = SpecialObjs ss, ... }, ... } = regs
    in
        ss
    end

fun getClassClassSlot (regs:REGS) = (#classClass (getSpecials regs))
fun getInterfaceClassSlot (regs:REGS) = (#interfaceClass (getSpecials regs))
fun getNamespaceClassSlot (regs:REGS) = (#namespaceClass (getSpecials regs))

fun getObjectClassSlot (regs:REGS) = (#objectClass (getSpecials regs))
fun getArrayClassSlot (regs:REGS) = (#arrayClass (getSpecials regs))
fun getFunctionClassSlot (regs:REGS) = (#functionClass (getSpecials regs))
fun getStringClassSlot (regs:REGS) = (#stringClass (getSpecials regs))
fun getStringWrapperClassSlot (regs:REGS) = (#stringWrapperClass (getSpecials regs))

fun getNumberClassSlot (regs:REGS) = (#numberClass (getSpecials regs))
fun getIntClassSlot (regs:REGS) = (#intClass (getSpecials regs))
fun getUintClassSlot (regs:REGS) = (#uintClass (getSpecials regs))
fun getDoubleClassSlot (regs:REGS) = (#doubleClass (getSpecials regs))
fun getDecimalClassSlot (regs:REGS) = (#decimalClass (getSpecials regs))

fun getBooleanClassSlot (regs:REGS) = (#booleanClass (getSpecials regs)) 
fun getBooleanWrapperClassSlot (regs:REGS) = (#booleanWrapperClass (getSpecials regs)) 

fun getBooleanTrueSlot (regs:REGS) = (#booleanTrue (getSpecials regs)) 
fun getBooleanFalseSlot (regs:REGS) = (#booleanFalse (getSpecials regs)) 
fun getDoubleNaNSlot (regs:REGS) = (#doubleNaN (getSpecials regs)) 

fun getCaches (regs:REGS) =
    let 
        val { aux = Aux { objCache = ObjCache vc, ... }, ... } = regs
    in
        vc
    end

fun findInCache cacheGetter 
                cacheQuery 
                (regs:REGS) 
                key = 
    let 
        val c = cacheGetter regs
    in
        cacheQuery ((!c), key)
    end

fun updateCache cacheGetter
                cacheNumItems
                cacheInsert
                (regs:REGS)
                (k,v) =
    let
        val c = cacheGetter regs 
    in
        if cacheNumItems (!c) < cachesz
        then ((c := cacheInsert ((!c), k, v)); v)
        else v
    end

fun getDoubleCache (regs:REGS) = (#doubleCache (getCaches regs)) 
fun getUIntCache (regs:REGS) = (#uintCache (getCaches regs)) 
fun getIntCache (regs:REGS) = (#intCache (getCaches regs)) 
fun getNsCache (regs:REGS) = (#nsCache (getCaches regs)) 
fun getNmCache (regs:REGS) = (#nmCache (getCaches regs)) 
fun getStrCache (regs:REGS) = (#strCache (getCaches regs)) 

val findInDoubleCache = findInCache getDoubleCache Real64Map.find
val findInUIntCache = findInCache getUIntCache Real64Map.find
val findInIntCache = findInCache getIntCache Real64Map.find
val findInNsCache = findInCache getNsCache NsMap.find
val findInNmCache = findInCache getNmCache NmMap.find
val findInStrCache = findInCache getStrCache StrMap.find

val updateDoubleCache = updateCache getDoubleCache Real64Map.numItems Real64Map.insert
val updateUIntCache = updateCache getUIntCache Real64Map.numItems Real64Map.insert
val updateIntCache = updateCache getIntCache Real64Map.numItems Real64Map.insert
val updateNsCache = updateCache getNsCache NsMap.numItems NsMap.insert
val updateNmCache = updateCache getNmCache NmMap.numItems NmMap.insert
val updateStrCache = updateCache getStrCache StrMap.numItems StrMap.insert

val defaultDecimalContext = 
	{ precision = 34,
	  mode = DecimalParams.HalfEven } 

fun makeGlobalScopeWith (global:OBJ) 
    : SCOPE =
    Scope { object = global,
            parent = NONE,
            temps = ref [],
            decimal = defaultDecimalContext,
            kind = GlobalScope }

fun makeInitialRegs (prog:Fixture.PROGRAM)
                    (glob:OBJ)                     
    : REGS =
    let 
        val prof = Profiler 
                       { profileMap = ref StrListMap.empty,
                         doProfile = ref NONE }
        val ocache = ObjCache 
                     { doubleCache = ref Real64Map.empty,
                       uintCache = ref Real64Map.empty,
                       intCache = ref Real64Map.empty,
                       nsCache = ref NsMap.empty,
                       nmCache = ref NmMap.empty,
                       strCache = ref StrMap.empty }
        val specials = SpecialObjs 
                       { classClass = ref NONE,
                         interfaceClass = ref NONE,
                         namespaceClass = ref NONE,
                         objectClass = ref NONE,
                         arrayClass = ref NONE,
                         functionClass = ref NONE,
                         stringClass = ref NONE,
                         stringWrapperClass = ref NONE,
                         numberClass = ref NONE,
                         intClass = ref NONE,
                         uintClass = ref NONE,
                         doubleClass = ref NONE,
                         decimalClass = ref NONE,
                         booleanClass = ref NONE,
                         booleanWrapperClass = ref NONE,

                         booleanTrue = ref NONE,
                         booleanFalse = ref NONE,
                         doubleNaN = ref NONE }
        val aux = Aux { booting = ref false,
                        specials = specials,
                        stack = ref [],
                        objCache = ocache,
                        profiler = prof }
    in        
        { this = glob,
          global = glob,          
          thisFun = NONE,
          scope = makeGlobalScopeWith glob,
          prog = prog,
          aux = aux }
    end

(* native function stuff *)

(* 
 * FIXME: it is probably tidier if we push this into aux, but it's really 
 * not a high priority; they only get set at SML-heap-load time anyways.
 *)

val nativeFunctions: (Ast.NAME * NATIVE_FUNCTION) list ref = ref []

fun registerNativeFunction (name:Ast.NAME)
                           (func:NATIVE_FUNCTION)
    : unit =
    (trace ["registering native function: ", LogErr.name name];
     nativeFunctions := (name, func) :: (!nativeFunctions))


fun getNativeFunction (name:Ast.NAME)
    : NATIVE_FUNCTION =
    let
        fun search [] = LogErr.hostError ["native function not found: ",
					                      LogErr.name name]
          | search ((n,f)::bs) =
            if n = name
            then f
            else search bs
    in
        search (!nativeFunctions)
    end

end




