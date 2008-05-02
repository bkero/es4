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

structure StrKey = struct type ord_key = Ustring.STRING val compare = (fn (a,b) => Ustring.compare a b) end
structure StrMap = SplayMapFn (StrKey);

structure Real64Key = struct type ord_key = Real64.real val compare = Real64.compare end
structure Real64Map = SplayMapFn (Real64Key);

structure IntKey = struct type ord_key = Int.int val compare = Int.compare end
structure IntMap = SplayMapFn (IntKey);
          
fun nameEq (a:Ast.NAME) (b:Ast.NAME) = ((#id a) = (#id b) andalso (#ns a) = (#ns b))

val cachesz = 4096
                                       
type ATTRS = { dontDelete: bool,
               dontEnum: bool,
               readOnly: bool,
               isFixed: bool }     

type IDENTIFIER = Ustring.STRING

type NAMESPACE = Ast.NAMESPACE

datatype VAL = Object of OBJ
             | Null
             | Undef

     and OBJ =
         Obj of { ident: OBJ_IDENTIFIER,
                  tag: TAG,
                  props: PROP_BINDINGS,
                  rib: Ast.RIB ref,
                  proto: VAL ref }

     and TAG =
         ObjectTag of Ast.FIELD_TYPE list
       | ArrayTag of Ast.TYPE list
       | InstanceTag of Ast.INSTANCE_TYPE
       | MagicTag of MAGIC
       | NoTag

     and OBJ_CACHE = 
         ObjCache of 
         {
          doubleCache: (OBJ Real64Map.map) ref,
          nsCache: (OBJ NsMap.map) ref,
          nmCache: (OBJ NmMap.map) ref,
          strCache: (OBJ StrMap.map) ref,
          tyCache: (Ast.TYPE IntMap.map) ref (* well, mostly objs *)
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
          typeInterface : (OBJ option) ref,
          classClass : (OBJ option) ref,
          interfaceClass : (OBJ option) ref,
          namespaceClass : (OBJ option) ref,

          objectClass : (OBJ option) ref,
          arrayClass : (OBJ option) ref,
          functionClass : (OBJ option) ref,

          stringClass : (OBJ option) ref,
          stringWrapperClass : (OBJ option) ref,

          numberClass : (OBJ option) ref,
          doubleClass : (OBJ option) ref,
          decimalClass : (OBJ option) ref,

          booleanClass : (OBJ option) ref,
          booleanWrapperClass : (OBJ option) ref,

          booleanTrue : (OBJ option) ref,
          booleanFalse : (OBJ option) ref,
          doubleNaN : (OBJ option) ref,

          generatorClass : (OBJ option) ref
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
       | Type of Ast.TYPE
       | NativeFunction of NATIVE_FUNCTION
       | Generator of GEN

     and SCOPE =
         Scope of { object: OBJ,
                    parent: SCOPE option,
                    temps: TEMPS,
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
          langEd: int ref,
          booting: bool ref,
          specials: SPECIAL_OBJS,
          stack: FRAME list ref,
          objCache: OBJ_CACHE, 
          profiler: PROFILER 
         }

     and GEN_STATE = NewbornGen of (unit -> GEN_SIGNAL)
                   | DormantGen of (GEN_SIGNAL -> GEN_SIGNAL)
                   | RunningGen
                   | ClosedGen

     and GEN_SIGNAL = YieldSig of VAL
                    | SendSig of VAL
                    | ThrowSig of VAL
                    | StopSig
                    | CloseSig

     and GEN = Gen of GEN_STATE ref

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

     and REGS = 
         { 
          scope: SCOPE,
          this: OBJ,
          thisFun: OBJ option,
          thisGen: OBJ option,
          global: OBJ,
          prog: Fixture.PROGRAM,          
          aux: AUX
         }

     and NATIVE_FUNCTION =
         { func: ({ scope: SCOPE, 
                    this: OBJ, 
                    thisFun: OBJ option,
                    thisGen: OBJ option,
                    global: OBJ, 
                    prog: Fixture.PROGRAM, 
                    aux: AUX } (* REGS *)
                  -> VAL list -> VAL),
           length: int }

     and OBJ_IDENTIFIER =
         int

(* Important to model "fixedness" separately from
 * "dontDelete-ness" because fixedness affects
 * which phase of name lookup the name is found during.
 *)

     and TEMPS = (Ast.TYPE * TEMP_STATE) list ref

     and PROP = { ty: Ast.TYPE,
                  state: PROP_STATE,
                  attrs: ATTRS }

     and PROP_BINDINGS = { max_seq: int,
			               bindings: { seq: int,
				                       prop: (* PROP *)
				                                 { ty: Ast.TYPE,   
					                               state: PROP_STATE,
					                               attrs: ATTRS } } NameMap.map } ref 
			 
			 
fun isObject (v:VAL) : bool =
    case v of
        Object _ => true
      | _ => false


fun isDouble (Object (Obj {tag = MagicTag (Double _), ...})) = true
  | isDouble _ = false

fun isDecimal (Object (Obj {tag = MagicTag (Decimal _), ...})) = true
  | isDecimal _ = false

fun isString (Object (Obj {tag = MagicTag (String _), ...})) = true
  | isString _ = false

fun isBoolean (Object (Obj {tag = MagicTag (Boolean _), ...})) = true
  | isBoolean _ = false

fun isNamespace (Object (Obj {tag = MagicTag (Namespace _), ...})) = true
  | isNamespace _ = false

fun isClass (Object (Obj {tag = MagicTag (Class _), ...})) = true
  | isClass _ = false

fun isInterface (Object (Obj {tag = MagicTag (Interface _), ...})) = true
  | isInterface _ = false

fun isFunction (Object (Obj {tag = MagicTag (Function _), ...})) = true
  | isFunction _ = false
                   
fun isType (Object (Obj {tag = MagicTag (Type _), ...})) = true
  | isType _ = false

fun isNativeFunction (Object (Obj {tag = MagicTag (NativeFunction _), ...})) = true
  | isNativeFunction _ = false
                         
fun isNumeric ob = isDouble ob orelse isDecimal ob
                                      
fun isNull Null = true
  | isNull _ = false

fun isUndef Undef = true
  | isUndef _ = false

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
               (searchId:Ast.IDENTIFIER)
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
        {ty=Ast.UndefinedType  ,
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

fun hasMagic (Obj { tag = MagicTag _, ... }) = true
  | hasMagic _ = false

fun setRib (obj:OBJ)
           (r:Ast.RIB)
    : unit =
    let
        val Obj { rib, ... } = obj
    in
        rib := r
    end

fun getRib (obj:OBJ)
    : Ast.RIB =
    let
        val Obj { rib, ... } = obj
    in
        !rib
    end

fun getRibs (scope:SCOPE) 
    : Ast.RIBS = 
      let   
          val Scope {object, parent, ...} = scope
          val rib = getRib object
      in
          case parent of 
              NONE => [rib]
            | SOME p => rib :: (getRibs p)
      end


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

fun newObject (t:TAG)
              (p:VAL)
    : OBJ =
    Obj { ident = nextIdent (),
          tag = t,
          props = newPropBindings (),
          proto = ref p,
          rib = ref [] }

fun newObjectNoTag _
    : OBJ =
    newObject NoTag Null

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
      (* XXX: why does this trump the toString method? *)
      | Generator _ => Ustring.fromString "[object Generator]"


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

        (* FIXME: elaborate printing of type expressions. *)
        fun mag m = case m of
                        String s => ("\"" ^ (Ustring.toAscii s) ^ "\"")
                      | m => Ustring.toAscii (magicToUstring m) ^ (magType m)
                
        fun tag (Obj ob) =
            case (#tag ob) of
                (* FIXME: elaborate printing of structural tags. *)
                ObjectTag _ => "<Object>"
              | ArrayTag _ => "<Arrray>"
              | InstanceTag t => "<Instance " ^ (typ (Ast.InstanceType t)) ^ ">"
              | MagicTag m => "<Magic " ^ (mag m) ^ ">"
              | NoTag => "<NoTag>"

        fun printVal indent _ Undef = TextIO.print "undefined\n"
          | printVal indent _ Null = TextIO.print "null\n"

          | printVal indent 0 (Object obj) = TextIO.print ((tag obj) ^ "\n")
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
                val Obj { props, proto, ... } = obj
		val { bindings, ... } = !props
            in
                TextIO.print "Obj {\n";
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


(*
 * To get from any object to its CLS, you work out the
 * "nominal base" of the object's tag. You can then find
 * a fixed prop in the global object that has a "Class"
 * magic value pointing to the CLS.
 *)

fun nominalBaseOfTag (to:TAG)
    : Ast.NAME =
    case to of
        ObjectTag _ => Name.public_Object
      | ArrayTag _ => Name.public_Array
      | InstanceTag ity => (#name ity)
      | MagicTag (Boolean _) => Name.ES4_boolean
      | MagicTag (Double _) => Name.ES4_double
      | MagicTag (Decimal _) => Name.ES4_decimal
      | MagicTag (String _) => Name.ES4_string
      | MagicTag (Namespace _) => Name.ES4_Namespace
      | MagicTag (Class _) => Name.intrinsic_Class
      | MagicTag (Interface _) => Name.intrinsic_Interface
      | MagicTag (Function _) => Name.public_Function
      | MagicTag (Type _) => Name.intrinsic_Type
      | MagicTag (NativeFunction _) => Name.public_Function
      | MagicTag (Generator _) => Name.helper_GeneratorImpl
      | NoTag => error ["nominalBaseOfTag on NoTag"]
        
                                  
fun getObjMagic (Obj { tag = MagicTag m, ... }) = SOME m
  | getObjMagic _ = NONE

fun getMagic (Object (Obj { tag = MagicTag m, ... })) = SOME m
  | getMagic _ = NONE

fun needMagic (Object (Obj { tag = MagicTag m, ... })) = m
  | needMagic _ = error ["require object with magic"]

fun needClass (Object (Obj {tag = MagicTag (Class c), ...})) = c
  | needClass _ = error ["require class object"]

fun needInterface (Object (Obj {tag = MagicTag (Interface i), ...})) = i
  | needInterface _ = error ["require interface object"]

fun needFunction (Object (Obj {tag = MagicTag (Function f), ...})) = f
  | needFunction _ = error ["require function object]"]

fun needNamespace (Object (Obj {tag = MagicTag (Namespace n), ...})) = n
  | needNamespace _ = error ["require namespace object"]

fun needNamespaceOrNull Null = Name.publicNS
  | needNamespaceOrNull (Object (Obj {tag = MagicTag (Namespace n), ...})) = n
  | needNamespaceOrNull _ = error ["require namespace object"]

fun needType (Object (Obj {tag = MagicTag (Type t), ...})) = t
  | needType _ = error ["require type object"]

fun needDouble (Object (Obj {tag = MagicTag (Double d), ...})) = d
  | needDouble _ = error ["require double object"]

fun needDecimal (Object (Obj {tag = MagicTag (Decimal d), ...})) = d
  | needDecimal _ = error ["require decimal object"]

fun needBoolean (Object (Obj {tag = MagicTag (Boolean b), ...})) = b
  | needBoolean _ = error ["require boolean object"]

fun needString (Object (Obj {tag = MagicTag (String s), ...})) = s
  | needString _ = error ["require string object"]



(* Call stack and debugging stuff *)

(* An approximation of an invocation argument list, for debugging. *)
fun approx (arg:VAL)
    : string =
    case arg of
        Null => "null"
      | Undef => "undefined"
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

fun setLangEd (regs:REGS) 
              (newLangEd:int)
    : unit =
    let 
        val { aux = Aux { langEd, ...}, ... } = regs
    in
        langEd := newLangEd
    end

fun getLangEd (regs:REGS)               
    : int =
    let 
        val { aux = Aux { langEd, ...}, ... } = regs
    in
        !langEd
    end

fun getSpecials (regs:REGS) =
    let 
        val { aux = Aux { specials = SpecialObjs ss, ... }, ... } = regs
    in
        ss
    end

fun getTypeInterfaceSlot (regs:REGS) = (#typeInterface (getSpecials regs))
fun getClassClassSlot (regs:REGS) = (#classClass (getSpecials regs))
fun getInterfaceClassSlot (regs:REGS) = (#interfaceClass (getSpecials regs))
fun getNamespaceClassSlot (regs:REGS) = (#namespaceClass (getSpecials regs))

fun getObjectClassSlot (regs:REGS) = (#objectClass (getSpecials regs))
fun getArrayClassSlot (regs:REGS) = (#arrayClass (getSpecials regs))
fun getFunctionClassSlot (regs:REGS) = (#functionClass (getSpecials regs))
fun getStringClassSlot (regs:REGS) = (#stringClass (getSpecials regs))
fun getStringWrapperClassSlot (regs:REGS) = (#stringWrapperClass (getSpecials regs))

fun getNumberClassSlot (regs:REGS) = (#numberClass (getSpecials regs))
fun getDoubleClassSlot (regs:REGS) = (#doubleClass (getSpecials regs))
fun getDecimalClassSlot (regs:REGS) = (#decimalClass (getSpecials regs))

fun getBooleanClassSlot (regs:REGS) = (#booleanClass (getSpecials regs)) 
fun getBooleanWrapperClassSlot (regs:REGS) = (#booleanWrapperClass (getSpecials regs)) 

fun getBooleanTrueSlot (regs:REGS) = (#booleanTrue (getSpecials regs)) 
fun getBooleanFalseSlot (regs:REGS) = (#booleanFalse (getSpecials regs)) 
fun getDoubleNaNSlot (regs:REGS) = (#doubleNaN (getSpecials regs)) 

fun getGeneratorClassSlot (regs:REGS) = (#generatorClass (getSpecials regs))

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
fun getNsCache (regs:REGS) = (#nsCache (getCaches regs)) 
fun getNmCache (regs:REGS) = (#nmCache (getCaches regs)) 
fun getStrCache (regs:REGS) = (#strCache (getCaches regs)) 
fun getTyCache (regs:REGS) = (#tyCache (getCaches regs)) 

val findInDoubleCache = findInCache getDoubleCache Real64Map.find
val findInNsCache = findInCache getNsCache NsMap.find
val findInNmCache = findInCache getNmCache NmMap.find
val findInStrCache = findInCache getStrCache StrMap.find
val findInTyCache = findInCache getTyCache IntMap.find

val updateDoubleCache = updateCache getDoubleCache Real64Map.numItems Real64Map.insert
val updateNsCache = updateCache getNsCache NsMap.numItems NsMap.insert
val updateNmCache = updateCache getNmCache NmMap.numItems NmMap.insert
val updateStrCache = updateCache getStrCache StrMap.numItems StrMap.insert
val updateTyCache = updateCache getTyCache IntMap.numItems IntMap.insert

fun makeGlobalScopeWith (global:OBJ) 
    : SCOPE =
    Scope { object = global,
            parent = NONE,
            temps = ref [],
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
                       nsCache = ref NsMap.empty,
                       nmCache = ref NmMap.empty,
                       strCache = ref StrMap.empty,
                       tyCache = ref IntMap.empty }
        val specials = SpecialObjs 
                       { typeInterface = ref NONE,
                         classClass = ref NONE,
                         interfaceClass = ref NONE,
                         namespaceClass = ref NONE,
                         objectClass = ref NONE,
                         arrayClass = ref NONE,
                         functionClass = ref NONE,
                         stringClass = ref NONE,
                         stringWrapperClass = ref NONE,
                         numberClass = ref NONE,
                         doubleClass = ref NONE,
                         decimalClass = ref NONE,
                         booleanClass = ref NONE,
                         booleanWrapperClass = ref NONE,

                         booleanTrue = ref NONE,
                         booleanFalse = ref NONE,
                         doubleNaN = ref NONE,
                         generatorClass = ref NONE }
        val aux = Aux { booting = ref false,
                        langEd = ref 4,
                        specials = specials,
                        stack = ref [],
                        objCache = ocache,
                        profiler = prof }
    in        
        { this = glob,
          global = glob,          
          thisFun = NONE,
          thisGen = NONE,
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

(* begin names experiment *)

type IDENTIFIER = Ast.IDENTIFIER
type NAMESPACE = Ast.NAMESPACE
type NAME = Ast.NAME

type CLASS = Ast.CLS
type OBJECT = OBJ

type NAMESPACE_SET = NAMESPACE list
type OPEN_NAMESPACES = NAMESPACE_SET list 

fun head x = hd x (* return the first element of a list *)
fun tail x = tl x (* return all but the first element of a list *)



fun getBindingNamespaces (object: OBJECT, 
                          identifier: IDENTIFIER,
                          namespaces: NAMESPACE_SET,
                          fixedOnly: bool)
    : NAMESPACE_SET =
    (* 
     * get the namespaces of names (optionally, only the names of
     * fixed properties) that have a certain identifier
     * and any of a certain set of namespaces, and are bound in a
     * certain object. 
     *)
    (* INFORMATIVE *)
    let
        val Obj { props, ... } = object
        fun tryNS ns = 
            let
                val name = { id = identifier, ns = ns }
            in
                case findProp props name of 
                    NONE => NONE
                  | SOME {attrs={isFixed, ...}, ...} => 
                    if fixedOnly
                    then if isFixed
                         then SOME ns
                         else NONE
                    else SOME ns
            end
    in
        List.mapPartial tryNS namespaces
    end
        
fun getInstanceBindingNames (class: CLASS) 
    : NAME list =
    (* get the instance bindings of a class *)
    (* INFORMATIVE *)
    (* FIXME: implement! *)
    []

fun getPrototypeObject (Obj {proto, ...}: OBJECT)
    : OBJECT option =
    (* get the prototype (as in '[[proto]]') object of an object *)
    case !proto of 
        Object obj => SOME obj
      | _ => NONE

fun objectSearch (NONE, _, _, _) = NONE
  | objectSearch (SOME object: OBJECT option, 
                  namespaces: NAMESPACE_SET, 
                  identifier: IDENTIFIER, 
                  fixedOnly: bool)
    : (OBJECT * NAMESPACE_SET) option =
    let
        val matches = getBindingNamespaces (object, identifier, namespaces, fixedOnly)
    in
        case matches of
            [] => if fixedOnly
                  then NONE 
                  else objectSearch (getPrototypeObject (object), namespaces, identifier, fixedOnly)
          | _ => SOME (object, matches)
    end

fun objectListSearch ([], _, _, _) = NONE
  | objectListSearch (objects: OBJECT list, 
                      namespaces: NAMESPACE_SET, 
                      identifier: IDENTIFIER,
                      fixedOnly: bool)
    : (OBJECT * NAMESPACE_SET) option =
    let
        val object = head (objects)
        val matches = objectSearch (SOME object, namespaces, identifier, fixedOnly)
    in
        case matches of
            NONE => objectListSearch (tail (objects), namespaces, identifier, fixedOnly)
          | _ => matches
    end

fun inheritedClassesOf (object: OBJECT)
    : CLASS list =
    []

fun getObjId (Obj { ident, ...}) = ident

fun findName (globalObj: OBJECT, objects: OBJECT list, identifier: IDENTIFIER, openNamespaces: OPEN_NAMESPACES, globalNames: Ast.NAME_SET)
    : (OBJECT * NAME) option =
    let
        val namespaces = List.concat (openNamespaces)
        val matches = objectListSearch (objects, namespaces, identifier, true)
        val matches' = case matches of 
                           NONE => objectListSearch (objects, namespaces, identifier, false)
                         | _ => matches
    in
        case matches' of
            NONE => NONE
          | SOME (object, namespace :: []) => SOME (object, {ns=namespace, id=identifier})
          | SOME (object, namespaces') =>
            let
                val matches'' = Fixture.selectNamespacesByOpenNamespaces (openNamespaces, namespaces')
            in
                case matches'' of
                    namespace :: [] => SOME (object, {ns=namespace,id=identifier})
                  | [] => NONE
                  | _ =>
                    let
                        val classList = inheritedClassesOf (object)
                        val matches''' = Fixture.selectNamespacesByClass (classList, namespaces, identifier)
                    in 
                        case matches''' of
                            namespace :: [] => SOME (object, {ns=namespace, id=identifier})
                          | [] => raise (LogErr.NameError "internal error")
                          | _ => 
                            if (getObjId object) = (getObjId globalObj) 
                            then 
                                case Fixture.selectNamespacesByGlobalNames (identifier, matches''', globalNames) of
                                    namespace :: [] => SOME (object, {ns=namespace,id=identifier})
                                  | [] => raise (LogErr.NameError "internal error")
                                  | _ => raise (LogErr.NameError "ambiguous reference")
                            else
                                raise (LogErr.NameError "ambiguous reference")
                    end
            end
    end

end

