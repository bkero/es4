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

open Ast

(* Local tracing machinery *)

val doTrace = ref false
val traceStack = ref false
fun log ss = LogErr.log ("[mach] " :: ss)
fun trace ss = if (!doTrace) then log ss else ()
fun error ss = LogErr.machError ss
fun error0 ss = LogErr.machError ss

structure StrListKey = struct type ord_key = string list val compare = List.collate String.compare end
structure StrListMap = SplayMapFn (StrListKey);

structure NsKey = struct type ord_key = NAMESPACE val compare = NameKey.cmpNS end
structure NsMap = SplayMapFn (NsKey);

structure NmKey = struct type ord_key = NAME val compare = NameKey.compare end
structure NmMap = SplayMapFn (NmKey);

structure StrKey = struct type ord_key = Ustring.STRING val compare = (fn (a,b) => Ustring.compare a b) end
structure StrMap = SplayMapFn (StrKey);

structure Real64Key = struct type ord_key = Real64.real val compare = Real64.compare end
structure Real64Map = SplayMapFn (Real64Key);

structure IntKey = struct type ord_key = Int.int val compare = Int.compare end
structure IntMap = SplayMapFn (IntKey);
          
fun nameEq (a:NAME) (b:NAME) = ((#id a) = (#id b) andalso (#ns a) = (#ns b))

val cachesz = 4096

(* 
 * FIXME: these are supposed to be Removable, Enumerable, Writable and Fixed. 
 * Unfortunately it means we have to go invert the sense of the first three
 * all through the code. Yay.
 *)

datatype WRITABILITY = ReadOnly | WriteOnce | Writable

type ATTRS = { removable: bool,
               enumerable: bool,
               fixed: bool,
               writable: WRITABILITY }

type IDENTIFIER = Ustring.STRING

type NAMESPACE = NAMESPACE

datatype VALUE = Undefined
               | Null
               | Object of OBJ

     and OBJ =
         Obj of { props: PROPERTY_BINDINGS,                  
                  proto: VALUE,
                  ident: OBJ_IDENTIFIER,
                  tag: TAG
                , rib: RIB ref     (* INFORMATIVE *)
                }

     and TAG =
         ObjectTag of FIELD_TYPE list
       | ArrayTag of (TYPE list * TYPE option)
       | PrimitiveTag of PRIMITIVE
       | InstanceTag of CLASS
       | NoTag

     and OBJ_CACHE = 
         ObjCache of 
         {
          doubleCache: (OBJ Real64Map.map) ref,
          nsCache: (OBJ NsMap.map) ref,
          nmCache: (OBJ NmMap.map) ref,
          strCache: (OBJ StrMap.map) ref,
          tyCache: (TYPE IntMap.map) ref (* well, mostly objs *)
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
         Frame of { name: string, args: VALUE list }

(*
 * Primitive is visible only to the interpreter;
 * it is not visible to users.
 *)

     and PRIMITIVE =
         BooleanPrimitive of bool
       | DoublePrimitive of Real64.real
       | DecimalPrimitive of Decimal.DEC
       | StringPrimitive of Ustring.STRING
       | NamespacePrimitive of NAMESPACE
       | ClassPrimitive of CLASS
       | InterfacePrimitive of INTERFACE
       | FunctionPrimitive of FUN_CLOSURE
       | TypePrimitive of TYPE
       | NativeFunctionPrimitive of NATIVE_FUNCTION  (* INFORMATIVE *)
       | GeneratorPrimitive of GEN

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
                    | ValTemp of VALUE



     (* 
      * One might imagine that namespaces, methods and the 'arguments'
      * object in a function can all be stored as instances of the
      * classes NamespacePrimitive, FunctionPrimitive and Array, respectively. This
      * works in some contexts, but leads to feedback loops when
      * constructing the classes FunctionPrimitive, NamespacePrimitive and Array
      * themselves. So instead of eagerly instantiating such values,
      * we allocate them as the following 3 "lazy" property states. If
      * anyone performs a "get" on these property states, a NamespacePrimitive,
      * FunctionPrimitive or Array object (respectively) is constructed. We
      * then ensure that the NamespacePrimitive, FunctionPrimitive and Array
      * constructors, settings and initializers (in the builtins) do
      * *not* cause any "get" operations on properties in these
      * states.
      *
      * FIXME: The 'arguments' object can't be an array.
      *)

     and PROPERTY_STATE = UninitProp
                        | ValProp of VALUE
                        | NamespaceProp of NAMESPACE  (* INFORMATIVE *)  (* FIXME: Unify these as 'LazyProp' perhaps? *) 
                        | MethodProp of FUN_CLOSURE       (* INFORMATIVE *)
                        | ValListProp of VALUE list       (* INFORMATIVE *)
                        | NativeFunctionProp of NATIVE_FUNCTION  (* INFORMATIVE *)
                        | TypeProp
                        | TypeVarProp
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

     and GEN_SIGNAL = YieldSig of VALUE
                    | SendSig of VALUE
                    | ThrowSig of VALUE
                    | StopSig
                    | CloseSig

     and GEN = Gen of GEN_STATE ref

withtype FUN_CLOSURE =
         { func: FUNC,
           this: OBJ option,
           env: SCOPE }

     and REGS = 
         { 
          scope: SCOPE,
          this: OBJ,
          thisFun: OBJ option,
          thisGen: OBJ option,
          global: OBJ,
          rootRib: RIB
          , aux: AUX                      (* INFORMATIVE *)
         }

     and NATIVE_FUNCTION =                
         { func: ({ scope: SCOPE, 
                    this: OBJ, 
                    thisFun: OBJ option,
                    thisGen: OBJ option,
                    global: OBJ, 
                    rootRib: RIB, 
                    aux: AUX } (* REGS *)
                  -> VALUE list -> VALUE),
           length: int }

     and OBJ_IDENTIFIER = (* LDOTS *)
         int
         
(* Important to model "fixedness" separately from
 * "removable-ness" because fixedness affects
 * which phase of name lookup the name is found during.
 *)

     and TEMPS = (TYPE * TEMP_STATE) list ref

     and PROPERTY = { ty: TYPE,
                      state: PROPERTY_STATE,
                      attrs: ATTRS }

     and PROPERTY_BINDINGS = (* LDOTS *)
         { max_seq: int,
		   bindings: { seq: int,
				       prop: (* PROPERTY *)
				       { ty: TYPE,   
					     state: PROPERTY_STATE,
					     attrs: ATTRS } } NameMap.map } ref 
			 
			 
fun isObject (v:VALUE) : bool =
    case v of
        Object _ => true
      | _ => false


fun isDouble (Object (Obj {tag = PrimitiveTag (DoublePrimitive _), ...})) = true
  | isDouble _ = false

fun isDecimal (Object (Obj {tag = PrimitiveTag (DecimalPrimitive _), ...})) = true
  | isDecimal _ = false

fun isString (Object (Obj {tag = PrimitiveTag (StringPrimitive _), ...})) = true
  | isString _ = false

fun isBoolean (Object (Obj {tag = PrimitiveTag (BooleanPrimitive _), ...})) = true
  | isBoolean _ = false

fun isNamespace (Object (Obj {tag = PrimitiveTag (NamespacePrimitive _), ...})) = true
  | isNamespace _ = false

fun isClass (Object (Obj {tag = PrimitiveTag (ClassPrimitive _), ...})) = true
  | isClass _ = false

fun isInterface (Object (Obj {tag = PrimitiveTag (InterfacePrimitive _), ...})) = true
  | isInterface _ = false

fun isFunction (Object (Obj {tag = PrimitiveTag (FunctionPrimitive _), ...})) = true
  | isFunction _ = false
                   
fun isType (Object (Obj {tag = PrimitiveTag (TypePrimitive _), ...})) = true
  | isType _ = false

fun isNativeFunction (Object (Obj {tag = PrimitiveTag (NativeFunctionPrimitive _), ...})) = true
  | isNativeFunction _ = false
                         
fun isNumeric ob = isDouble ob orelse isDecimal ob
                                      
fun isNull Null = true
  | isNull _ = false

fun isUndef Undefined = true
  | isUndef _ = false

(*
 * The "machine type" of a value here is an ES3-ism. It exists only for
 * compatibility, and has nothing to do with the ES4 type system.
 *
 * The important part is that in ES3 algorithms, a machine value has
 * exactly *one* of these types. No overlap!
 *)

datatype MACHTY = TYNULL | TYUNDEF | TYNUMBER | TYSTRING | TYBOOLEAN | TYOBJECT

fun es3Type (v:VALUE) : MACHTY =
    if isNull v then TYNULL
    else if isUndef v then TYUNDEF
    else if isNumeric v then TYNUMBER
    else if isString v then TYSTRING
    else if isBoolean v then TYBOOLEAN
    else TYOBJECT

fun isSameType (va:VALUE) (vb:VALUE) : bool =
    es3Type va = es3Type vb

(* Binding operations. *)

fun newPropBindings _ : PROPERTY_BINDINGS =
    ref { max_seq = 0, bindings = NameMap.empty }

fun addProp (b:PROPERTY_BINDINGS)
            (n:NAME)
            (x:PROPERTY)
    : unit =
    let
	val { max_seq, bindings } = !b	
	val s = max_seq + 1
	val binding = { seq = s, prop = x }
	val bindings = NameMap.insert (bindings, n, binding)
    in
	b := { max_seq = s, bindings = bindings }
    end

fun delProp (b:PROPERTY_BINDINGS)
            (n:NAME)
    : unit =
    let
	val { max_seq, bindings } = !b	
	val (bindings, _) = NameMap.remove (bindings, n)
    in
	b := { max_seq = max_seq, bindings = bindings }
    end

fun findProp (b:PROPERTY_BINDINGS)
             (n:NAME)
    : PROPERTY option =
    let
	val { bindings, ... } = !b
    in
	case NameMap.find (bindings, n) of
	    NONE => NONE
	  | SOME { prop, ... } => SOME prop
    end

fun matchProps (fixedProps:bool)
               (b:PROPERTY_BINDINGS)
               (searchId:IDENTIFIER)
               (nss:NAMESPACE list)
    : NAME list =
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

fun getProp (b:PROPERTY_BINDINGS)
            (n:NAME)
    : PROPERTY =
    case findProp b n of
        SOME p => p
      | NONE =>
        (*
         * If not found, then cons up a temporary property
         * with value undefined. Any property not found
         * errors would have been caught by evalRefExpr
         *)
        {ty=UndefinedType  ,
         state=ValProp Undefined,
         attrs={removable=true,  (* unused attrs *)
                enumerable=false,
                writable=Writable,
                fixed=false}}


fun hasProp (b:PROPERTY_BINDINGS)
            (n:NAME)
    : bool =
    case findProp b n of
        NONE => false
      | SOME _ => true

fun hasFixedProp (b:PROPERTY_BINDINGS)
                 (n:NAME)
    : bool =
    case findProp b n of
        NONE => false
      | SOME {attrs={fixed, ...}, ...} => fixed

fun hasPrimitive (Obj { tag = PrimitiveTag _, ... }) = true
  | hasPrimitive _ = false

fun setRib (obj:OBJ)
           (r:RIB)
    : unit =
    let
        val Obj { rib, ... } = obj
    in
        rib := r
    end

fun getRib (obj:OBJ)
    : RIB =
    let
        val Obj { rib, ... } = obj
    in
        !rib
    end

fun getRibs (scope:SCOPE) 
    : RIBS = 
      let   
          val Scope {object, parent, ...} = scope
          val rib = getRib object
      in
          case parent of 
              NONE => [rib]
            | SOME p => rib :: (getRibs p)
      end


fun setPropEnumerable (props:PROPERTY_BINDINGS)
                    (n:NAME)
                    (enumerable:bool)
    : unit =
    case findProp props n of
        SOME prop =>
        let
            val attrs = (#attrs prop)
            val newProp = { ty = (#ty prop),
                            state = (#state prop),
                            attrs = { removable = (#removable attrs),
                                      enumerable = enumerable,
                                      writable = (#writable attrs),
                                      fixed = (#fixed attrs) } }
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
              (p:VALUE)
    : OBJ =
    Obj { ident = nextIdent (),
          tag = t,
          props = newPropBindings (),
          proto = p,
          rib = ref [] }

fun newObjectNoTag _
    : OBJ =
    newObject NoTag Null

fun getProto (ob:OBJ)
    : VALUE =
    let
         val Obj {proto, ...} = ob
    in
        proto
    end

fun getTemp (temps:TEMPS)
            (n:int)
    : VALUE =
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
            (v:VALUE)
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

fun primitiveToUstring (primitive:PRIMITIVE)
    : Ustring.STRING =
    case primitive of
        DoublePrimitive n => NumberToString n
      | DecimalPrimitive d => Ustring.fromString (Decimal.toString d)
      | StringPrimitive s => s
      | BooleanPrimitive true => Ustring.true_
      | BooleanPrimitive false => Ustring.false_
      | NamespacePrimitive ns => Ustring.fromString (LogErr.namespace ns)
      | ClassPrimitive _ => Ustring.fromString "[class ClassPrimitive]"
      | InterfacePrimitive _ => Ustring.fromString "[interface InterfacePrimitive]"
      | FunctionPrimitive _ => Ustring.fromString "[function FunctionPrimitive]"
      | TypePrimitive _ => Ustring.fromString "[type TypePrimitive]"
      | NativeFunctionPrimitive _ => Ustring.fromString "[function FunctionPrimitive]"
      (* XXX: why does this trump the toString method? *)
      | GeneratorPrimitive _ => Ustring.fromString "[object GeneratorPrimitive]"


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


fun inspect (v:VALUE)
            (d:Int32.int)
    : unit =
    let
        val pad = "          "
        fun p 0 s = List.app TextIO.print s
          | p n s = (TextIO.print pad; p (n-1) s)

        fun nl _ = TextIO.print "\n";

        fun att {removable,enumerable,writable,fixed} =
            if not removable
               andalso not enumerable
               andalso (writable = ReadOnly)
               andalso not fixed
            then ""
            else
                (" ("
                 ^ (if removable then "R," else "")
                 ^ (if enumerable then "E," else "")
                 ^ (case writable of 
                        ReadOnly => ""
                      | WriteOnce => "WO"
                      | Writable => "W")
                 ^ (if fixed then "F" else "")
                 ^ ") ")

        fun id (Obj ob) = Int.toString (#ident ob)

        fun typ t = LogErr.ty t

        fun magType t = 
            case t of 
                ClassPrimitive c => 
                let
                    val Class { classType, ... } = c
                in
                    (" : instanceType=" ^ (typ (ClassType c)) ^ ", classType=" ^ (typ classType))
                end
              | InterfacePrimitive i => (" : instanceType=" ^ (typ (InterfaceType i)))
              | FunctionPrimitive { func = Func { ty=ty0, ... }, ... } => 
                (" : " ^ (typ ty0))
              | TypePrimitive t => (" = " ^ (typ t))
              | _ => ""

        (* FIXME: elaborate printing of type expressions. *)
        fun mag m = case m of
                        StringPrimitive s => ("\"" ^ (Ustring.toAscii s) ^ "\"")
                      | m => Ustring.toAscii (primitiveToUstring m) ^ (magType m)
                
        fun tag (Obj ob) =
            case (#tag ob) of
                (* FIXME: elaborate printing of structural tags. *)
                ObjectTag _ => "<Object>"
              | ArrayTag _ => "<Arrray>"
              | InstanceTag t => "<Instance " ^ (typ (ClassType t)) ^ ">"
              | PrimitiveTag m => "<Primitive " ^ (mag m) ^ ">"
              | NoTag => "<NoTag>"

        fun printVal indent _ Undefined = TextIO.print "undefined\n"
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
                p indent ["  proto = "]; subVal indent (proto);
                p indent ["  props = ["]; nl();
                NameMap.appi prop bindings;
                p indent ["          ] }"]; nl()
            end
    in
        printVal 0 d v
    end


(*
 * To get from any object to its CLASS, you work out the
 * "nominal base" of the object's tag. You can then find
 * a fixed prop in the global object that has a "ClassPrimitive"
 * primitive value pointing to the CLASS.
 *)

fun nominalBaseOfTag (to:TAG)
    : NAME =
    case to of
        ObjectTag _ => Name.public_Object
      | ArrayTag _ => Name.public_Array
      | InstanceTag (Class {name, ...}) => name
      | PrimitiveTag (BooleanPrimitive _) => Name.ES4_boolean
      | PrimitiveTag (DoublePrimitive _) => Name.ES4_double
      | PrimitiveTag (DecimalPrimitive _) => Name.ES4_decimal
      | PrimitiveTag (StringPrimitive _) => Name.ES4_string
      | PrimitiveTag (NamespacePrimitive _) => Name.ES4_Namespace
      | PrimitiveTag (ClassPrimitive _) => Name.intrinsic_Class
      | PrimitiveTag (InterfacePrimitive _) => Name.intrinsic_Interface
      | PrimitiveTag (FunctionPrimitive _) => Name.public_Function
      | PrimitiveTag (TypePrimitive _) => Name.intrinsic_Type
      | PrimitiveTag (NativeFunctionPrimitive _) => Name.public_Function
      | PrimitiveTag (GeneratorPrimitive _) => Name.helper_GeneratorImpl
      | NoTag => error ["nominalBaseOfTag on NoTag"]
        
                                  
fun getObjPrimitive (Obj { tag = PrimitiveTag m, ... }) = SOME m
  | getObjPrimitive _ = NONE

fun getPrimitive (Object (Obj { tag = PrimitiveTag m, ... })) = SOME m
  | getPrimitive _ = NONE

fun needPrimitive (Object (Obj { tag = PrimitiveTag m, ... })) = m
  | needPrimitive _ = error ["require object with primitive"]

fun needClass (Object (Obj {tag = PrimitiveTag (ClassPrimitive c), ...})) = c
  | needClass _ = error ["require class object"]

fun needInterface (Object (Obj {tag = PrimitiveTag (InterfacePrimitive i), ...})) = i
  | needInterface _ = error ["require interface object"]

fun needFunction (Object (Obj {tag = PrimitiveTag (FunctionPrimitive f), ...})) = f
  | needFunction _ = error ["require function object]"]

fun needNamespace (Object (Obj {tag = PrimitiveTag (NamespacePrimitive n), ...})) = n
  | needNamespace _ = error ["require namespace object"]

fun needNamespaceOrNull Null = Name.publicNS
  | needNamespaceOrNull (Object (Obj {tag = PrimitiveTag (NamespacePrimitive n), ...})) = n
  | needNamespaceOrNull _ = error ["require namespace object"]

fun needType (Object (Obj {tag = PrimitiveTag (TypePrimitive t), ...})) = t
  | needType _ = error ["require type object"]

fun needDouble (Object (Obj {tag = PrimitiveTag (DoublePrimitive d), ...})) = d
  | needDouble _ = error ["require double object"]

fun needDecimal (Object (Obj {tag = PrimitiveTag (DecimalPrimitive d), ...})) = d
  | needDecimal _ = error ["require decimal object"]

fun needBoolean (Object (Obj {tag = PrimitiveTag (BooleanPrimitive b), ...})) = b
  | needBoolean _ = error ["require boolean object"]

fun needString (Object (Obj {tag = PrimitiveTag (StringPrimitive s), ...})) = s
  | needString _ = error ["require string object"]



(* Call stack and debugging stuff *)

(* An approximation of an invocation argument list, for debugging. *)
fun approx (arg:VALUE)
    : string =
    case arg of
        Null => "null"
      | Undefined => "undefined"
      | Object ob =>
        if hasPrimitive ob
        then
            let
                val str = Ustring.toAscii (primitiveToUstring (needPrimitive arg))
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
         (args:VALUE list) 
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

fun makeInitialRegs (rootRib:RIB)
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
          rootRib = rootRib,
          aux = aux }
    end

(* native function stuff *)

(* 
 * FIXME: it is probably tidier if we push this into aux, but it's really 
 * not a high priority; they only get set at SML-heap-load time anyways.
 *)

val nativeFunctions: (NAME * NATIVE_FUNCTION) list ref = ref []

fun registerNativeFunction (name:NAME)
                           (func:NATIVE_FUNCTION)
    : unit =
    (trace ["registering native function: ", LogErr.name name];
     nativeFunctions := (name, func) :: (!nativeFunctions))


fun getNativeFunction (name:NAME)
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

type IDENTIFIER = IDENTIFIER
type NAMESPACE = NAMESPACE
type NAME = NAME

type CLASS = CLASS
type OBJECT = OBJ

type NAMESPACE_SET = NAMESPACE list
type OPEN_NAMESPACES = NAMESPACE_SET list 

fun getScopeObjectAndKind ( Scope {object, kind, ...}: SCOPE) = (object, kind)

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
                  | SOME {attrs={fixed, ...}, ...} => 
                    if fixedOnly
                    then if fixed
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
    case proto of 
        Object obj => SOME obj
      | _ => NONE

fun searchObject (NONE, _, _, _) = NONE

  | searchObject (SOME object : OBJECT option, 
                  identifier  : IDENTIFIER, 
                  namespaces  : NAMESPACE_SET, 
                  fixedOnly   : bool)
    : (OBJECT * NAMESPACE_SET) option =
    let
        val matches = getBindingNamespaces (object, 
                                            identifier, 
                                            namespaces, 
                                            fixedOnly)
    in
        case matches of
            [] 
            => if fixedOnly then 
                   NONE 
               else
                   searchObject (getPrototypeObject (object), 
                                 identifier, 
                                 namespaces, 
                                 fixedOnly)

          | _ 
            => SOME (object, matches)
    end

fun objectListSearch ([], _, _, _) = NONE

  | objectListSearch (objects    : OBJECT list,
                      namespaces : NAMESPACE_SET,
                      identifier : IDENTIFIER,
                      fixedOnly  : bool)
    : (OBJECT * NAMESPACE_SET) option =
    let
        val object = hd objects
        val matches = searchObject (SOME object, 
                                    identifier, 
                                    namespaces, 
                                    fixedOnly)
    in
        case matches of
            NONE 
            => objectListSearch (tl objects, 
                                 namespaces, 
                                 identifier, 
                                 fixedOnly)

          | _ 
            => matches
    end

(*
fun searchMutableScopeObject (object: OBJECT,
                              namespaces: NAMESPACE_SET,
                              identifier: IDENTIFIER)
    : (OBJECT * NAMESPACE_SET) option =
    let
        val result = searchObject (SOME object, identifier, namespaces, true)
    in 
        case result of
            NONE => searchObject (SOME object, identifier, namespaces, false)
          | _ => result
    end
*)

(* FIXME need to handle eval scopes specially too *)
fun searchScope (scope      : SCOPE,
                 namespaces : NAMESPACE_SET,
                 identifier : IDENTIFIER,
                 fixedOnly  : bool)
    : (OBJECT * NAMESPACE_SET) option =
    let
        val (object, kind) = getScopeObjectAndKind (scope)
    in 
        case (kind, fixedOnly) of
            (WithScope, true) 
            => searchObject (SOME object, identifier, namespaces, false)

          | (WithScope, false) 
            => NONE

          | (_,_)            
            => searchObject (SOME object, identifier, namespaces, fixedOnly)
    end

and searchScopeChainOnce (NONE, _, _, _) = NONE

  | searchScopeChainOnce (SOME scope : SCOPE option,
                          identifier : IDENTIFIER,
                          namespaces : NAMESPACE_SET,
                          fixedOnly  : bool)
    : (OBJECT * NAMESPACE_SET) option =
    let
        val matches = searchScope (scope, namespaces, identifier, fixedOnly)
        val Scope { parent, ... } = scope
    in
        case matches of
            NONE 
            => searchScopeChainOnce (parent, identifier, namespaces, fixedOnly)

          | _
            => matches
    end

fun searchScopeChain (scope      : SCOPE option,
                      identifier : IDENTIFIER,
                      namespaces : NAMESPACE_SET)
    : (OBJECT * NAMESPACE_SET) option =
    let 
        val result = searchScopeChainOnce(scope, identifier, namespaces, true)
    in
        case result of
            NONE
            => searchScopeChainOnce(scope, identifier, namespaces, false)

          | SOME _
            => result
    end

fun instanceRibsOf (object: OBJECT)
    : RIBS =
    []

fun getObjId (Obj { ident, ...}) = ident

fun findName (globalObj: OBJECT, objects: OBJECT list, identifier: IDENTIFIER, openNamespaces: OPEN_NAMESPACES)
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
                        val instanceRibs = instanceRibsOf (object)
                        val matches''' = Fixture.selectNamespacesByClass (instanceRibs, namespaces, identifier)
                    in 
                        case matches''' of
                            namespace :: [] => SOME (object, {ns=namespace, id=identifier})
                          | [] => raise (LogErr.NameError "internal error")
                          | _ =>  raise (LogErr.NameError ("ambiguous reference: " ^ Ustring.toAscii identifier))
                    end
            end
    end

end
