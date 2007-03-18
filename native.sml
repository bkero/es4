(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* Host functions provided to implement standard library. *)

structure Native = struct 

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[native] " :: ss) else ()
fun error ss = LogErr.hostError ss

fun nthAsA (f:Mach.VAL -> 'a) 
           (vals:Mach.VAL list) 
           (n:int) 
    : 'a = 
    if n >= length vals
    then error ["trying to fetch arg #",
                           (Int.toString n), 
                           " from arg list of length ",
                           (Int.toString (length vals))]
    else 
        f (List.nth (vals, n))


fun nthAsObj (vals:Mach.VAL list) 
             (n:int) 
    : Mach.OBJ = 
    let 
        fun f Mach.Undef = error ["Wanted Object, got Undef"]
          | f Mach.Null = error ["Wanted Object, got Null"]
          | f (Mach.Object ob) = ob
    in
        nthAsA f vals n
    end


fun nthAsObjAndCls (vals:Mach.VAL list) 
                   (n:int) 
    : (Mach.OBJ * Mach.CLS_CLOSURE) = 
    let
        val obj = nthAsObj vals n
        val Mach.Obj { magic, ... } = obj
    in
        case !magic of 
            SOME (Mach.Class c) => (obj, c)
          | _ => error ["Wanted class, got other"]
    end


fun nthAsStr (vals:Mach.VAL list) 
             (n:int) 
    : string = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.String s) => s
          | _ => error ["Wanted string, got other"]
    end


fun nthAsInt (vals:Mach.VAL list) 
             (n:int) 
    : Int32.int = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.Int n) => n
          | _ => error ["Wanted int, got other"]
    end

fun nthAsUInt (vals:Mach.VAL list) 
              (n:int) 
    : Word32.word = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.UInt n) => n
          | _ => error ["Wanted uint, got other"]
    end


fun nthAsBool (vals:Mach.VAL list) 
              (n:int) 
    : bool = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.Bool b) => b
          | _ => error ["Wanted Boolean, got other"]
    end

fun nthAsByteArray (vals:Mach.VAL list) 
                   (n:int) 
    : Word8Array.array = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.ByteArray b) => b
          | _ => error ["Wanted ByteArray, got other"]
    end


fun propQuery (vals:Mach.VAL list) 
              (f:(Mach.PROP_BINDINGS -> Ast.NAME -> bool))
    : Mach.VAL = 
    let 
        val Mach.Obj { props, ...} = nthAsObj vals 0
        val id = nthAsStr vals 1
        val ns = Ast.Internal ""
    in
        Mach.newBoolean (f props {id=id, ns=ns})
    end

fun arrayToList (arr:Mach.OBJ) 
    : Mach.VAL list = 
    let 
        val ns = Ast.Internal ""
        val len = Word32.toInt 
                      (Mach.coerceToUInt 
                           (Mach.needMagic 
                                (Eval.getValue (arr, {id="length", ns=ns}))))
        fun build i vs = 
            if i < 0
            then vs
            else 
                let
                    val n = {id=(Int.toString i), ns=ns}
                    val curr = if Eval.hasValue arr n
                               then Eval.getValue (arr, n)
                               else Mach.Undef
                in
                    build (i-1) (curr::vs)
                end
    in
        build (len-1) []
    end


(* 
 * Given a class object, run the standard object-construction
 * protocol for it (and its base classes, initializers, settings,
 * ctors). Return the resulting instance, always an Object!
 *
 * magic native function construct(cls:Class!, args:[*]) : Object!;
 *)
fun construct (vals:Mach.VAL list)
    : Mach.VAL = 
    let
        val (obj, cls) = nthAsObjAndCls vals 0
        val args = arrayToList (nthAsObj vals 1)
    in
        Eval.constructClassInstance obj cls args
    end


(* 
 * Retrieve the [[Class]] property of o
 * 
 * magic native function getClassName(o : Object!) : string; 
 *)
fun getClassName (vals:Mach.VAL list) 
    : Mach.VAL = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals 0
        (* FIXME: is this right? *)
        val str = case !magic of 
                      SOME (Mach.Function _) => "Function"
                    | SOME (Mach.NativeFunction _) => "Function"
                    | _ => "Object"
    in
        Mach.newString str
    end

    
(* 
 * Retrieve the possibly null [[Prototype]] property of o 
 * 
 * magic native function getPrototype(o : Object!) : Object;
 *)
fun getPrototype (vals:Mach.VAL list) 
    : Mach.VAL = 
    let 
        val Mach.Obj { proto, ... } = nthAsObj vals 0
    in
        !proto
    end


(*
 * Return true iff o has a local property named by p.
 * 
 * magic native function hasOwnProperty(o : Object!, p : string) : Boolean;
 *)
fun hasOwnProperty (vals:Mach.VAL list) 
    : Mach.VAL = 
    propQuery vals Mach.hasProp


(*
 * Return true if the property p does exists locally on o and its
 * DontEnum bit is set
 * 
 * magic native function getPropertyIsDontEnum(o : Object!, p : string) : Boolean;
 *)
fun getPropertyIsDontEnum (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        fun f props n = 
            if Mach.hasProp props n
            then (#dontEnum (#attrs (Mach.getProp props n)))
            else false
    in
        propQuery vals f
    end


(* 
 * Return true if the property p does exists locally on o and its
 * DontDelete bit is set
 * 
 * magic native function getPropertyIsDontDelete(o : Object!, p : string) : Boolean;
 *)

fun getPropertyIsDontDelete (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        fun f props n = 
            if Mach.hasProp props n
            then (#dontDelete (#attrs (Mach.getProp props n)))
            else false
    in
        propQuery vals f
    end
    

(* Provided that the property p exists locally on o, set its DontEnum
 * flag according to f.  If the property p does not exist locally on
 * o, it does nothing.
 * 
 * magic native function setPropertyIsDontEnum(o : Object!, p : string, f : Boolean) : void;
 *)
fun setPropertyIsDontEnum (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val Mach.Obj { props, ...} = nthAsObj vals 0
        val id = nthAsStr vals 1
        val ns = Ast.Internal ""
        val n = { id = id, ns = ns }
        val b = nthAsBool vals 2
    in
        if Mach.hasProp props n
        then 
            let 
                val prop = Mach.getProp props n
                val attrs = (#attrs prop)
                val newProp = { ty = (#ty prop),
                                state = (#state prop),
                                attrs = { dontDelete = (#dontDelete attrs),
                                          dontEnum = b,
                                          readOnly = (#readOnly attrs),
                                          isFixed = (#isFixed attrs) } }
            in
                Mach.delProp props n;
                Mach.addProp props n newProp
            end
        else ();
        Mach.Undef
    end


(*
 * Retrieve the [[Value]] property of o
 * 
 * magic native function getValue(o : Object!) : *;
 *) 
fun getValue (vals:Mach.VAL list) 
    : Mach.VAL = 
    (* 
     * FIXME: is this right? I'm assuming the point is to rebuild the VAL
     * in the "normal" state associated with its magic: no props,
     * appropriate proto, etc.
     *)
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals 0
    in
        case !magic of 
            NONE => Mach.Undef
          | SOME (Mach.UInt u) => Mach.newUInt u
          | SOME (Mach.Int i) => Mach.newInt i
          | SOME (Mach.Double d) => Mach.newDouble d
          | SOME (Mach.Decimal d) => Mach.newDecimal d
          | SOME (Mach.String s) => Mach.newString s
          | SOME (Mach.Bool b) => Mach.newBoolean b
          | SOME (Mach.Namespace ns) => Mach.newNamespace ns
          | SOME (Mach.Class cc) => Mach.newClass (#env cc) (#cls cc) 
          | SOME (Mach.Interface i) => Mach.newIface (#env i) (#iface i) 
          | SOME (Mach.Function f) => Mach.newFunc (#env f) (#func f)
          | SOME (Mach.Type t) => Mach.newType t
          | SOME (Mach.NativeFunction f) => Mach.newNativeFunction f
          | SOME (Mach.ByteArray b) => Mach.newByteArray b
    end


(*
 * Set the [[Value]] of o to v
 * 
 * magic native function setValue(o : Object!, v : * ) : void;
 *)
fun setValue (vals:Mach.VAL list) 
    : Mach.VAL = 
    (* 
     * FIXME: is this right? I'm assuming the point is to set (or reset to empty)
     * the magic slot of 'o' to whatever magic can be found in 'v'.
     *)
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals 0
        val v = case vals of 
                    [_,x] => x
                  | _ => error ["bad number of arguments to setValue"]            
    in
        case v of 
            Mach.Object (Mach.Obj ob) => magic := !(#magic ob)
          | _ => magic := NONE;
        Mach.Undef
    end


(*
 * Given a function object, a this object, and an array of argument
 * values, call the function with the this object and arguments. 
 *
 * magic native function apply(fn : Function!, t : Object!, args : Array) : *;
 *)
fun apply (vals:Mach.VAL list) 
    : Mach.VAL = 
    let 
        val fnObj = nthAsObj vals 0
        val thisObj = nthAsObj vals 1
        val argsObj = nthAsObj vals 2
        val argsList = arrayToList argsObj
    in
        Eval.evalCallExpr (SOME thisObj) fnObj argsList
    end


(* Given a function object, arguments to bind and source to run, 
 * compiles the source and arguments into a magic function value
 * and sets the magic slot inside the function to contain it.
 *
 * magic native function compileInto(fn: Function!, argNames: [String!], src: String!): void;
 *)
fun compileInto (vals:Mach.VAL list) 
    : Mach.VAL = 
    (*
     * We synthesize a token stream here that feeds back into the parser.
     *)
    let 
        val funcObj = nthAsObj vals 0
        fun ident v = Token.Identifier (Mach.toString v)
        val argIdents = map ident (arrayToList (nthAsObj vals 1))
        val argList = case argIdents of
                          [] => []
                        | x::xs => (x :: (List.concat 
                                              (map (fn i => [Token.Comma, i]) xs)))
        val source = nthAsStr vals 2
        val lines = [source] (* FIXME: split lines *)
        val lineTokens = Parser.lexLines lines
        val funcTokens = [Token.Function, Token.LeftParen]
                         @ argList
                         @ [Token.LeftParen]
                         @ lineTokens
        val (_,funcExpr) = Parser.functionExpression (funcTokens, 
                                                      Parser.NOLIST, 
                                                      Parser.ALLOWIN)

        val funcExpr = Defn.defExpr (Defn.topEnv()) funcExpr
        val funcVal = Eval.evalExpr Mach.globalScope funcExpr
    in
        case funcVal of 
            Mach.Object obj =>
            let 
                val Mach.Obj { magic, ...} = funcObj
                val sname = {ns=Ast.Internal "", id="source"}
                val sval = (Mach.newString source)
            in
                Eval.setValue obj sname sval;
                magic := Mach.getObjMagic obj
            end
          | _ => error ["function did not compile to object"];
        Mach.Undef
    end

(* 
 * Given a string object 'src', copy its internal string data into
 * another string object 'dest', replacing whatever data might
 * have been in 'dest' to begin with.
 * 
 * magic native function setStringValue(dest : string, src : string) : void;
 *)
fun setStringValue (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val Mach.Obj { magic, ...} = nthAsObj vals 0
        val src = nthAsStr vals 1
    in
        magic := SOME (Mach.String src);
        Mach.Undef
    end


(* Given a string and a position in that string, return the
 * numeric value of the character at that position in the
 * string.
 *
 * magic native function charCodeAt(s : string, pos : uint) : uint;
 *)
fun charCodeAt (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val s = nthAsStr vals 0
        val i = nthAsUInt vals 1
    in
        Mach.newUInt (Word32.fromInt (Char.ord (String.sub (s, (Word32.toInt i)))))
    end


(*
 * Given a numeric character value, return a string of length 1
 * whose element 0 is the character with that same value.
 *
 * magic native function fromCharCode(ch : uint) : string;
 *)
fun fromCharCode (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val i = nthAsUInt vals 0
    in
        Mach.newString (Char.toString (Char.chr (Word32.toInt i)))
    end


(* 
 * Given a string object, return the number of characters in the
 * string.
 *
 * magic native function stringLength(s : string) : uint;
 *)
fun stringLength (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val s = nthAsStr vals 0
    in
        Mach.newUInt (Word32.fromInt (String.size s))
    end
    

(* 
 * Given two string objects A and B , return a new string object
 * containing the characters from A followed by the characters
 * from B.
 * 
 * magic native function stringAppend(a : string, b : string) : string;
 *)
fun stringAppend (vals:Mach.VAL list)
    : Mach.VAL = 
    let
        val a = nthAsStr vals 0
        val b = nthAsStr vals 1
    in
        Mach.newString (a ^ b)
    end
    
    
(*
 * Get the byte at index idx.  Unspecified behavior if that index
 * does not have data (it's OK to crash the system).
 * 
 * magic native function getByteArrayByte(ba : ByteArray!, idx : uint) : uint;
 *)
fun getByteArrayByte (vals:Mach.VAL list)
    : Mach.VAL =
    let 
        val b = nthAsByteArray vals 0
        val i = nthAsUInt vals 1
    in
        Mach.newUInt (Word32.fromInt (Word8.toInt (Word8Array.sub (b, (Word32.toInt i)))))
    end

(*
 * Set the byte at index idx to val, which will be truncated to
 * the low 8 bits before being stored.
 *
 * magic native function setByteArrayByte(ba : ByteArray!, idx : uint, val : uint) : void;
 *)
fun setByteArrayByte (vals:Mach.VAL list)
    : Mach.VAL =
    let 
        val b = nthAsByteArray vals 0
        val i = nthAsUInt vals 1
        val v = nthAsUInt vals 2
    in
        Word8Array.update(b, Word32.toInt i, Word8.fromInt (Word32.toInt v));
        Mach.Undef
    end


(* 
 * 15.1.2.1
 * intrinsic native function eval(x)
 *)

fun eval (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::eval"]
    
(* 
 * 15.1.2.2
 * intrinsic native function parseInt(string:string, radix:int)
 *)
fun parseInt (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::parseInt"]
    

(* 
 * 15.1.2.3 
 * intrinsic native function parseFloat(string:string);
 *)
fun parseFloat (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::parseFloat"]


(* 
 * 15.1.2.4
 * intrinsic native function isNaN( number:* ):boolean;
 *)
fun isNaN (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::isNaN"]


(* 
 * 15.1.2.5
 * intrinsic native function isFinite( number:* ):boolean;
 *)
fun isFinite (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::isFinite"]


(* 
 * 15.1.3.1
 * intrinsic native function decodeURI(encodedURI);
 *)
fun decodeURI (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::decodeURI"]


(* 
 * 15.1.3.2
 * intrinsic native function decodeURIComponent(encodedURIComponent);
 *)
fun decodeURIComponent (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::decodeURIComponent"]

(* 
 * 15.1.3.3
 * intrinsic native function encodeURI(uri);
 *)
fun encodeURI (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::encodeURI"]


(* 
 * 15.1.3.4
 * intrinsic native function encodeURIComponent(uriComponent);
 *)
fun encodeURIComponent (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::encodeURIComponent"]


(* Some helpers not specified in the wiki at the moment. Maybe get rid
 * of them eventually? *)

fun print (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        fun printOne v = TextIO.print (Mach.toString v) 
    in
        List.app printOne vals; 
        Mach.Undef
    end
    
fun assert (vals:Mach.VAL list) 
    : Mach.VAL = 
    if nthAsBool vals 0
    then Mach.Undef
    else error ["intrinsic::assert() failed"]

(* Register all the native functions in this file. *)
fun registerNatives _ = 
    let
        fun addFn ns name f = 
            Mach.registerNativeFunction { ns = ns, id = name } f
    in
        addFn (Ast.UserNamespace "magic") "construct" construct;
        addFn (Ast.UserNamespace "magic") "getClassName" getClassName;
        addFn (Ast.UserNamespace "magic") "getPrototype" getPrototype;
        addFn (Ast.UserNamespace "magic") "hasOwnProperty" hasOwnProperty;
        addFn (Ast.UserNamespace "magic") "getPropertyIsDontEnum" getPropertyIsDontEnum;
        addFn (Ast.UserNamespace "magic") "getPropertyIsDontDelete" getPropertyIsDontDelete;
        addFn (Ast.UserNamespace "magic") "setPropertyIsDontEnum" setPropertyIsDontEnum;
        addFn (Ast.UserNamespace "magic") "getValue" getValue;
        addFn (Ast.UserNamespace "magic") "setValue" setValue;
        addFn (Ast.UserNamespace "magic") "apply" apply;
        addFn (Ast.UserNamespace "magic") "compileInto" compileInto;
        addFn (Ast.UserNamespace "magic") "setStringValue" setStringValue;
        addFn (Ast.UserNamespace "magic") "charCodeAt" charCodeAt;
        addFn (Ast.UserNamespace "magic") "fromCharCode" fromCharCode;
        addFn (Ast.UserNamespace "magic") "stringLength" stringLength;
        addFn (Ast.UserNamespace "magic") "stringAppend" stringAppend;
        addFn (Ast.UserNamespace "magic") "getByteArrayByte" getByteArrayByte;
        addFn (Ast.UserNamespace "magic") "setByteArrayByte" setByteArrayByte;

        addFn Ast.Intrinsic "eval" eval;
        addFn Ast.Intrinsic "parseInt" parseInt;
        addFn Ast.Intrinsic "parseFloat" parseFloat;
        addFn Ast.Intrinsic "isNaN" isNaN;
        addFn Ast.Intrinsic "isFinite" isFinite;
        addFn Ast.Intrinsic "decodeURI" decodeURI;
        addFn Ast.Intrinsic "decodeURIComponent" decodeURIComponent;
        addFn Ast.Intrinsic "encodeURI" encodeURI;
        addFn Ast.Intrinsic "encodeURIComponent" encodeURIComponent;
        
        addFn Ast.Intrinsic "print" print;
        addFn Ast.Intrinsic "assert" assert

    end

end
