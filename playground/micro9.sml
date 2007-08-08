(* -*- mode: sml; mode: font-lock; tab-width: 60; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* vim: set ts=4 sw=4 et: *)

(*
For option 9 - 23jul07 - Cormac.

This is an implementation of a toy language to express and clarify a
number of issues relating to the ES4 type system in a simpler context,
and may also help to shake out bugs in the type system earlier.

High-level lessons:
 - AnyType is NOT a top type 

 - In strict mode, programs without '*' give no run-time casts, and
   safety bit never gets set.

 - In standard mode, programs without '*' which would compile in
   strict mode, also give no run-time casts, and safety bit never gets
   set.

 - we can *clear* the safe bit, if a subtype check holds!

 - Generics are a little tricky but they work out

 - Type closures are quite tricky, since the evaluator works via
   environments whereas the verifier works via substitution (much
   easier). This works by having subtyping and compatibility use
   environments, and the verifier simply passes the empty environment.
   The resulting type closures may contain free variables, for type
   references that are not known at verify-time.

 - type X = ... etc all seem fine, and can be removed by
   the verifier.

*)

Control.Print.printDepth := 100;
fun debug s = ();

(*********** environments ***********)
(* variable names are just strings. *)

exception UnboundVariable of string
exception AlreadyBound of string

type 'a ENV = (string * 'a) list

val 'a emptyEnv : 'a ENV = []

fun extend (env:'a ENV) (x:string) (v:'a) : 'a ENV =
    (x,v)::env

fun lookup (env:'a ENV) (x:string) : 'a =
    case env of
        [] => raise UnboundVariable x
      | (y,v)::r => if (x=y) then v else lookup r x

fun lookup2 (env:'a ENV) (x:string) : 'a option =
    case env of
        [] => NONE
      | (y,v)::r => if (x=y) then SOME v else lookup2 r x

fun checkUnbound (env:'a ENV) (x:string) : unit =
    case env of
        [] => ()
      | (y,_)::r =>
        if (x=y)
        then raise AlreadyBound x
        else checkUnbound r x

fun envToString (env:'a ENV) (f:'a->string) : string =
    "[" ^ 
    (List.foldl (fn ((x,a),r) => (x^"="^(f a)^", "^r)) "" env)
    ^"]"

(********** Gensym, for alpha renaming **********)

val gensymCounter : int ref = ref 0
fun gensym (s:string) : string =
    let
    in
        gensymCounter := 1+(!gensymCounter);
        s ^ "$" ^  (Int.toString (!gensymCounter))
    end

(*********** Types ***********)
type label = string;

datatype LIKE =
         Like
       | Exact
       | Wrap
           
datatype TYPE =
         AnyType                         (* the type "*" *)
       | IntType
       | TypeType                        (* the type of types *)
       | FunType of LIKE * TYPE * TYPE   (* function(T1):T2   *)
       | ObjType of (label * TYPE) list  (* {l:T,...}  *)
       | DynObjType                      (* {*} *)
       | VarType of string               (* a reference to a type variable "X" *)
       | GenericType of string * TYPE    (* forall X. T, or from "type T.<X> = ..." *)
       | AppType of TYPE * TYPE          (* T1.<T2>     *)

fun likeToString (l:LIKE) : string =
    case l of Like => "like " | Exact => "" | Wrap => "wrap "

fun typeToString (t:TYPE) : string =
    case t of
        AnyType => "*"
      | IntType => "int"
      | FunType (l,t1,t2) => "function(" ^ (likeToString l)^(typeToString t1) ^ "):" ^ (typeToString t2)
      | ObjType tyfields => 
        "{" ^ 
        (List.foldl (fn ((l,t),r) => (l^":"^(typeToString t)^", "^r)) "" tyfields)
        ^"}"
      | DynObjType => "{*}"
      | VarType y => y
      | GenericType (y,t1) => "(function.<" ^ y ^ "> " ^ (typeToString t1)^")"
      | AppType (t1,t2) => (typeToString t1)^".<"^(typeToString t2)^">"
      | TypeType => "type"

(*********** Expressions ***********)

datatype EXPR =
         IntExpr of int
       | VarExpr of string                      (* variable reference *)
       | LetExpr of string * TYPE * EXPR * EXPR (* let x:T = e1 in e2 *)
       | FunExpr of string * LIKE * TYPE * TYPE * EXPR (* arg and result types on fns, like in ES4 *)
       | CastExpr of TYPE * EXPR                
       | AppExpr of EXPR * EXPR                 (* application  e1(e2) *)
       | ObjExpr of (label*EXPR) list * TYPE    (* {l:e,...}:T *)
       | GetExpr of EXPR * label                (* e.l *)
       | SetExpr of EXPR * label * EXPR         (* e.l = e' *)
       | ExpectedTypeExpr of TYPE * EXPR        (* not in source, inserted by verifier *)
       | TypeExpr of TYPE                       (* like  "type(...)" in ES4 *)
       | LetTypeExpr of string * TYPE * EXPR    (* like "type X = ..." in ES4 *)
       | GenericExpr of string * EXPR * TYPE    (* like function.<X>():T {e} in ES4 *)
       | AppTypeExpr of EXPR * TYPE             (* like  e.<T>  in ES4, e[T] in TAPL *)

(*********** String conversion ************)

fun exprToString (e:EXPR) : string =
    case e of
        IntExpr n => (Int.toString n)
      | VarExpr x => x
      | LetExpr (x,t,e,body) => 
        "let "^x^":"^(typeToString t)^"="^(exprToString e)^" in "^(exprToString body)^" end"
      | FunExpr (x,l,t1,t2,e) =>
        "function("^x^":"^(likeToString l)^(typeToString t1)^"):"^(typeToString t2)^" { "^(exprToString e)^" }"
      | CastExpr (ty,e) =>
        "("^(exprToString e)^" to "^(typeToString ty)^")"
      | AppExpr (e1,e2) =>
        (exprToString e1)^"("^(exprToString e2)^")"
      | ObjExpr (efields,t) => 
        "{" ^ 
        (List.foldl (fn ((l,e),r) => l^":"^(exprToString e)^", "^r) "" efields)
        ^"}:"^(typeToString t)
      | GetExpr (e,l) => (exprToString e)^"."^l
      | SetExpr (e1, l, e2) =>
        (exprToString e1)^"."^l^"="^(exprToString e2)
      | TypeExpr t =>
        "type("^(typeToString t)^")"
      | GenericExpr (x,e,t) => "function.<"^x^">:"^(typeToString t)^" {"^(exprToString e) ^"}"
      | AppTypeExpr (e,t) => (exprToString e)^".<"^(typeToString t)^">"
      | ExpectedTypeExpr (t,e) => "("^(exprToString e)^"):"^(typeToString t)
      | LetTypeExpr (x,t,body) => "type "^x^"="^(typeToString t)^" in "^(exprToString body)^" end"

(*********** Substitution on types ***********)

fun substType (t:TYPE) (x:string) (s:TYPE) =
    case t of
        AnyType => t
      | IntType => t
      | TypeType => t
      | FunType (l,t1,t2) =>
        FunType (l,substType t1 x s, substType t2 x s)
      | ObjType tyfields => ObjType (List.map (fn (l,t1) => (l,substType t1 x s)) tyfields)
      | VarType y =>
        if x=y 
        then s
        else t
      | GenericType (y,t1) =>
        let val y' = gensym y
            val t1' = substType t1 y (VarType y')
            val t1'' = substType t1' x s
        in
            GenericType (y',t1'')
        end
      | AppType (t1,t2) =>
        AppType (substType t1 x s, substType t2 x s)

(*********** Run-time values ***********)

(* Every closure has a "safe" bit.
 * This Safe bit, together with the Strict/Standard mode, constrain the relationship between
 * the dynamic type of the closure  and the static type of the reference to the closure,
 * as follows:
 *
 * SAFEBIT     MODE                    typing relation
 * Safe        Strict or Standard      subtyping
 * Unsafe      Strict                  compatibility
 * Unsafe      Standard                no constraint
 *)

datatype SAFEBIT =
         Safe
       | Unsafe

(* SAFE BITS UNUSED RIGHT NOW *)

datatype VAL =
         IntVal of int
       | FunVal of string * LIKE * TYPE * TYPE * EXPR * VAL ENV * SAFEBIT
       | GenericVal of string * EXPR * TYPE * VAL ENV
       | ObjVal of VAL ENV * TYPE * (label * VAL ref) list * SAFEBIT (* close over env for type *)
       | TypeVal of TYPE_CLOSURE (* for an expression variable bound to a type *)
       | TypeVar of TYPE_CLOSURE (* for a type variable bound to a type *)
      withtype
         TYPE_CLOSURE = VAL ENV * TYPE

fun valToString (v:VAL) : string =
    case v of
        IntVal n => (Int.toString n)
      | FunVal (x,l,t1,t2,e,n,b) =>
        "function("^x^":"^(likeToString l)^(typeToString t1)^"):"^(typeToString t2)^" { "^(exprToString e)
        ^" } ENV "^(valEnvToString n)
      | ObjVal (n,t,vfields,b) => 
        "{" ^ 
        (List.foldl (fn ((l,v),r) => l^":"^(valToString (!v))^", "^r) "" vfields)
        ^"}:"^(typeToString t)
        ^" ENV "^(valEnvToString n)
      | GenericVal (x,e,t,n) => 
        "function.<"^x^">:"^(typeToString t)^" {"^(exprToString e) ^"}"^" ENV "^(valEnvToString n)
      | TypeVal (n,t) => "(TypeVal "^(typeToString t)^" ENV "^(valEnvToString n)^")"
      | TypeVar (n,t) => "(TypeVar "^(typeToString t)^" ENV "^(valEnvToString n)^")"

and valEnvToString (n:(VAL ENV)) : string =
    envToString  n valToString

(********** Type normalization **********)

exception BadType of TYPE

(* types are never evaluated, just normalized wrt variable lookup and beta reduction
 * at the top level. Nested types may not be normalized.
 *)

fun normalizeType ((n,t):TYPE_CLOSURE) : TYPE_CLOSURE =
    let
        val _ = debug ("normalizing: "^(typeToString t)^"\n")
        val (m,r) =
            case t of
                VarType x =>
                let in
                    case lookup2 n x of
                        SOME (TypeVar tycl) => normalizeType tycl
                      | NONE => (n,t)
                end
              | AppType (t1,t2) =>
                let val (n1,t1') = normalizeType (n,t1) in
                    case t1' of
                        GenericType (x,body) =>
                        normalizeType
                            (extend n x (TypeVar (n,t2)),
                             body)
                      | _ => raise BadType t
                end
              | _ =>
                (* cannot reduce *)
                (n,t)
         val _ = debug ("normalized: "^(typeToString t)^" to "^(typeToString r)^"\n")
    in
        (m,r)
    end
    
(********** Subtyping, compatibility **********)

fun compatibleType (tyClosure1:TYPE_CLOSURE) (tyClosure2:TYPE_CLOSURE) : bool =
    let val (n1,t1) = normalizeType tyClosure1
        val (n2,t2) = normalizeType tyClosure2
    in
	(t1=t2)
	orelse 
        case (t1,t2) of
            (_,AnyType) => true
          | (AnyType,_) => true
          | (FunType(_,s1,t1), FunType(_,s2,t2)) =>
            (compatibleType (n2,s2) (n1,s1)) andalso (compatibleType (n1,t1) (n2,t2))
          | (ObjType fields1, ObjType fields2) =>
            List.all
                (fn (l1,t1) =>
                    (List.exists
                         (fn (l2,t2) =>
                             (l1=l2) andalso (bicompatibleType (n1,t1) (n2,t2)))
                     fields2))
            fields1
          | (ObjType _, DynObjType) => true
          | (DynObjType, ObjType _) => true
          | (VarType x, VarType y) =>
            (* neither is bound in its environment, due to normalization *)
            x=y
          | (GenericType (x1,s1), GenericType (x2,s2)) =>
            let val x = TypeVar (emptyEnv, VarType (gensym "X")) in
                compatibleType
                    (extend n1 x1 x, s1)
                    (extend n2 x2 x, s2)
            end
          | _ => false
    end

and bicompatibleType (tyClosure1:TYPE_CLOSURE) (tyClosure2:TYPE_CLOSURE) : bool =
    compatibleType tyClosure1 tyClosure2
    andalso
    compatibleType tyClosure2 tyClosure1

fun subType (tyClosure1:TYPE_CLOSURE) (tyClosure2:TYPE_CLOSURE) : bool =
    let val (n1,t1) = normalizeType tyClosure1
        val (n2,t2) = normalizeType tyClosure2
        (* val _ = print ("Subtype?: "^(typeToString t1)^" <: "^(typeToString t2)^"\n") *)
    in
        (t1=t2)
        orelse 
        case (t1,t2) of
            (_, AnyType) => true
          | (FunType(l1,s1,t1), FunType(l2,s2,t2)) =>
            (l1=l2 orelse l1=Like) andalso
            ( (if l1=Like then compatibleType else subType) (n2,s2) (n1,s1)) andalso 
            (subType (n1,t1) (n2,t2))
          | (ObjType tyfields1, ObjType tyfields2) =>
            List.all
                (fn (l1,t1) =>
                    (List.exists
                         (fn (l2,t2) =>
                             (l1=l2) andalso (equalType (n1,t1) (n2,t2)))
                     tyfields2))
            tyfields1
          | (ObjType _, DynObjType) => true
          | (VarType x, VarType y) =>
            (* neither is bound in its environment, due to normalization *)
            x=y
          | (GenericType (x1,s1), GenericType (x2,s2)) =>
            let val x = TypeVar (emptyEnv, VarType (gensym "X")) in
                subType (extend n1 x1 x, s1)
                        (extend n2 x2 x, s2)
            end
          | _ => false
    end

and equalType (tyClosure1:TYPE_CLOSURE) (tyClosure2:TYPE_CLOSURE) : bool =
    subType tyClosure1 tyClosure2
    andalso
    subType tyClosure2 tyClosure1

(********** Evaluation **********)

exception InternalError of string
exception NotAClosure of VAL
exception NotARef of VAL
exception ConversionError of (VAL * TYPE_CLOSURE)

fun typeOfVal (v:VAL) : TYPE_CLOSURE =
    case v of
        IntVal _ => (emptyEnv,IntType)
      | FunVal (_,l,t1,t2,_,n,_) => (n, FunType (l,t1,t2))
      | ObjVal (n,t,_,_) => (n,t)
      | TypeVal _ => ([], TypeType)
      | GenericVal (x,e,t,n) => (n,GenericType(x,t))
      | _ => raise InternalError "Unexpected VAL in typeOfVal"


fun markSafeBit (v:VAL) (s:SAFEBIT): VAL =
    case v of
        IntVal _ => v
      | FunVal (x,l,t1,t2,body,env,_) => FunVal(x,l,t1,t2,body,env,s)
      | ObjVal (n,t,f,_) => ObjVal(n,t,f,s)
      | TypeVal _ => v
      | _ => raise InternalError "Unexpected VAL in markSafeBit"


fun compatibleValue (v:VAL) (l:LIKE) (tycl:TYPE_CLOSURE) : bool =
    let val (n,t) = normalizeType tycl
    in
        case l of
            Exact => subType (typeOfVal v) (n,t)
          | Like =>
            case (v,t) of
                (ObjVal (n,DynObjType,vfields,_), ObjType tfields) =>
                List.all
                    (fn (l1,t1) =>
                        (List.exists
                             (fn (l2,v2) =>
                                 (l1=l2) andalso (compatibleValue (!v2) Like (n,t1)))
                             vfields))
                    tfields
              | _ => compatibleType (typeOfVal v) (n,t)
    end

fun convert (v:VAL) (l:LIKE) ((n,t):TYPE_CLOSURE) : VAL =
    let val (n,t) = normalizeType (n,t) 
    in
        if compatibleValue v l (n,t)
        then v
        else 
            let val _ = print ("val : "^(valToString v)^"\n")
                val _ = print ("type: "^(likeToString l)^(typeToString t)^" ENV "^(valEnvToString n)^"\n")
            in
                raise ConversionError (v,(n,t))
            end
    end

fun eval (n:VAL ENV) (e:EXPR) : VAL =
    let val _ = print ("   Eval'ing: "^(exprToString e)^" ENV "^(valEnvToString n)^"\n")
        val r = 
    case e of
        IntExpr n => IntVal n
      | VarExpr x => lookup n x
      | LetExpr (x,t,e,body) => eval (extend n x (convert (eval n e) Exact (n,t))) body
      (* following no longer needed, removed by verifier *)
      | LetTypeExpr (x,t,body) => eval (extend n x (TypeVar (n,t))) body
      | TypeExpr t => TypeVal (n,t)
      | FunExpr (x,l,t1,t2,e) => FunVal (x,l,t1,t2,e,n,Safe)
      | GenericExpr (x,e,t) => GenericVal (x,e,t,n)
      | CastExpr (ty,e) => convert (eval n e) Exact (n,ty) (* FIXME *)
      | ExpectedTypeExpr (retTy, e) => convert (eval n e) Exact (* FIXME *) (n,retTy)
      | AppExpr (e1,e2) =>
        let in
            case (eval n e1) of
                FunVal (x,l,argTy,retTy,body,n2,_) =>
                let val argVal  = (eval n e2)
                    val argVal' = convert argVal l (n2,argTy)
                    val resVal  = eval (extend n2 x argVal') body
                    (* tail call issues here *)
                    val resVal' = convert resVal Exact (n2,retTy)
                in
                    resVal'
                end
              | v => raise NotAClosure v  (* Can happen if e1 has static type "*" *)
        end
      | ObjExpr (efields,t) => 
        let val vfields = List.map (fn (l,e) => (l, ref (eval n e))) efields
        in
            ObjVal (n, t, vfields, Safe)
        end
      | GetExpr (e,l) =>
        let val ObjVal (_,_,vfields,safebit) = eval n e
            val valref = lookup vfields l
        in
            (!valref)  
        end
      | SetExpr (e1,l,e2) =>
        let val ObjVal (n2,t,vfields,safebit) = eval n e1
            val valref = lookup vfields l
            val v = eval n e2
        in
            valref := convert v Exact (n2,t);
            v
        end
      | AppTypeExpr (e,t) =>
        let val GenericVal(x,body,t2,n2) = eval n e in
            convert (eval (extend n2 x (TypeVar (n,t))) body) Exact (n2,t2)
        end
        val _ = print ("   Result: "^(valToString r)^" from "^(exprToString e)^" ENV "^(valEnvToString n)^"\n")
    in r end

(*********** Tests **********)


fun printStackTrace e =
    let val ss = SMLofNJ.exnHistory e
        val s = General.exnMessage e
        val name = General.exnName e
        val details = if s = name then "" else (" [" ^ s ^ "]")
    in
        TextIO.print ("Uncaught exception " ^ name ^ details ^ "\n");
        case ss of
             [] => ()
           | (s::ss') => (
                             TextIO.print ("  raised at: " ^ s ^ "\n");
                             List.app (fn s' => TextIO.print ("             " ^ s' ^ "\n")) ss'
                         )
    end




fun go  (e:EXPR) =
    let val _ = print ("Evaluating  : "^(exprToString e)^"\n")
        val v = eval [] e
    in
        ()
    end



(*
fun go (mode:MODE) (e:EXPR) =
    let val _ = print ("Verifying  : "^(exprToString e)^"\n")
        val (e',ty) = verifyExpr mode [] e
        val _ = print ("compiles to: "^(exprToString e')^"\n")
        val _ = print ("with type  : "^(typeToString ty)^"\n")
        val v = eval [] e'
    in
        ()
    end
*)
(*
    handle e => (print "Ex\n"; printStackTrace e; ())
*)

fun go2 (e:EXPR) =
    ( (*go Standard e,  *)
     go  e)

val idint : EXPR = FunExpr("x",Like,IntType,IntType,VarExpr "x")
val idany : EXPR = FunExpr("x",Like,AnyType,AnyType,VarExpr "x")
val idbad : EXPR = FunExpr("x",Like,AnyType,IntType,VarExpr "x");
val idbad2: EXPR = FunExpr("x",Like,IntType,AnyType,VarExpr "x");

(go2 idint);
(go2 idany);
(go2 idbad);
(go2 idbad2);

(go2
     (LetExpr ("f" ,AnyType, idint,
               (AppExpr (VarExpr "f",
                         IntExpr 4
)))));

(go2
     (LetTypeExpr ("X", IntType,
                  LetExpr ("f" ,
                           AnyType,
                           FunExpr ("x", Like, VarType "X", IntType,
                                    VarExpr "x"),
                           AppExpr (VarExpr "f",
                                    IntExpr 4)))));



(go2
     (LetExpr ("polyId" ,
               GenericType ("X", FunType(Like, VarType "X", VarType "X")),
               GenericExpr ("Y",
                            FunExpr ("y", Like, VarType "Y", VarType "Y", VarExpr "y"),
                            FunType(Like, VarType"Y",VarType "Y")),
               LetTypeExpr ("X", IntType,
                            AppExpr( AppTypeExpr (VarExpr "polyId", VarType "X"),
                                     IntExpr 4)))));

(go2
 (LetExpr ("r",
           AnyType,
           ObjExpr ([("l",IntExpr 0)], ObjType [("l", IntType)]),
           GetExpr (VarExpr "r", "l"))));

(go2
 (LetTypeExpr ("POLYID",
               GenericType ("X",  FunType(Like, VarType "X", VarType "X")),
               LetExpr ("polyId" ,
                        VarType "POLYID",
                        GenericExpr ("Y",
                                     FunExpr ("y", Like, VarType "Y", VarType "Y", VarExpr "y"),
                                     FunType(Like, VarType"Y",VarType "Y")),
                        LetTypeExpr ("INT",
                                     IntType,
                                     LetExpr ("f",
                                              AppType (VarType "POLYID", VarType "INT"),
                                              AppTypeExpr (VarExpr "polyId", VarType "INT"),
                                              AppExpr (VarExpr "f",
                                                       IntExpr 4)))))));



(print "Done\n\n\n");
