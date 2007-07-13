(* -*- mode: sml; mode: font-lock; tab-width: 60; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* vim: set ts=4 sw=4 et: *)

(*

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

datatype TYPE =
         AnyType                         (* the type "*" *)
       | IntType
       | TypeType                        (* the type of types *)
       | FunType of TYPE * TYPE          (* function(T1):T2   *)
       | ObjType of (label * TYPE) list  (* {l:T,...}  *)
       | VarType of string               (* a reference to a type variable "X" *)
       | GenericType of string * TYPE    (* forall X. T, or from "type T.<X> = ..." *)
       | AppType of TYPE * TYPE          (* T1.<T2>     *)

fun typeToString (t:TYPE) : string =
    case t of
        AnyType => "*"
      | IntType => "int"
      | FunType (t1,t2) => "function(" ^ (typeToString t1) ^ "):" ^ (typeToString t2)
      | ObjType tyfields => 
        "{" ^ 
        (List.foldl (fn ((l,t),r) => (l^":"^(typeToString t)^", "^r)) "" tyfields)
        ^"}"
      | VarType y => y
      | GenericType (y,t1) => "(function.<" ^ y ^ "> " ^ (typeToString t1)^")"
      | AppType (t1,t2) => (typeToString t1)^".<"^(typeToString t2)^">"
      | TypeType => "type"

(*********** Expressions ***********)

datatype EXPR =
         IntExpr of int
       | VarExpr of string                      (* variable reference *)
       | LetExpr of string * TYPE * EXPR * EXPR (* let x:T = e1 in e2 *)
       | FunExpr of string * TYPE * TYPE * EXPR (* arg and result types on fns, like in ES4 *)
       | CastExpr of TYPE * EXPR                
       | AppExpr of EXPR * EXPR                 (* application  e1(e2) *)
       | ObjExpr of (label*EXPR) list * TYPE    (* {l:e,...}:T *)
       | GetExpr of EXPR * label                (* e.l *)
       | SetExpr of EXPR * label * EXPR         (* e.l = e' *)
       | ExpectedTypeExpr of TYPE * EXPR        (* not in source, inserted by verifier *)
       | TypeExpr of TYPE                       (* like  "type(...)" in ES4 *)
       | LetTypeExpr of string * TYPE * EXPR    (* like "type X = ..." in ES4 *)
       | GenericExpr of string * EXPR           (* like function.<X>() {e} in ES4 *)
       | AppTypeExpr of EXPR * TYPE             (* like  e.<T>  in ES4, e[T] in TAPL *)

(*********** String conversion ************)

fun exprToString (e:EXPR) : string =
    case e of
        IntExpr n => (Int.toString n)
      | VarExpr x => x
      | LetExpr (x,t,e,body) => 
        "let "^x^":"^(typeToString t)^"="^(exprToString e)^" in "^(exprToString body)^" end"
      | FunExpr (x,t1,t2,e) =>
        "function("^x^":"^(typeToString t1)^"):"^(typeToString t2)^" { "^(exprToString e)^" }"
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
      | GenericExpr (x,e) => "function.<"^x^">{"^(exprToString e) ^"}"
      | AppTypeExpr (e,t) => (exprToString e)^".<"^(typeToString t)^">"
      | ExpectedTypeExpr (t,e) => "("^(exprToString e)^"):"^(typeToString t)
      | LetTypeExpr (x,t,body) => "type "^x^"="^(typeToString t)^" in "^(exprToString body)^" end"

(*********** Substitution on types ***********)

fun substType (t:TYPE) (x:string) (s:TYPE) =
    case t of
        AnyType => t
      | IntType => t
      | TypeType => t
      | FunType (t1,t2) =>
        FunType (substType t1 x s, substType t2 x s)
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

datatype VAL =
         IntVal of int
       | FunVal of string * TYPE * TYPE * EXPR * VAL ENV * SAFEBIT
       | GenericVal of string * EXPR * VAL ENV
       | ObjVal of VAL ENV * TYPE * (label * VAL ref) list * SAFEBIT (* close over env for type *)
       | TypeVal of TYPE_CLOSURE (* for an expression variable bound to a type *)
       | TypeVar of TYPE_CLOSURE (* for a type variable bound to a type *)
      withtype
         TYPE_CLOSURE = VAL ENV * TYPE

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

fun subType (tyClosure1:TYPE_CLOSURE) (tyClosure2:TYPE_CLOSURE) : bool =
    let val (n1,t1) = normalizeType tyClosure1
        val (n2,t2) = normalizeType tyClosure2
        (* val _ = print ("Subtype?: "^(typeToString t1)^" <: "^(typeToString t2)^"\n") *)
    in
        case (t1,t2) of
            (AnyType,AnyType) => true
          | (IntType,IntType) => true
          | (TypeType,TypeType) => true
          | (FunType(s1,t1), FunType(s2,t2)) =>
            (subType (n2,s2) (n1,s1)) andalso (subType (n1,t1) (n2,t2))
          | (ObjType tyfields1, ObjType tyfields2) =>
            List.all
                (fn (l1,t1) =>
                    (List.exists
                         (fn (l2,t2) =>
                             (l1=l2) andalso (equalType (n1,t1) (n2,t2)))
                     tyfields2))
            tyfields1
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

fun compatibleType (tyClosure1:TYPE_CLOSURE) (tyClosure2:TYPE_CLOSURE) : bool =
    let val (n1,t1) = normalizeType tyClosure1
        val (n2,t2) = normalizeType tyClosure2
    in
        case (t1,t2) of
            (_,AnyType) => true
          | (AnyType,_) => true
          | (TypeType,TypeType) => true
          | (IntType,IntType) => true
          | (FunType(s1,t1), FunType(s2,t2)) =>
            (compatibleType (n2,s2) (n1,s1)) andalso (compatibleType (n1,t1) (n2,t2))
          | (ObjType fields1, ObjType fields2) =>
            List.all
                (fn (l1,t1) =>
                    (List.exists
                         (fn (l2,t2) =>
                             (l1=l2) andalso (bicompatibleType (n1,t1) (n2,t2)))
                     fields2))
            fields1
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

(*********** Verify routines ***********)

exception TypeError of string

fun typeError (s:string) =
    let in
        print ("Type Error: "^s);
        raise (TypeError s)
    end

datatype MODE =
         Standard
       | Strict

datatype BIND =
         ProgVar of TYPE
       | UnknownTypeVar
       | KnownTypeVar of TYPE

(* As soon as types are encountered in the AST, they are (deeply)
normalized wrt beta reduction and * type variable bindings, during
verify-time.
 *)

fun verifyType (n:BIND ENV) (t:TYPE) : TYPE =
    let
    in
        case t of
            AnyType => t
          | IntType => t
          | TypeType => t
          | FunType (s,t) =>
            FunType (verifyType n s,
                     verifyType n t)
          | ObjType tyfields => 
            let 
                (* should check for duplicate labels *)
                val tyfields' = (List.map
                                     (fn (l,t) => (l, verifyType n t))
                                     tyfields)
            in
                ObjType tyfields'
            end
          | VarType x =>
            let in
                case lookup n x of
                    KnownTypeVar t => t
                  | UnknownTypeVar => t
                  | _ => typeError ("Unknown type variable "^x)
            end
          | GenericType (x,t) =>
            GenericType (x, verifyType (extend n x UnknownTypeVar) t)
          | AppType (t1,t2) =>
            let val t1 = verifyType n t1
                val t2 = verifyType n t2
            in
                case t1 of
                    GenericType (x,body) =>
                    verifyType
                        (extend n x (KnownTypeVar t2))
                        body
                  | _ => raise BadType t
            end
    end

(* The expression "e" of type "s" is being converted to type "t" in mode "mode".
 * This function either returns "e" itself, raises a type error,
 * or wraps "e" in a cast to "t", as appropriate.
 *)

fun check (mode:MODE)  (e:EXPR) (s:TYPE) (t:TYPE) : EXPR =
    if (subType (emptyEnv,s) (emptyEnv,t))
    then e
    else if (compatibleType (emptyEnv,s) (emptyEnv,t)) orelse mode=Standard
    then
        (* t may have free type vars bound in n, but they will be bound at run-time as well *)
        CastExpr (t,e)
    else typeError ("Expression "^(exprToString e)^" of type "^(typeToString s)^" is not compatible with type "^(typeToString t))

(* Type checks expression "e" in type environment "n" and mode "mode",
 * and returns a pair of
 * an expression (which is "e" with extra casts where necessary)
 * and the inferred type of "e".
 *
 * The resulting expression satisfies the invariant that if an expression of type "s"
 * is bound to a variable of type "t", then "s" is a subTyp  of "t" - this invariant
 * is ensured via dynamic casts if necessary, both in Strict and Standard mode.
 *
 * The env "n" has already been applied to the resulting type.
 *
 * No duplicate variables allowed in BIND ENV, ever!
 *)

fun verifyExpr (mode:MODE) (n:BIND ENV) (e:EXPR) : (EXPR * TYPE) =
    let fun expected ((e,t):(EXPR * TYPE)) : (EXPR * TYPE) = (ExpectedTypeExpr (t,e), t)
        val _ = debug ("verifying  "^(exprToString e)^"\n")
    in
    case e of
        IntExpr n => (e,IntType)
      | VarExpr x =>
        let val ty =
                case lookup n x of
                    (* Don't allow type variables in expressions *)
                    ProgVar t => t
                  | _ => typeError ("Unbound variable "^x)
        in (e,ty)
        end
      | LetTypeExpr (x,t,body) =>
        let val t = verifyType n t
            val _ = checkUnbound n x
            val n' = extend n x (KnownTypeVar t)
            val (body',bodyTy) = verifyExpr mode n' body
        in
            (body', bodyTy)
    (* 
     the LetTypeExpr goes away here, and "x" is already not free in bodyTy.
     was: (LetTypeExpr (x, t, body'), substType bodyTy x t)
     *)
        end
      | LetExpr (x,t,e,body) =>
        let val t = verifyType n t
            val e' = verifyAndCheck mode n e t
            val n' = extend n x (ProgVar t)
            val (body',bodyTy) = verifyExpr mode n' body
        in
            (LetExpr (x, t, e', body'), bodyTy)
        end
      | FunExpr (x,t1,t2,e) =>
        let val t1 = verifyType n t1
            val t2 = verifyType n t2
            val e' = verifyAndCheck mode (extend n x (ProgVar t1)) e t2
        in
            (FunExpr(x,t1,t2,e'), FunType(t1,t2))
        end
      | CastExpr (ty,e) =>
        let val ty = verifyType n ty
            val (e',ty') = verifyExpr mode n e
        in
            (CastExpr(ty,e'), ty)
        end
      | AppExpr (e1,e2) =>
        let val (e1',ty1) = verifyExpr mode n e1
        in
        case ty1 of
            FunType (argty,resty) =>
            expected (AppExpr(e1', verifyAndCheck mode n e2 argty),
                      resty)
          | AnyType =>
            (* eval will cast e1' to (*->*) because of the safety bit *)
            expected (AppExpr( e1',
                               verifyAndCheck mode n e2 AnyType),
                      AnyType)
          | _ => typeError ("Bad function type "^(typeToString ty1))
        end
      | ObjExpr (efields,ty) =>
        let val ty' = verifyType n ty
            val ObjType tfields = ty'
            val efields' =
                List.map
                    (fn (l,e) =>
                        case lookup2 tfields l of
                            NONE =>
                            (* l not in type, no type constraints *)
                            let val (e',t') = verifyExpr mode n e
                            in (l,e')
                            end
                          | SOME t =>
                            (l, verifyAndCheck mode n e t))
                    efields
        in
            (ObjExpr (efields',ty'), ty')
        end
      | GetExpr (e,l) =>
        let val (e',t) = verifyExpr mode n e
        in
            case t of
                ObjType tyfields =>
                expected (GetExpr (e',l), lookup tyfields l)
              | AnyType =>
                expected (GetExpr (CastExpr (ObjType [(l,AnyType)], e'), l),
                          AnyType)
              | _ => typeError ("Bad get expr "^(typeToString t))
        end
      | SetExpr (e1, l, e2) =>
        let val (e1',t1) = verifyExpr mode n e1
        in
            case t1 of
                ObjType tyfields =>
                let val s = lookup tyfields l in
                    (SetExpr (e1', l,
                              verifyAndCheck mode n e2 s),
                     s)
                end
              | AnyType =>
                (SetExpr (CastExpr(ObjType [(l,AnyType)], e1'), l,
                          verifyAndCheck mode n e2 AnyType),
                 AnyType)
              | _ => typeError ("Bad set expr "^(typeToString t1))
        end
      | TypeExpr t =>   (* like  "type(...)" in ES4 *)
        let val t = verifyType n t in
            (TypeExpr t, TypeType)
        end
       | GenericExpr (x,e) =>
         let val _ = checkUnbound n x
             val n' = extend n x UnknownTypeVar
             val (e',s) = verifyExpr mode n' e
         in
             (GenericExpr (x,e), GenericType (x,s))
         end
       | AppTypeExpr (e,t) =>
         let val (e',s) = verifyExpr mode n e
             val t = verifyType n t
         in
             case s of
                 GenericType (x,s'') =>
                 (AppTypeExpr (e',t), substType s'' x t)
               | _ => typeError ("Not a generic function: "^(exprToString e))
         end
       | ExpectedTypeExpr _ => typeError "ExpectedTypeExpr in source"
    end

and verifyAndCheck (mode:MODE) (n:BIND ENV) (e:EXPR) (t:TYPE) =
    let val _ = verifyType n t
        val (e',t') = verifyExpr mode n e in
        check mode e' t' t
    end

(********** Evaluation **********)

exception InternalError of string
exception NotAClosure of VAL
exception NotARef of VAL
exception ConversionError of (VAL * TYPE_CLOSURE)

fun typeOfVal (v:VAL) : TYPE_CLOSURE =
    case v of
        IntVal _ => (emptyEnv,IntType)
      | FunVal (_,t1,t2,_,n,_) => (n, FunType (t1,t2))
      | ObjVal (n,t,_,_) => (n,t)
      | TypeVal _ => ([], TypeType)
      | _ => raise InternalError "Unexpected VAL in typeOfVal"

fun markSafeBit (v:VAL) (s:SAFEBIT): VAL =
    case v of
        IntVal _ => v
      | FunVal (x,t1,t2,body,env,_) => FunVal(x,t1,t2,body,env,s)
      | ObjVal (n,t,f,_) => ObjVal(n,t,f,s)
      | TypeVal _ => v
      | _ => raise InternalError "Unexpected VAL in markSafeBit"

(* Converts a value "v" to type "t",
 * raising an error if the type of "v" is not compatibleTypeClosure with "t",
 * and setting the unsafe bit if the type of "v" is not a subTypeClosure of "t".
 *)

fun convert (v:VAL) ((n,t):TYPE_CLOSURE) : VAL =
    let val (m,s) = typeOfVal v in
        if subType (m,s) (n,t)
        then markSafeBit v Safe  (* want to mark safe *)
        else if compatibleType  (m,s) (n,t)
        then markSafeBit v Unsafe
        else
            let val _ = print ("ConversionError from "^(typeToString s)^" to "^(typeToString t)^"\n")
            in
                raise ConversionError (v,(n,t))
            end
    end


fun eval (n:VAL ENV) (e:EXPR) : VAL =
    case e of
        IntExpr n => IntVal n
      | VarExpr x => lookup n x
      | LetExpr (x,t,e,body) => eval (extend n x (eval n e)) body
      (* following no longer needed, removed by verifier
      | LetTypeExpr (x,t,body) => eval (extend n x (TypeVar (n,t))) body
      *)
      | TypeExpr t => TypeVal (n,t)
      | FunExpr (x,t1,t2,e) => FunVal (x,t1,t2,e,n,Safe)
      | GenericExpr (x,e) => GenericVal (x,e,n)
      | CastExpr (ty,e) => convert (eval n e) (n,ty)
      | ExpectedTypeExpr (retTy, AppExpr (e1,e2)) =>
        let in
            case (eval n e1) of
                FunVal (x,argTy,_,body,n2,safebit) =>
                let val argVal  = (eval n e2)
                    val argVal' = case safebit of Safe => argVal
                                                | Unsafe => convert argVal (n2,argTy)
                    val resVal  = eval (extend n2 x argVal') body
                    (* tail call issues here *)
                    val resVal' = case safebit of Safe => resVal
                                                | Unsafe => convert resVal (n,retTy)
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
      | ExpectedTypeExpr (t, GetExpr (e,l)) =>
        let val ObjVal (_,_,vfields,safebit) = eval n e
            val valref = lookup vfields l
        in
            case safebit of
                Safe => !valref
              | Unsafe => convert (!valref) (n,t)
        end
      | SetExpr (e1,l,e2) =>
        let val ObjVal (n2,t,vfields,safebit) = eval n e1
            val valref = lookup vfields l
            val v = eval n e2
        in
            case safebit of
                Safe => valref := v
              | Unsafe => valref := convert v (n2,t);
            v
        end
      | AppTypeExpr (e,t) =>
        let val GenericVal(x,body,n2) = eval n e in
            eval (extend n2 x (TypeVal (n,t))) body
        end

(*********** Tests **********)

fun go (mode:MODE) (e:EXPR) =
    let val _ = print ("Verifying  : "^(exprToString e)^"\n")
        val (e',ty) = verifyExpr mode [] e
        val _ = print ("compiles to: "^(exprToString e')^"\n")
        val _ = print ("with type  : "^(typeToString ty)^"\n")
        val v = eval [] e'
    in
        v
    end

fun go2 (e:EXPR) =
    ( (*go Standard e,  *)
     go Strict e)

val idint : EXPR = FunExpr("x",IntType,IntType,VarExpr "x")
val idany : EXPR = FunExpr("x",AnyType,AnyType,VarExpr "x")
val idbad : EXPR = FunExpr("x",AnyType,IntType,VarExpr "x");
val idbad2 : EXPR = FunExpr("x",IntType,AnyType,VarExpr "x");

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
                           FunExpr ("x", VarType "X", IntType,
                                    VarExpr "x"),
                           AppExpr (VarExpr "f",
                                    IntExpr 4)))));



(go2
     (LetExpr ("polyId" ,
               GenericType ("X", FunType(VarType "X", VarType "X")),
               GenericExpr ("Y",
                            FunExpr ("y", VarType "Y", VarType "Y", VarExpr "y")),
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
               GenericType ("X",  FunType(VarType "X", VarType "X")),
               LetExpr ("polyId" ,
                        VarType "POLYID",
                        GenericExpr ("Y",
                                     FunExpr ("y", VarType "Y", VarType "Y", VarExpr "y")),
                        LetTypeExpr ("INT",
                                     IntType,
                                     LetExpr ("f",
                                              AppType (VarType "POLYID", VarType "INT"),
                                              AppTypeExpr (VarExpr "polyId", VarType "INT"),
                                              AppExpr (VarExpr "f",
                                                       IntExpr 4)))))));



(print "Done\n\n\n");
