(* -*- mode: sml; mode: font-lock; tab-width: 60; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* vim: set ts=4 sw=4 et: *)

(* This is an implementation of a toy language to express and clarify
 * a number of issues relating to the ES4 type system in a simpler
 * context, and may also help to shake out bugs in the type system earlier. 
 * - Cormac
 *)

(*********** environments ***********)
(* variable names are just strings,
 * environments are polymorphic to support both value and type environments
 *)

exception UnboundVariable of string

type 'a ENV = (string * 'a) list

fun extend (env:'a ENV) (x:string) (v:'a) : 'a ENV =
    (x,v)::env

fun lookup (env:'a ENV) (x:string) : 'a =
    case env of
        [] => raise UnboundVariable x
      | (y,v)::r => if (x=y) then v else lookup r x

(*********** Types, subtyping, and compatibility ***********)

datatype TYPE =
         AnyType  (* the type "*" *)
       | IntType
       | FunType of TYPE * TYPE
       | RefType of TYPE

(* Q: Is AnyType the top type? *)

fun subtype (t1:TYPE) (t2:TYPE) : bool =
    case (t1,t2) of
        (AnyType,AnyType) => true
      | (IntType,IntType) => true
      | (FunType(s1,t1), FunType(s2,t2)) =>
        (subtype s2 s1) andalso (subtype t1 t2)
      | (RefType s, RefType t) => (subtype s t) andalso (subtype t s)
      | _ => false

fun compatible (t1:TYPE) (t2:TYPE) : bool =
    case (t1,t2) of
        (_,AnyType) => true
      | (AnyType,_) => true
      | (IntType,IntType) => true
      | (FunType(s1,t1), FunType(s2,t2)) =>
        (compatible s2 s1) andalso (compatible t1 t2)
      | (RefType s, RefType t) =>
        (compatible s t) andalso (compatible t s)
      | _ => false

(*********** The expression language **********)

datatype EXPR =
         IntExpr of int
       | VarExpr of string
       | LetExpr of string * TYPE * EXPR * EXPR
       | FunExpr of string * TYPE * TYPE * EXPR  (* arg and result types *)
       | CastExpr of TYPE * EXPR
       | AppExpr of EXPR * EXPR
       | RefExpr of TYPE * EXPR     (* allocate, dereference, and update ref cells *)
       | GetExpr of EXPR
       | SetExpr of EXPR * EXPR
       | ExpectedType of TYPE * EXPR

(*********** Verify routines ***********)

(* some type errors *)

exception BadFunExpr of EXPR * TYPE
exception BadRefExpr of EXPR * TYPE
exception StaticTypeError of (EXPR * TYPE * TYPE)

datatype MODE =
         Standard
       | Strict

(* The expression "e" of type "s" is being converted to type "t" in mode "mode".
 * This function either returns "e" itself, raises a type error,
 * or wraps "e" in a cast to "t", as appropriate.
 *)

fun check (mode:MODE) (e:EXPR) (s:TYPE) (t:TYPE) : EXPR =
    if (subtype s t)
    then e
    else if (compatible s t) orelse mode=Standard
    then CastExpr (t,e)
    else raise StaticTypeError (e,s,t)

(* Type checks expression "e" in type environment "n" and mode "mode",
 * and returns a pair of 
 * an expression (which is "e" with extra casts where necessary)
 * and the inferred type of "e".
 *
 * The resulting expression satisfies the invariant that if an expression of type "s"
 * is bound to a variable of type "t", then "s" is a subtype of "t" - this invariant
 * is ensured via dynamic casts if necessary, both in Strict and Standard mode.
 *)
       
fun verify (mode:MODE) (n:TYPE ENV) (e:EXPR) : (EXPR * TYPE) =
    case e of
        IntExpr n => (e,IntType)
      | VarExpr x => (e, lookup n x)
      | LetExpr (x,t,e,body) => 
        let val e' = verifyAndCheck mode n e t
            val n' = extend n x t
            val (body',bodyTy) = verify mode n' body
        in
            (LetExpr (x, t, e', body'), bodyTy)
        end
      | FunExpr (x,t1,t2,e) => 
        let val e' = verifyAndCheck mode (extend n x t1) e t2
        in
            (FunExpr(x,t1,t2,e'), FunType(t1,t2))
        end
      | CastExpr (ty,e) =>
        let val (e',ty) = verify mode n e 
        in
            (CastExpr(ty,e'), ty)
        end
      | AppExpr (e1,e2) =>
        let val (e1',ty1) = verify mode n e1
        in
        case ty1 of
            FunType (argty,resty) =>
            (ExpectedType (resty, 
                           AppExpr(e1', 
                                   verifyAndCheck mode n e2 argty)), 
             resty)
          | AnyType =>
            (ExpectedType (AnyType, 
                           AppExpr( CastExpr( FunType(AnyType,AnyType), e1'),
                                    verifyAndCheck mode n e2 AnyType)),
             AnyType)
          | _ => raise BadFunExpr (e1',ty1)
        end
      | RefExpr (t,e) =>
        (RefExpr (t, verifyAndCheck mode n e t), RefType t)
      | GetExpr e =>
        let val (e',t) = verify mode n e in
            (ExpectedType (t, GetExpr e'), t)
        end
      | SetExpr (e1, e2) =>
        let val (e1',t1) = verify mode n e1
        in
            case t1 of
                RefType s =>
                (SetExpr (e1', 
                          verifyAndCheck mode n e2 s),
                 s)
              | AnyType =>
                (SetExpr (CastExpr(RefType AnyType, e1'), 
                          verifyAndCheck mode n e2 AnyType),
                 AnyType)
              | _ => raise BadRefExpr (e1',t1)
        end

and verifyAndCheck (mode:MODE) (n:TYPE ENV) (e:EXPR) (t:TYPE) =
    let val (e',t') = verify mode n e in
        check mode e' t' t
    end


(*********** Evaluation, run-time values, and conversion ***********)

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
       | ClosureVal of string * TYPE * TYPE * EXPR * VAL ENV * SAFEBIT
       | RefVal of TYPE * VAL ref * SAFEBIT

exception NotAClosure of VAL
exception NotARef of VAL
exception ConversionError of (VAL * TYPE)

fun typeOfVal (v:VAL) : TYPE =
    case v of
        IntVal _ => IntType
      | ClosureVal (_,t1,t2,_,_,_) => FunType (t1,t2)      
      | RefVal (t,_,_) => RefType t

fun markUnsafe (v:VAL) : VAL =
    case v of
        IntVal _ => v
      | ClosureVal (x,t1,t2,body,env,_) => ClosureVal(x,t1,t2,body,env,Unsafe)
      | RefVal (t,v,_) => RefVal(t,v,Unsafe)

(* Converts a value "v" to type "t", 
 * raising an error if the type of "v" is not compatible with "t",
 * and setting the unsafe bit if the type of "v" is not a subtype of "t".
 *)

fun convert (v:VAL) (t:TYPE) : VAL =
    let val s = typeOfVal v in
        if subtype s t
        then v
        else if compatible s t
        then markUnsafe v
        else raise ConversionError (v,t)
    end

fun eval (n:VAL ENV) (e:EXPR) : VAL =
    case e of
        IntExpr n => IntVal n
      | VarExpr x => lookup n x
      | LetExpr (x,t,e,body) => eval (extend n x (eval n e)) body
      | FunExpr (x,t1,t2,e) => ClosureVal (x,t1,t2,e,n,Safe)
      | CastExpr (ty,e) => convert (eval n e) ty        
      | ExpectedType (retTy, AppExpr (e1,e2)) =>
        let in
            case (eval n e1) of
                ClosureVal (x,argTy,_,body,n2,safebit) =>
                let val argVal  = (eval n e2)
                    val argVal' = case safebit of Safe => argVal | Unsafe => convert argVal argTy
                    val resVal  = eval (extend n2 x argVal') body
                    val resVal' = case safebit of Safe => resVal | Unsafe => convert resVal retTy
                in
                    resVal'
                end
              | v => raise NotAClosure v
        end
      | RefExpr (t,e) => RefVal(t, ref (eval n e), Safe)
      | ExpectedType (t, GetExpr e) =>
        let in
            case eval n e of
                RefVal (_,r,Safe) => !r
              | RefVal (s,r,Unsafe) => convert (!r) t
        end
      | SetExpr (e1,e2) =>
        let in
            case eval n e1 of
                RefVal (t,r,safebit) =>
                let val v = eval n e2 in
                    case safebit of 
                        Safe => r := v 
                      | Unsafe => r := convert v t;
                    v
                end
              | v => raise NotARef v
        end
        
(*********** Tests **********)

fun go (mode:MODE) (e:EXPR) : (EXPR*VAL) =
    let val (e',ty) = verify mode [] e 
        val v = eval [] e'
    in 
        (e',v)
    end

fun go2 (e:EXPR) =
    (go Standard e, go Strict e)

val idint : EXPR = FunExpr("x",IntType,IntType,VarExpr "x")
val idany : EXPR = FunExpr("x",AnyType,AnyType,VarExpr "x")
val idbad : EXPR = FunExpr("x",AnyType,IntType,VarExpr "x");

(*
(go2 idint);
(go2 idany);
(go2 idbad);
*)

(go2
     (LetExpr ("f" ,AnyType, idint,
               (AppExpr (VarExpr "f", 
                         IntExpr 4 
)))))
