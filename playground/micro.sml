(* -*- mode: sml; mode: font-lock; tab-width: 60; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* vim: set ts=4 sw=4 et: *)

(* This is an implementation of a toy language to express and clarify
a number of issues relating to the ES4 type system in a simpler
context, and may also help to shake out bugs in the type system
earlier.  

High-level lessons:
 - AnyType is really a top type

 - In strict mode, programs without '*'
   give no run-time casts, and safety bit never gets set.

 - In standard mode, programs without '*' which would compile in strict mode, 
   also give no run-time casts, and safety bit never gets set.

 - we can *clear* the safe bit, if a subtype check holds!

 - Generics are a little tricky but they work out

 - Type closures are quite tricky, since the evaluator works via environments
   whereas the verifier works via substitution (much easier),
   and so subtyping etc needs to support both. Here, we use a notion of VAL
   that includes variants for run-time data and also for compile-time reasoning.

 - should type variable be allowed to appear in program code?

 - type X = ... etc all seem fine.
 
 *)

Control.Print.printDepth := 100;


(*********** environments ***********)
(* variable names are just strings. *)

exception UnboundVariable of string

type 'a ENV = (string * 'a) list

val 'a emptyEnv : 'a ENV = []

fun extend (env:'a ENV) (x:string) (v:'a) : 'a ENV =
    (x,v)::env

fun lookup (env:'a ENV) (x:string) : 'a =
    case env of
        [] => raise UnboundVariable x
      | (y,v)::r => if (x=y) then v else lookup r x

(*********** Types and expressions ***********)

datatype TYPE =
         AnyType  (* the type "*" *)
       | IntType
       | TypeType (* the type of types *)
       | FunType of TYPE * TYPE
       | RefType of TYPE
       | VarType of string
       | GenericType of string * TYPE

datatype EXPR =
         IntExpr of int
       | VarExpr of string
       | LetExpr of string * TYPE * EXPR * EXPR
       | FunExpr of string * TYPE * TYPE * EXPR  (* arg and result types on fns, like in ES4 *)
       | CastExpr of TYPE * EXPR
       | AppExpr of EXPR * EXPR
       | RefExpr of TYPE * EXPR                (* allocate, dereference, and update ref cells *)
       | GetExpr of EXPR
       | SetExpr of EXPR * EXPR
       | ExpectedTypeExpr of TYPE * EXPR       (* not in source, inserted by verifier *)
       | TypeExpr of TYPE                      (* like  "type(...)" in ES4 *)
       | LetTypeExpr of string * TYPE * EXPR   (* like "type X = ..." in ES4 *)
       | GenericExpr of string * EXPR          (* like function.<X>() {e} in ES4 *)
       | AppTypeExpr of EXPR * TYPE            (* like  e.<T>  in ES4, e[T] in TAPL *)

(*********** String conversion ************)

fun typeToString (t:TYPE) : string =
    case t of
        AnyType => "*"
      | IntType => "int"
      | FunType (t1,t2) => "function(" ^ (typeToString t1) ^ ") :" ^ (typeToString t2) 
      | RefType t1 => "ref" ^ (typeToString t1)
      | VarType y => y
      | GenericType (y,t1) => "(function.<" ^ y ^ "> " ^ (typeToString t1)^")"

fun exprToString (e:EXPR) : string =
    case e of
        IntExpr n => (Int.toString n)
      | VarExpr x => x
      | LetExpr (x,t,e,body) => "let "^x^":"^(typeToString t)^"="^(exprToString e)^" in "^(exprToString body)^" end"
      | FunExpr (x,t1,t2,e) => 
        "function("^x^":"^(typeToString t1)^"):"^(typeToString t2)^" { "^(exprToString e)^" }"
      | CastExpr (ty,e) =>
        "("^(exprToString e)^" cast "^(typeToString ty)^")"
      | AppExpr (e1,e2) =>
        (exprToString e1)^"("^(exprToString e2)^")"
      | RefExpr (t,e) => "ref "^(exprToString e)^":"^(typeToString t)
      | GetExpr e => "!"^(exprToString e)
      | SetExpr (e1, e2) =>
        (exprToString e1)^":="^(exprToString e2)
      | TypeExpr t =>  
        "type("^(typeToString t)^")"
      | GenericExpr (x,e) => "function.<"^x^">{"^(exprToString e) ^"}"
      | AppTypeExpr (e,t) => (exprToString e)^".<"^(typeToString t)^">"
      | ExpectedTypeExpr (t,e) => "("^(exprToString e)^"):"^(typeToString t)
      | LetTypeExpr (x,t,body) => "type "^x^"="^(typeToString t)^" in "^(exprToString body)^" end"

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
       | RefVal of VAL ENV * TYPE * VAL ref * SAFEBIT (* close over env for type *)
       | TypeVal of TYPE_CLOSURE (* for an expression variable bound to a type *)
       | TypeVar of TYPE_CLOSURE (* for a type variable bound to a type *)
       (* following used by verifier *)
       | UnknownProgVar of TYPE
       | UnknownTypeVar
withtype
         TYPE_CLOSURE = VAL ENV * TYPE

(********** Gensym, for alpha renaming **********)

val gensymCounter : int ref = ref 0
fun gensym (s:string) : string = 
    let
    in 
        gensymCounter := 1+(!gensymCounter);
        s ^ "$" ^  (Int.toString (!gensymCounter))
    end

(*********** Substitution on types ***********)

fun substType (t:TYPE) (x:string) (s:TYPE) =
    case t of
        AnyType => t
      | IntType => t
      | TypeType => t
      | FunType (t1,t2) => 
        FunType (substType t1 x s, substType t2 x s)
      | RefType t1 => RefType (substType t1 x s)
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
        
(********** Conversion **********)

exception NotAType of VAL
exception BadType of TYPE

(* types are never evaluated, just normalized,
 * but normalization of a TYPE_CLOSURE where no variable is bound to UnknownTypeVar
 * is the same as evaluation.
 * That is, evaluation is just a special case of partial evaluation (aka normalization)
 *)

fun normalizeTypeClosure ((n,t):TYPE_CLOSURE) : TYPE_CLOSURE =
    let in
        case t of
            VarType x =>
            let in
                case lookup n x of
                    TypeVar tyClosure => tyClosure
                  | UnknownTvar => (n,t)
            end
          | _ =>
            (* cannot reduce *)
            (n,t)
    end
    
fun subTypeClosure (tyClosure1:TYPE_CLOSURE) (tyClosure2:TYPE_CLOSURE) : bool =
    let val (n1,t1) = normalizeTypeClosure tyClosure1
        val (n2,t2) = normalizeTypeClosure tyClosure2
        (* val _ = print ("Subtype?: "^(typeToString t1)^" <: "^(typeToString t2)^"\n") *)
    in
        case (t1,t2) of
            (_,AnyType) => true
          | (IntType,IntType) => true
          | (TypeType,TypeType) => true
          | (FunType(s1,t1), FunType(s2,t2)) =>
            (subTypeClosure (n2,s2) (n1,s1)) andalso (subTypeClosure (n1,t1) (n2,t2))
          | (RefType t1, RefType t2) => 
            equalTypeClosure (n1,t1) (n2,t2) 
          | (VarType x, VarType y) =>
            (* neither is bound in its environment, due to normalization *)
            x=y
          | (GenericType (x1,s1), GenericType (x2,s2)) => 
            subTypeClosure ((extend n1 x1 UnknownTypeVar), s1)
                           ((extend n2 x1 UnknownTypeVar), substType s2 x2 (VarType x1))
          | _ => false
    end

and equalTypeClosure (tyClosure1:TYPE_CLOSURE) (tyClosure2:TYPE_CLOSURE) : bool =
    subTypeClosure tyClosure1 tyClosure2
    andalso
    subTypeClosure tyClosure2 tyClosure1

fun compatibleTypeClosure (tyClosure1:TYPE_CLOSURE) (tyClosure2:TYPE_CLOSURE) : bool =
    let val (n1,t1) = normalizeTypeClosure tyClosure1
        val (n2,t2) = normalizeTypeClosure tyClosure2
    in
        case (t1,t2) of
            (_,AnyType) => true
          | (AnyType,_) => true
          | (TypeType,TypeType) => true
          | (IntType,IntType) => true
          | (FunType(s1,t1), FunType(s2,t2)) =>
            (compatibleTypeClosure (n2,s2) (n1,s1)) andalso (compatibleTypeClosure (n1,t1) (n2,t2))
          | (RefType t1, RefType t2) =>
            bicompatibleTypeClosure (n1,t1) (n2,t2) 
          | (VarType x, VarType y) =>
            (* neither is bound in its environment, due to normalization *)
            x=y
          | (GenericType (x1,s1), GenericType (x2,s2)) => 
            compatibleTypeClosure ((extend n1 x1 UnknownTypeVar), s1)
                                  ((extend n2 x1 UnknownTypeVar), substType s2 x2 (VarType x1))
          | _ => false
    end

and bicompatibleTypeClosure (tyClosure1:TYPE_CLOSURE) (tyClosure2:TYPE_CLOSURE) : bool =
    compatibleTypeClosure tyClosure1 tyClosure2
    andalso
    compatibleTypeClosure tyClosure2 tyClosure1

(********** Evaluation **********)

exception InternalError of string
exception NotAClosure of VAL
exception NotARef of VAL
exception ConversionError of (VAL * TYPE_CLOSURE)

fun typeOfVal (v:VAL) : TYPE_CLOSURE =
    case v of
        IntVal _ => (emptyEnv,IntType)
      | FunVal (_,t1,t2,_,n,_) => (n, FunType (t1,t2))      
      | RefVal (n,t,_,_) => (n,RefType t)
      | TypeVal _ => ([], TypeType)
      | _ => raise InternalError "Unexpected VAL in typeOfVal"

fun markSafeBit (v:VAL) (s:SAFEBIT): VAL =
    case v of
        IntVal _ => v
      | FunVal (x,t1,t2,body,env,_) => FunVal(x,t1,t2,body,env,s)
      | RefVal (n,t,v,_) => RefVal(n,t,v,s)
      | TypeVal _ => v
      | _ => raise InternalError "Unexpected VAL in markSafeBit"

(* Converts a value "v" to type "t", 
 * raising an error if the type of "v" is not compatibleTypeClosure with "t",
 * and setting the unsafe bit if the type of "v" is not a subTypeClosure of "t".
 *)

fun convert (v:VAL) (t:TYPE_CLOSURE) : VAL =
    let val s = typeOfVal v in
        if subTypeClosure s t
        then markSafeBit v Safe  (* want to mark safe *)
        else if compatibleTypeClosure s t
        then markSafeBit v Unsafe
        else raise ConversionError (v,t)
    end

(* The evaluator never evaluates types, it just treats them as opaque objects,
 * and closes over them to create TYPE_CLOSURES, which are passed to the
 * subTypeClosure and compatibleTypeClosure algorithms. 
 * These algorithms perform normalization (essentially type evalation) where necessary.
 *)

fun eval (n:VAL ENV) (e:EXPR) : VAL =
    case e of
        IntExpr n => IntVal n
      | VarExpr x => lookup n x
      | LetExpr (x,t,e,body) => eval (extend n x (eval n e)) body
      | LetTypeExpr (x,t,body) => eval (extend n x (TypeVar (n,t))) body
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
              | v => raise NotAClosure v
        end
      | RefExpr (t,e) => RefVal(n, t, ref (eval n e), Safe)
      | ExpectedTypeExpr (t, GetExpr e) =>
        let val RefVal (_,_,r,safebit) = eval n e
        in
            case safebit of
                Safe => !r
              | Unsafe => convert (!r) (n,t)
        end
      | SetExpr (e1,e2) =>
        let val RefVal (n2,t,r,safebit) = eval n e1 
            val v = eval n e2 
        in
            case safebit of 
                Safe => r := v 
              | Unsafe => r := convert v (n2,t);
            v
        end
      | AppTypeExpr (e,t) =>
        let val GenericVal(x,body,n2) = eval n e in
            eval (extend n2 x (TypeVal (n,t))) body
        end

(*********** Verify routines ***********)

(* some type errors *)

exception BadFunExpr of EXPR * TYPE
exception BadRefExpr of EXPR * TYPE
exception StaticTypeError of (EXPR * TYPE * TYPE)
exception InvalidTypeVar of string
exception IllTypedExpr of EXPR * string

datatype MODE =
         Standard
       | Strict

type TVAL = VAL
(* Informally, a TVAL is a subtype of VAL containing:
       | TypeVar of TYPE_CLOSURE (* for a type variable bound to a type *)
       | UnknownProgVar of TYPE_CLOSURE
       | UnknownTypeVar
*)

(* The expression "e" of type "s" is being converted to type "t" in mode "mode".
 * This function either returns "e" itself, raises a type error,
 * or wraps "e" in a cast to "t", as appropriate.
 *)

fun check (mode:MODE) (n:TVAL ENV) (e:EXPR) (s:TYPE) (t:TYPE) : EXPR =
    if (subTypeClosure (n,s) (n,t))
    then e
    else if (compatibleTypeClosure (n,s) (n,t)) orelse mode=Standard
    then
        (* t may have free type vars bound in n, but they will be bound at run-time as well *)
        CastExpr (t,e)
    else raise StaticTypeError (e,s,t)

(* Type checks expression "e" in type environment "n" and mode "mode",
 * and returns a pair of 
 * an expression (which is "e" with extra casts where necessary)
 * and the inferred type of "e".
 *
 * The resulting expression satisfies the invariant that if an expression of type "s"
 * is bound to a variable of type "t", then "s" is a subTyp  of "t" - this invariant
 * is ensured via dynamic casts if necessary, both in Strict and Standard mode.
 *
 * The env "n" needs to be applied to the resulting type.
 *)

fun verifyExpr (mode:MODE) (n:TVAL ENV) (e:EXPR) : (EXPR * TYPE) =
    let fun expected ((e,t):(EXPR * TYPE)) : (EXPR * TYPE) = (ExpectedTypeExpr (t,e), t) in
    case e of
        IntExpr n => (e,IntType)
      | VarExpr x => 
        let val ty =
                case lookup n x of
                    (* Don't allow type variables in expressions *)
                    UnknownProgVar t => t
                  | _ => raise IllTypedExpr (e, "unbound variable")
        in (e,ty)
        end
      | LetTypeExpr (x,t,body) => 
        let val _ = verifyType mode n t 
            val n' = extend n x (TypeVar (n,t))
            val (body',bodyTy) = verifyExpr mode n' body
        in
            (LetTypeExpr (x, t, body'), substType bodyTy x t)
        end
      | LetExpr (x,t,e,body) => 
        let val e' = verifyAndCheck mode n e t
            val n' = extend n x (UnknownProgVar t)
            val (body',bodyTy) = verifyExpr mode n' body
        in
            (LetExpr (x, t, e', body'), bodyTy)
        end
      | FunExpr (x,t1,t2,e) => 
        let val _ = verifyType mode n t1
            val e' = verifyAndCheck mode (extend n x (UnknownProgVar t1)) e t2
        in
            (FunExpr(x,t1,t2,e'), FunType(t1,t2))
        end
      | CastExpr (ty,e) =>
        let val _ = verifyType mode n ty
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
            expected (AppExpr( CastExpr( FunType(AnyType,AnyType), e1'),
                                    verifyAndCheck mode n e2 AnyType),
                      AnyType)
          | _ => raise BadFunExpr (e1',ty1)
        end
      | RefExpr (t,e) =>
        (RefExpr (t, verifyAndCheck mode n e t), RefType t)
      | GetExpr e =>
        let val (e',t) = verifyExpr mode n e 
        in
            case t of
                RefType s =>
                expected (GetExpr e', s)
              | AnyType =>
                expected (GetExpr (CastExpr (RefType AnyType, e')),
                          AnyType)
              | _ => raise BadRefExpr (e',t)
        end
      | SetExpr (e1, e2) =>
        let val (e1',t1) = verifyExpr mode n e1
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
      | TypeExpr t =>   (* like  "type(...)" in ES4 *)
        let val _ = verifyType mode n t in
            (TypeExpr t, TypeType)
        end
       | GenericExpr (x,e) =>
         let val n' = extend n x UnknownTypeVar
             val (e',s) = verifyExpr mode n' e
         in
             (GenericExpr (x,e), GenericType (x,s))
         end
       | AppTypeExpr (e,t) =>
         let val (e',s) = verifyExpr mode n e
             val _ = verifyType mode n t
         in
             case s of
                 GenericType (x,s') =>
                 (AppTypeExpr (e',t), substType s' x t)
               | _ => raise IllTypedExpr (e,"Not a generic function")
         end
       | ExpectedTypeExpr _ => raise IllTypedExpr (e, "ExpectedTypeExpr in source")
    end

and verifyAndCheck (mode:MODE) (n:TVAL ENV) (e:EXPR) (t:TYPE) =
    let val _ = verifyType mode n t
        val (e',t') = verifyExpr mode n e in
        check mode n e' t' t
    end

and verifyType (mode:MODE) (n:TVAL ENV) (t:TYPE) : unit =
    let in
        case t of
            AnyType => ()
          | IntType => ()
          | TypeType => ()
          | FunType (s,t) => 
            let in
                verifyType mode n s; 
                verifyType mode n t
            end
          | RefType t => verifyType mode n t
          | VarType x =>
            let in
                case lookup n x of
                    TypeVar _ => ()
                  | UnknownTypeVar => ()
                  | _ => raise InvalidTypeVar x
            end
          | GenericType (x,t) =>
            verifyType mode (extend n x UnknownTypeVar) t
    end

(*********** Tests **********)

fun go (mode:MODE) (e:EXPR) : (EXPR*VAL) =
    let val _ = print ("Verifying  : "^(exprToString e)^"\n")
        val (e',ty) = verifyExpr mode [] e 
        val _ = print ("compiles to: "^(exprToString e')^"\n")
        val _ = print ("with type  : "^(typeToString ty)^"\n")
        val v = eval [] e'
    in 
        (e',v)
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
           RefExpr (IntType, IntExpr 0),
           GetExpr (VarExpr "r"))))

