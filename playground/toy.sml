(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)

(* This is an implementation of a toy language to express and clarify
 * a number of issues relating to the ES4 type system in a simpler
 * context, and may also help to shake out bugs in the type system earlier. 
 *)

exception UnboundVariable of string
exception NotAClosure of string

datatype TYPE =
	 AnyType
       | IntType
       | FunType of TYPE * TYPE

fun subtype (t1:TYPE) (t2:TYPE) : bool =
    case (t1,t2) of
	(AnyType,AnyType) => true
      | (IntType,IntType) => true
      | (FunType(s1,t1), FunType(s2,t2)) =>
	(subtype s2 s1) andalso (subtype t1 t2)

fun compatible (t1:TYPE) (t2:TYPE) : bool =
    case (t1,t2) of
	(_,AnyType) => true
      | (AnyType,_) => true
      | (IntType,IntType) => true
      | (FunType(s1,t1), FunType(s2,t2)) =>
	(compatible s2 s1) andalso (compatible t1 t2)

datatype EXPR =
	 IntExpr of int
       | FunExpr of string * TYPE * EXPR
       | AppExpr of EXPR * EXPR
       | VarExpr of string
       | CastExpr of TYPE * EXPR

datatype VAL =
	 IntVal of int
       | ClosureVal of string * TYPE * EXPR * ((string * VAL) list)

type ENV = (string * VAL) list

fun extend (env:ENV) (x:string) (v:VAL) : ENV =
    (x,v)::env

fun lookup (env:ENV) (x:string) : VAL =
    case env of
	[] => raise UnboundVariable x
      | (y,v)::r => if (x=y) then v else lookup r x

fun eval (n:ENV) (e:EXPR) : VAL =
    case e of
	IntExpr n => IntVal n
      | FunExpr (x,t,e) => ClosureVal (x,t,e,n)
      | AppExpr (e1,e2) =>
	let in
	    case (eval n e1) of
		ClosureVal (x,ty,body,n2) =>
		eval (extend n2 x (eval n e2)) body
	      | _ => raise NotAClosure ""
	end
      | VarExpr x => lookup n x
      | 
