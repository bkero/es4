/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */


/*
  For option 9 - 3aug07 - Cormac.

  This is an implementation of a toy language to express and clarify a
  number of issues relating to the ES4 type system in a simpler context,
  and may also help to shake out bugs in the type system earlier.

  Now re-implemented in ES4 to gain experience with ES4.
*/

type IDENT = String;
type LABEL = String;

/*** Types ***/
{
    class Type {}

    class AnyType extends Type {}      /* the type "*" */
    let anyType : AnyType = new AnyType();

    class IntType extends Type {}       
    let intType : IntType = new IntType();

    class FunType extends Type {       /* function(arg):res   */
        const arg : TYPE;
        const res : TYPE;
        function FunType (arg: TYPE, res: TYPE) {
            this.arg = arg;
            this.res = res;
        }
    }

    /** following are unused */
    class TypeType extends Type {}     /* the type of types */
    class ObjType extends Type {       /* {l:T,...}  */
        const tfields : [[LABEL,TYPE]];
    }
    class DynObjType extends Type {}   /* {*} */
    class VarType extends Type {       /* a reference to a type variable "X" */
        const x : IDENT;
    }
    class GenericType extends Type {   /* forall X. T, or from "type T.<X> = ..." */
        const x : IDENT;
        const body : TYPE;
    }
    class AppType extends Type {       /* T1.<T2>     */
        const T1 : Type;
        const T2 : Type;
    }
}

/*** Expressions ***/
{
    class Expr {}
    class IntExpr extends Expr {
        const n : int;
        function IntExpr(n: Int) { this.n = n; }
    }
    class VarExpr extends Expr {
        const x : IDENT;
        function VarExpr(x: IDENT) { this.x = x; }
    }
    class FunExpr extends Expr {
        const x : IDENT;
        const argt : Type;
        const rest : Type;
        const body : Expr;
    }
    class AppExpr extends Expr {
        const fn : Expr;
        const arg : Expr;
    }
}

/*** Environments ***/
{
    class Env {
        function extend(x:IDENT,a:*) : Env {
            return new ExtendEnv(x,a,this);
        }
        function lookup(x:IDENT) : * {
            raise ("Unbound variable: "+x);
        } 
    }
    class EmptyEnv extends Env {
    }
    class ExtendEnv extends Env {
        const x:IDENT;
        const a:*;
        const next:Env;

        function ExtendEnv(x:IDENT,a:*,next:Env) {
            this.x = x;
            this.a = a;
            this.next=next;
        }

        function lookup(y:IDENT) : * {
            if (x==y) {
                return a;
            } else {
                return next.lookup(y);
            }
        }
    }

    const emptyEnv : Env = new EmptyEnv();
}

/********** Gensym, for alpha renaming **********/
{
    var gensymCounter : int = 0;
    function gensym (s:String) : String {
        gensymCounter++;
        return s + "$" + gensymCounter;
    }
}

/*********** Substitution on types ***********/

function substType (t:Type, x:string, s:Type) : Type {
    switch type(t) {
        case (t:(AnyType,IntType)) { 
            return t; 
        }
        case (t:FunType) { 
            return new FunType( substType(t.arg, x, s),
                                substType(t.res, x, s) );
        }
        }
}
 
type TYPE_CLOSURE = [ENV, Type];

function normalizeType (tycl:TYPE_CLOSURE) : TYPE_CLOSURE {
    return tycl;
}

/********** Subtyping, compatibility **********/

function compatibleType (tycl1:TYPE_CLOSURE, tycl2:TYPE_CLOSURE) : Boolean {
    let [n1,t1] = normalizeType(tycl1);
    let [n2,t2] = normalizeType(tycl2);
    
    if (type(t1)==AnyType || type(t2)==AnyType) { return true; }

    switch type(t1) {
        case (t1: FunType) {
            switch type(t2) {
                case (t2: FunType) {
                    return compatibleType( [n2, t2.arg], [t1, t1.arg] ) &&
                        compatibleType( [n1, t1.res], [n2, t2.res] );
                }
                case (t2: *) { 
                    return false;
                }
                }
        }
        case (t1: *) {
            return false;
        }
        }
}

function subType (tycl1:TYPE_CLOSURE, tycl2:TYPE_CLOSURE) : Boolean {
    let [n1,t1] = normalizeType tycl1;
    let [n2,t2] = normalizeType tycl2;
    
    if (type(t2)==AnyType) { return true; }

    switch type(t1) {
        case (t1: FunType) {
            switch type(t2) {
                case (t2: FunType) {
                    return subType( [n2, t2.arg], [t1, t1.arg] ) &&
                        subType( [n1, t1.res], [n2, t2.res] );
                }
                case (t2: *) { 
                    return false;
                }
                }
        }
        case (t1: *) {
            return false;
        }
        }
}

/*** String conversions ***/

function valToString (v:VAL) : String {
    return "...";
}

function typeToString (t:TYPE) : String {
    return "...";
}

function envToString (n:ENV) : String {
    return "...";
}

/*********** Run-time values ***********/

type VAL =
    ( int
      , [FunExpr, Env] );

type VALENV = Env;

function typeOfVal (v:VAL) : TYPE_CLOSURE {
    switch type (v) {
        case (v: int) {
            return [emptyEnv, new IntType()];
        }
        case (v: [FunExpr, Env]) {
            return [emptyEnv, 
                    new FunType( v(0).argt, v(0).rest )];
        }
        }
}

function compatibleValue (v:VAL, tycl:TYPE_CLOSURE) : Boolean {
    let [n,t] = normalizeType tycl;
    return subType (typeOfVal(v), [n,t]);
}

function convert (v:VAL, [n,t]:TYPE_CLOSURE) : VAL {
    let [n,t] = normalizeType [n,t];
    if (compatibleValue(v, [n,t])) {
        return v;
    } else {
        print ("val : "+(valToString v)+"\n");
        print ("type: "+(likeToString l)+(typeToString t)+" ENV "+(valEnvToString n)+"\n");
        raise "ConversionError";
    }
}

function eval (n:VALENV, e:EXPR) : VAL {
    print ("   Eval'ing: "+(exprToString e)+" ENV "+(valEnvToString n)+"\n");
    switch type (e) {
        case (e:IntExpr) {
            return e.n;
        }
        case (e:VarExpr) {
            return n.lookup(e.x);
        }
        case (e:FunExpr) {
            return [e,n];
        }
        case (e:AppExpr) {
            switch type (eval(n, e.fn)) {
                case (fnval: [FunExpr, Env]) {
                    let argval = eval( n, e.arg );
                    let argva2l = convert( argval, fnval.argt );
                    let n2 = n.extend(fnval, x,argval2 );
                    let resval = eval( n2, fnval.body );
                    let resval2 = convert( resval, fnval.rest );
                    return resval2;
                }
                }
        }
        }
}

/*********** Tests **********/

function go (e:Expr) {
    print ("Evaluating  : "+(exprToString e)+"\n");
    eval( emptyEnv, e);
}

let idint : Expr = new FunExpr("x",intType,intType,VarExpr "x")
let idany : Expr = new FunExpr("x",anyType,anyType,VarExpr "x")
let idbad : Expr = new FunExpr("x",anyType,intType,VarExpr "x");
let idbad2: Expr = new FunExpr("x",intType,anyType,VarExpr "x");

go (idint);
go (idany);
go (idbad);
go (idbad2);

/*
  (go
  (LetExpr ("f" ,anyType, idint,
  (AppExpr (VarExpr "f",
  IntExpr 4
  )))));

  (go
  (LetTypeExpr ("X", intType,
  LetExpr ("f" ,
  anyType,
  FunExpr ("x", Like, VarType "X", intType,
  VarExpr "x"),
  AppExpr (VarExpr "f",
  IntExpr 4)))));



  (go
  (LetExpr ("polyId" ,
  GenericType ("X", FunType(Like, VarType "X", VarType "X")),
  GenericExpr ("Y",
  FunExpr ("y", Like, VarType "Y", VarType "Y", VarExpr "y"),
  FunType(Like, VarType"Y",VarType "Y")),
  LetTypeExpr ("X", intType,
  AppExpr( AppTypeExpr (VarExpr "polyId", VarType "X"),
  IntExpr 4)))));

  (go
  (LetExpr ("r",
  anyType,
  ObjExpr ([("l",IntExpr 0)], ObjType [("l", intType)]),
  GetExpr (VarExpr "r", "l"))));

  (go
  (LetTypeExpr ("POLYID",
  GenericType ("X",  FunType(Like, VarType "X", VarType "X")),
  LetExpr ("polyId" ,
  VarType "POLYID",
  GenericExpr ("Y",
  FunExpr ("y", Like, VarType "Y", VarType "Y", VarExpr "y"),
  FunType(Like, VarType"Y",VarType "Y")),
  LetTypeExpr ("INT",
  intType,
  LetExpr ("f",
  AppType (VarType "POLYID", VarType "INT"),
  AppTypeExpr (VarExpr "polyId", VarType "INT"),
  AppExpr (VarExpr "f",
  IntExpr 4)))))));



  (print "Done\n\n\n");

*/
