/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */


/*
  For option 9 - 3aug07 - Cormac.

  This is an implementation of a toy language to express and clarify a
  number of issues relating to the ES4 type system in a simpler context,
  and may also help to shake out bugs in the type system earlier.

  Now re-implemented in ES4 to gain experience with ES4.
*/

type Ident = String;
type Label = String;

/*** Types ***/
{
    class Type {} 

    class AnyType extends Type {      /* the type "*" */
        function toString() { return "*";}
    }

    class IntType extends Type {
        function toString() { return "int"; }
    }

    class DynObjType extends Type {   /* {*} */
        function toString() { return "{*}"; }
    }

    var anyType    : AnyType    = new AnyType();
    var intType    : IntType    = new IntType();
    var dynObjType : DynObjType = new DynObjType();

    class FunType extends Type {       /* function(arg):res   */
        const arg, res : Type;
        function FunType (arg, res) : arg = arg, res = res {} 
        function toString() { return "(function("+arg+"):"+res+")"; }
    }

    class LikeType extends Type {
        const arg : Type;
        function LikeType (arg) : arg = arg {} 
        function toString() { return "like "+arg; }
    }

    class WrapType extends Type {
        const arg : Type;
        function WrapType (arg) : arg = arg {} 
        function toString() { return "wrap "+arg; }
    }

    class ObjType extends Type {       /* {l:T,...}  */
        const tfields : [[Label,Type]];
        function ObjType(tfields) : tfields = tfields {};
        function toString() {
            return toStringFields(this.tfields);
        }
    }

    function toStringFields(a:[[Label,*]]) : String {
        let s = "{";
        let prefix = "";
        for(i in a) {
            s = s + prefix + a[i][0] + ":" + a[i][1];
            prefix = ",";
        }
        return s+"}";
    }

    function findInFields( a:[[Label,*]], f : Label ) : * {
        for(let i in a) {
            if( a[i][0] == f ) {
                return a[i][1];
            }
        }
        throw "Field "+f+" not found.";
    }

    /** following are unused */
    class TypeType {}     /* the type of types */
    class VarType {       /* a reference to a type variable "X" */
        const x : Ident;
    }
    class GenericType {   /* forall X. T, or from "type T.<X> = ..." */
        const x : Ident;
        const body : Type;
    }
    class AppType {       /* T1.<T2>     */
        const T1 : Type;
        const T2 : Type;
    }
}

/*** Expressions ***/
{
    type Expr =
        ( int, 
          Ident,
          FunExpr,
          AppExpr,
          LetExpr,
          ObjExpr );

    class FunExpr {
        const x : Ident;
        const argt : Type;
        const rest : Type;
        const body : Expr;
        function FunExpr(x, argt, rest, body) : 
            x = x, argt = argt, rest = rest, body = body
            {}
        function toString() {
            return "function("+x+":"+argt+"):"+rest+" { "+body+" }";
        }
    }

    class AppExpr {
        const fn : Expr;
        const arg : Expr;
        function AppExpr(fn, arg) : fn = fn, arg = arg {} 
        function toString() { return ""+(fn)+"("+arg+")"; }
    }

    class LetExpr {
        const x : String;
        const t : Type;
        const e : Expr;
        const body : Expr;
        function LetExpr(x, t, e, body) : x=x, t=t, e=e, body=body {}
        function toString() { return "let "+x+":"+t+"="+e+" in "+body; }
    }

    class LikeExpr {
        const arg : Expr;
        const ty : Type;
        function LikeExpr(arg, ty) : arg = arg, ty = ty {} 
        function toString() { return ""+arg+" like "+ty; }
    }

    class WrapExpr {
        const arg : Expr;
        const ty : Type;
        function WrapExpr(arg, ty) : arg = arg, ty = ty {} 
        function toString() { return ""+arg+" wrap "+ty; }
    }

    class ObjExpr { // {l:e, ...}:T
        const efields : [[Label,Expr]];
        const ty : Type;
        function ObjExpr(efields,ty) : efields = efields, ty = ty {}
        function toString() { return toStringFields(efields)+":"+ty; }
    }

    class GetExpr { // e.l
        const e : Expr;
        const l : Label;
        function GetExpr(e,l) : e=e, l=l {}
        function toString() { return ""+e+"."+l; }
    }

    class SetExpr { // e1.l = e2;
        const e1,e2 : Expr;
        const l : Label;
        function SetExpr(e1,l,e2) : e1=e1, l=l, e2=e2 {}
        function toString() { return ""+e1+"."+l+"="+e2; }
    }

    /*
       | TypeExpr of TYPE                       (* like  "type(...)" in ES4 *)
       | LetTypeExpr of string * TYPE * EXPR    (* like "type X = ..." in ES4 *)
       | GenericExpr of string * EXPR * TYPE    (* like function.<X>():T {e} in ES4 *)
       | AppTypeExpr of EXPR * TYPE             (* like  e.<T>  in ES4, e[T] in TAPL *)
    */
}

/*** Environments ***/
{
    class Env {
        function extend(x:Ident,a:*) : Env {
            return new ExtendEnv(x,a,this);
        }
        function lookup(x:Ident) : * {
            throw ("Unbound variable: "+x);
        } 
        function toString() { return "mt"; }
    }

    class EmptyEnv extends Env {
    }

    class ExtendEnv extends Env {
        const x:Ident;
        const a:*;
        const next:Env;

        function ExtendEnv(x:Ident,a:*,next:Env) : x=x, a=a, next=next {}

        override function lookup(y:Ident) : * {
            if (x==y) {
                return a;
            } else {
                return next.lookup(y);
            }
        }
        override function toString() {
            return ""+x+"=" + a+",";
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

/********** Subtyping, compatibility **********/

function compatibleType (t1:Type, t2:Type) : Boolean {
    
    if ( t1==t2 || t1==anyType || t2==anyType) { return true; }

    switch type(t2) {
        case (t2: (LikeType,WrapType) ) {
            return compatibleType( t1, t2.arg );
        }

        case (t2: *) {
            switch type(t1) {
                case (t1: (LikeType, WrapType)) {
                    return compatibleType( t1.arg, t2 );
                }
                case (t1: FunType) {
                    switch type(t2) {
                        case (t2: FunType) {
                            return compatibleType( t2.arg, t1.arg ) &&
                                   compatibleType( t1.res, t2.res );
                        }
                        }
                }
                case (t1: ObjType) {
                    switch type (t2) {
                        case (t2:DynObjType) {
                            return true;
                        }
                        case (t2: ObjType) {
                            for(let i in t2.tfields) {
                                let [f,tf] = t2.tfields[i];
                                if (!bicompatibleType( tf,  
                                                       findInFields( t1.tfields, f))) {
                                    return false;
                                }
                            }
                            return true;
                        }
                        }
                }
                case (t1: DynObjType) {
                    switch type (t2) {
                        case (t2: ObjType) {
                            return true;
                        }
                        }
                }
                }
        }
        }
    return false;
}

function bicompatibleType (t1:Type, t2:Type) : Boolean {
    return compatibleType( t1, t2 ) &&
           compatibleType( t2, t1 );
}

function subType (t1:Type, t2:Type) : Boolean {
    
    if (t1==t2 || t2==anyType) { return true; }
    
    switch type(t2) {
        case (t2: (LikeType, WrapType) ) {
            return compatibleType( t1, t2.arg );
        }
        
        case (t2: *) {
            switch type(t1) {
                case (t1: FunType) {
                    switch type(t2) {
                        case (t2: FunType) {
                            return subType( t2.arg, t1.arg ) &&
                                   subType( t1.res, t2.res );
                        }
                        }
                }
                case (t1: ObjType) {
                    switch type (t2) {
                        case (t2: DynObjType) {
                            return true;
                        }
                        case (t2: ObjType) {
                            for(let i in t2.tfields) {
                                let [f,tf] = t2.tfields[i];
                                if (!equalType( tf,  
                                                findInFields( t1.tfields, f))) {
                                    return false;
                                }
                            }
                            return true; // all fields match
                        }
                    }
                }
                }
        }
        }
    return false;
}

/*********** Run-time values ***********/

type Val =
    ( WrapableVal,
      Wrapper );

type WrapableVal = // something that could be wrapped
    ( int,
      Closure,
      ObjVal
      );

type Closure = [FunExpr, Env];
type Wrapper = [WrapableVal, Type];
type ValEnv = Env;

class ObjVal {
    const vfields : [[Label, Val]];
    const ty : Type;
    function ObjVal(vfields, ty) : vfields = vfields, ty = ty {}
    function toString() {
        return toStringFields(vfields) + ":"+ty;
    }
}

/** Linking Values and their Types **/

function typeOfVal (v:Val) : Type {
    switch type (v) {
        case (v: int) {
            return intType;
        }
        case ([fn, n]: [FunExpr, Env]) {
            // let [fn, n] = v;
            return new FunType(fn.argt, fn.rest);
        }
        case ([cl, t]: [WrapableVal, Type]) {
            return t;
        }
        case (v:ObjVal) {
            return v.ty;
        }
        }
}

function compatibleValue( v:Val, t:Type ) : Boolean {
    let tv = typeOfVal(v);
    print ("compatibleValue "+v+" of type "+tv+" to type "+t);
    if (subType( tv, t )) return true;
    if (!compatibleType( tv, t )) return false;

    // compatible but not subtype
    // do deep check ...

    switch type (v) {
        case (v:ObjVal) {
            switch type (t) {
                case (t:ObjType) {
                    for(let i in t.tfields) {
                        let [f,tf] = t.tfields[i];
                        print ("compatibleValue "+v+" of type "+tv+" to type "+t+" field "+f);
                        if (!compatibleValue( findInFields( v.vfields, f), tf )) {
                            return false;
                        }
                    }
                }
                }
        }
        case (v:Wrapper) {
            let [v,vt] = v;
            return compatibleValue( v. t );
        }
        }
    
    return true;
}        

function convert (v:Val, t:Type) : Val {
    let tv = typeOfVal(v);
    print ("Converting value "+v+" of type "+tv+" to type "+t);

    switch type(t) {
        case (t:LikeType) {
            if (compatibleValue(v, t.arg)) {
                return v;
            } else {
                throw "Cannot convert1 value "+v+" to type "+t;
            }
        }
        case (t:WrapType) {
            if (subType(tv, t.arg)) {
                return v;
            }  if (compatibleValue(v, t.arg)) {
                // remove old wrapper, if any, and re-wrap
                switch type (v) {
                    case (v:Wrapper) {
                        let [w,tw] = v;
                        return [w, t.arg];
                    }
                    case (v:Val) {
                        return [v, t.arg];
                    }
                    }
            } else {
                throw "Cannot convert2 value "+v+" to type "+t;
            }
        }
        case (t:Type) {
            if (subType (tv, t)) {
                return v;
            } else {
                print ("val : "+(v));
                print ("type: "+(t));
                throw "Cannot convert3 value "+(v)+" to type "+(t);
            }
        }
        }
}

function eval (n:ValEnv, e:Expr) : Val {
    //print ("  Eval'ing: ");
    //print ("  Eval'ing: "+(e));
    print ("  Eval'ing: "+(e)+" ENV "+n);
    let r = eval2(n,e);
    print ("    Result: "+(r));
    return r;
}

function eval2 (n:ValEnv, e:Expr) : Val {
    switch type (e) {
        case (e:int) {
            return e;
        }
        case (e:Ident) {
            return n.lookup(e);
        }
        case (e:FunExpr) {
            return [e,n];
        }
        case (e:LikeExpr) {
            let v = eval( n, e.arg );
            if (compatibleValue( v,  e.ty )) {
                return 1;
            } else {
                return 0;
            }
        }
        case (e:WrapExpr) {
            let v = eval( n, e.arg );
            return convertValue( v, new WrapType(e.ty) );
        }
                    
        case (e:AppExpr) {
            switch type (eval(n, e.fn)) {
                case (closure: [FunExpr, Env]) {
                    let [fn,n2] = closure;
                    let argval = eval( n, e.arg );
                    let argval2 = convert( argval, fn.argt );
                    let n3 = n2.extend(fn.x, argval2 );
                    let resval = eval( n3, fn.body );
                    let resval2 = convert( resval, fn.rest );
                    return resval2;
                }
                }
        }
        case (e:LetExpr) {
            let {x:x, t:t, e:e2, body:body} = e;
            let argval = eval( n, e2 );
            let argval2 = convert( argval, t );
            let n2 = n.extend( x, argval2 );
            let resval = eval( n2, body );
            return resval;
        }
        case (e:ObjExpr) {
            let vfields = [];
            for(i in e.efields) {
                vfields[i] = [ e.efields[i][0],
                               eval( n, e.efields[i][1]) ];
            }
            return new ObjVal(vfields, e.ty);
        }
        }   
}

/*********** Tests **********/

function go (e:Expr) {
    print ("Evaluating: ");
    print ("Evaluating: "+e);
    let r = eval( emptyEnv, e);
    print ("Result    : "+r);

}

let idint : Expr = new FunExpr("x", intType, intType, "x");
let idany : Expr = new FunExpr("x", anyType, anyType, "x");
let idbad : Expr = new FunExpr("x", anyType, intType, "x");
let idbad2: Expr = new FunExpr("x", intType, anyType, "x");

let intintfn : Type = new FunType( intType, intType );
let anyanyfn : Type = new FunType( anyType, anyType );

/*
(go
 (new LetExpr ("f" ,anyType, idint,
               (new AppExpr ("f",4 )))));

(go
 (new LetExpr ("f" , new LikeType(intintfn), idany,
               (new AppExpr ("f",4 )))));

*/

/* Error:
(go
 (new LetExpr ("f" , anyanyfn, idint,
               (new AppExpr ("f",4 )))));

(go
    (new LetExpr("x",
                 fint,
                 new ObjExpr([["f", 4]], dynObjType),
                 "x")));

*/

let fint : Type = new ObjType([["f",intType]]);

(go
    (new LetExpr("x",
                 new LikeType(fint),
                 new ObjExpr([["f", 4]], dynObjType),
                 "x")));

/*
go (3);
go (idint);
go (new AppExpr(idint,3));

go (idany);
go (idbad);
go (idbad2);



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
