/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */


/*
  For option 9 - 3aug07 - Cormac.

  This is a small language implementation to explore the like and wrap types in a simple context,
  and may also help to shake out bugs in the type system earlier.
  Now re-implemented in ES4 to gain experience with ES4.

  Conclusions:
  
  - It appears that function(int):int <: function(*):*
    in analogy to {f:int} <: {*}

  - Wrap types are recursive

  - No unwrap option (yet)

  - No strict mode (yet)

  - All checks done dynamically, no optimizations explored.
   
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

    var anyType    : AnyType    = new AnyType();
    var intType    : IntType    = new IntType();
    var mtObjType  : ObjType    = new ObjType([]);
}

/*** Helper functions for fields ***/

function toStringFields(a:[[Label,*]]) : String {
    let s = "{";
    let prefix = "";
    for(i in a) {
        s = s + prefix + a[i][0] + ":" + a[i][1];
        prefix = ",";
    }
    return s+"}";
}

function isInFields( a:[[Label,*]], f : Label ) : Boolean {
    for(let i in a) {
        if( a[i][0] == f ) {
            return true;
        }
    }
    return false;
}

function findInFields( a:[[Label,*]], f : Label ) : * {
    for(let i in a) {
        if( a[i][0] == f ) {
            return a[i][1];
        }
    }
    throw "Field "+f+" not found in ["+a+"]";
}

function setInFields( a:[[Label,*]], f : Label, to:* ) {
    for(let i in a) {
        if( a[i][0] == f ) {
            a[i][1] = to;
            return;
        }
    }
    throw "Field "+f+" not found.";
}

/*** Expressions ***/
{
    type Expr =
        ( int, 
          Ident,
          ExprC );

    class ExprC {}

    class FunExpr extends ExprC {
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

    class AppExpr extends ExprC {
        const fn : Expr;
        const arg : Expr;
        function AppExpr(fn, arg) : fn = fn, arg = arg {} 
        function toString() { return ""+(fn)+"("+arg+")"; }
    }

    class LetExpr extends ExprC {
        const x : String;
        const t : Type;
        const e : Expr;
        const body : Expr;
        function LetExpr(x, t, e, body) : x=x, t=t, e=e, body=body {}
        function toString() { return "let "+x+":"+t+"="+e+" in "+body; }
    }

    class LikeExpr extends ExprC {
        const arg : Expr;
        const ty : Type;
        function LikeExpr(arg, ty) : arg = arg, ty = ty {} 
        function toString() { return ""+arg+" like "+ty; }
    }

    class ObjExpr extends ExprC { // {l:e, ...}:T
        const efields : [[Label,Expr]];
        const ty : ObjType;
        function ObjExpr(efields,ty) : efields = efields, ty = ty {}
        function toString() { return toStringFields(efields)+":"+ty; }
    }

    class GetExpr extends ExprC { // e.l
        const e : Expr;
        const l : Label;
        function GetExpr(e,l) : e=e, l=l {}
        function toString() { return ""+e+"."+l; }
    }

    class SetExpr extends ExprC { // e1.l = e2;
        const e1,e2 : Expr;
        const l : Label;
        function SetExpr(e1,l,e2) : e1=e1, l=l, e2=e2 {}
        function toString() { return ""+e1+"."+l+":="+e2; }
    }
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

// FIXME: not complete yet!
function compatibleType (t1:Type, t2:Type) : Boolean {
    print ("compatibleType "+t1+" and "+t2);
    
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
                }
        }
        }
    return false;
}

function bicompatibleType (t1:Type, t2:Type) : Boolean {
    return compatibleType( t1, t2 ) &&
           compatibleType( t2, t1 );
}

// FIXME: not complete yet
function subType (t1:Type, t2:Type) : Boolean {
    print ("subtype "+t1+" and "+t2+"    t1=anytype is "+(t1==anyType));
    
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
                            return (t2.arg==anyType || subType( t2.arg, t1.arg )) &&
                                   subType( t1.res, t2.res );
                        }
                        }
                }
                case (t1: ObjType) {
                    switch type (t2) {
                        case (t2: ObjType) {
                            for(let i in t2.tfields) {
                                let [f,tf] = t2.tfields[i];
                                if (!isInFields( t1.tfields, f)) return false;
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

function equalType (t1:Type, t2:Type) : Boolean {
    return subType( t1, t2 ) &&
           subType( t2, t1 );
}

/*********** Run-time values ***********/

type Val = 
    ( int,
      ClosureVal,
      ObjVal,
      WrapVal
      );

type ValEnv = Env;

class ObjVal {
    const vfields : [[Label, Val]];
    const ty : ObjType;
    function ObjVal(vfields, ty) : vfields = vfields, ty = ty {}
    function toString() {
        return toStringFields(vfields) + ":"+ty;
    }
}

class WrapVal {   // typeof(v) compatible with ty but not a subtype
    const v : Val;
    const ty : Type;
    function WrapVal(v,ty) : v=v, ty=ty {}
    function toString() { return "("+v+" wrap "+ty+")"; }
}

class ClosureVal {   // typeof(v) compatible with ty but not a subtype
    const fn : FunExpr;
    const env : ValEnv;
    function ClosureVal(fn,env) : fn=fn, env=env {}
    function toString() { return "("+fn+" env "+env+")"; }
}

/** Linking Values and their Types **/

function typeOfVal (v:Val) : Type {
    switch type (v) {
        case (v: int) {
            return intType;
        }
        case (v: ClosureVal) {
            return new FunType(v.fn.argt, v.fn.rest);
        }
        case (v: WrapVal) {
            return v.ty;
        }
        case (v:ObjVal) {
            return v.ty;
        }
        }
}

function compatibleValue( v:Val, t:Type ) : Boolean {
    let tv = typeOfVal(v);
    print ("compatibleValue "+v+" of type "+tv+" to type "+t);
    if (subType( tv, t )) {
        print (""+tv+" is a subtype of "+t);
        return true;
    }
    print("Not subtype");

    switch type (v) {
        case (v:ObjVal) {
            print ("ObjVal");
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
        case (v:Type) {
            if (!compatibleType( tv, t )) {
                print ("compatibleValue "+v+" of type "+tv+" to type "+t+" incompatible types");
                return false;
            }
            print ("compatibleValue "+v+" of type "+tv+" to type "+t+" compatible types");
            return true;
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
                print ("Converting value "+v+" of type "+tv+" to type "+t+" succeeds w/o change");
                return v;
            } else {
                throw "Cannot convert1 value "+v+" to type "+t;
            }
        }
        case (t:WrapType) {
            if (subType(tv, t.arg)) {
                print ("Converting value "+v+" of type "+tv+" to type "+t+" succeeds w/o change");
                return v;
            } else if (compatibleValue(v, t.arg)) {
                return new WrapVal( v, t.arg ); 
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

var indent = "  ";

function eval (n:ValEnv, e:Expr) : Val {
    //print ("  Eval'ing: ");
    //print ("  Eval'ing: "+(e));
    print (indent+"Eval'ing: "+(e)+" ENV "+n);
    let oldIndent = indent;
    indent = indent + "    ";
    let r = eval2(n,e);
    indent = oldIndent;
    print (indent+"Result  : "+(r));
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
            return new ClosureVal(e,n);
        }
        case (e:LikeExpr) {
            let v = eval( n, e.arg );
            if (compatibleValue( v,  e.ty )) {
                return 1;
            } else {
                return 0;
            }
        }
        case (e:AppExpr) {
            function apply( fn:Val, arg:Val ) : Val {
                switch type (fn) {
                    case (closure: ClosureVal) {
                        let { fn:fn, env:env } = closure;
                        let bodyEnv = env.extend( fn.x, convert( arg, fn.argt ) );
                        let res = eval( bodyEnv, fn.body );
                        return convert( res, fn.rest );
                    }
                    case (wrap: WrapVal) {
                        return convert( apply( wrap.v, 
                                               convert( arg, wrap.ty.arg )),
                                        wrap.ty.res );
                    }
                    }
            }
            return apply( eval( n, e.fn ),
                          eval( n, e.arg ));
        }
        case (e:LetExpr) {
            let {x:x, t:t, e:e2, body:body} = e;
            let argval = eval( n, e2 );
            let argval2 = convert( argval, t );
            let n2 = n.extend( x, argval2 );
            let resval = eval( n2, body );
            return resval;
        }
        case ( e : ObjExpr) {
            let {efields:efields, ty:ty} = e;
            let vfields = [];
            for(i in e.efields) {
                let l:Label = efields[i][0];
                let tyl : Type = anyType;
                if (isInFields(ty.tfields,l)) {
                    tyl = findInFields( ty.tfields, l );
                }
                vfields[i] = [ l,
                               convert( eval( n, efields[i][1] ),
                                        tyl )];
            }
            // make sure all fields in type defined
            for(i in ty.tfields) {
                findInFields( efields, ty.tfields[i][0] );
            }
            return new ObjVal(vfields, ty);
        }
        case (e : GetExpr) {
            function get( o:Val, l:Label ) : Val {
                switch type (o) {
                    case (o: ObjVal) {
                        // no read barrier
                        return findInFields( o.vfields, l );
                    }
                    case (o: WrapVal) {
                        let {v:obj, ty:objTy} = o;
                        return convert( get(obj, l),
                                        findInFields( objTy.tfields, l ));
                    }
                    }
            }
            return get( eval( n, e.e ), e.l );
        }
        case (e : SetExpr) {
            function set( o:Val, l:Label, to:Val ) : Val {
                switch type (o) {
                    case (o: ObjVal) {
                        // write barrier
                        if (isInFields( o.ty.tfields, l )) {
                            let tyf = findInFields( o.ty.tfields, l );
                            to = convert( to, tyf );
                        }
                        setInFields( o.vfields, l, to);
                        return to; // FIXME: which return type:
                    }
                    case (o: WrapVal) {
                        let {v:obj, ty:objTy} = o;
                        return set( obj, l,
                                    convert( to, findInFields( objTy.tfields, l )));
                    }
                    }
            }
            return set( eval( n, e.e1 ), 
                        e.l, 
                        eval( n, e.e2 ));
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


go (3);
go (idint);
go (new AppExpr(idint,3));

go (idany);
go (idbad);
go (idbad2);

function testCall( fn, fnty ) {
    go(new LetExpr("f", fnty, fn, new AppExpr ("f",4 )));
}

testCall( idint, intintfn );
testCall( idint, anyType  );

/* Following errors are correctly detected:
testCall( idany, intintfn );
*/
testCall( idint, anyanyfn ); // should this work???

testCall( idint, new LikeType(intintfn));
testCall( idany, new LikeType(intintfn));
testCall( idany, new LikeType(anyanyfn));
testCall( idint, new LikeType(anyanyfn));

testCall( idint, new WrapType(intintfn));
testCall( idany, new WrapType(intintfn));
testCall( idany, new WrapType(anyanyfn));
testCall( idint, new WrapType(anyanyfn));

/*
(go
    (new LetExpr("x",
                 fint,
                 new ObjExpr([["f", 4]], mtObjType),
                 "x")));
*/

let fint : Type = new ObjType([["f",intType]]);
let feint : Type = new ObjType([["f",intType],["extra",intType]]);

function testGet( tyo, tyv ) {
    go(new LetExpr("x",
                   tyv,
                   new ObjExpr([["f", 4], ["extra", 20]], tyo),
                   new LetExpr("dummy", 
                               intType,
                               new SetExpr("x", "f", 5),
                               new GetExpr("x", "f") )));
}

testGet( mtObjType, mtObjType );
testGet( fint,      fint );
testGet( feint,     fint );
testGet( feint,     feint );
// error: testGet( fint,       feint );
testGet( fint,       anyType );

/* Following error correctly detected:
testGet( mtObjType, fint );
*/

testGet( mtObjType, new LikeType(mtObjType));
testGet( fint,      new LikeType(fint));
testGet( fint,      new LikeType(mtObjType));
testGet( mtObjType, new LikeType(fint));

testGet( mtObjType, new WrapType(mtObjType));
testGet( fint,      new WrapType(fint));
testGet( fint,      new WrapType(mtObjType));
testGet( mtObjType, new WrapType(fint));

/*
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
