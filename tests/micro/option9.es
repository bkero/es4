/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */

/*
  For option 9 - 3aug07 - Cormac.
  Updated Nov 8 with a notion of units.

  This is a small language implementation to explore the like and wrap types in a simple context,
  and may also help to shake out bugs in the type system earlier.
   
*/

type Ident = string;
type Label = string;

/*** Types  T ::= * | int | T->T | like T | {l:T, ...}
 */
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

type NonLikeType = (AnyType, IntType, FunType, ObjType);

/*** Helper functions for fields ***/

function toStringFields(a:[[Label,*]]) : string {
    let s = "{";
    let prefix = "";
    for(let i in a) {
	//print("a[i]="+a[i]);
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

/*** Expressions 
 *   e ::= n | x | fun(x:T):T {e} | e(e) | let x=e in e | {l:e,...} | e.l | e.l:=e | e wrap T
 */

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
    const x : Ident;
    const t : Type;
    const e : Expr;
    const body : Expr;
    function LetExpr(x, t, e, body) : x=x, t=t, e=e, body=body {}
    function toString() { return "let "+x+":"+t+"="+e+" in "+body; }
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

class WrapExpr extends ExprC {
    const e : Expr;
    const ty : Type;
    function WrapExpr(e,ty) : e=e, ty=ty {}
    function toString() { return "("+e+" wrap "+t+")"; }
}

/** A unit is a list of variable-expression bindings, essentially a letrec.
 *  A program is a sequence of units.
 */

type unit = [[Ident,Expr]];
type program = [unit];

/*** Environments ***/

class Env {
    function extend(x:Ident,a:*) : Env {
        return new ExtendEnv(x,a,this);
    }
    function lookup(x:Ident) : * {
        throw ("Unbound variable: "+x);
    } 
    function toString() { return "mt"; }
}

class EmptyEnv extends Env {}

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

/********** Gensym, for alpha renaming **********/
{
    var gensymCounter : int = 0;

    function gensym (s:string) : string {
        gensymCounter++;
        return s + "$" + gensymCounter;
    }
}

/********** Subtyping, compatibility **********/

function compatibleType (t1:Type, t2:Type) : Boolean {
    print ("compatibleType "+t1+" and "+t2);
    
    if ( t1==t2 || t1==anyType || t2==anyType) { return true; }

    switch type(t2) {
        case (t2: LikeType ) {
            return compatibleType( t1, t2.arg );
        }

        case (t2: *) {
            switch type(t1) {
                case (t1: LikeType) {
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
                                if (!isInFields( t1.tfields, f) ||
				    !bicompatibleType( tf,  
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

function subType (t1:Type, t2:Type) : Boolean {
    print ("subtype "+t1+" and "+t2+"    t1=anytype is "+(t1==anyType));
    
    if (t1==t2 || t2==anyType) { return true; }
    
    switch type(t2) {
        case (t2: LikeType) {
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
                        case (t2: ObjType) {
                            for(let i in t2.tfields) {
                                let [f,tf] = t2.tfields[i];
                                if (!isInFields( t1.tfields, f) || 
                                    !equalType( tf,  
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

function valIsType( v:Val, t:Type ) : Boolean {
    let tv = typeOfVal(v);
    print ("valIsType "+v+" of type "+tv+" to type "+t);
    if (subType( tv, t )) {
        print (""+tv+" is a subtype of "+t);
        return true;
    }
    print("Not subtype");
    
    switch type(t) {
        case (t:LikeType) {
            switch type (v) {
                case (vty:ObjVal) {
                    print ("ObjVal");
                    switch type (t.arg) {
                        case (targ:ObjType) {
                            for(let i in targ.tfields) {
                                let [f,tf] = targ.tfields[i];
                                print ("valIsType "+v
                                       +" of type "+tv+" to type "+t+" field "+f);
                                if (!isInFields( v.vfields, f) ||
				    !valIsType( findInFields( v.vfields, f), tf )) {
                                    return false;
                                }
                            }
                        }
                        }
                }
                case (vty:Type) {
                    if (!compatibleType( tv, targ )) {
                        print ("valIsType "+v+" of type "+tv
                               +" to type "+t+" incompatible types");
                        return false;
                    }
                    print ("valIsType "+v+" of type "+tv
                           +" to type "+t+" compatible types");
                    return true;
                }
                }
        }
        case (t:Type) {
            return false;
        }
        }
    
    return true;
}        

function checkValIsType(v:Val, t:Type) {
    if( valIsType(v,t) ) return;
    throw "Value "+v+" does not match expected type "+t;
}

function wrap (v:Val, t:Type) : Val {
    let tv = typeOfVal(v);
    print ("Wrapping value "+v+" of type "+tv+" to type "+t);
    if (subType(tv,t)) {
	print("no wrap necessary");
        return v;
    } else {
	print("wrapping");
        return new WrapVal(v,t);
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
        case (e:AppExpr) {
            function apply( fn:Val, arg:Val ) : Val {
                switch type (fn) {
                    case (closure: ClosureVal) {
                        let { fn:fn, env:env } = closure;
                        checkValIsType( arg, fn.argt );
                        let bodyEnv = env.extend( fn.x, arg );
                        let res = eval( bodyEnv, fn.body );
                        checkValIsType( res, fn.rest );
                        return res;
                    }
                    case (w: WrapVal) {
                        return wrap( apply( w.v, 
                                            wrap( arg, w.ty.arg )),
                                     w.ty.res );
                    }
                    }
            }
            return apply( eval( n, e.fn ),
                          eval( n, e.arg ));
        }
        case (e:LetExpr) {
            let {x:x, t:t, e:e2, body:body} = e;
            let argval = eval( n, e2 );
	    checkValIsType( argval, t);
            let n2 = n.extend( x, argval );
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
		let v:Val = eval( n, efields[i][1] );
		checkValIsType( v, tyl );
                vfields[i] = [l,v];
            }
            // make sure all fields in type defined
	    print("done");
            for(i in ty.tfields) {
		print("i="+i+" "+ty.tfields);
                findInFields( efields, ty.tfields[i][0] );
            }
	    print("done");
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
                        return wrap( get(obj, l),
                                        findInFields( objTy.tfields, l ));
                    }
                    }
            }
            return get( eval( n, e.e ), e.l );
        }
        case (e : SetExpr) {
            function set( o:Val, l:Label, to:Val ) : Val {
		print("set: "+o);
                switch type (o) {
                    case (o: ObjVal) {
                        // write barrier
                        if (isInFields( o.ty.tfields, l )) {
                            let tyf = findInFields( o.ty.tfields, l );
			    checkValIsType( to, tyf );
                        }
                        setInFields( o.vfields, l, to);
                        return to; // FIXME: which return type:
                    }
                    case (o: WrapVal) {
                        let {v:obj, ty:objTy} = o;
                        return set( obj, l,
                                    wrap( to, findInFields( objTy.tfields, l )));
                    }
                    }
            }
            return set( eval( n, e.e1 ), 
                        e.l, 
                        eval( n, e.e2 ) );
        }
        }   
}


let topLevelEnv : Env = emptyEnv;

class TopLevelEnv extends Env {

    override function lookup(x:Ident) : * {

        return topLevelEnv.lookup(x);

    }
}

function evalProg (p:program) : * {
    print("Evaluating program: "+p);
    for (let i in p) {
	print("Evaluating unit: "+p[i]);
	for (let j in p[i]) {
            let [x,e] = p[i][j];
	    print("Evaluating "+x+"="+e);
            let v : Val = eval(new TopLevelEnv(), e);
	    print("Binding "+x+" to "+v);
            topLevelEnv = topLevelEnv.extend(x,v);
	}
    }
    

}

/************ Optional Verifier **************/

type TypeEnv = Env; // maps vars to Types

function stripLike(t:Type) : NonLikeType {
    switch type (t) {
        case (t:LikeType) {
            return stripLike(t.arg);
        }
        case (t:NonLikeType) {
            return t;
        }
        }
}

function verify (n:TypeEnv, e:Expr) : NonLikeType {
/*
    switch type (e) {
        case (e:int) {
            return intType;
        }
        case (e:Ident) {
            return n.lookup(e);
        }
        case (e:FunExpr) {
            let xtype : Type;
            switch type (e.argt) {
                case (argt: WrapType) {
                    xtype = argt.arg;
                }
                case (argt: Type) {
                    xtype = argt;
                }
                }
            let t : Type = verify ( n.extend( e.x, xtype ),
                                    e.body );
            if (!subType( t, e.rest ))
                throw ("Body type "+t+" not a subtype of expected return type "+e.rest);
            return new FunType( argt, e.rest );
        }
        case (e:AppExpr) {
            let tfn : Type = verify( n, e.fn );
            let targ : Type = verify( n, e.arg );
            function apply( fn:Val, arg:Val ) : Val {
                switch type (fn) {
                    case (closure: ClosureVal) {
                        let { fn:fn, env:env } = closure;
                        let bodyEnv = env.extend( fn.x, wrap( arg, fn.argt ) );
                        let res = eval( bodyEnv, fn.body );
                        return wrap( res, fn.rest );
                    }
                    case (w: WrapVal) {
                        return wrap( apply( w.v, 
                                            wrap( arg, w.ty.arg )),
                                     w.ty.res );
                    }
                    }
            }
            return apply( eval( n, e.fn ),
                          eval( n, e.arg ));
        }
        case (e:LetExpr) {
            let {x:x, t:t, e:e2, body:body} = e;
            let argval = eval( n, e2 );
            checkValIsType( argval, t );
            let n2 = n.extend( x, argval );
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
                let vi : Val = eval( n, efields[i][1] );
                checkValInType( vi, tyl );
                vfields[i] = [ l, vi ];
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
                        return wrap( get(obj, l),
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
                            checkValIsType( to, tyf );
                        }
                        setInFields( o.vfields, l, to);
                        return to; // FIXME: which return type:
                    }
                    case (o: WrapVal) {
                        let {v:obj, ty:objTy} = o;
                        return set( obj, l,
                                    wrap( to, findInFields( objTy.tfields, l )));
                    }
                    }
            }
            return set( eval( n, e.e1 ), 
                        e.l, 
                        eval( n, e.e2 ));
        }
        }   
*/
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
testCall( idint, anyanyfn );
*/

testCall( idint, new LikeType(intintfn));
testCall( idany, new LikeType(intintfn));
testCall( idany, new LikeType(anyanyfn));
testCall( idint, new LikeType(anyanyfn));

// testCall( idint, new WrapType(intintfn));
// testCall( idany, new WrapType(intintfn));
// testCall( idany, new WrapType(anyanyfn));
// testCall( idint, new WrapType(anyanyfn));

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

evalProg( [[["x",1]]] );
evalProg( [[["x",1],["y","x"]]] );

/*
testGet( mtObjType, new WrapType(mtObjType));
testGet( fint,      new WrapType(fint));
testGet( fint,      new WrapType(mtObjType));
testGet( mtObjType, new WrapType(fint));

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

  (go (LetTypeExpr ("POLYID", GenericType ("X", FunType(Like, VarType
  "X", VarType "X")), LetExpr ("polyId" , VarType "POLYID",
  GenericExpr ("Y", FunExpr ("y", Like, VarType "Y", VarType "Y",
  VarExpr "y"), FunType(Like, VarType"Y",VarType "Y")), LetTypeExpr
  ("INT", intType, LetExpr ("f", AppType (VarType "POLYID", VarType
  "INT"), AppTypeExpr (VarExpr "polyId", VarType "INT"), AppExpr
  (VarExpr "f", IntExpr 4)))))));



  (print "Done\n\n\n");

*/
