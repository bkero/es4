(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Verify = struct

open LogErr

type RIB = Ast.FIXTURES

type ENV = { returnType: Ast.TYPE_EXPR option,
             strict: bool,
             ribs: RIB list }

fun withReturnType { returnType=_, strict, ribs } returnType =
    { returnType=returnType, strict=strict, ribs=ribs }

fun withRibs { returnType, strict, ribs=_ } ribs =
    { returnType=returnType, strict=strict, ribs=ribs }

fun withStrict { returnType, strict=_, ribs } strict =
    { returnType=returnType, strict=strict, ribs=ribs }

fun withRib { returnType, strict, ribs} extn =
    { returnType=returnType, strict=strict, ribs=extn :: ribs }

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[verify] " :: ss) else ()
fun error ss = LogErr.verifyError ss

(****************************** standard types *************************)

(* TODO: what is the proper way to resolve these built-ins? *)
(* FIXME: change Ast to have a variant of TypeName(?) that should be looked up in the global class table *)
fun builtInType (s:Ustring.STRING) : Ast.TYPE_EXPR
  = Ast.NominalType (Name.intrinsic s)

val boolType      = builtInType Ustring.boolean_
val numberType    = builtInType Ustring.number_
val doubleType    = builtInType Ustring.double_
val decimalType   = builtInType Ustring.decimal_ 
val intType       = builtInType Ustring.int_
val uintType      = builtInType Ustring.uint_
val stringType    = builtInType Ustring.string_
val regexpType    = builtInType Ustring.regexp_
val exceptionType = builtInType Ustring.exception_
val namespaceType = builtInType Ustring.Namespace_
val typeType      = builtInType Ustring.Type_
val undefinedType = Ast.SpecialType Ast.Undefined
val nullType      = Ast.SpecialType Ast.Null
val anyType       = Ast.SpecialType Ast.Any

(****************************** misc auxiliary functions *************************)

fun assert b s = if b then () else (raise Fail s)

fun checkForDuplicates [] = ()
  | checkForDuplicates (x::xs) =
    if List.exists (fn y => x = y) xs
    then error ["concurrent definition"]
    else checkForDuplicates xs
      
fun unOptionDefault NONE def = def
  | unOptionDefault (SOME v) _ = v

fun flattenOptionList NONE = []
  | flattenOptionList (SOME l) = l

val gensymCounter : int ref = ref 0
fun gensym (s:Ustring.STRING) : Ustring.STRING = 
    let
    in 
	gensymCounter := 1+(!gensymCounter);
	Ustring.fromString (Ustring.toString s^"$"^(Int.toString (!gensymCounter)))
    end

(************************* Normalized types *********************************)

type TYPE_VALUE = Ast.TYPE_EXPR  (* Invariant: normalized *)

(*  A normalized type is one of

 and TYPE_EXPR =
         SpecialType of SPECIAL_TY
       | UnionType of TYPE_EXPR list
       | ArrayType of TYPE_EXPR list
       | FunctionType of FUNC_TYPE           
       | ObjectType of FIELD_TYPE list
       | AppType of 
           { base: TYPE_EXPR,  -- not a function type
             args: TYPE_EXPR list }
       | NullableType of 
           { expr:TYPE_EXPR,
             nullable:bool }
       | InstanceType of
           { name: NAME, 
             typeParams: IDENT list, 
             ty: TYPE_EXPR,
             isDynamic: bool }
       | NominalType of NAME

and excludes
       | TypeName of IDENT_EXPR
       | ElementTypeRef of (TYPE_EXPR * int)
       | FieldTypeRef of (TYPE_EXPR * IDENT)
*)

(************************* Substitution on Types *********************************)

(* TODO: normalized types?? *)

fun substTypeExpr (s:(Ast.IDENT*TYPE_VALUE) list) (t:TYPE_VALUE):Ast.TYPE_EXPR = 
    let in
	case t of
	    Ast.UnionType ts => 
	    Ast.UnionType (map (substTypeExpr s) ts)
	  | Ast.ArrayType ts => 
	    Ast.ArrayType (map (substTypeExpr s) ts)
	  | Ast.AppType {base, args} => 
	    Ast.AppType {base=substTypeExpr s base, args=map (substTypeExpr s) args}
	  | Ast.NullableType {expr, nullable}
	    => Ast.NullableType {expr=substTypeExpr s expr, nullable=nullable}

	  | Ast.TypeName ((Ast.Identifier {ident=ident, openNamespaces=_})) =>
	    let in
		case List.find 
			 (fn (id,ty) => id=ident)
			 s 
		 of
		    (* TODO: we're dropping the nullable here, is that right? *)
		    SOME (_,ty) => ty 
		  | NONE => t
	    end
	  | Ast.ObjectType fields =>
	    Ast.ObjectType (map (fn {name,ty} => 
				{name=name,ty=substTypeExpr s ty})
			fields)
	  | Ast.SpecialType st => Ast.SpecialType st

	  | Ast.FunctionType { typeParams, params, result, thisType, hasRest, minArgs } =>
	    (* Need to uniquify typeParams to avoid capture *)
	    let val oldNew = map (fn id => (id, gensym id)) typeParams 
		    val nuSub = 
		        map 
			        (fn (oldId,newId) => 
			            (oldId, 
			             Ast.TypeName (Ast.Identifier {ident=newId,openNamespaces=[]})))
			        oldNew
		    val bothSubs = fn t => substTypeExpr s (substTypeExpr nuSub t)
		    val nuTypeParams = map (fn (oldId,newId) => newId) oldNew
		(* val nuParams = map (fn t => substVarBinding s (substVarBinding nuSub t)) params *)
		in
		    Ast.FunctionType {typeParams=nuTypeParams,
			                  params = List.map bothSubs params,
			                  result = bothSubs result,      
			                  thisType = Option.map bothSubs thisType,
			                  hasRest=hasRest, minArgs = minArgs }
		end
    end 

(************************* Handling Types *********************************)

fun verifyTypeExpr (env:ENV)
                   (ty:Ast.TYPE_EXPR)
    : TYPE_VALUE =
    let in
        trace ["type checking and normalizing a type"];
        if (!doTrace) then Pretty.ppType ty else ();

	    case ty of
	        Ast.SpecialType _ => 
            ty
	      | Ast.UnionType tys => 
            Ast.UnionType (verifyTypeExprs env tys)
	      | Ast.ArrayType tys => 
            Ast.ArrayType (verifyTypeExprs env tys)
	      | Ast.NullableType {expr=ty,nullable} => 
            Ast.NullableType {expr=verifyTypeExpr env ty, nullable=nullable}
	      | Ast.AppType {base,args} =>
	        let val base' = verifyTypeExpr env base
                val args' = verifyTypeExprs env args
            in case base' of
                   Ast.FunctionType {typeParams,params,result,thisType,hasRest,minArgs} =>
                   let val _ =	assert (length args = length typeParams);
		               val sub = ListPair.zip (typeParams,args)
		               fun applySub t = substTypeExpr sub t
                       val result = 
		                   Ast.FunctionType { typeParams=[],
			                                  params=map (substTypeExpr sub) params,
			                                  result=substTypeExpr sub result,
			                                  thisType= Option.map (substTypeExpr sub) thisType,
			                                  hasRest=hasRest, minArgs=minArgs }
                   in
                       verifyTypeExpr env result
                   end
                 | _ => Ast.AppType {base=base', args=args'}
	        end
	      | Ast.FunctionType {typeParams, params, result, thisType, hasRest, minArgs} =>
	        let 
	            (* Add the type parameters to the environment. *)
	            val env' : ENV = 
                    withRib env (List.map 
                                     (fn id => (Ast.PropName (Name.internal id),
                                                Ast.TypeVarFixture)) 
                                     typeParams)
                val params' = verifyTypeExprs env' params
                val result' = verifyTypeExpr env' result
                val thisType' = Option.map (verifyTypeExpr env') thisType
            in
                Ast.FunctionType { typeParams=typeParams, params=params', result=result', 
                                   thisType=thisType', hasRest=hasRest, minArgs=minArgs }
            end
	      | Ast.ObjectType fields => 
	        let val fields' =
		            map (fn {name,ty} => {name=name, ty=verifyTypeExpr env ty})
			            fields
                val names = map (fn {name,ty} => name) fields
                val _ = checkForDuplicates names
            in
                Ast.ObjectType fields'
	        end	

	  | _ => 
        let in
            Pretty.ppType ty;
            unimplError ["verifyTypeExpr"]
        end
    end
    
and verifyTypeExprs  (env:ENV) 
		             (tys:Ast.TYPE_EXPR list) 
    : TYPE_VALUE list = 
    (List.map (verifyTypeExpr env) tys)

(*
 * TODO: when type checking a function body, handle CalledEval
 * TODO: during type checking, if you see naked invocations of "eval" (that
 *       exact name), raise CalledEval
 *)

fun mergeTypes t1 t2 =
	(*FIXME*)
    t1


(************************* Compatibility *********************************)

fun checkCompatible (t1:TYPE_VALUE) 
		            (t2:TYPE_VALUE) 
    : unit = 
    if isCompatible t1 t2
    then ()
    else let in
	     TextIO.print ("Types are not compatible\n");
	     Pretty.ppType t1;
	     Pretty.ppType t2;
	     verifyError ["Types are not compatible"]
	 end

and isCompatible (t1:TYPE_VALUE) 
		         (t2:TYPE_VALUE) 
    : bool = 
    let 
    in
        trace ["Checking compatible - First type:"];
        if (!doTrace) then Pretty.ppType t1 else ();
	    trace ["Second type: "];
        if (!doTrace) then Pretty.ppType t2 else ();

	    (t1=t2) orelse
	    (t1=anyType) orelse
	    (t2=anyType) orelse
	    case (t1,t2) of
	        (Ast.UnionType types1,_) => 
	        List.all (fn t => isCompatible t t2) types1
	      | (_, Ast.UnionType types2) =>
	        (* t1 must exist in types2 *)
	        List.exists (fn t => isCompatible t1 t) types2 
	      | (Ast.ArrayType types1, Ast.ArrayType types2) => 
	        (* arrays are invariant, every entry should be compatible in both directions *)
	        let fun check (h1::t1) (h2::t2) =
		            (isCompatible h1 h2)
		            andalso
		            (isCompatible h2 h1)
		            andalso
		            (case (t1,t2) of
			             ([],[]) => true
		               | ([],_::_) => check [h1] t2
		               | (_::_,[]) => check t1 [h2]
		               | (_::_,_::_) => check t1 t2)
	        in
		        check types1 types2
	        end
            
	      | (Ast.ArrayType _, 
	         Ast.TypeName (Ast.Identifier {ident=Ustring.UniString "Array", openNamespaces=[]})) 
	        => true
               
	      | (Ast.ArrayType _, 
	         Ast.TypeName (Ast.Identifier {ident=Ustring.UniString "Object", openNamespaces=[]})) 
	        => true
               
	      | (Ast.FunctionType _, 
	         Ast.TypeName (Ast.Identifier {ident=Ustring.UniString "Function", openNamespaces=[]})) 
	        => true
               
	      | (Ast.FunctionType _, 
	         Ast.TypeName (Ast.Identifier {ident=Ustring.UniString "Object", openNamespaces=[]}))
	        => true
               
	      | (Ast.AppType {base=base1,args=args1}, Ast.AppType {base=base2,args=args2}) => 
	        (* We keep types normalized wrt beta-reduction, 
	         * so base1 and base2 must be class or interface types.
	         * Type arguments are covariant, and so must be intra-compatible - CHECK 
	         *)
	        false
	        
	      | (Ast.ObjectType fields1, Ast.ObjectType fields2) =>
	        false
            
	      | (Ast.FunctionType 
		         {typeParams=typeParams1,
		          params  =params1, 
		          result  =result1,
		          thisType=thisType1,
		          hasRest =hasRest1,
	              minArgs=minArgs},
	         Ast.FunctionType 
		         {typeParams=typeParams2,
		          params=params2, 
		          result=result2, 
		          thisType=thisType2,
		          hasRest=hasRest2,
		          minArgs=minArgs2}) =>
	        let
	        in
		        (* TODO: Assume for now that functions are not polymorphic *)
		        assert (typeParams1 = [] andalso typeParams2=[]) "cannot handle polymorphic fns";
		        assert (not hasRest1 andalso not hasRest2) "cannot handle rest args";
                
		        ListPair.all (fn (t1,t2) => isCompatible t1 t2) (params1,params2)
		        andalso
		        isCompatible result1 result2
	        end
            
            
	      (* catch all *)
	      | _ => unimplError ["isCompatible"]
    end
    
fun checkBicompatible (ty1:TYPE_VALUE) 
		              (ty2:TYPE_VALUE)
    : unit = 
    let in
	    checkCompatible ty1 ty2;
	    checkCompatible ty2 ty1
    end
    
fun checkConvertible (ty1:TYPE_VALUE) 
                     (ty2:TYPE_VALUE)
    : unit =
    (* TODO: int to float, etc, and to() methods *)
    checkCompatible ty1 ty2
    

(******************** Verification **************************************************)

(*
    HEAD
*)

and verifyInits (env:ENV) (inits:Ast.INITS)
    : Ast.INITS =
    List.map (fn (name, expr) =>
                 let
                     val (expr', t) = verifyExpr env expr
                 in
                     (name, expr')
                 end)
             inits

and verifyHead (env:ENV) ((fixtures, inits):Ast.HEAD)
    : Ast.HEAD =
    (fixtures, verifyInits env inits)

(*
    EXPR
*)

and verifyExpr (env:ENV) 
               (expr:Ast.EXPR) 
    : (Ast.EXPR * Ast.TYPE_EXPR) = 
    let
        fun verifySub e = 
            let
                val (e,t) = verifyExpr env e
            in
                e
            end
        fun return (e, t) =
            (Ast.ExpectedTypeExpr (t, e), t)
        val dummyType = Ast.SpecialType Ast.Any
    in
        case expr of 
            Ast.TernaryExpr (t, e1, e2, e3) =>
            let
                val e1' = verifySub e1
                val e2' = verifySub e2
                val e3' = verifySub e3
            in
                return (Ast.TernaryExpr (t, e1', e2', e3'), dummyType)
            end
            
          | Ast.BinaryExpr (b, e1, e2) =>
            let
                val e1' = verifySub e1
                val e2' = verifySub e2
            in
                return (Ast.BinaryExpr (b, e1', e2'), dummyType)
            end

          | Ast.ExpectedTypeExpr (t, e) =>
            internalError ["Defn produced ExpectedTypeExpr"]

          | Ast.BinaryTypeExpr (b, e, te) =>
            let
                val e' = verifySub e
                val te' = verifyTypeExpr env te
            in
                return (Ast.BinaryTypeExpr (b, e', te'), dummyType)
            end

          | Ast.UnaryExpr (u, e) =>
            let
                val e' = verifySub e
            in
                return (Ast.UnaryExpr (u, e'), dummyType)
            end

          | Ast.TypeExpr t => 
            let
                val t' = verifyTypeExpr env t
            in
                return (Ast.TypeExpr t', dummyType)
            end

          | Ast.ThisExpr =>
            return (Ast.ThisExpr, dummyType)

          | Ast.YieldExpr eo =>
            let
                val eo' = Option.map verifySub eo
            in
                return (Ast.YieldExpr eo', dummyType)
            end

          | Ast.SuperExpr eo =>
            let
                val eo' = Option.map verifySub eo
            in
                return (Ast.SuperExpr eo', dummyType)
            end
            
          | Ast.LiteralExpr le => 
            (* TODO *)
            return (Ast.LiteralExpr le, dummyType)
            
          | Ast.CallExpr {func, actuals} => 
            let
                val func' = verifySub func
                val actuals' = List.map verifySub actuals
            in
                return (Ast.CallExpr { func = func',
                                       actuals = actuals' }, dummyType)
            end

          | Ast.ApplyTypeExpr { expr, actuals } =>
            let
                val expr' = verifySub expr
                val actuals' = List.map (verifyTypeExpr env) actuals
            in
                return (Ast.ApplyTypeExpr { expr = expr',
                                            actuals = actuals' }, dummyType)
            end

          | Ast.LetExpr { defs, body, head } =>
            let
                val defs' = defs (* TODO *)
                val head' = Option.map (verifyHead env) head
                (* TODO: verify body with `head' fixtures in env *)
                val body' = verifySub body
            in
                return (Ast.LetExpr { defs = defs',
                                      body = body',
                                      head = head' }, dummyType)
            end

          | Ast.NewExpr { obj, actuals } => 
            let
                val obj' = verifySub obj
                val actuals' = List.map verifySub actuals
            in
                return (Ast.NewExpr { obj = obj',
                                      actuals = actuals' }, dummyType)
            end

          | Ast.ObjectRef { base, ident, pos } =>
            let
                val base' = verifySub base
                val ident' = ident (* TODO *)
            in
                return (Ast.ObjectRef { base=base', ident=ident', pos=pos }, dummyType)
            end

          | Ast.LexicalRef { ident, pos } =>
            let
                val ident' = ident (* TODO *)
            in
                return (Ast.LexicalRef { ident=ident', pos=pos }, dummyType)
            end

          | Ast.SetExpr (a, le, re) => 
            let
                val le' = verifySub le
                val re' = verifySub re
            in
                return (Ast.SetExpr (a, le', re'), dummyType)
            end

          | Ast.GetTemp n => 
            (* TODO: these only occur on the RHS of compiled destructuring assignments. how to type-check? *)
            return (Ast.GetTemp n, dummyType)

          | Ast.GetParam n => 
            internalError ["GetParam not eliminated by Defn"]

          | Ast.ListExpr es => 
            let
                val es' = List.map verifySub es
            in
                return (Ast.ListExpr es', dummyType)
            end

          | Ast.SliceExpr (a, b, c) => 
            let
                val a' = verifySub a
                val b' = verifySub b
                val c' = verifySub c
            in
                return (Ast.SliceExpr (a, b, c), dummyType)
            end

          | Ast.InitExpr (it, head, inits) =>
            let
                val it' = it (* TODO *)
                val head' = verifyHead env head
                val inits' = verifyInits env inits
            in
                return (Ast.InitExpr (it', head', inits'), dummyType)
            end

    end
    
    
and verifyExprs (env:ENV) 
             (exprs:Ast.EXPR list) 
    : Ast.EXPR list * Ast.TYPE_EXPR list = 
    let
        val es = ListPair.unzip (map (verifyExpr env) exprs)
    in
        es
    end

and verifyExprAndCheck (env:ENV)
                       (expr:Ast.EXPR)
                       (expectedType:TYPE_VALUE)
    : Ast.EXPR =
    let val (expr',ty') = verifyExpr env expr
        val _ = if #strict env
                then checkCompatible ty' expectedType
                else ()
    in
        expr'
    end

(*
    STMT
*)

and verifyStmt (env:ENV) 
               (stmt:Ast.STMT) 
    : Ast.STMT = 
    let fun verifySub s = verifyStmt env s
    in
        case stmt of
            Ast.EmptyStmt => 
            Ast.EmptyStmt
         
          | Ast.ExprStmt e => 
            let
                val (expr,ty) = verifyExpr env e
            in
                Ast.ExprStmt expr
            end

          | Ast.ForInStmt fe => (*TODO*)
            Ast.ForInStmt fe
            
          | Ast.ThrowStmt es =>
            let val (es',_) = verifyExpr env es
            in
                Ast.ThrowStmt es'
            end

          | Ast.ReturnStmt es =>
            let val (es',ty) = verifyExpr env es
            in
                if #strict env
                then
	                case #returnType env of
	                    NONE => verifyError ["return not allowed here"]
                      | SOME retTy => checkCompatible ty retTy
                else ();
                Ast.ReturnStmt es'
            end
            
          | Ast.BreakStmt i => 
            Ast.BreakStmt i

          | Ast.ContinueStmt i =>
            Ast.ContinueStmt i

          | Ast.BlockStmt block =>
            Ast.BlockStmt (verifyBlock env block)
            
          | Ast.ClassBlock { ns, ident, name, block } =>
            Ast.ClassBlock { ns=ns, ident=ident, name=name, 
                             block=verifyBlock env block }
            
          | Ast.LabeledStmt (id, s) =>
            Ast.LabeledStmt (id, verifySub s)
            
          | Ast.LetStmt block =>
            Ast.LetStmt (verifyBlock env block) 

          | Ast.WhileStmt {cond,body,labels,fixtures=NONE} => 
            Ast.WhileStmt {cond=verifyExprAndCheck env cond boolType,
                           body=verifySub body,
                           labels=labels,
                           fixtures=NONE}
            
          | Ast.DoWhileStmt {cond,body,labels,fixtures=NONE} => 
            Ast.DoWhileStmt {cond=verifyExprAndCheck env cond boolType,
                             body=verifySub body,
                             labels=labels,
                             fixtures=NONE}

          | Ast.ForStmt  { defn=_, fixtures, init, cond, update, labels, body } => 
            let val fixtures' = verifyFixturesOption env fixtures
                val env' = withRib env fixtures'
                val init' = verifyStmts env' init
                val cond' = verifyExprAndCheck env' cond boolType
                val (update',_) = verifyExpr env' update
                val body' = verifyStmt env' body
            in
                Ast.ForStmt  { defn=NONE, fixtures=SOME fixtures', init=init', cond=cond', 
                               update=update', labels=labels, body=body' } 
            end
          | Ast.IfStmt {cnd, els, thn} => 
            Ast.IfStmt {cnd=verifyExprAndCheck env cnd boolType, 
                        els=verifySub els, 
                        thn=verifySub thn}
            
          | Ast.WithStmt {obj, ty, body} => (*TODO*)
            Ast.WithStmt {obj=obj, ty=ty, body=body}
        
          | Ast.TryStmt {block, catches, finally} =>
            Ast.TryStmt {block=verifyBlock env block, 
                         catches=List.map (verifyCatchClause env) catches, 
                         finally=Option.map (verifyBlock env) finally }
        
          | Ast.SwitchStmt {cond, cases, mode, labels} => (*TODO*)
            Ast.SwitchStmt {cond=cond, cases=cases, mode=mode, labels=labels}
        
          | Ast.SwitchTypeStmt {cond, ty, cases} => (*TODO*)
            Ast.SwitchTypeStmt {cond=cond, ty=ty, cases=cases}
            
          | Ast.Dxns x => (*TODO*)
            Ast.Dxns x

          | _ => error ["Shouldn't happen: failed to match in Verify.verifyStmt"]

    end

and verifyCatchClause (env:ENV)
                      ({bindings, ty, fixtures, block}:Ast.CATCH_CLAUSE)
    : Ast.CATCH_CLAUSE =
    let val fixtures' = verifyFixturesOption env fixtures
        val env' = withRib env fixtures'
        val block' = verifyBlock env' block
    in
        {bindings=bindings, ty=ty, 
         fixtures=SOME fixtures', block=block'}
    end

and verifyStmts (env) (stmts:Ast.STMT list)
    : Ast.STMT list =
    List.map (verifyStmt env) stmts

and verifyBlock (env:ENV) 
                (b:Ast.BLOCK) 
    : Ast.BLOCK =
    let
    in case b of
        Ast.Block { head, body, pos, pragmas=pragmas, defns=defns } =>
            let
                val _ = LogErr.setPos pos
                val head = Option.map (verifyHead env) head
                val body = verifyStmts env body
            in
                Ast.Block { pragmas = pragmas,
                            defns = defns,
                            body = body,
                            head = head,
                            pos = pos }
            end
    end

(*
    FIXTURES
*)

and verifyFixture (env:ENV)
		          (f:Ast.FIXTURE) 
    : Ast.FIXTURE = (*TODO*)
    let in
        case f of
         Ast.NamespaceFixture ns =>
         Ast.NamespaceFixture ns
       | Ast.ClassFixture (Ast.Cls {name, extends, implements, classFixtures, instanceFixtures,
                                    instanceInits, constructor, classType, instanceType }) =>
         (*TODO*)
         f
       | Ast.TypeVarFixture =>
         Ast.TypeVarFixture 
       | Ast.TypeFixture ty =>
         Ast.TypeFixture (verifyTypeExpr env ty)
       | Ast.ValFixture {ty, readOnly} =>
         Ast.ValFixture {ty=verifyTypeExpr env ty, readOnly=readOnly}
       | Ast.MethodFixture { func, ty, readOnly, override, final } =>
         (* TODO *)
         f
       | _ => unimplError ["in verifyFixture"]
    end

and verifyFixturesOption (env:ENV) 
		                 (fs:Ast.FIXTURES option)  
    : Ast.FIXTURES = 
    let in
        case fs of
            SOME fixtures => map (fn (name,fixture) => (name,verifyFixture env fixture)) fixtures
          | _ => internalError ["missing fixtures"]
    end

(*
    PROGRAM
*)

and topEnv () = { ribs = [!Defn.topFixtures],
                  strict = false,
                  returnType = NONE }

and verifyPackage (p:Ast.PACKAGE)
    : Ast.PACKAGE =
    raise UnimplError

and verifyProgram (p:Ast.PROGRAM) 
    : Ast.PROGRAM =
    let 
    in case p of
        { packages, fixtures, block } =>
            let
                val _ = LogErr.setPos NONE
                val e = topEnv ()
                val block = verifyBlock e block
                val result = { packages = packages,
                               block = block,
                               fixtures = fixtures }
            in
                trace ["verification complete"];
                (if !doTrace 
                 then Pretty.ppProgram result
                 else ());
                result
            end
    end
end



(********************************** OLD **************************************

(*
 * INVARIANTS:
 *   - all typed libraries in host environment must be DontDelete
 *   - all typed libraries in host environment must carry compatible runtime type constraints
 *)

(****************************** type environments *************************)

type ID = FIXTURE_NAME
		
fun checkForDuplicateExtension extensions =
    let val (names, _) = ListPair.unzip extensions
    in
      checkForDuplicates names
end

fun extendEnv (env:TYPE_ENV) (id:ID) (k:KIND) : TYPE_ENV = (id,k)::env
fun extendEnvWithTypeVars (params:ID list) (env:TYPE_ENV):TYPE_ENV =
    let in
	checkForDuplicates params;
	foldl (fn (p,env) => extendEnv env p TypeVar) env params
    end
fun extendEnvs (env:TYPE_ENV) (ext:TYPE_ENV) =
    let in
	ext @ env
    end
fun fixtureNameToString (TempName i) = "TempName" ^ (Int.toString(i))
  | fixtureNameToString (PropName { ns, id }) = id

fun lookupIdNamespace (env:TYPE_ENV) 
		      (id:Ast.IDENT) 
		      (ns:NAMESPACE)
    : KIND option =
    case List.find (fn (i,_) => i=PropName {id=id,ns=ns}) env of
	NONE => NONE
      | SOME (_,k) => SOME k

fun lookupIdNamespaces (env:TYPE_ENV) 
		       (id:Ast.IDENT) 
		       (nss : NAMESPACE list)
    : KIND option =
    let val theMatches
	  = List.mapPartial (lookupIdNamespace env id) nss
    in
	case theMatches of
	    [] => NONE
	  | [k] => SOME k



(******************** Expressions **************************************************)

and verifyIdentExpr (env as {env,this,...}:RIB) 
		    (ide:Ast.IDENT_EXPR) 
    : Ast.TYPE_EXPR = 
    let
    in 
	case ide of
	    Ast.QualifiedIdentifier { qual, ident } =>
	    let in
		checkCompatible (verifyExpr env qual) namespaceType;
		anyType
	    end	    
	  | Ast.QualifiedExpression { qual, expr } =>
	    let in
		checkCompatible (verifyExpr env qual) namespaceType;
		checkCompatible (verifyExpr env qual) stringType;
		anyType
	    end
	  | Ast.AttributeIdentifier idexpr =>
	    let in
		verifyIdentExpr env idexpr;
		anyType
	    end
	  | Ast.Identifier { ident, openNamespaces } =>
	    let val k : KIND = lookupIdNamespacess env ident openNamespaces
	    in
		case k of
		    TypeVar => verifyError ["Attempt to refer to type variable ",
                                    ident,
                                    " as a program variable"]
		  | ProgVar (ty,read_only) => ty
	    end
    end

and verifyExpr (env as {env,this,...}:RIB) 
	       (e:EXPR) 
    : Ast.TYPE_EXPR = 
    let
    in 
      TextIO.print ("type checking expr: env len " ^ (Int.toString (List.length env)) ^"\n");
      Pretty.ppExpr e;
      TextIO.print "\n";
      case e of
	LiteralExpr LiteralNull => nullType
      | LiteralExpr (LiteralInt _) => intType
      | LiteralExpr (LiteralUInt _) => uintType
      | LiteralExpr (LiteralDecimal _) => decimalType
      | LiteralExpr (LiteralDouble _) => doubleType
      | LiteralExpr (LiteralBoolean _) => boolType
      | LiteralExpr (LiteralString _) => stringType
      | LiteralExpr (LiteralRegExp _) => regexpType
      | LiteralExpr (LiteralArray { exprs, ty }) =>
        (* EXAMPLES:
           [a, b, c] : [int, Boolean, String]
           [a, b, c] : Array
           [a, b, c] : *
           [a, b, c] : Object
        *)
        let val annotatedTy = unOptionDefault ty anyType
            val inferredTy = ArrayType (map (fn elt => verifyExpr env elt) exprs)
        in
          checkCompatible inferredTy annotatedTy;
          annotatedTy
        end
      | LiteralExpr (LiteralObject { expr=fields, ty }) =>
        let val annotatedTy = unOptionDefault ty anyType
            val inferredTy = ObjectType (map (verifyField env) fields)
        in
          checkCompatible inferredTy annotatedTy;
          annotatedTy
        end
      | LiteralExpr (LiteralFunction (Func { param=(fixtures,inits), block, ty, ... }))
	=>
	 let
	     val env1 = verifyFunctionType env ty 
	     val extensions = verifyFixtures env1 fixtures
	     val env2 = withEnvExtn env1 extensions
	 in
	     checkForDuplicateExtension extensions;
(** FIXME: inits are now settings and are BINDINGS 
	     verifyStmts env2 inits;
*)
	     verifyBlock env2 block;
	     Ast.FunctionType ty
         end
      | LexicalRef { ident, pos } =>
	verifyIdentExpr env ident
      | ListExpr l => List.last (List.map (verifyExpr env) l)
      | LetExpr {defs=_, body, head=SOME (fixtures,inits) } =>  (* FIXME: inits added *)
          let val extensions = verifyFixtures env fixtures
          in
	    checkForDuplicateExtension extensions;
	    verifyExpr (withEnvExtn env extensions) body
	  end
       | ThisExpr => this
       | UnaryExpr (unop, arg) => verifyUnaryExpr env unop arg
       | BinaryExpr (binop, lhs, rhs ) => verifyBinaryExpr env (binop, lhs, rhs)
       | BinaryTypeExpr (binop, lhs, rhs ) => verifyBinaryTypeExpr env (binop, lhs, rhs)
       | TrinaryExpr (triop, a,b,c ) => verifyTrinaryExpr env (triop, a,b,c)

       | CallExpr { func, actuals } => verifyCallExpr env func actuals
       | ApplyTypeExpr {expr, actuals} =>
	 (* Can only instantiate Functions, classes, and interfaces *)
	 let val exprTy = verifyExpr env expr;
	     val typeParams = 
		 case exprTy of
		     Ast.FunctionType {typeParams, ...} => typeParams
                   (* TODO: class and interface types *)
		   | _ => verifyError ["Cannot instantiate a non-polymorphic type"]
	 in
	     List.app (fn t => verifyTypeExpr env t) actuals;
	     if (List.length typeParams) = (List.length actuals)
	     then ()
	     else verifyError ["Wrong number of type arguments"];
	     normalizeType (Ast.AppType { base=exprTy, args=actuals })
	 end

       | TypeExpr ty => 
	 let in
	     verifyTypeExpr env ty;
	     typeType
	 end

       | _ => (TextIO.print "verifyExpr incomplete: "; Pretty.ppExpr e; raise Match)
    end
    
(*
     and LITERAL =
       | LiteralXML of EXPR list
       | LiteralNamespace of NAMESPACE

       | LiteralObject of
         { name: EXPR,
           init: EXPR } list

     and EXPR =
        | YieldExpr of EXPR option
       | SuperExpr of EXPR option

       | Ref of { base: EXPR option,
                  ident: Ast.IDENT_EXPR }
       | NewExpr of { obj: EXPR,x
                      actuals: EXPR list }


     and Ast.IDENT_EXPR =
         Ast.QualifiedIdentifier of { qual : EXPR,
                                  ident : Ustring.STRING }
       | Ast.QualifiedExpression of { qual : EXPR,
                                  expr : EXPR }
       | Ast.AttributeIdentifier of Ast.IDENT_EXPR
       | Ast.Identifier of Ast.IDENT
       | Ast.Expression of EXPR   (* for bracket exprs: o[x] and @[x] *)

*)


and verifyCallExpr  (env as {env,this,...}:RIB) 
		    func actuals
    : Ast.TYPE_EXPR =
    let val functy = verifyExpr env func;
	val actualsTy = (map (fn a => verifyExpr env a) actuals)
    in case normalizeType functy of
	   Ast.SpecialType Ast.Any =>
	   (* not much to do *)
	   anyType
	 | Ast.FunctionType { typeParams, params, result, thisType, hasRest, minArgs }
	   => 
	   let 
	   in
	       if not (null typeParams)
	       then verifyError ["Attempt to apply polymorphic function to values"]
	       else ();
	       (* check this has compatible type *)
	       case thisType of
		   NONE => ()  (* this lexically bound, nothing to check *)
		 | SOME t => checkCompatible this t;
	       (* check compatible parameters *)
	       let val (normalParams, restParam) =
		       if hasRest
		       then (List.rev (List.tl (List.rev params)), SOME (List.last params))
		       else (params, NONE)
		   (* handleArgs normalParams actualTys *)
		   fun handleArgs [] [] = ()
		     | handleArgs (p::pr) (a::ar) =				
		       let in
			   checkCompatible a p;
			   handleArgs pr ar
		       end
		     | handleArgs [] (_::_) = 
		       if hasRest 
		       then (* all ok, no type checking to do on rest arg *) ()
		       else verifyError ["Too many args to function"]
		     | handleArgs (_::_) [] =
		       verifyError ["Not enough args to function"]
	       in
		   handleArgs params actualsTy;
		   result
	       end
	   end
	 | _ => verifyError ["Function expression does not have a function type"]
    end
    

(* TODO: verifyPattern returns a pair of env extension and (inferred) type?
         or takes a type (checked) and returns just extension? *)
(*
and verifyPattern (env:RIB) (Ast.IdentifierPattern name) = (
  | verifyPattern env (Ast.ObjectPattern props) =
  | verifyPattern env (Ast.ArrayPattern elts) =
  | verifyPattern env (Ast.SimplePattern expr) = ??
*)

and verifyExprList (env as {env,this,...}:RIB)
		   (l:EXPR list) 
    : Ast.TYPE_EXPR = 
    let
    in 	
	List.last (List.map (verifyExpr env) l)
    end
    
and verifyField (env:RIB) 
		({kind,name,init}:FIELD) 
    : FIELD_TYPE =
	let
		val Ast.Identifier {ident,...} = name
	in
	    {name=ident, ty = verifyExpr env init }
	end

(* This type checksTODO: this needs to return some type structure as well *)

(* deprecated due to fixtures
and verifyBinding (env:RIB) 
		  (Binding {init,pattern,ty}) 
    : TYPE_ENV =
    let val ty = unOptionDefault ty anyType in
	case init of
	    SOME expr => checkCompatible (verifyExpr env expr) ty
	  | NONE => ();
	case pattern of
	    IdentifierPattern ident => [(ident,SOME ty)]
    end

(*
 and VAR_BINDING =
         Binding of { init: EXPR option,
                      pattern: PATTERN,
                      ty: Ast.TYPE_EXPR option }

*)


and verifyVarBinding (env:RIB) 
		     (Binding {init, pattern, ty}:VAR_BINDING) 
    : TYPE_ENV =
    let in
	case pattern of
	    IdentifierPattern ident =>
	    [(ident,SOME (unOptionDefault ty anyType))]
	    end
    
and verifyVarBindings (env:RIB) 
		      (vs:VAR_BINDING list) 
    : TYPE_ENV =
    case vs of
	[] => []
      | h::t => (verifyVarBinding env h) @ (verifyVarBindings env t)
*)

and verifyUnaryExpr (env:RIB) 
		    (unop:UNOP)
		    (arg:EXPR) 
    : Ast.TYPE_EXPR =
    let val argType = verifyExpr env arg
	fun checkNumeric () = 
	    let in
		checkBicompatible boolType argType;
		argType
	    end
    in
	case unop of
	    Void => (verifyExpr env arg; undefinedType)
          | Typeof => (verifyExpr env arg; stringType)

	  (* Assume arg is an l-value *)
          | PreIncrement _  => checkNumeric ()
          | PostIncrement _ => checkNumeric ()
          | PreDecrement _  => checkNumeric ()
          | UnaryPlus _     => checkNumeric ()
          | UnaryMinus _    => checkNumeric ()
          | BitwiseNot      => 
	    let in checkConvertible argType uintType; uintType end
          | LogicalNot    => 
	    let in checkConvertible argType boolType; boolType end
	    
  (*
             Delete => (case arg of
                            Ref {base=NONE,ident=???} =>
                          | Ref {base=SOME baseExpr,ident=???} =>
                          | _ => verifyError ["can only delete ref expressions"])
  *)
 
(*    
          | MakeNamespace
          | Type
*)
          | _ => 
	    let in
		TextIO.print "verifyUnaryExpr incomplete: "; 
		Pretty.ppExpr (UnaryExpr (unop,arg)); 
		raise Match
	    end
    end

and verifyBinaryExpr (env:RIB) (bop:BINOP, lhs:EXPR, rhs:EXPR) =
    let
    in
	case bop of
	    _ => 
	    let in
		TextIO.print "verifyBinaryExpr incomplete: "; 
		Pretty.ppExpr (BinaryExpr (bop,lhs,rhs)); 
		raise Match
	    end
    end

and verifyBinaryTypeExpr (env:RIB) 
			 (bop:BINTYPEOP, arg:EXPR, t:Ast.TYPE_EXPR) =
    let val argType = verifyExpr env arg	
    in
	verifyTypeExpr env t;
	case bop of
	    Cast =>
	    let in
		(* TODO: check *)
		(* checkConvertible argType t; *)
		t
	    end
	  | To =>
	    let in
		(* TODO: check *)
		checkConvertible argType t;
		t
	    end
	  | Is =>
	    let in
		(* TODO: check *)
		checkConvertible argType t;
		boolType
	    end
    end


and verifyTrinaryExpr (env:RIB) (triop:TRIOP, a:EXPR, b:EXPR, c:EXPR) =
    case triop of
	Cond =>
	let val aty = verifyExpr env a
	    val bty = verifyExpr env b
	    val cty = verifyExpr env c
	in
	    checkConvertible aty boolType;
	    (*FIXME*)
	    checkConvertible bty cty;
	    cty
	end

(****************************** Verifying Statements ********************************)

and verifyStmts env ss = List.app (fn s => verifyStmt env s) ss

and verifyStmt (env as {this,env,lbls:(Ast.IDENT list),retTy}:RIB) (stmt:STMT) =
   let
   in
       TextIO.print ("type checking stmt: env len " ^ (Int.toString (List.length env)) ^"\n");
       Pretty.ppStmt stmt;
       TextIO.print "\n";
   case stmt of
    EmptyStmt => ()
  | ExprStmt e => (verifyExpr env e; ())
  | IfStmt {cnd,thn,els} => 
    let in
	checkCompatible (verifyExpr env cnd) boolType;
	verifyStmt env thn;
	verifyStmt env els
    end

  | (DoWhileStmt {cond,body,labels,fixtures} | WhileStmt {cond,body,labels,fixtures}) => 
    let in
	checkCompatible (verifyExpr env cond) boolType;
	verifyStmt (withLbls env (labels@lbls)) body
    end

  | ReturnStmt e => 
    let in
	case retTy of
	    NONE => verifyError ["return not allowed here"]
          | SOME retTy => checkCompatible (verifyExpr env e) retTy
    end

  | (BreakStmt NONE | ContinueStmt NONE) =>  
    let in
	case lbls of
	    [] => verifyError ["Not in a loop"]
	  | _ => ()
    end

  | (BreakStmt (SOME lbl) | ContinueStmt (SOME lbl)) => 
    let in
	if List.exists (fn x => x=(lbl)) lbls
	then ()
	else verifyError ["No such label"]
    end

  | BlockStmt b => verifyBlock env b

  | LabeledStmt (lab, s) => 
	verifyStmt (withLbls env ((lab)::lbls)) s
 
  | ThrowStmt t => 
	checkCompatible (verifyExpr env t) exceptionType

(* deprecated due to fixtures 
  | LetStmt (defns, body) =>
    let val extensions = List.concat (List.map (fn d => verifyBinding env d) defns)
    in
        checkForDuplicateExtension extensions;
        verifyStmt (withEnv (env, foldl extendEnv env extensions)) body  
    end
*)

  | ForStmt { defn=_, fixtures, init, cond, update, labels, body } =>
    let val extensions = verifyFixturesOption env fixtures
        val env' = withEnvExtn env extensions
(* NOT USED 
	fun verifyExprs exprs = 
	    let in
		if List.length exprs = 0
		then boolType
		else List.last (List.map (fn e => verifyExpr env' e) exprs)
	    end
*)
    in
  	verifyStmts env' init;
	checkCompatible (verifyExpr env' cond) boolType;
	verifyExpr env' update;
	verifyStmt (withLbls env' (labels@lbls)) body
    end

  | SwitchStmt { cond, cases, ... } =>  
    let val ty = verifyExpr env cond
    in
	List.app
	    (fn {label,body,inits} =>	(* FIXME: verify inits *)
		let in
		    (Option.app 
			 (fn e => checkBicompatible ty (verifyExpr env e))
			 label);
		    verifyBlock env body
		end)
	    cases
    end

  | TryStmt { block, catches, finally } =>
    let 
    in
	verifyBlock env block;
	case finally of
	    NONE => ()
	  | SOME block => verifyBlock env block;
	List.app
	(fn {bindings, ty, fixtures, block} =>
	    ())
	catches
    end

(* TODO: InitStmt for function defn test *)
    
  | _ => (TextIO.print "verifyStmt incomplete: "; Pretty.ppStmt stmt; raise Match)

(*
       | ForEachStmt of FOR_ENUM_STMT
       | ForInStmt of FOR_ENUM_STMT
       | SuperStmt of EXPR list

       | WithStmt of { obj: EXPR,
                       body: STMT }

       | TryStmt of { body: BLOCK,
                      catches: (FORMAL * BLOCK) list,
                      finally: BLOCK }
*)

   end

(***************************** Verifying Definitions ****************************************)


(*
and verifyDefn (env as {this,env,lbls,retTy}:RIB) (d:DEFN) : (TYPE_ENV * int list) =
    let
    in
	case d of
(* FIXME
	    VariableDefn {bindings,...} => 
	    (List.concat (List.map (verifyVarBinding env) bindings), []) 
	  | 
*)
	    FunctionDefn { kind, func,... } =>
	    let val Func {name, fsig, fixtures, inits, body } = func
		val FunctionSignature { typeParams, params, inits, 
					returnType, thisType, hasRest } 
		  = fsig
		val { kind, ident } = name
		val env3 = verifyFunctionSignature env fsig
	    in
		verifyBlock env3 body;
		([(ident, SOME (Ast.FunctionType fsig))],[])
            end
	    
	  | InterfaceDefn {ident,ns,nonnullable,params,extends,body} =>
	    let val nuEnv = extendEnvWithTypeVars params env
	    in
		verifyBlock (withEnv (env,nuEnv)) body;
		List.app
		(fn (superi:Ast.IDENT_EXPR) =>
		    (* need to check that this interface implements _all_ methods of superinterface ident *)
		    ()
		)
		extends;
		([],[])
	    end
		

(*
and INTERFACE_DEFN =
         { ident: Ast.IDENT,
           ns: EXPR,
           nonnullable: bool,
           params: Ast.IDENT list,
           extends: Ast.IDENT_EXPR list,
           body: BLOCK }
     *)
	    
	  | d => (TextIO.print "verifyDefn incomplete: "; Pretty.ppDefinition d; raise Match)
    end


and verifyDefns env ([]:DEFN list) : (TYPE_ENV * int list) = ([], [])
  | verifyDefns env ((d::ds):DEFN list) : (TYPE_ENV * int list) =
        let val (extensions1, classes1) = verifyDefn env d
            val (extensions2, classes2) = verifyDefns env ds
        in
            (extensions1 @ extensions2, classes1 @ classes2)
        end
*)

(******************** Fixtures **************************************************)

(* fixtures at the block level *)

(******************** Blocks **************************************************)




and verifyBlock (env as {env,...}) 
		(Block {pragmas,defns=_,body,head,pos}) =
    let val SOME (fixtures,inits) = head 
	val extensions = verifyFixtures env fixtures
        val env' = withEnvExtn env extensions
    in
	verifyStmts env' body
    end

fun verifyProgram (prog as { packages, fixtures, block }) =    
    let
    in 
      TextIO.print ("type checking program\n");
      Pretty.ppFixtures (!Defn.topFixtures);
      TextIO.print "\n";
      Pretty.ppProgram prog;
      TextIO.print "\n";
      verifyBlock {this=anyType, env=[], lbls=[], retTy=NONE} block; 
      true
    end

(* CF: Let this propagate to top-level to see full trace
    handle VerifyError msg => 
	   let in
     	       TextIO.print "Ill typed exception: "; 
     	       TextIO.print msg; 
     	       TextIO.print "\n"; 
     	       false
	   end

*)   
				    
end

************************************)
