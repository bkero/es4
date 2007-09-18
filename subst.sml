(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 * 
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 * 
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 * 
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * End of Terms and Conditions
 * 
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 *)
structure Subst = struct

(* 
 * AST-wide name substitution mechanism.
 *)

datatype 'a HELPER = 
	 Helper of 
	 { state : 'a,
	   ribs: Ast.RIBS,
	   unit: Ast.UNIT_NAME option,
	   visitIdent : Ast.IDENT -> Ast.IDENT }

fun typeParamsToRib (params:Ast.IDENT list)
    : Ast.RIB =
    let
	fun f id = (Ast.PropName (Name.nons id), 
		    Ast.TypeVarFixture)
    in
	map f params
    end

fun withRib (rib:Ast.RIB)
	    (helper:'a HELPER)
    : 'a HELPER = 
    let
	val Helper { state, ribs, unit, visitIdent } = helper
    in
	Helper { state = state,
		 ribs = rib :: ribs,
		 unit = unit,
		 visitIdent = visitIdent }
    end

fun withRibOpt (rib:Ast.RIB option)
	       (helper:'a HELPER)
    : 'a HELPER =
    case rib of 
	NONE => helper
      | SOME r => withRib r helper

fun withHeadRib (head:Ast.HEAD)
		(helper:'a HELPER)
    : 'a HELPER =
    let
	val Ast.Head (rib, inits) = head
    in
	withRib rib helper 
    end

fun withUnit (unit:Ast.UNIT_NAME option)
	     (helper:'a HELPER)
    : 'a HELPER = 
    let
	val Helper { state, ribs, unit, visitIdent } = helper
    in
	Helper { state = state,
		 ribs = ribs,
		 unit = unit,
		 visitIdent = visitIdent }
    end

fun substTy (helper:'a HELPER)
	    (ty:Ast.TY)
    : Ast.TY = 
    let
	val Ast.Ty { expr, ... } = ty
	val Helper { state, ribs, unit, visitIdent } = helper
    in
	Ast.Ty { expr = substType helper expr,
		 env = ribs,
		 unit = unit }
    end

and substTys (helper:'a HELPER)
	     (tys:Ast.TY list)
    : Ast.TY list = 
    map (substTy helper) tys

and substType (helper:'a HELPER) 
	      (te:Ast.TYPE_EXPR) 
    : Ast.TYPE_EXPR = 
    case te of 

        Ast.TypeName ie => 
        Ast.TypeName (substIdentExpr helper ie)
        
      | Ast.SpecialType t => 
        Ast.SpecialType t
	
      | Ast.UnionType tys => 
        Ast.UnionType (substTypes helper tys)
	
      | Ast.ArrayType tys => 
        Ast.ArrayType (substTypes helper tys)
		
      | Ast.ElementTypeRef (t, i) => 
        Ast.ElementTypeRef (substType helper t, i)

      | Ast.FieldTypeRef (t, i) => 
        Ast.FieldTypeRef (substType helper t, i)

      | Ast.ObjectType fields => 
        Ast.ObjectType 
	(map (fn {name, ty} => 
		 {name=name,
		  ty=substType helper ty}) fields)

      | Ast.AppType {base, args} => 
        Ast.AppType {base=substType helper base,
                     args=substTypes helper args}
	
      | Ast.LamType {params, body} => 
	let
	    val Helper { visitIdent, ... } = helper
	    val params = map visitIdent params
	    val rib = typeParamsToRib params
	in
            Ast.LamType {params=params,
			 body=substType (withRib rib helper) body}
	end
            
      | Ast.NullableType {expr, nullable} => 
        Ast.NullableType {expr=substType helper expr, 
			  nullable=nullable}
            
      | Ast.FunctionType {params, result, thisType, hasRest, minArgs} =>
        Ast.FunctionType {params=substTypes helper params,
                          result=substType helper result,
                          thisType=Option.map (substType helper) thisType,
                          hasRest=hasRest,
                          minArgs=minArgs}

      | Ast.InstanceType { name, typeArgs, nonnullable, superTypes, 
			   ty, conversionTy, dynamic} => 
	Ast.InstanceType { name = substName helper name,
			   typeArgs = substTypes helper typeArgs,
			   nonnullable = nonnullable,
			   superTypes = substTypes helper superTypes,
			   ty = substType helper ty,
			   conversionTy = Option.map (substType helper) conversionTy,
			   dynamic = dynamic }


and substExpr (helper:'a HELPER) 
	      (e:Ast.EXPR)
    : Ast.EXPR = 
    case e of 
        Ast.TernaryExpr (a, b, c) => 
	Ast.TernaryExpr (substExpr helper a, substExpr helper b, substExpr helper c)
	
      | Ast.BinaryExpr (bop, a, b) => 
        Ast.BinaryExpr (bop, substExpr helper a, substExpr helper b)
	
      | Ast.BinaryTypeExpr (bop, e, ty) => 
        Ast.BinaryTypeExpr (bop, substExpr helper e, substTy helper ty)
	
      | Ast.ExpectedTypeExpr (ty, e) => 
        Ast.ExpectedTypeExpr (substTy helper ty, substExpr helper e)
	
      | Ast.UnaryExpr (uop, e) => 
        Ast.UnaryExpr (uop, substExpr helper e)

      | Ast.TypeExpr ty => 
        Ast.TypeExpr (substTy helper ty)

      | Ast.ThisExpr => 
        Ast.ThisExpr

      | Ast.YieldExpr eo => 
        Ast.YieldExpr (Option.map (substExpr helper) eo)
        
      | Ast.SuperExpr eo => 
        Ast.SuperExpr (Option.map (substExpr helper) eo)
	
      | Ast.LiteralExpr lit => 
        Ast.LiteralExpr (substLit helper lit)
	
      | Ast.CallExpr {func, actuals} => 
        Ast.CallExpr {func = substExpr helper func,
                      actuals = substExprs helper actuals }

      | Ast.ApplyTypeExpr { expr, actuals } => 
        Ast.ApplyTypeExpr { expr = substExpr helper expr,
                            actuals = substTys helper actuals }
	
      | Ast.LetExpr { defs, head, body } =>
	(* defs should be reduced to head by the time we're called *)
	let
	    val head = Option.map (substHead helper) head
	    val helper = case head of 
			     NONE => helper
			   | SOME h => withHeadRib h helper
	in
            Ast.LetExpr { defs = defs, 
			  head = head,
			  body = substExpr helper body }
	end
	
      | Ast.NewExpr { obj, actuals } => 
        Ast.NewExpr { obj = substExpr helper obj,
                      actuals = substExprs helper actuals }
	
      | Ast.ObjectRef { base, ident, loc } => 
        Ast.ObjectRef { base = substExpr helper base,
                        ident = substIdentExpr helper ident,
                        loc = loc }
	
      | Ast.LexicalRef {ident, loc} => 
        Ast.LexicalRef {ident = substIdentExpr helper ident,
                        loc = loc }

      | Ast.SetExpr (aop, a, b) => 
        Ast.SetExpr (aop, substExpr helper a, substExpr helper b)
	
      | Ast.ListExpr es => 
        Ast.ListExpr (substExprs helper es)
        
      | Ast.InitExpr (targ, head, inits) => 
	let
	    val head = substHead helper head
	in
            Ast.InitExpr (targ, 
			  head, 
			  substInits (withHeadRib head helper) inits)
	end
                
      | Ast.SliceExpr (a,b,c) => 
        Ast.SliceExpr (substExpr helper a, 
		       substExpr helper b, 
		       substExpr helper c)
        
      | Ast.GetTemp i => 
        Ast.GetTemp i
        
      | Ast.GetParam i => 
        Ast.GetParam i

and substStmt (helper:'a HELPER)
	      (stmt:Ast.STMT)
    : Ast.STMT = 
    let
	val Helper { visitIdent, ... } = helper
    in
	case stmt of
	    Ast.EmptyStmt => 
	    Ast.EmptyStmt

	  | Ast.ExprStmt e => 
	    Ast.ExprStmt (substExpr helper e)

	  (* InitStmts should be reduced away by the time we're called *)
	  | Ast.InitStmt i => 
	    Ast.InitStmt i

	  (* FIXME: semantics of class blocks elude me. 
	   * Are we supposed to switch ribs here? 
	   * which of these are defined in which phase? *)
	  | Ast.ClassBlock { ns, ident, name, block } => 
	    Ast.ClassBlock { ns = Option.map (substExpr helper) ns,
			     ident = visitIdent ident,
			     name = Option.map (substName helper) name,
			     block = substBlock helper block }

	  | Ast.ForInStmt { isEach, defn, obj, rib, next, labels, body } => 
	    let
		(* FIXME: shouldn't 'labels' go in the rib somehow? *)
		val labels = map visitIdent labels
		val rib = Option.map (substRib helper) rib
		val helper' = withRibOpt rib helper
	    in
		Ast.ForInStmt { isEach = isEach,
				defn = defn,
				obj = substExpr helper obj,
				rib = rib,
				next = substStmt helper' next,
				labels = labels,
				body = substStmt helper' body }
	    end

	  | Ast.ThrowStmt e => 
	    Ast.ThrowStmt (substExpr helper e)
	  | Ast.ReturnStmt e => 
	    Ast.ReturnStmt (substExpr helper e)

	  | Ast.BreakStmt iopt => 
	    Ast.BreakStmt (Option.map visitIdent iopt)

	  | Ast.ContinueStmt iopt =>
	    Ast.ContinueStmt (Option.map visitIdent iopt)

	  | Ast.BlockStmt block => 
	    Ast.BlockStmt (substBlock helper block)
	    
	  | Ast.LabeledStmt (ident, stmt) => 
	    let
		val ident = visitIdent ident
	    in
		Ast.LabeledStmt (ident, substStmt helper stmt)
	    end

	  | Ast.LetStmt block => 
	    Ast.LetStmt (substBlock helper block)

	  | Ast.WhileStmt {cond, rib, body, labels} => 
	    let
		val rib = Option.map (substRib helper) rib
		val labels = map visitIdent labels
	    in
		Ast.WhileStmt { cond = substExpr helper cond,
				rib = rib,
				body = substStmt (withRibOpt rib helper) body,
				labels = labels }
	    end
	    
	  | Ast.DoWhileStmt {cond, rib, body, labels} => 
	    let
		val labels = map visitIdent labels
		val rib = Option.map (substRib helper) rib
	    in
		Ast.DoWhileStmt { cond = substExpr helper cond,
				  rib = rib,
				  body = substStmt (withRibOpt rib helper) body,
				  labels = labels }
	    end

	  | Ast.ForStmt { rib, defn, init, cond, update, labels, body } =>
	    let
		val labels = map visitIdent labels
		val rib = Option.map (substRib helper) rib
		val helper' = withRibOpt rib helper
	    in
		Ast.ForStmt { rib = rib,
			      defn = defn,
			      init = map (substStmt helper') init,
			      cond = substExpr helper' cond,
			      update = substExpr helper' update,
			      labels = labels,
			      body = substStmt helper' body }
	    end

       | Ast.IfStmt {cnd, thn, els} =>
	 Ast.IfStmt { cnd = substExpr helper cnd,
		      thn = substStmt helper thn,
		      els = substStmt helper els }

       | Ast.WithStmt {obj, ty, body} => 
	 Ast.WithStmt { obj = substExpr helper obj,
			ty = substTy helper ty,
			body = substStmt helper body }

       | Ast.TryStmt {block, catches, finally} => 
	 Ast.TryStmt {block = substBlock helper block,
		      catches = map (substCatchClause helper) catches,
		      finally = Option.map (substBlock helper) finally }

       | Ast.SwitchStmt { mode, cond, labels, cases } => 
	 let
	     val labels = map visitIdent labels
	 in
	     Ast.SwitchStmt { mode = mode,
			      cond = substExpr helper cond,
			      labels = labels,
			      cases = map (substCase helper) cases }
	 end

       | Ast.SwitchTypeStmt { cond, ty, cases } => 
	 Ast.SwitchTypeStmt { cond = substExpr helper cond,
			      ty = substTy helper ty,
			      cases = map (substCatchClause helper) cases }

       | Ast.DXNStmt {expr} => 
	 Ast.DXNStmt {expr = substExpr helper expr }
    end

and substCatchClause (helper:'a HELPER) 
		     (cc:Ast.CATCH_CLAUSE)
    : (Ast.CATCH_CLAUSE) = 
    let
	val { bindings, ty, rib, inits, block } = cc
	val rib = case rib of NONE => []
			    | SOME r => r
	val inits = case inits of NONE => []
				| SOME i => i
	val synthHead (* huh? *) = Ast.Head (rib, inits)
	val Ast.Head (rib, inits) = substHead helper synthHead
    in
	{ bindings = bindings, 
	  ty = substTy helper ty,
	  rib = SOME rib,
	  inits = SOME inits,
	  block = substBlock (withRib rib helper) block }
    end

and substCase (helper:'a HELPER) 
	      (c:Ast.CASE)
    : (Ast.CASE) = 
    let
	val { label, inits, body } = c
    in
	{ label = Option.map (substExpr helper) label,
	  inits = Option.map (substInits helper) inits,
	  body = substBlock helper body }
    end

and substTypes (helper:'a HELPER) 
	       (tes:Ast.TYPE_EXPR list)
    : (Ast.TYPE_EXPR list) = 
    map (substType helper) tes

and substExprs (helper:'a HELPER) 
	       (es:Ast.EXPR list)
    : (Ast.EXPR list) = 
    map (substExpr helper) es

and substCtor (helper:'a HELPER) 
	      (ctor:Ast.CTOR)
    : Ast.CTOR = 
    let
	val Ast.Ctor { settings, superArgs, func } = ctor 
    in
	(* FIXME: review this, it's quite subtle and probably wrong. *)
	Ast.Ctor { settings = substHead helper settings,
		   superArgs = substExprs helper superArgs,
		   func = substFunc helper func }
    end

and substFixture (helper:'a HELPER)
		 (name:Ast.FIXTURE_NAME, fixture:Ast.FIXTURE)
    : (Ast.FIXTURE_NAME * Ast.FIXTURE) =
    let
	val name' = case name of 
			Ast.TempName i => 
			Ast.TempName i
		      | Ast.PropName n => 
			Ast.PropName (substName helper n)
	val fixture' = case fixture of 
			   Ast.NamespaceFixture ns => 
			   Ast.NamespaceFixture (substNamespace helper ns)
			 | Ast.TypeFixture ty => 
			   Ast.TypeFixture (substTy helper ty)
			 | Ast.MethodFixture { func, ty, readOnly, 
					       override, final, abstract } => 
			   Ast.MethodFixture { func = substFunc helper func,
					       ty = substTy helper ty,
					       readOnly = readOnly,
					       override = override,
					       final = final,
					       abstract = abstract }
			 | Ast.ValFixture { ty, readOnly } => 
			   Ast.ValFixture { ty = substTy helper ty,
					    readOnly = readOnly }

			 | Ast.VirtualValFixture { ty, getter, setter } => 
			   Ast.VirtualValFixture { ty = substTy helper ty,
						   getter = Option.map (substFuncDefn helper) getter,
						   setter = Option.map (substFuncDefn helper) setter }
    in
	(name', fixture')
    end

and substRib (helper:'a HELPER) 
	     (rib:Ast.RIB) 
    : Ast.RIB = 
    map (substFixture helper) rib

and substHead (helper:'a HELPER) 
	      (head:Ast.HEAD) 
    : Ast.HEAD = 
    let
	val Ast.Head (rib, inits) = head
    in
	Ast.Head (substRib helper rib, 
		  substInits helper inits)
    end

and substIdentExpr (helper:'a HELPER) 
		   (ie:Ast.IDENT_EXPR)
    : Ast.IDENT_EXPR = 
    let
	val Helper { visitIdent, ... } = helper
    in
	case ie of
	    Ast.Identifier { ident, openNamespaces } => 
	    Ast.Identifier { ident = visitIdent ident,
			     openNamespaces = openNamespaces }
	    
	  | Ast.QualifiedExpression { qual, expr } => 
	    Ast.QualifiedExpression { qual = substExpr helper qual,
				      expr = substExpr helper expr }
	    
	  | Ast.AttributeIdentifier ie => 
	    Ast.AttributeIdentifier (substIdentExpr helper ie)
	    
	  | Ast.ExpressionIdentifier { expr, openNamespaces } => 
	    Ast.ExpressionIdentifier { expr = substExpr helper expr,
				       openNamespaces = openNamespaces }
	    
	  | Ast.QualifiedIdentifier { qual, ident } => 	
	    Ast.QualifiedIdentifier { qual = substExpr helper qual,
				      ident = visitIdent ident }
	    
	  | Ast.UnresolvedPath ( idents, ident ) => 
	    Ast.UnresolvedPath ( idents, substIdentExpr helper ident )
	    
	  | Ast.WildcardIdentifier => 
	    Ast.WildcardIdentifier
    end
	    
and substInit (helper:'a HELPER)
	      (name:Ast.FIXTURE_NAME, expr:Ast.EXPR)
    : (Ast.FIXTURE_NAME * Ast.EXPR) =
    let
	val name' = case name of 
			Ast.TempName i => 
			Ast.TempName i
		      | Ast.PropName n  => 
			Ast.PropName ( substName helper n ) 
	val expr' = substExpr helper expr 
    in
	(name', expr')
    end


and substName (helper:'a HELPER)
	      (n:Ast.NAME)
    : Ast.NAME = 
    let
	val { id, ns } = n
	val Helper { visitIdent, ... } = helper
    in
	{ id = visitIdent id,
	  ns = ns }
    end

			
and substInits (helper:'a HELPER)
	       (inits:Ast.INITS)
    : Ast.INITS = 
    map (substInit helper) inits

and substField (helper:'a HELPER)
	       (field:Ast.FIELD)
    : Ast.FIELD = 
    let
	val { kind, name, init } = field
    in
	{ kind = kind,
	  name = substIdentExpr helper name,
	  init = substExpr helper init }
    end
 
and substLit (helper:'a HELPER) 
	     (lit:Ast.LITERAL)
    : Ast.LITERAL = 
    case lit of 	
         Ast.LiteralNull => 
         Ast.LiteralNull

       | Ast.LiteralUndefined => 
	 Ast.LiteralUndefined

       | Ast.LiteralContextualDecimal str => 
	 Ast.LiteralContextualDecimal str

       | Ast.LiteralContextualDecimalInteger str => 
	 Ast.LiteralContextualDecimalInteger str 

       | Ast.LiteralContextualHexInteger str => 
	 Ast.LiteralContextualHexInteger str 

       | Ast.LiteralDouble r => 
	 Ast.LiteralDouble r

       | Ast.LiteralDecimal d => 
	 Ast.LiteralDecimal d 

       | Ast.LiteralInt i => 
	 Ast.LiteralInt i 

       | Ast.LiteralUInt u => 
	 Ast.LiteralUInt u

       | Ast.LiteralBoolean b => 
	 Ast.LiteralBoolean b

       | Ast.LiteralString s => 
	 Ast.LiteralString s

       | Ast.LiteralArray { exprs, ty } =>
	 Ast.LiteralArray { exprs = substExprs helper exprs,
			    ty = Option.map (substTy helper) ty }

       | Ast.LiteralXML exprs => 
	 Ast.LiteralXML (substExprs helper exprs)

       | Ast.LiteralNamespace ns => 
	 Ast.LiteralNamespace ns

       | Ast.LiteralObject { expr, ty } => 
	 Ast.LiteralObject { expr = map (substField helper) expr,
			     ty = Option.map (substTy helper) ty }

       | Ast.LiteralFunction func => 
	 Ast.LiteralFunction (substFunc helper func)

       | Ast.LiteralRegExp s => 
	 Ast.LiteralRegExp s

and substFunc (helper:'a HELPER) 
	      (func:Ast.FUNC)
    : Ast.FUNC = 
    let 
	val Helper { visitIdent, ... } = helper
	val Ast.Func { name={ kind, ident }, 
		       typeParams,
		       fsig, native, block, 
		       param, defaults, ty, loc } =  func
	val Helper { visitIdent, ... } = helper
	val ident = visitIdent ident
	(* FIXME: review this, it's quite subtle and probably wrong. *)
	val typeParams = map visitIdent typeParams
	val rib = typeParamsToRib typeParams
	val param = substHead (withRib rib helper) param
    in
	Ast.Func { name = {kind = kind, ident = ident},
		   typeParams = typeParams,
		   (* fsig should be reduced to param and defaults by the time we're called *)
		   fsig = fsig, 
		   native = native,
		   param = param,
		   block = substBlock (withHeadRib param helper) block,
		   defaults = substExprs (withRib rib helper) defaults,
		   ty = substTy helper ty,
		   loc = loc }
    end

and substFuncDefn (helper:'a HELPER) 
		  (defn:Ast.FUNC_DEFN)
    : Ast.FUNC_DEFN =
    let
	val { kind, ns, final, override,
              prototype, static, abstract, func } = defn
    in
	{ kind = kind,
	  ns = Option.map (substExpr helper) ns,
	  final = final,
	  override = override,
	  prototype = prototype,
	  static = static,
	  abstract = abstract,
	  func = substFunc helper func }
    end

and substNamespace (helper:'a HELPER)
		   (ns:Ast.NAMESPACE)
    : Ast.NAMESPACE =
    let
        val Helper { visitIdent, ... } = helper
    in	
	case ns of 
	    Ast.Intrinsic => Ast.Intrinsic
	  | Ast.OperatorNamespace => Ast.OperatorNamespace
	  | Ast.Private id => 
	    Ast.Private (visitIdent id)
	  | Ast.Protected id => 
	    Ast.Protected (visitIdent id)
	  | Ast.Public id => 
	    Ast.Public (visitIdent id)
	  | Ast.Internal id => 
	    Ast.Internal (visitIdent id)
	  | Ast.UserNamespace us => 
	    Ast.UserNamespace us
	  | Ast.AnonUserNamespace i => 
	    Ast.AnonUserNamespace i
	  | Ast.LimitedNamespace (id, ns) => 
	    Ast.LimitedNamespace (visitIdent id, 
				  substNamespace helper ns)
    end


and substPragma (helper:'a HELPER)
		(pragma:Ast.PRAGMA)
    : Ast.PRAGMA =
    let
        val Helper { visitIdent, ... } = helper
    in
	case pragma of 
	    Ast.UseNamespace e => 
	    Ast.UseNamespace (substExpr helper e)
	    
	  | Ast.UseDefaultNamespace e => 
	    Ast.UseDefaultNamespace (substExpr helper e)
	    
	  | Ast.UseNumber nt => 
	    Ast.UseNumber nt
	    
	  | Ast.UseRounding dm =>
	    Ast.UseRounding dm
	    
	  | Ast.UsePrecision i => 
	    Ast.UsePrecision i
	    
	  | Ast.UseStrict => Ast.UseStrict
			     
	  | Ast.UseStandard => Ast.UseStandard
			       
	  | Ast.Import {package, name, alias} => 
	    Ast.Import { package = map visitIdent package,
			 name = visitIdent name,
			 alias = Option.map visitIdent alias }
    end


and substBlock (helper:'a HELPER) 
	       (block:Ast.BLOCK)
    : Ast.BLOCK =
    let
	val Ast.Block {pragmas, defns, head, body, loc} = block
	val head = Option.map (substHead helper) head
	val helper = case head of 
			 NONE => helper
		       | SOME h => withHeadRib h helper
    in
	Ast.Block { pragmas = map (substPragma helper) pragmas,
		    defns = defns, 
		    head = head,
		    body = map (substStmt helper) body,
		    loc = loc }
    end

end
