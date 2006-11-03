structure Pretty = struct

exception UnimplementedException of string

structure PP = PPStreamFn(structure Token = StringToken 
                          structure Device = SimpleTextIODev)
open Ast

fun ppImportQual stream QualStar = PP.string stream "*"
  | ppImportQual stream (QualName n) = PP.string stream n

fun ppNumberType stream n = 
    PP.string stream 
    (case n of 
	 DECIMAL => "DECIMAL" | DOUBLE => "DOUBLE" | INT => "INT" | UINT => "UINT" | NUMBER => "NUMBER")

fun ppRoundingMode stream r =
    PP.string stream 
    (case r of 
	 CEILING => "CEILING" | FLOOR => "FLOOR" | UP => "UP" | DOWN => "DOWN" 
       | HALF_UP => "HALF_UP" | HALF_DOWN => "HALF_DOWN" | HALF_EVEN => "HALF_EVEN")

fun ppTriOp stream COND = PP.string stream "COND"

fun ppBinOp stream b = 
    PP.string stream 
    (case b of 
	 PLUS => "PLUS" | MINUS => "MINUS" | TIMES => "TIMES" | DIVIDE => "DIVIDE" | REMAINDER => "REMAINDER"
       | LEFT_SHIFT => "LEFT_SHIFT" | RIGHT_SHIFT => "RIGHT_SHIFT" | RIGHT_SHIFT_UNSIGNED => "RIGHT_SHIFT_UNSIGNED"
       | BITWISE_AND => "BITWISE_AND" | BITWISE_OR => "BITWISE_OR" | BITWISE_XOR => "BITWISE_XOR"
       | LOGICAL_AND => "LOGICAL_AND" | LOGICAL_OR => "LOGICAL_OR" | LOGICAL_XOR => "LOGICAL_XOR"
       | INSTANCEOF => "INSTANCEOF" | IS => "IS" | CAST => "CAST" | TO => "TO" | IN => "IN"
       | EQUALS => "EQUALS" | NOT_EQUALS => "NOT_EQUALS" | STRICT_EQUALS => "STRICT_EQUALS" | STRICT_NOT_EQUALS => "STRICT_NOT_EQUALS"
       | LESS => "LESS" | LESS_OR_EQUAL => "LESS_OR_EQUAL" | GREATER => "GREATER" | GREATER_OR_EQUAL => "GREATER_OR_EQUAL"
       | COMMA => "COMMA" | DEFVAR => "DEFVAR" | ASSIGN => "ASSIGN"
       | ASSIGN_PLUS => "ASSIGN_PLUS" | ASSIGN_MINUS => "ASSIGN_MINUS" | ASSIGN_TIMES => "ASSIGN_TIMES" | ASSIGN_DIVIDE => "ASSIGN_DIVIDE"
       | ASSIGN_REMAINDER => "ASSIGN_REMAINDER" | ASSIGN_LEFT_SHIFT => "ASSIGN_LEFT_SHIFT"
       | ASSIGN_RIGHT_SHIFT => "ASSIGN_RIGHT_SHIFT" | ASSIGN_RIGHT_SHIFT_UNSIGNED => "ASSIGN_RIGHT_SHIFT_UNSIGNED"
       | ASSIGN_BITWISE_AND => "ASSIGN_BITWISE_AND" | ASSIGN_BITWISE_OR => "ASSIGN_BITWISE_OR" | ASSIGN_BITWISE_XOR => "ASSIGN_BITWISE_XOR"
       | ASSIGN_LOGICAL_AND => "ASSIGN_LOGICAL_AND" | ASSIGN_LOGICAL_OR => "ASSIGN_LOGICAL_OR" | ASSIGN_LOGICAL_XOR => "ASSIGN_LOGICAL_XOR")

fun ppUnOp stream u =
    PP.string stream 
    (case u of 
	 DELETE => "DELETE" | VOID => "VOID" | TYPEOF => "TYPEOF" | PRE_INCREMENT => "PRE_INCREMENT" | PRE_DECREMENT => "PRE_DECREMENT"
       | POST_INCREMENT => "POST_INCREMENT" | POST_DECREMENT => "POST_DECREMENT" | UNARY_PLUS => "UNARY_PLUS" | UNARY_MINUS => "UNARY_MINUS" 
       | BITWISE_NOT => "BITWISE_NOT" | LOGICAL_NOT => "LOGICAL_NOT" | MAKE_NAMESPACE => "MAKE_NAMESPACE")

fun ppNullOp stream n = 
    PP.string stream 
    (case n of 
	 THIS => "THIS"
       | EMPTY => "EMPTY")

fun ppVarDefnTag stream v = 
    PP.string stream 
    (case v of 
	 CONST => "CONST" | VAR => "VAR" | LETVAR => "LETVAR" | LETCONST => "LETCONST")

fun ppBuiltinNamespace stream n = 
    PP.string stream 
    (case n of 
	 PUBLIC => "PUBLIC" | PRIVATE => "PRIVATE" | PROTECTED => "PROTECTED" | INTERNAL => "INTERNAL")

fun sexp stream func = 
    (PP.openBox stream;
     PP.string stream "(";
     func ();
     PP.string stream ")";
     PP.closeBox stream)

fun sexp1 stream head func = 
    (PP.openBox stream;
     PP.string stream "(";
     PP.string stream head;
     PP.space;
     func ();
     PP.string stream ")";
     PP.closeBox stream)

fun ppVisibility stream (Namespace (kind,id)) =
    sexp stream (fn _ => (PP.string stream "NAMESPACE";  (* todo: print kind *)
			  PP.space stream;
			  PP.string stream id))
 
fun ppPrimAnnotation stream a = 
    PP.string stream 
    (case a of 
	 NAMED => "NAMED" | NULLABLE => "NULLABLE" | NONNULLABLE => "NONNULLABLE")

fun ppSpecialTy stream t = 
    PP.string stream
    (case t of
	 ANY => "ANY" | NULL => "NULL" | UNDEFINED => "UNDEFINED" | NOTYPE => "NOTYPE")

fun bind stream = 
    ((fn _ => PP.space stream), 
     sexp stream, 
     sexp1 stream, 
     PP.string stream)

fun bindx stream ppFunc = 
    let 
	val f = ppFunc stream
	val fs = List.app (fn x => (PP.space stream; ppFunc stream x))
	fun f1 head x = sexp1 stream head (fn _ => ppFunc stream x)
	fun f1s head xs = sexp1 stream head 
				(fn _ => List.app 
					     (fn x => (PP.space stream; 
						       ppFunc stream x)) xs)
    in
	(f, fs, f1, f1s)
    end


fun ppDirective stream d = 
    let 
	val (sp, sx, sx1, str) = bind stream
    in
	sx
	(fn _ => 
	    case d of 			       
		UseNamespace es => (str "USE"; sp (); str "NAMESPACE"; sp (); 
				    List.app (fn e => (sp (); ppExpr stream e)) es)
				   
	      | UseNumber n => (str "USE"; sp (); str "NUMBER"; sp ();
				ppNumberType stream n)
			       
	      | UseRounding r => (str "USE"; sp(); str "ROUNDING"; sp ();
				  ppRoundingMode stream r)
				 
	      | Import {package, qualifier, alias} => 		
		(str "IMPORT";
		 sx (fn _ => (str "PACKAGE"; sp (); str package));
		 sx (fn _ => (str "QUALIFIER"; sp (); ppImportQual stream qualifier));
		 case alias of 
		     NONE => ()
		   | SOME a => sx (fn _ => (str "ALIAS"; sp(); str a))))
    end

and ppIdentOrExpr stream (Ident i) = 
    sexp1 stream "IDENT" (fn _ => PP.string stream i)

  | ppIdentOrExpr stream (Expr e) =
    sexp1 stream "EXPR" (fn _ => ppExpr stream e)
	

and ppExpr stream e = 
    let 
	val (sp, sx, sx1, str) = bind stream
	val (ex, exs, ex1, ex1s) = bindx stream ppExpr
    in
	case e of 
	    TrinaryExpr (t, e1, e2, e3) =>
	    (sx (fn _ => (ppTriOp stream t; exs [e1, e2, e3])))

	  | BinaryExpr (b, e1, e2) => 
	    (sx (fn _ => (ppBinOp stream b; exs [e1, e2])))

	  | UnaryExpr (u, e1) => 
	    (sx (fn _ => (ppUnOp stream u; exs [e1])))

	  | NullaryExpr n => ppNullOp stream n
			     
	  | YieldExpr y => ex1 "YIELD" y
(* todo	  | SuperExpr s => ex1s "SUPER" s   *)
	  | LiteralExpr lit => (sx1 "LITERAL" 
				    (fn _ => ppLiteral stream lit))
			       
	  | CallExpr {func, actuals} => 
	    (sx1 "CALL" (fn _ => ((ex1 "FUNC" func);
				  (ex1s "ACTUALS" actuals))))

	  | Property {indirect, obj, field} => 
	    (sx1 "PROPERTY" (fn _ => (if indirect 
				      then str "INDIRECT" 
				      else (); 
				      (ex1 "OBJ" obj);
				      (ex1 "FIELD" field))))

(*	  | QualIdent {qual, ident, opennss} => 
	    (sx1 "QUALIDENT" 
		 (fn _ => ((case qual of
				NONE => ()
			      | SOME qual => 
				(sx1 "QUAL" 
				     (fn _ => ppExpr stream qual)));
			   (sx1 "IDENT" 
				(fn _ => PP.string stream ident));
			   (case opennss of
				[] => ()
			      | x::xs => 
                    (sx1 "OPEN_NAMESPACES" 
				     (fn _ => List.app 
						  (fn n => (sp (); str n))
						  opennss))))))

*)	    
	  | AttrQualIdent { indirect, operand } => 
	    (sx1 "ATTRQUALIDENT" 
		 (fn _ => (if indirect 
			   then str "INDIRECT" 
			   else (); 
			   sx1 "OPERAND" 
			       (fn _ => ppIdentOrExpr stream operand))))

	  | LetExpr {defs, body} => 
	    (sx1 "LET" 
	     (fn _ => (List.app (ppVarDefn stream) defs;
		       ppExpr stream body)))

	  | NewExpr {obj, actuals} => 
	    (sx1 "NEW" (fn _ => ((ex1 "OBJ" obj);
				 (ex1s "ACTUALS" actuals))))
    end

and ppVarDefn stream v = 
    let 
	val (sp, sx, sx1, str) = bind stream
	val (ex, exs, ex1, ex1s) = bindx stream ppExpr
    in
	case v of
	    SimpleDefn {tag, init, attrs, name, ty} => 
	    (sx1 "SIMPLE_DEFN" 
	     (fn _ => ((sx1 "TAG" (fn _ => ppVarDefnTag stream tag));		       
		       (case init of 
			    NONE => ()
			  | SOME e => ex1 "INIT" e);
		       (sx1 "ATTRS" (fn _ => ppAttrs stream attrs));
		       (sx1 "NAME" (fn _ => str name));
		       (case ty of
			    NONE => ()
			  | SOME t => 
			    (sx1 "TYPE" (fn _ => ppType stream t))))))

	  | DestructuringDefn {tag, init, attrs, temp, postInit, names, ty} => 
	    (sx1 "DESTRUCTURING_DEFN" 
	     (fn _ => ((sx1 "TAG" (fn _ => ppVarDefnTag stream tag));		       
		       (case init of 
			    NONE => ()
			  | SOME e => ex1 "INIT" e);
		       (sx1 "ATTRS" (fn _ => ppAttrs stream attrs));
		       (sx1 "TEMP" (fn _ => str temp));
		       (sx1 "NAMES" (fn _ => List.app (fn n => (sp (); str n)) names));
		       (case postInit of 
			    NONE => ()
			  | SOME e => ex1 "POST_INIT" e);
		       (case ty of
			    NONE => ()
			  | SOME t => 
			    (sx1 "TYPE" (fn _ => ppType stream t))))))
    end

and ppFuncTy stream { paramTypes, 
		      returnType,
		      boundThisType,
		      hasRest } =
    let
	val (sp, sx, sx1, str) = bind stream
	val (ex, exs, ex1, ex1s) = bindx stream ppExpr
	val (ty, tys, ty1, ty1s) = bindx stream ppType
    in
	(sx1 "FUNCTY"
	     (fn _ => ((ty1s "PARAM_TYPES" paramTypes);
		       (ty1 "RETURN_TYPE" returnType);
		       (case boundThisType of
			    NONE => ()
			  | SOME bt => (ty1 "BOUND_THIS_TYPE" bt));
		       (if hasRest then str "HAS_REST" else ()))))
    end



and ppType stream t = 
    let 
	val (sp, sx, sx1, str) = bind stream
	val (ex, exs, ex1, ex1s) = bindx stream ppExpr
	val (ty, tys, ty1, ty1s) = bindx stream ppType

	fun ppPrimaryTy {name, annotation} = 
	    (sx1 "PRIMARYTY" 
		 (fn _ => ((sx1 "NAME" (fn _ => str name));
			   (sx1 "ANNOTATION" (fn _ => ppPrimAnnotation stream annotation)))))
    in
	case t of
	    SpecialType st => sx1 "SPECIALTY" (fn _ => ppSpecialTy stream st)
	  | UnionType ts => ty1s "UNION" ts
	  | ArrayType ts => ty1s "ARRAY" ts
	  | PrimaryType p => ppPrimaryTy p
	  | FunctionType fty => ppFuncTy stream fty
	  | RecordType fields => 
	    (sx1 "RECORDTY"
		 (fn _ => List.app 
			      (fn (e,t) => (sx1 "FIELD" 
						(fn _ => ((ex1 "NAME" e); (ty1 "TYPE" t))))) 
			      fields))
	    
	  | InstantiationType {base, params} =>
	    (sx1 "INSTANTIATION" 
	     (fn _ => ((sx1 "BASE" (fn _ => ppPrimaryTy base));
		       (ty1s "PARAMS" params))))
    end

and ppAttrs stream (Attributes { vis, override, 
				 static, final, dynamic,
				 prototype, nullable }) =
    let 
	val (sp, sx, sx1, str) = bind stream
	fun flag b f = if b then (sp(); str f) else ()
    in
	sx1 "ATTRIBUTES" (fn _ => 
			     ((ppVisibility stream vis);
			      (flag override "OVERRIDE");
			      (flag static "STATIC");
			      (flag final "FINAL");
			      (flag dynamic "DYNAMIC");
			      (flag prototype "PROTOTYPE");
			      (flag nullable "NULLABLE")))
    end

and ppLiteral stream lit = 
    let
	val (sp, sx, sx1, str) = bind stream
	val (ex, exs, ex1, ex1s) = bindx stream ppExpr
	fun bl b = if b then str "TRUE" else str "FALSE"		  
    in
	case lit of
	    LiteralNull => str "NULL"
	  | LiteralUndefined => str "UNDEFINED"
	  | LiteralNumber r => sx1 "REAL" (fn _ => str (Real.toString r))
	  | LiteralBoolean b => bl b
	  | LiteralString s => (sx1 "STRING" (fn _ => str s))
	  | LiteralArray a => ex1s "ARRAY" a
	  | LiteralXML x => ex1s "XML" x
	  | LiteralNamespace vis => (sx1 "BUILTIN_NAMESPACE" 
				       (fn _ => ppVisibility stream vis))
	  | LiteralObject fields => 
	    (sx1 "OBJECT" 
		 (fn _ => 
		     List.app 
			 (fn {name,init} => 
			     (sx1 "FIELD" 
				  (fn _ => 
				      ((ex1 "NAME" name);
				       (ex1 "INIT" init)))))
			 fields))

	  | LiteralRegExp {pattern, global, multiline, caseInsensitive} =>
	    (sx1 "REGEXP" (fn _ => ((sx1 "GLOBAL" (fn _ => bl global));
				    (sx1 "MULTILINE" (fn _ => bl multiline));
				    (sx1 "CASE_INSENSITIVE" (fn _ => bl caseInsensitive)))))
    end

and ppFormal stream {name, ty, init, isRest} = 
    let
	val (sp, sx, sx1, str) = bind stream
	val (ex, exs, ex1, ex1s) = bindx stream ppExpr
	val (_, _, ty1, ty1s) = bindx stream ppType
    in
	(sx1 "FORMAL" 
	 (fn _ => (str name;
		   (case ty of 
			NONE => ()
		      | SOME t => (ty1 "TYPE" t));
		   (case init of
			NONE => ()
		      | SOME e => (ex1 "INIT" e));
		   if isRest 
		   then str "IS_REST"
		   else ())))
    end

and ppFuncDefn stream f = 
    raise (UnimplementedException "unfinished")

and ppDefinition stream d = 
    let
	val (sp, sx, sx1, str) = bind stream
	val (ex, exs, ex1, ex1s) = bindx stream ppExpr
	val (vd, vds, vd1, vd1s) = bindx stream ppVarDefn
	val (fd, fds, fd1, fd1s) = bindx stream ppFuncDefn
	val (df, dfs, df1, df1s) = bindx stream ppDefinition
	val (st, sts, st1, st1s) = bindx stream ppStmt
	val (ty, tys, ty1, ty1s) = bindx stream ppType
    in
	case d of 
	    NamespaceDefn {name, init} => 
	    (sx1 "NAMESPACE_DEFN" (fn _ => (str name; (ex1 "INIT" init))))

	  | ClassDefn {name, attrs, params, extends, 
		       implements, instanceVars, vars, 
		       constructor, methods, initializer} => 

	    (sx1 "CLASS_DEFN" 
	     (fn _ => (str name;
		       ppAttrs stream attrs;
		       (sx1 "PARAMS" (fn _ => List.app (fn p => (sp (); str p)) params));
		       (ty1s "EXTENDS" extends);
		       (ty1s "IMPLEMENTS" implements);
		       (vd1s "INSTANCEVARS" instanceVars);
		       (vd1s "VARS" vars);
		       (fd1 "CONSTRUCTOR" constructor);
		       (fd1s "METHODS" methods);
		       (st1s "INITIALIZER" initializer))))

	  | _ => raise (UnimplementedException "unfinished")
    end

and ppBlock stream {directives, defns, stmts} = 
    let
	val (sp, sx, sx1, str) = bind stream
	val (df, dfs, df1, df1s) = bindx stream ppDefinition
	val (st, sts, st1, st1s) = bindx stream ppStmt
    in
	(sx1 "BLOCK_STMT"
	     (fn _ => 
		 ((sx1 "DIRECTIVES" (fn _ => List.app (ppDirective stream) directives));
		  (df1s "DEFINITIONS" defns);
		  (st1s "STATEMENTS" stmts)))) 
    end

and ppStmt stream s = 
    let
	val (sp, sx, sx1, str) = bind stream
	val (ex, exs, ex1, ex1s) = bindx stream ppExpr
	val (vd, vds, vd1, vd1s) = bindx stream ppVarDefn
	val (df, dfs, df1, df1s) = bindx stream ppDefinition
	val (st, sts, st1, st1s) = bindx stream ppStmt
	fun bl b = if b then str "TRUE" else str "FALSE"

	fun ppContLabel NONE = ()
	  | ppContLabel (SOME lbl) = (sx1 "CONT_LABEL" (fn _ => str lbl))
				   
	fun ppForEnumStmt head {isVar, init, obj, defns, contLabel, body} =
	    (sx1 head
		 (fn _ => ((sx1 "IS_VAR" (fn _ => bl isVar));
			   (ex1 "INIT" init);
			   (ex1 "OBJ" obj);
			   (vd1s "DEFNS" defns);
			   (ppContLabel contLabel);
			   (st1 "BODY" body))))
	    
	fun ppWhileStmt head {cond, body, contLabel} = 
	    (sx1 head 
		 (fn _ => ((ex1 "COND" cond);
			   (ppContLabel contLabel);
			   (st1 "BODY" body))))	    
    in
	case s of 
	    EmptyStmt => str "EMPTY_STMT"
	  | ExprStmt e => ex1 "EXPR_STMT" e
	  | DefineStmt d => vd1 "DEFINE_STMT" d
	  | ForEachStmt fe => ppForEnumStmt "FOREACH_STMT" fe
	  | ForInStmt fi => ppForEnumStmt "FORIN_STMT" fi
	  | ThrowStmt e => ex1 "THROW_STMT" e
	  | ReturnStmt e => ex1 "RETURN_STMT" e

	  | BreakStmt lab => 
	    (sx1 "BREAK_STMT"
	     (fn _ => case lab of 
			  NONE => ()
			| SOME lbl => str lbl))

	  | ContinueStmt lab => 
	    (sx1 "CONTINUE_STMT" 
		 (fn _ => case lab of 
			      NONE => ()
			    | SOME lbl => str lbl))

	  | BlockStmt (Block {defns=dfs,directives=drs,stmts=sts}) => ppBlock stream  {defns=dfs,directives=drs,stmts=sts}

	  | LabeledStmt (lab, stmt) => 
	    (sx1 "LABELED_STMT" (fn _ => (str lab; st stmt)))

	  | LetStmt (defs,Block {defns=dfs,directives=drs,stmts=sts}) => 
	    (sx1 "LET_STMT" 
		 (fn _ => ((vd1s "DEFINITIONS" defs);
			   (ppBlock stream {defns=dfs,directives=drs,stmts=sts}))))

	  | SuperStmt es => ex1s "SUPER_STMT" es
	  | WhileStmt w => ppWhileStmt "WHILE_STMT" w
	  | DoWhileStmt d => ppWhileStmt "DOWHILE_STMT" d

	  | ForStmt {isVar, defns, init, cond, update, contLabel, body} =>
	    (sx1 "FOR_STMT"
		 (fn _ => ((sx1 "IS_VAR" (fn _ => bl isVar));
			   (vd1s "DEFNS" defns);
			   (ex1 "INIT" init);
			   (ex1 "COND" cond);
			   (ex1 "UPDATE" update);
			   (ppContLabel contLabel);
			   (st1 "BODY" body))))

	  | IfStmt {cond, consequent, alternative} =>
	    (sx1 "IF_STMT" 
	     (fn _ => ((ex1 "COND" cond);
		       (st1 "CONSEQUENT" consequent);
		       (st1 "ALTERNATIVE" alternative))))

	  | WithStmt {obj, body} =>
	    (sx1 "WITH_STMT"
	     (fn _ => ((ex1 "OBJ" obj);
		       (st1 "BODY" body))))

	  | TryStmt {body=Block {defns=dfs,directives=drs,stmts=sts}, catches, finally=Block {defns=fdfs,directives=fdrs,stmts=fsts}} => 
	    (sx1 "TRY_STMT"
	     (fn _ => ((sx1 "BODY" (fn _ => ppBlock stream {defns=dfs,directives=drs,stmts=sts}));
		       (List.app (fn (formal,Block {defns=dfs,directives=drs,stmts=sts}) => 
				     (sx1 "CATCH" (fn _ => ((ppFormal stream formal);
							    (ppBlock stream {defns=dfs,directives=drs,stmts=sts})))))
				 catches);
		       (sx1 "FINALLY" (fn _ => ppBlock stream {defns=fdfs,directives=fdrs,stmts=fsts})))))

	  | SwitchStmt {cond, cases, default} => 
	    (sx1 "SWITCH_STMT" 
	     (fn _ => ((ex1 "COND" cond);
		       List.app 
		       (fn (e, stmts) => 
			   (sx1 "CASE" (fn _ => (ex e; sts stmts))))
		       cases;
		       (st1s "DEFAULT" default))))
    end

end
