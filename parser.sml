
structure Parser = struct

open Token

exception ParseError

datatype alpha =
    ALLOWLIST
  | NOLIST

datatype beta =
    ALLOWIN
  | NOIN

datatype omega =
    ABBREV
  | NOSHORTIF
  | FULL

fun log ss = 
    (TextIO.print "log: "; 
     List.app TextIO.print ss; 
     TextIO.print "\n")

val trace_on = false

fun trace ss =
	if trace_on then log ss else ()
(*

Identifier    
    Identifier
    ContextuallyReservedIdentifier
*)

fun identifier ts =
    let
    in case ts of
        Identifier(str) :: tr => (tr,str)
	  | Call :: tr => (tr,"call")
      | Debugger :: tr => (tr,"debugger")
      | Decimal :: tr => (tr,"decimal")
      | Double :: tr => (tr,"double")
      | Dynamic :: tr => (tr,"dynamic")
      | Each :: tr => (tr,"each")
      | Final :: tr => (tr,"final")
      | Get :: tr => (tr,"get")
      | Goto :: tr => (tr,"goto")
      | Include :: tr => (tr,"include")
      | Int :: tr => (tr,"int")
      | Namespace :: tr => (tr,"namespace")
      | Native :: tr => (tr,"native")
      | Number :: tr => (tr,"number")
      | Override :: tr => (tr,"override")
      | Prototype :: tr => (tr,"prototype")
      | Rounding :: tr => (tr,"rounding")
      | Standard :: tr => (tr,"standard")
      | Strict :: tr => (tr,"strict")
      | UInt :: tr => (tr,"uint")
      | Set :: tr => (tr,"set")
      | Static :: tr => (tr,"static")
      | Type :: tr => (tr,"type")
      | Xml :: tr => (tr,"xml")
      | Yield :: tr => (tr,"yield")
      | _ => raise ParseError
    end

(*
    PropertyIdentifier ->
        Identifier
        *
*)

and propertyIdentifier ts =
    let 
        val _ = trace([">> propertyIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        Mult :: tr => (tr,"*")
      | _ => 
            let
                val (ts1,nd1) = identifier ts
            in
                (trace(["<< propertyIdentifier with next=",tokenname(hd(ts1))]);(ts1,nd1)) 
            end
    end

(*
    Qualifier
        ReservedNamespace
        PropertyIdentifier
*)

and qualifier ts =
    let
    in case ts of
        (Internal :: _ | Intrinsic :: _ | Private :: _ | Protected :: _ | Public :: _) => 
          let
              val (ts1,nd1) = reservedNamespace (ts)
          in
              (ts1,Ast.LiteralExpr(nd1))
          end
      | _ => 
          let
              val (ts1,nd1) = propertyIdentifier (ts)
          in
              (ts1,Ast.Ref{base=NONE,ident=Ast.Identifier nd1})
          end
    end

and reservedNamespace ts =
    let val _ = trace([">> reservedNamespace with next=",tokenname(hd(ts))])
    in case ts of
        Internal :: tr => 
			(tr, Ast.LiteralNamespace(Ast.Internal("put package name here")))
      | Intrinsic :: tr => 
			(tr, Ast.LiteralNamespace(Ast.Intrinsic))
      | Private :: tr => 
			(tr, Ast.LiteralNamespace(Ast.Private))
      | Protected :: tr => 
			(tr, Ast.LiteralNamespace(Ast.Protected))
      | Public :: tr => 
			(tr, Ast.LiteralNamespace(Ast.Public("")))
      | _ => raise ParseError
    end

(*
	SimpleQualifiedIdentifier    
	    PropertyIdentifier
	    Qualifier  ::  PropertyIdentifier
	    Qualifier  ::  ReservedIdentifier
	    Qualifier  ::  Brackets

	ExpressionQualifiedIdentifer    
	    ParenListExpression  ::  PropertyIdentifier
	    ParenListExpression  ::  ReservedIdentifier
	    ParenListExpression  ::  Brackets

	left factored: 

	SimpleQualifiedIdentifier
    	ReservedNamespace :: QualifiedIdentifierPrime
	    PropertyIdentifier :: QualifiedIdentifierPrime
    	PropertyIdentifier

	ExpressionQualifiedIdentifer    
    	ParenListExpression  ::  QualifiedIdentifierPrime

	QualifiedIdentifierPrime
    	PropertyIdentifier
	    ReservedIdentifier
    	Brackets
*)

and simpleQualifiedIdentifier ts =
    let val _ = trace([">> simpleQualifiedIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        (Internal :: _ | Intrinsic :: _ | Private :: _ | Protected :: _ | Public :: _) => 
          let 
              val (ts1, nd1) = reservedNamespace(ts)
          in case ts1 of
              DoubleColon :: ts2 => qualifiedIdentifier'(ts2,Ast.LiteralExpr(nd1))
            | _ => raise ParseError
          end
      | _ => 
          	let
              	val (ts1, nd1) = propertyIdentifier(ts)
          	in case ts1 of
              	DoubleColon :: _ => 
					qualifiedIdentifier'(tl ts1,Ast.Ref ({base=NONE,ident=Ast.Identifier nd1}))
              | _ => 
					(trace(["<< simpleQualifiedIdentifier with next=",tokenname(hd(ts1))]);
					(ts1,Ast.Identifier nd1))
          end
    end

and expressionQualifiedIdentifier (ts) =
    let 
		val (ts1,nd1) = parenListExpression(ts)
    in case ts1 of
        DoubleColon :: ts2 => qualifiedIdentifier'(ts2,nd1)
      | _ => raise ParseError
    end

and reservedOrPropertyIdentifier ts =
    case isreserved(hd ts) of
        true => (tl ts, tokenname(hd ts))
      | false => propertyIdentifier(ts)

and qualifiedIdentifier' (ts1, nd1) : (token list * Ast.IDENT_EXPR) =
    let val _ = trace([">> qualifiedIdentifier' with next=",tokenname(hd(ts1))]) 
    in case ts1 of
        LeftBracket :: ts => 
			let
				val (ts2,nd2) = brackets (ts1)
                val (ts3,nd3) = (ts2,Ast.QualifiedExpression({qual=nd1,expr=nd2}))

			in
				(ts3,nd3)
			end
      | tk :: ts =>
            let
                val (ts2,nd2) = reservedOrPropertyIdentifier(ts1)
				val qid = Ast.QualifiedIdentifier({qual=nd1, ident=nd2})
                val (ts3,nd3) = (ts2,qid)
            in
                (ts3,nd3)
            end
	  | _ => raise ParseError
    end

(*
    NonAttributeQualifiedIdentifier    
        SimpleQualifiedIdentifier
        ExpressionQualifiedIdentifier
*)

and nonAttributeQualifiedIdentifier ts =
    let val _ = trace([">> nonAttributeQualifiedIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftParen :: _ => expressionQualifiedIdentifier(ts)
      | _ => simpleQualifiedIdentifier(ts)
    end

(*
    AttributeIdentifier    
        @  Brackets
        @  NonAttributeQualifiedIdentifier
*)

and attributeIdentifier ts =
    let val _ = trace([">> attributeIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        At :: LeftBracket :: _ => 
			let
				val (ts1,nd1) = brackets(tl ts)
			in
				(ts1,Ast.AttributeIdentifier (Ast.Expression nd1))
			end
      | At :: _ => 
			let
				val (ts1,nd1) = nonAttributeQualifiedIdentifier(tl ts)
			in
				(ts1,Ast.AttributeIdentifier nd1)
			end
      | _ => 
			raise ParseError
    end

(*

    QualifiedIdentifier    
        AttributeIdentifier
        NonAttributeQualifiedIdentifier

*)

and qualifiedIdentifier ts =
    let val _ = trace([">> qualifiedIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        At :: _ => attributeIdentifier(ts)
      | _ => nonAttributeQualifiedIdentifier(ts)
    end

(*
    SimpleTypeIdentifier    
        PackageIdentifier  .  Identifier
        NonAttributeQualifiedIdentifier
*)

and simpleTypeIdentifier ts =
    let val _ = trace([">> simpleTypeIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        PackageIdentifier :: Dot :: ts1 => 
            let val (ts2,nd2) = identifier(ts1) 
				val nd' = Ast.QualifiedIdentifier(
					       {qual=Ast.LiteralExpr(Ast.LiteralNamespace(Ast.Public("p"))),
						    ident=nd2})
			in 
				(ts2,nd') 
			end
      | _ => nonAttributeQualifiedIdentifier(ts)
    end

(*
    TypeIdentifier    
        SimpleTypeIdentifier
        SimpleTypeIdentifier  .<  TypeExpressionList  >
*)

and typeIdentifier ts =
    let val _ = trace([">> typeIdentifier with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = simpleTypeIdentifier ts
    in case ts1 of
        LeftDotAngle :: _ => 
            let
                val (ts2,nd2) = typeExpressionList (tl ts1)
            in case ts2 of
                RIGHTANGLE :: _ => (tl ts2,Ast.TypeIdentifier {ident=nd1,typeParams=nd2})
              | _ => raise ParseError
            end
      | _ => (ts1, nd1)        
    end

(*
    ParenExpression    
        (  AssignmentExpressionallowLet, allowIn  )
*)

and parenExpression ts =
    let val _ = trace([">> parenExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftParen :: ts1 => 
            let
                val (ts2,nd2:Ast.EXPR) = assignmentExpression (ts1,ALLOWLIST,ALLOWIN)
            in case ts2 of
                RightParen :: ts3 => (ts3,nd2)
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    ParenListExpression    
        (  ListExpression(ALLOWIN)  )
*)

and parenListExpression ts =
    let val _ = trace([">> parenListExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftParen :: ts1 => 
            let
                val (ts2,nd2) = listExpression (ts1,ALLOWIN)
            in case ts2 of
                RightParen :: ts3 => (ts3,nd2)
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
	FunctionExpression(allowList, b)	
		function  FunctionSignature  Block
		function  FunctionSignature  ListExpressionb
		function  Identifier  FunctionSignature  Block
		function  Identifier  FunctionSignature  ListExpressionb
		
	FunctionExpression(noList, b)
		function  FunctionSignature  Block
		function  Identifier  FunctionSignature  Block
*)

and functionExpression (ts,alpha,beta) =
    let val _ = trace([">> functionExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        Function :: ts1 => 
			let
			in case ts1 of
				(LeftDotAngle | LeftParen) :: _ => 
					let
						val (ts3,nd3) = functionSignature ts1
					in case (ts3,alpha) of
						(LeftBrace :: _,_) => 
							let
								val (ts4,nd4) = block ts3
							in
								(ts4,Ast.FunExpr {ident=NONE,sign=nd3,body=nd4})
							end
					  | (_,ALLOWLIST) => 
							let
								val (ts4,nd4) = listExpression (ts3,beta)
							in
								(ts4,Ast.FunExpr{ident=NONE,sign=nd3,
									body=Ast.Block { pragmas=[],defns=[],stmts=[Ast.ReturnStmt nd4] }})

							end
					  | _ => raise ParseError
					end
		      | _ => 
					let
						val (ts2,nd2) = identifier ts1
						val (ts3,nd3) = functionSignature ts2
					in case (ts3,alpha) of
						(LeftBrace :: _,_) => 
							let
								val (ts4,nd4) = block ts3
							in
								(ts4,Ast.FunExpr {ident=SOME nd2,sign=nd3,body=nd4})
							end
					  | (_,ALLOWLIST) => 
							let
								val (ts4,nd4) = listExpression (ts3,beta)
							in
								(ts4,Ast.FunExpr{ident=SOME nd2,sign=nd3,
									body=Ast.Block { pragmas=[],defns=[],stmts=[Ast.ReturnStmt nd4] }})

							end
					  | _ => raise ParseError
					end
			end
      | _ => raise ParseError
    end

(*
    FunctionSignature
        .<  TypeParameterList  > (  Parameters  )  ResultType
        (  Parameters  )  ResultType

    re-factored:

    FunctionSignature
        .<  TypeParameterList  > FunctionSignaturePrime
        FunctionSignaturePrime

    FunctionSignaturePrime
        (  Parameters  )  ResultType
*)

and functionSignature ts =
    let val _ = trace([">> functionSignature with next=",tokenname(hd(ts))]) 
		fun functionSignature' (ts, nd1) : (token list * Ast.FUNC_SIGN) =
            case ts of
                LeftParen :: ts1 =>
                   	let
                       	val (ts2, nd2) = parameters ts1
                   	in case ts2 of
                       	RightParen :: tsx =>
                           	let
                               	val (ts3,nd3) = resultType tsx
                           	in
								(log(["functionSignature with next=",tokenname(hd ts3)]);
                                (ts3,Ast.FunctionSignature {typeparams=nd1,params=nd2,resulttype=nd3}))
                           	end
                      | _ => raise ParseError
                   	end
              	| _ => raise ParseError
    in case ts of
        LeftDotAngle :: ts1 => 
            let
                val (ts2,nd2) = typeParameterList ts1
            in
                let
                in case ts2 of
                    GreaterThan :: ts3 => functionSignature' (ts3, nd2)
                  | _ => raise ParseError
                end
            end
      | _ => functionSignature' (ts, [])
    end

(*
    TypeParametersList    
        Identifier
        Identifier  ,  TypeParameterList

    left factored:

    TypeParameterList
        Identifier TypeParameterListPrime

    TypeParameterListPrime
        «empty»
        ,  Identififier  TypeParameterListPrime
*)

and typeParameterList (ts) : token list * string list =
    let val _ = trace([">> typeParameterList with next=",tokenname(hd(ts))]) 
		fun typeParameterList' (ts, lst) =
	    	let
    		in case ts of
        		Comma :: _ =>
           			let
               			val (ts1,nd1) = identifier(tl ts)
               			val (ts2,nd2) = typeParameterList' (ts1,nd1::lst)
           			in
             			(ts2,nd2)
           			end
			  | _ => (ts,lst)
    end
        val (ts1,nd1) = identifier ts
        val (ts2,nd2) = typeParameterList' (ts1,nd1::nil)
    in
        (ts2,nd2)
    end

(*
    Parameters    
        «empty»
        NonemptyParameters(ALLOWLIST)

    NonemptyParameters    
        ParameterInit
        ParameterInit  ,  NonemptyParameters
        RestParameter
*)

and parameters ts =
    let val _ = trace([">> parameters with next=",tokenname(hd(ts))]) 
		fun nonemptyParameters ts = 
			let
			in case ts of
				TripleDot :: _ => 
					let
						val (ts1,nd1) = restParameter ts
					in case ts1 of
						RightParen :: _ => (ts1,nd1::[])
					  | _ => raise ParseError
					end					
			  | _ => 
					let
						val (ts1,nd1) = parameterInit ts
					in case ts1 of
						RightParen :: _ => (ts1,nd1::[])
					  | Comma :: ts2 =>
							let
								val (ts3,nd3) = nonemptyParameters ts2
							in
								(ts3,nd1::nd3)
							end
					  | _ => raise ParseError
					end
			end
	in case ts of 
		RightParen :: ts1 => (ts,[])
	  | _ => nonemptyParameters ts
	end

(*
    ParameterInit
        Parameter
        Parameter  =  NonAssignmentExpression(ALLOWIN)
*)

and parameterInit ts = 
    let val _ = trace([">> parameterInit with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = parameter ts
	in case ts1 of
		Assign :: ts2 => 
			let
				val (ts3,nd3) = nonAssignmentExpression (ts2,NOLIST,ALLOWIN)
			in 
				raise ParseError (* fixme: (ts3,{name=name,ty=ty,tag=tag,init=SOME nd3,isRest=false})*)
			end
	  | _ => (ts1,nd1)
	end

(*
    Parameter    
        ParameterAttributes TypedIdentifier(ALLOWIN)
        ParameterAttributes TypedPattern

    ParameterAttributes    
        «empty»
        const
*)

and parameter (ts) : token list * Ast.FORMAL =
    let val _ = trace([">> parameter with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = parameterAttributes (ts)
	in case ts1 of
		(LeftBracket | LeftParen) :: _ => 
			let
				val (ts2,{ptrn,ty}) = typedPattern (ts1)
			in
				(ts2,{pattern=ptrn,ty=ty,tag=nd1,isRest=false,init=NONE})
			end
	  | _ => 
			let
				val (ts2:token list,{p,t}) = typedIdentifier (ts1)
			in
				(ts2,{pattern=p,ty=t,tag=nd1,isRest=false,init=NONE})
			end			
	end

and parameterAttributes (ts) = 
    let val _ = trace([">> parameterAttributes with next=",tokenname(hd(ts))]) 
	in case ts of 
		Const :: ts1 => (ts1,Ast.Const)
	  | ts1 => (ts1,Ast.Var)
	end

(*
    RestParameter    
        ...
        ...  ParameterAttributes TypedIdentifier
        ...  ParameterAttributes TypedPattern
*)

and restParameter (ts:token list) = 
    let val _ = trace([">> restParameter with next=",tokenname(hd(ts))]) 
	in case ts of
		DOTDOTDOT :: _ =>
			let
			in case tl ts of
				RightParen :: _ => 
					(tl ts,{pattern=Ast.SimplePattern(Ast.LiteralExpr(Ast.LiteralString(""))),
							  ty=NONE,tag=Ast.Var,isRest=true,init=NONE}) 
			  | _ =>
					let
						val (ts1:token list,{pattern,ty,tag,...}) = parameter (tl ts)
					in
						(ts1,{pattern=pattern,ty=ty,tag=tag,isRest=true,init=NONE})
					end
			end
	  | _ => raise ParseError
	end

(*
	TypedIdentifier
    	Identifier
	    Identifier  :  TypeExpressionx
*)

and typedIdentifier (ts) =
    let val _ = trace([">> typedIdentifier with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = identifier ts
		val rf = Ast.Ref({base=NONE,ident=Ast.Identifier nd1})
	in case ts1 of
		Colon :: _ => 
			let
				val (ts2,nd2) = typeExpression (tl ts1)
			in
				(ts2, {p=Ast.SimplePattern(rf),t=SOME nd2})
			end
	  | _ => 
			let
			in
				(trace(["<< typedIdentifier with next=",tokenname(hd(ts1))]);
				(ts1, {p=Ast.SimplePattern(rf),t=NONE}))
			end
		
	end

(*
	TypedPattern    
    	Pattern
	    Pattern  :  TypeExpression
*)

and typedPattern ts =
	let
		val (ts1,nd1) = pattern ts
	in case ts of 
		Colon :: _ =>
			let
				val (ts2,nd2) = typeExpression (tl ts)
			in
				trace(["<< typedPattern with next=",tokenname(hd ts2)]);
				(ts2,{ptrn=nd1,ty=SOME nd2})
			end
	  | _ => raise ParseError
	end
    
(*
    ResultType
        «empty»
        :  void
        :  TypeExpression
*)

and resultType ts = 
    let val _ = trace([">> resultType with next=",tokenname(hd(ts))]) 
    in case ts of
        Colon :: Void :: ts1 => (ts1,Ast.SpecialType(Ast.NoType))
      | Colon :: _ => 
			let
				val (ts1,nd1) = typeExpression (tl ts)
			in
				log(["<< resultType with next=",tokenname(hd ts1)]);
				(ts1,nd1)
			end
	  | ts1 => (ts1,Ast.SpecialType(Ast.Undefined))
    end

(*
    ObjectLiteral    
        {  FieldList  }
        {  FieldList  }  :  RecordType
*)

and objectLiteral ts = 
    let val _ = trace([">> objectLiteral with next=",tokenname(hd(ts))]) 
	in case ts of
		LeftBrace :: _ => 
			let
				val (ts1,nd1) = fieldList (tl ts)
			in case ts1 of
				RightBrace :: Colon :: _ => 
					let
						val (ts2,nd2) = recordType (tl (tl ts1))
					in
						(ts2,Ast.LiteralObject {expr=nd1,ty=SOME nd2})
					end
			  | RightBrace :: _ => 
					(tl ts1,Ast.LiteralObject {expr=nd1,ty=NONE})
			  | _ => raise ParseError
			end
	  | _ => raise ParseError
	end

(*
    FieldList    
        «empty»
        NonemptyFieldList(allowLet)

    NonemptyFieldList(alpha)
        LiteralField(alpha)
        LiteralField(noLet)  ,  NonemptyFieldList(alpha)
*)

and fieldList ts =
    let val _ = trace([">> fieldList with next=",tokenname(hd(ts))]) 
		fun nonemptyFieldList (ts) =
			let
				val (ts1,nd1) = literalField(ts)
			in case ts1 of
				Comma :: ts2 => 
					let
						val (ts3,nd3) = nonemptyFieldList (ts2)
					in
						(ts3,nd1::nd3)
					end
			  | _ => (ts1,nd1::[])
			end
	in case ts of
		RightBrace :: ts1 => (ts1,[])
	  | _ => 
		let
			val (ts1,nd1) = nonemptyFieldList (ts)
		in
			(ts1,nd1)
 		end
	end

(*
    LiteralField    
        FieldName  :  AssignmentExpression(NOLIST, ALLOWIN)
		get  Identifier  FunctionCommon
		set  Identifier  FunctionCommon

    FieldName    
        NonAttributeQualifiedIdentifier
        StringLiteral
        NumberLiteral
        ReservedIdentifier
*)

and literalField (ts) =
    let val _ = trace([">> literalField with next=",tokenname(hd(ts))]) 
	in case ts of
		Get :: _ =>
			let
				val (ts1,nd1) = fieldName (tl ts)
				val (ts2,nd2) = functionCommon (ts1)
			in
				(ts2,{name=nd1,init=nd2})
			end
	  | Set :: _ =>
			let
				val (ts1,nd1) = fieldName (tl ts)
				val (ts2,nd2) = functionCommon (ts1)
			in
				(ts2,{name=nd1,init=nd2})
			end
	  | _ => 
			let
				val (ts1,nd1) = fieldName ts
			in case ts1 of
				Colon :: _ =>
					let
						val (ts2,nd2) = assignmentExpression (tl ts1,NOLIST,ALLOWIN)
					in
						(ts2,{name=nd1,init=nd2})
					end
			  | _ => raise ParseError
			end
	end

and fieldName ts =
    let val _ = trace([">> fieldName with next=",tokenname(hd(ts))]) 
	in case ts of
		StringLiteral s :: ts1 => (ts1,Ast.LiteralExpr(Ast.LiteralString(s)))
	  | NumberLiteral n :: ts1 => (ts1,Ast.LiteralExpr(Ast.LiteralNumber(n)))
	  | _ => 
			let
				val (ts1,nd1) = nonAttributeQualifiedIdentifier (ts)
			in
				(ts1,Ast.Ref{base=NONE,ident=nd1})
			end
	end

and functionCommon ts =
    let val _ = trace([">> functionCommon with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = functionSignature ts
    in case ts1 of
        LeftBrace :: _ => 
            let
                val (ts2,nd2) = block ts1
            in
                (ts2,Ast.FunExpr {ident=NONE,sign=nd1,body=nd2})
            end
      | _ => raise ParseError
    end

(*
    ArrayLiteral    
        [  ElementList(ALLOWLIST)  ]
        [  ElementList(ALLOWLIST)  ]  :  ArrayType
*)

and arrayLiteral (ts) : (token list * Ast.EXPR) =
    let val _ = trace([">> arrayLiteral with next=",tokenname(hd(ts))]) 
	in case ts of
		LeftBracket :: _ => 
			let
				val (ts1,nd1) = elementList (tl ts)
			in case ts1 of
				RightBracket :: Colon :: _ => 
					let
						val (ts2,nd2) = arrayType (tl (tl ts1))
					in
						(ts2,Ast.LiteralExpr(Ast.LiteralArray {expr=nd1,ty=SOME nd2}))
					end
			  | RightBracket :: _ => 
					(tl ts1,Ast.LiteralExpr(Ast.LiteralArray {expr=nd1,ty=NONE}))
			  | _ => raise ParseError
			end
	  | _ => raise ParseError
	end

(*
    ElementList
        «empty»
        LiteralElement
        ,  ElementList
        LiteralElement  ,  ElementList

    LiteralElement
        AssignmentExpression(NOLIST, ALLOWIN)
*)

and elementList (ts) =
    let val _ = trace([">> elementList with next=",tokenname(hd(ts))]) 
	in case ts of
		RightBracket :: _ => (ts,[])
	  | Comma :: _ => 
			let
				val (ts1,nd1) = elementList (tl ts)
			in
				(ts1,Ast.LiteralExpr(Ast.LiteralUndefined) :: nd1)
			end
	  | _ =>
			let
				val (ts1,nd1) = assignmentExpression (ts,NOLIST,ALLOWIN)
			in case ts1 of
				Comma :: _ =>
					let
						val (ts2,nd2) = elementList (tl ts1)
					in
						(ts2,nd1::nd2)
					end
			  | _ => (ts1,nd1::[])
			end
	end

(*
    XMLInitialiser    
        XMLMarkup
        XMLElement
        <  >  XMLElementContent  </  >

    XMLElementContent    
        XMLMarkup  XMLElementContentopt
        XMLText  XMLElementContentopt
        XMLElement  XMLElementContentopt
        {  ListExpressionallowIn  }  XMLElementContentopt
    
    XMLElement    
        <  XMLTagContent  XMLWhitespaceopt  />
        <  XMLTagContent  XMLWhitespaceopt  >  XMLElementContent
                  </  XMLTagName  XMLWhitespaceopt  >
    
    XMLTagContent    
        XMLTagName  XMLAttributes
    
    XMLTagName    
        {  ListExpressionallowIn  }
        XMLName
    
    XMLAttributes    
        XMLWhitespace  {  ListExpressionallowIn  }
        XMLAttribute  XMLAttributes
        «empty»
    
    XMLAttribute    
        XMLWhitespace  XMLName  XMLWhitespaceopt  =  XMLWhitespaceopt  {  ListExpressionallowIn  }
        XMLWhitespace  XMLName  XMLWhitespaceopt  =  XMLWhitespaceopt  XMLAttributeValue
    
    XMLElementContent    
        {  ListExpressionallowIn  }  XMLElementContent
        XMLMarkup  XMLElementContent
        XMLText  XMLElementContent
        XMLElement  XMLElementContent
        «empty»
*)

(*
	PrimaryExpression(alpha,beta)
    	null
	    true
    	false
	    NumberLiteral
    	StringLiteral
	    this
    	RegularExpression
	    TypeIdentifier
    	AttributeIdentifier
	    XMLInitialiser
    	ParenListExpression
	    ArrayLiteral
    	ObjectLiteral
	    FunctionExpression(alpha,beta)
*)

and primaryExpression (ts,alpha,beta) =
    let val _ = trace([">> primaryExpression with next=",tokenname(hd ts)])
    in case ts of
        Null :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralNull))
      | True :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean true))
      | False :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean false))
      | NumberLiteral n :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralNumber n))
      | StringLiteral s :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralString s))
      | This :: ts1 => (ts1, Ast.NullaryExpr Ast.This)
      | At :: _ => 
			let
				val (ts1,nd1) = attributeIdentifier ts
			in
				(ts1,Ast.Ref {base=NONE,ident=nd1})
			end
      | LeftParen :: _ => parenListExpression ts
      | LeftBracket :: _ => arrayLiteral ts
      | LeftBrace :: _ => let val (ts1,nd1) = objectLiteral ts in (ts1,Ast.LiteralExpr nd1) end
      | Function :: _ => functionExpression (ts,alpha,beta)
	  | LexBreakDiv x :: _ => 
			let 
				val ts1 = (#lex_regexp x)()
			in case ts1 of
				RegExp :: _ => 
					let 
					in case ts1 of
						RegexpLiteral str :: _ =>
							(tl ts1,Ast.LiteralExpr(Ast.LiteralRegExp {str=str}))
					  | _ => raise ParseError
					end
			  | _ => raise ParseError
			end
(* todo:
      | (XmlMarkup | LessThan ) :: _ => xmlInitializer ts
*)
(* what's this?      | Eol :: ts1 => primaryExpression (ts1,alpha,beta) *)
      | _ => 
			let
				val (ts1,nd1) = typeIdentifier ts
			in
				(ts1,Ast.Ref {base=NONE,ident=nd1})
			end
    end

(*
	SuperExpression    
    	super
	    super  ParenExpression
*)

and superExpression ts =
    let val _ = trace([">> superExpression with next=",tokenname(hd(ts))]) 
	in case ts of
		Super :: _ =>
			let
		    in case tl ts of
        		LeftParen :: _ => 
		            let
	       		        val (ts1,nd1) = parenExpression(tl (tl ts))
		            in
						(ts1,Ast.SuperExpr(SOME(nd1)))
		            end
    		    | _ => 
					(tl ts,Ast.SuperExpr(NONE))
			end
	  | _ => raise ParseError
    end

(*
    MemberExpression    
        PrimaryExpression
        new  MemberExpression  Arguments
        SuperExpression  PropertyOperator
        MemberExpression  PropertyOperator

    Refactored:

    MemberExpression :    
        PrimaryExpression MemberExpressionPrime
        new MemberExpression Arguments MemberExpressionPrime
        SuperExpression  PropertyOperator  MemberExpressionPrime
    
    MemberExpressionPrime :    
        PropertyOperator MemberExpressionPrime
        «empty»
*)

and memberExpression (ts,alpha,beta) =
    let val _ = trace([">> memberExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        New :: _ =>
            let
                val (ts1,nd1) = memberExpression(tl ts,alpha,beta)
                val (ts2,nd2) = arguments(ts1)
                val (ts3,nd3) = memberExpressionPrime(ts2,Ast.CallExpr {func=nd1,actuals=nd2},alpha,beta)
            in
                (ts3,nd3)
            end
      | Super :: _ =>
            let
                val (ts2,nd2) = superExpression(ts)
                val (ts3,nd3) = propertyOperator(ts2,nd2)
                val (ts4,nd4) = memberExpressionPrime(ts3,nd3,alpha,beta)
               in
                (ts4,nd4)
            end
      | _ =>
            let
                val (ts3,nd3) = primaryExpression(ts,alpha,beta)
                val (ts4,nd4) = memberExpressionPrime(ts3,nd3,alpha,beta)
               in
                (trace(["<< memberExpression with next=",tokenname(hd ts4)]);(ts4,nd4))
            end
    end

and memberExpressionPrime (ts,nd,alpha,beta) =
    let val _ = trace([">> memberExpressionPrime with next=",tokenname(hd(ts))])
    in case ts of
        (LeftBracket :: _ | Dot :: _) =>
            let
                val (ts2,nd2) = propertyOperator(ts,nd)
            in
                memberExpressionPrime(ts2, nd2,alpha,beta)
            end
      | _ => (ts,nd)
    end

(*
    CallExpression    
        MemberExpression  Arguments
        CallExpression  Arguments
        CallExpression  PropertyOperator

    refactored:

    CallExpression :    
        MemberExpression Arguments CallExpressionPrime
    
    CallExpressionPrime :    
        Arguments CallExpressionPrime
        PropertyOperator CallExpressionPrime
        «empty»
*)

and callExpression (ts,alpha,beta) =
    let val _ = trace([">> callExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = memberExpression(ts,alpha,beta)
        val (ts2,nd2) = arguments(ts1)
    in 
        callExpressionPrime(ts2,Ast.CallExpr({func=nd1,actuals=nd2}),alpha,beta)
    end

and callExpressionPrime (ts,nd1,alpha,beta) =
    let val _ = trace([">> callExpressionPrime with next=",tokenname(hd(ts))])
    in case ts of
        (LeftBracket :: ts1 | Dot :: ts1) =>
            let
                val (ts2,nd2) = propertyOperator(ts1,nd1)
            in
                memberExpressionPrime(ts2, nd2,alpha,beta)
            end
      | LeftParen :: _ => 
            let
                val (ts2,nd2) = arguments(ts)
            in
                memberExpressionPrime(ts2,Ast.CallExpr({func=nd1,actuals=nd2}),alpha,beta)
            end
      | ts1 => (ts1,nd1)
    end

(*
    Arguments
        (  )
        (  ArgumentList(ALLOWLIST)  )
*)

and arguments (ts) : (token list * Ast.EXPR list)  =
    let val _ = trace([">> arguments with next=",tokenname(hd(ts))]) 
	in case ts of
        LeftParen :: RightParen :: ts1 => (ts1,[]) 
      | LeftParen :: ts1 => 
			let
		        val (ts2,nd2) = argumentList(ts1)
			in case ts2 of
				RightParen :: ts3 => (ts3,nd2)
			  | _ => raise ParseError
			end
	  | _ => raise ParseError
    end

(*
    ArgumentList
        AssignmentExpression(NOLIST, ALLOWIN)
        ArgumentList  ,  AssignmentExpression(NOLIST, ALLOWIN)

    refactored:

    ArgumentList
        AssignmentExpression(NOLIST,ALLOWIN) ArgumentListPrime

    ArgumentListPrime
        «empty»
        , AssignmentExpression(NOLIST,ALLOWIN) ArgumentListPrime
*)

and argumentList (ts) : (token list * Ast.EXPR list)  =
    let val _ = trace([">> argumentList with next=",tokenname(hd(ts))])
		fun argumentList' (ts) : (token list * Ast.EXPR list) =
		    let val _ = trace([">> argumentList with next=",tokenname(hd(ts))])
		    in case ts of
		        Comma :: _ => 
        		    let
                		val (ts1,nd1) = assignmentExpression(tl ts,NOLIST,ALLOWIN)
		                val (ts2,nd2) = argumentList'(ts1)
        		    in
                		(ts2,nd1::nd2)
		            end
		      | _ => (ts,[])
		    end
        val (ts1,nd1) = assignmentExpression(ts,NOLIST,ALLOWIN)
        val (ts2,nd2) = argumentList'(ts1)
    in
        (ts2,nd1::nd2)
    end

(*

PropertyOperator    
    .. QualifiedIdentifier
    .  ReservedIdentifier
    .  QualifiedIdentifier
    .  ParenListExpression
    .  ParenListExpression  ::  PropertyIdentifier
    .  ParenListExpression  ::  ReservedIdentifier
    .  ParenListExpression  ::  Brackets
    Brackets
    
*)

and propertyOperator (ts, nd) =
    let val _ = trace([">> propertyOperator with next=",tokenname(hd(ts))]) 
    in case ts of
        Dot :: ts1 =>
            let
            in case ts1 of
                LeftParen :: _ =>
                    let
                        val (ts2,nd2) = parenListExpression(ts1)
					in case ts2 of
						DoubleColon:: BRACKET :: _ => 
               			    let
                                val (ts4,nd4) = brackets(tl ts2)
   			                in
               			        (ts4,Ast.Ref {base=SOME nd,ident=Ast.Expression(nd4)})
                            end
					  | DoubleColon :: ts3 => 
                            let
                                val (ts4,nd4) = reservedOrPropertyIdentifier(ts3)
                            in
                                (ts4,Ast.Ref({base=SOME nd,ident=Ast.Identifier(nd4)}))
                            end
					  | _ => raise ParseError
                    end
              | _ => 
                    let
                        val (ts4,nd4) = reservedOrPropertyIdentifier(ts1)
                    in
                        (ts4,Ast.Ref({base=SOME nd,ident=Ast.Identifier(nd4)}))
                    end
            end
      | LeftBracket :: _ => 
            let
                val (ts4,nd4) = brackets(ts)
            in
                (ts4,Ast.Ref({base=SOME nd,ident=Ast.Expression(nd4)}))
            end
      | _ => raise ParseError
    end

(*

Brackets    
    [  ]
    [  ListExpression(allowIn)  ]
    [  ListExpression(allowIn)  :  ListExpression(allowIn)  ]
    
*)

and brackets (ts) =
    let val _ = trace([">> brackets with next=",tokenname(hd(ts))]) 
    in case ts of
		LeftBracket :: ts' =>
			let
				val (ts1,nd1) = listExpression (ts',ALLOWIN)
			in case ts1 of
				Colon :: ts'' => 
					let
						val (ts2,nd2) = listExpression (ts'',ALLOWIN)
					in case ts2 of
						RightBracket :: ts'' => (ts'',nd1) 
							(* fixme: need an ast for slice *)
					  | _ => raise ParseError
					end
			  | RightBracket :: ts'' => (ts'',nd1) 
			  | _ => raise ParseError
			end
	  | _ => raise ParseError
	end

(*

NewExpression    
    MemberExpression
    new  NewExpression
    
*)

and newExpression (ts,alpha,beta) =
    let val _ = trace([">> newExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        New :: ts1 =>
            let
                val (ts2,nd2) = newExpression(ts1,alpha,beta)
            in
                (ts2,Ast.NewExpr({obj=nd2,actuals=[]}))
            end
      | _ => memberExpression(ts,alpha,beta)
    end

(*
    LeftHandSideExpression    
        NewExpression
        CallExpression
    
    refactored:

    LeftHandSideExpression    
        new NewExpression
        MemberExpression Arguments CallExpressionPrime
        MemberExpression
*)

and leftHandSideExpression (ts,alpha,beta) =
    let val _ = trace([">> leftHandSideExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        New :: ts1 =>
            let
                val (ts2,nd2) = newExpression(ts1,alpha,beta)
            in
                (ts2,Ast.NewExpr({obj=nd2,actuals=[]}))
            end
      | _ =>
            let
                val (ts2,nd2) = memberExpression(ts,alpha,beta)
            in case ts2 of
                LeftParen :: _ =>
                    let
                        val (ts3,nd3) = arguments(ts2)
                        val (ts4,nd4) = callExpressionPrime(ts3,Ast.CallExpr {func=nd2,actuals=nd3},alpha,beta)
                    in
						(trace(["<< leftHandSideExpression with next=",tokenname(hd(ts4))]);(ts4,nd4))
                    end
              | _ => (trace(["<< leftHandSideExpression with next=",tokenname(hd(ts))]);(ts2,nd2))
            end
    end

(*

PostfixExpression    
    LeftHandSideExpression
    LeftHandSideExpression  [no line break]  ++
    LeftHandSideExpression  [no line break]  --
    
*)

and postfixExpression (ts,alpha,beta) =
    let val _ = trace([">> postfixExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = leftHandSideExpression(ts,alpha,beta)
    in case ts1 of
		PlusPlus :: ts2 => (ts2,Ast.UnaryExpr(Ast.PostIncrement,nd1))
	  | MinusMinus :: ts2 => (ts2,Ast.UnaryExpr(Ast.PostDecrement,nd1))
	  | _ => (trace(["<< postfixExpression"]);(ts1,nd1))
    end

(*

UnaryExpression    
    PostfixExpression
    delete  PostfixExpression
    void  UnaryExpression
    typeof  UnaryExpression
    ++   PostfixExpression
    --  PostfixExpression
    +  UnaryExpression
    -  UnaryExpression
    ~  UnaryExpression
    !  UnaryExpression
    type TypeExpression
    
*)

and unaryExpression (ts,alpha,beta) =
    let val _ = trace([">> unaryExpression with next=",tokenname(hd(ts))]) 
    in case ts of
		Delete :: ts1 => 
			let 
				val (ts2,nd2) = postfixExpression (ts1,alpha,beta) 
			in 
				(ts2,Ast.UnaryExpr(Ast.Delete,nd2)) 
			end
	  | Void :: ts1 => 
			let 
				val (ts2,nd2) = unaryExpression (ts1,alpha,beta) 
			in 
				(ts2,Ast.UnaryExpr(Ast.Void,nd2)) 
			end
	  | TypeOf :: ts1 => 
			let 
				val (ts2,nd2) = unaryExpression (ts1,alpha,beta) 
			in 
				(ts2,Ast.UnaryExpr(Ast.Typeof,nd2)) 
			end
	  | PlusPlus :: ts1 => 
			let 
				val (ts2,nd2) = postfixExpression (ts1,alpha,beta) 
			in 
				(ts2,Ast.UnaryExpr(Ast.PreIncrement,nd2)) 
			end
	  | MinusMinus :: ts1 => 
			let 
				val (ts2,nd2) = postfixExpression (ts1,alpha,beta) 
			in 
				(ts2,Ast.UnaryExpr(Ast.PreDecrement,nd2)) 
			end
	  | Plus :: ts1 => 
			let 
				val (ts2,nd2) = unaryExpression (ts1,alpha,beta) 
			in 
				(ts2,Ast.UnaryExpr(Ast.UnaryPlus,nd2)) 
			end
	  | BitwiseNot :: ts1 => 
			let 
				val (ts2,nd2) = unaryExpression (ts1,alpha,beta) 
			in 
				(ts2,Ast.UnaryExpr(Ast.BitwiseNot,nd2)) 
			end
	  | Not :: ts1 => 
			let 
				val (ts2,nd2) = unaryExpression (ts1,alpha,beta) 
			in 
				(ts2,Ast.UnaryExpr(Ast.LogicalNot,nd2)) 
			end
	  | Type :: ts1 => 
			let 
				val (ts2,nd2) = typeExpression (ts1)
			in 
				(ts2,Ast.TypeExpr(nd2)) 
			end
	  | _ => 
			postfixExpression (ts,alpha,beta)
    end
    
(*

MultiplicativeExpression    
    UnaryExpression
    MultiplicativeExpression  *  UnaryExpression
    MultiplicativeExpression  /  UnaryExpression
    MultiplicativeExpression  %  UnaryExpression

right recursive:

MultiplicativeExpression	
	UnaryExpression MultiplicativeExpressionPrime
	
MultiplicativeExpression'	
	*  UnaryExpression MultiplicativeExpressionPrime
	/  UnaryExpression MultiplicativeExpressionPrime
	%  UnaryExpression MultiplicativeExpressionPrime
	empty
    
*)

and multiplicativeExpression (ts,alpha,beta) =
    let val _ = trace([">> multiplicativeExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = unaryExpression (ts,alpha,beta)
		fun multiplicativeExpression' (ts1, nd1,alpha,beta) =
			case ts1 of
				Mult :: ts2 => 
					let 
						val (ts3,nd3) = unaryExpression (ts2,alpha,beta) 
					in 
						multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Times,nd1,nd3),alpha,beta) 
					end

			  | LexBreakDiv x :: _ =>
					let	
					in case (#lex_initial x)() of
						Div :: ts2 => 
							let 
								val (ts3,nd3) = unaryExpression (ts2,alpha,beta) 
            				in 
								multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Divide,nd1,nd3),alpha,beta) 
							end
					  | _ => raise ParseError
					end

			  | Modulus :: ts2 => 
					let 
						val (ts3,nd3) = unaryExpression (ts2,alpha,beta) 
					in 
						multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Remainder,nd1,nd3),alpha,beta) 
					end
			  | _ => (trace(["<< multiplicative"]);(ts1,nd1))
    in
        multiplicativeExpression' (ts1,nd1,alpha,beta)
    end

(*

AdditiveExpression    
    MultiplicativeExpression
    AdditiveExpression  +  MultiplicativeExpression
    AdditiveExpression  -  MultiplicativeExpression

right recursive: (see pattern of MultiplicativeExpression)

*)

and additiveExpression (ts,alpha,beta) =
    let val _ = trace([">> additiveExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = multiplicativeExpression (ts,alpha,beta)
		fun additiveExpression' (ts1, nd1,alpha,beta) =
			case ts1 of
				Plus :: ts2 => 
					let 
						val (ts3,nd3) = multiplicativeExpression (ts2,alpha,beta) 
					in 
						additiveExpression' (ts3,Ast.BinaryExpr(Ast.Plus,nd1,nd3),alpha,beta) 
					end
			  | Minus :: ts2 => 
					let 
						val (ts3,nd3) = multiplicativeExpression (ts2,alpha,beta) 
					in 
						additiveExpression' (ts3,Ast.BinaryExpr(Ast.Minus,nd1,nd3),alpha,beta) 
					end
			  | _ => 
					(trace(["<< additiveExpression"]);
					(ts1,nd1))
    in
        additiveExpression' (ts1,nd1,alpha,beta)
    end

(*

	ShiftExpression    
	    AdditiveExpression
	    ShiftExpression  <<  AdditiveExpression
	    ShiftExpression  >>  AdditiveExpression
    	ShiftExpression  >>>  AdditiveExpression
    
	right recursive: (see pattern of MultiplicativeExpression)

*)

and shiftExpression (ts,alpha,beta) =
    let val _ = trace([">> shiftExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = additiveExpression (ts,alpha,beta)
		fun shiftExpression' (ts1,nd1,alpha,beta) =
			case ts1 of
				LeftShift :: ts2 => 
					let 
						val (ts3,nd3) = additiveExpression (ts2,alpha,beta) 
					in 
						shiftExpression' (ts3,Ast.BinaryExpr(Ast.LeftShift,nd1,nd3),alpha,beta) 
					end
			  | RightShift :: ts2 => 
					let 
						val (ts3,nd3) = additiveExpression (ts2,alpha,beta)
					in 
						shiftExpression' (ts3,Ast.BinaryExpr(Ast.RightShift,nd1,nd3),alpha,beta) 
					end
			  | UnsignedRightShift :: ts2 => 
					let 
						val (ts3,nd3) = additiveExpression (ts2,alpha,beta) 
					in 
						shiftExpression' (ts3,Ast.BinaryExpr(Ast.RightShiftUnsigned,nd1,nd3),alpha,beta) 
					end
			  | _ => (trace(["<< shiftExpression"]);(ts1,nd1))
    in
        shiftExpression' (ts1,nd1,alpha,beta)
    end

(*
	RelationalExpression(ALLOWIN)
    	ShiftExpression
	    RelationalExpressionallowIn  <  ShiftExpression
    	RelationalExpressionallowIn  >  ShiftExpression
	    RelationalExpressionallowIn  <=  ShiftExpression
    	RelationalExpressionallowIn  >=  ShiftExpression
	    RelationalExpressionallowIn  in  ShiftExpression
    	RelationalExpressionallowIn  instanceof  ShiftExpression
	    RelationalExpressionallowIn  is  TypeExpression
	    RelationalExpressionallowIn  to  TypeExpression
	    RelationalExpressionallowIn  cast  TypeExpression

	RelationalExpression(NOIN)
    	ShiftExpression
	    RelationalExpressionallowIn  <  ShiftExpression
    	RelationalExpressionallowIn  >  ShiftExpression
	    RelationalExpressionallowIn  <=  ShiftExpression
    	RelationalExpressionallowIn  >=  ShiftExpression
    	RelationalExpressionallowIn  instanceof  ShiftExpression
	    RelationalExpressionallowIn  is  TypeExpression
	    RelationalExpressionallowIn  to  TypeExpression
	    RelationalExpressionallowIn  cast  TypeExpression

	right recursive: (see pattern of MultiplicativeExpression)

*)

and relationalExpression (ts,alpha, beta)=
    let val _ = trace([">> relationalExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = shiftExpression (ts,alpha,beta)
		fun relationalExpression' (ts1,nd1,alpha,beta) =
			case (ts1,beta) of
				(LessThan :: ts2,_) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,alpha,beta) 
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.Less,nd1,nd3),alpha,ALLOWIN) 
					end
			  | (GreaterThan :: ts2,_) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,alpha,beta)
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.Greater,nd1,nd3),alpha,ALLOWIN) 
					end
			  | (LessThanOrEquals :: ts2, _) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,alpha,beta) 
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.LessOrEqual,nd1,nd3),alpha,ALLOWIN) 
					end
			  | (GreaterThanOrEquals :: ts2, _) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,alpha,beta) 
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.GreaterOrEqual,nd1,nd3),alpha,ALLOWIN) 
					end
			  | (In :: ts2, ALLOWIN) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,alpha,beta) 
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.In,nd1,nd3),alpha,ALLOWIN) 
					end
			  | (InstanceOf :: ts2, _) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,alpha,beta) 
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.InstanceOf,nd1,nd3),alpha,ALLOWIN) 
					end
			  | (Is :: ts2, _) => 
					let 
						val (ts3,nd3) = typeExpression (ts2) 
					in 
						relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.Is,nd1,nd3),alpha,ALLOWIN) 
					end
			  | (To :: ts2, _) => 
					let 
						val (ts3,nd3) = typeExpression (ts2) 
					in 
						relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.To,nd1,nd3),alpha,ALLOWIN) 
					end
			  | (Cast :: ts2, _) => 
					let 
						val (ts3,nd3) = typeExpression (ts2) 
					in 
						relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.Cast,nd1,nd3),alpha,ALLOWIN) 
					end
			  | (_,_) => 
					(trace(["<< relationalExpression"]);(ts1,nd1))
    in
        relationalExpression' (ts1,nd1,alpha,beta)
    end

(*
	EqualityExpression(beta)
    	RelationalExpression(beta)
	    EqualityExpression(beta)  ==  RelationalExpression(beta)
	    EqualityExpression(beta)  !=  RelationalExpression(beta)
	    EqualityExpression(beta)  ===  RelationalExpression(beta)
	    EqualityExpression(beta)  !==  RelationalExpression(beta)

	right recursive: (see pattern of MultiplicativeExpression)

*)

and equalityExpression (ts,alpha,beta)=
    let val _ = trace([">> equalityExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = relationalExpression (ts,alpha,beta)
		fun equalityExpression' (ts1,nd1) =
			case ts1 of
				Equals :: ts2 => 
					let 
						val (ts3,nd3) = relationalExpression (ts2,alpha,beta) 
					in 
						equalityExpression' (ts3,Ast.BinaryExpr(Ast.Equals,nd1,nd3)) 
					end
			  | NotEquals :: ts2 => 
					let 
						val (ts3,nd3) = relationalExpression (ts2,alpha,beta) 
					in 
						equalityExpression' (ts3,Ast.BinaryExpr(Ast.NotEquals,nd1,nd3)) 
					end
			  | StrictEquals :: ts2 => 
					let 
						val (ts3,nd3) = relationalExpression (ts2,alpha,beta) 
					in 
						equalityExpression' (ts3,Ast.BinaryExpr(Ast.StrictEquals,nd1,nd3)) 
					end
			  | StrictNotEquals :: ts2 => 
					let 
						val (ts3,nd3) = relationalExpression (ts2,alpha,beta) 
					in 
						equalityExpression' (ts3,Ast.BinaryExpr(Ast.StrictNotEquals,nd1,nd3)) 
					end
			  | _ => 
					(trace(["<< equalityExpression"]);(ts1,nd1))
		
    in
        equalityExpression' (ts1,nd1)
    end

(*
	BitwiseAndExpression(beta)    
    	EqualityExpression(beta)
	    BitwiseAndExpressionr(beta)  &  EqualityExpression(beta)

	right recursive: (see pattern of MultiplicativeExpression)
*)

and bitwiseAndExpression (ts,alpha,beta)=
    let val _ = trace([">> bitwiseAndExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = equalityExpression (ts,alpha,beta)
		fun bitwiseAndExpression' (ts1,nd1) =
			case ts1 of
				BitwiseAnd :: ts2 => 
					let 
						val (ts3,nd3) = equalityExpression (ts2,alpha,beta) 
					in 
						bitwiseAndExpression' (ts3,Ast.BinaryExpr(Ast.BitwiseAnd,nd1,nd3)) 
					end
			  | _ => (trace(["<< bitwiseAnd"]);(ts1,nd1))
		
    in
        bitwiseAndExpression' (ts1,nd1)
    end

(*
	BitwiseXorExpressionb    
    	BitwiseAndExpressionb
	    BitwiseXorExpressionb  ^  BitwiseAndExpressionb

	right recursive: (see pattern of MultiplicativeExpression)
*)

and bitwiseXorExpression (ts,alpha,beta)=
    let val _ = trace([">> bitwiseXorExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = bitwiseAndExpression (ts,alpha,beta)
		fun bitwiseXorExpression' (ts1,nd1) =
			case ts1 of
				BitwiseXor :: ts2 => 
					let 
						val (ts3,nd3) = bitwiseAndExpression (ts2,alpha,beta) 
					in 
						bitwiseXorExpression' (ts3,Ast.BinaryExpr(Ast.BitwiseXor,nd1,nd3)) 
					end
			  | _ => (trace(["<< bitwiseXor"]);(ts1,nd1))
    in
        bitwiseXorExpression' (ts1,nd1)
    end

(*
	BitwiseOrExpressionb    
    	BitwiseXorExpressionb
	    BitwiseOrExpressionb  |  BitwiseXorExpressionb
*)

and bitwiseOrExpression (ts,alpha,beta)=
    let val _ = trace([">> bitwiseOrExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = bitwiseXorExpression (ts,alpha,beta)
		fun bitwiseOrExpression' (ts1,nd1) =
			case ts1 of
				BitwiseOr :: ts2 => 
					let 
						val (ts3,nd3) = bitwiseXorExpression (ts2,alpha,beta) 
					in 
						bitwiseOrExpression' (ts3,Ast.BinaryExpr(Ast.BitwiseOr,nd1,nd3)) 
					end
			  | _ => (trace(["<< bitwiseAnd"]);(ts1,nd1))
		
    in
        bitwiseOrExpression' (ts1,nd1)
    end

(*
	LogicalAndExpressionb    
    	BitwiseOrExpressionb
	    LogicalAndExpressionb  &&  BitwiseOrExpressionb
*)

and logicalAndExpression (ts,alpha,beta)=
    let val _ = trace([">> logicalAndExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = bitwiseOrExpression (ts,alpha,beta)
		fun logicalAndExpression' (ts1,nd1) =
			case ts1 of
				LogicalAnd :: ts2 => 
					let 
						val (ts3,nd3) = bitwiseOrExpression (ts2,alpha,beta) 
					in 
						logicalAndExpression' (ts3,Ast.BinaryExpr(Ast.LogicalAnd,nd1,nd3)) 
					end
			  | _ => 
					(trace(["<< logicalAndExpression"]);
					(ts1,nd1))
		
    in
        logicalAndExpression' (ts1,nd1)
    end

(*
	LogicalXorExpressionb    
    	LogicalAndExpressionb
	    LogicalXorExpressionb  ^^  LogicalAndExpressionb
*)

and logicalXorExpression (ts,alpha,beta) =
    let val _ = trace([">> logicalXorExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = logicalAndExpression (ts,alpha,beta)
		fun logicalXorExpression' (ts1,nd1) =
			case ts1 of
				LogicalXor :: ts2 => 
					let 
						val (ts3,nd3) = logicalAndExpression (ts2,alpha,beta) 
					in 
						logicalXorExpression' (ts3,Ast.BinaryExpr(Ast.LogicalXor,nd1,nd3)) 
					end
			  | _ => 
					(trace(["<< logicalXorExpression"]);
					(ts1,nd1))
		
    in
        logicalXorExpression' (ts1,nd1)
    end

(*
	LogicalOrExpressionb    
    	LogicalXorExpressionb
	    LogicalOrExpressionb  ||  LogicalXorExpressionb

*)

and logicalOrExpression (ts,alpha,beta) =
    let val _ = trace([">> logicalOrExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = logicalXorExpression (ts,alpha,beta)
		fun logicalOrExpression' (ts1,nd1) =
			case ts1 of
				LogicalXor :: ts2 => 
					let 
						val (ts3,nd3) = logicalXorExpression (ts2,alpha,beta) 
					in 
						logicalOrExpression' (ts3,Ast.BinaryExpr(Ast.LogicalXor,nd1,nd3)) 
					end
			  | _ => 
					(trace(["<< logicalOrExpression"]);
					(ts1,nd1))
		
    in
        logicalOrExpression' (ts1,nd1)
    end

(*
	ConditionalExpression(ALLOWLIST, beta)    
	    LetExpression(beta)
    	YieldExpression(beta)
	    LogicalOrExpression(beta)
    	LogicalOrExpression(beta)  ?  AssignmentExpression(ALLOWLIST,beta)  :  AssignmentExpression(ALLOWLIST,beta)


	ConditionalExpression(NOLIST, beta)    
    	SimpleYieldExpression
	    LogicalOrExpression(beta)
    	LogicalOrExpression(beta)  ?  AssignmentExpression(ALLOWLIST,beta) :  AssignmentExpression(NOLIST,beta)
    
*)

and conditionalExpression (ts,ALLOWLIST,beta) =
    let val _ = trace([">> conditionalExpression with next=",tokenname(hd(ts))])
    in case ts of
        Let :: _ => letExpression(ts,beta)
	  | Yield :: _ => yieldExpression(ts,beta)
      | _ => 
			let
				val (ts2,nd2) = logicalOrExpression(ts,ALLOWLIST,beta)
			in case ts2 of
				QuestionMark :: ts3 => 
					let
						val (ts4,nd4) = assignmentExpression(ts3,ALLOWLIST,beta)
					in case ts4 of
						Colon :: ts5 =>
							let
								val (ts6,nd6) = assignmentExpression(ts5,ALLOWLIST,beta)
							in
								(ts6,nd6)
							end
					  | _ => raise ParseError							
					end
			  | _ => 
					(ts2,nd2)
			end
		end
 
  | conditionalExpression (ts,NOLIST,beta) =
    let val _ = trace([">> conditionalExpression with next=",tokenname(hd(ts))])
    in case ts of
	    Yield :: _ => yieldExpression(ts,beta)
      | _ => 
			let
				val (ts2,nd2) = logicalOrExpression(ts,NOLIST,beta)
			in case ts2 of
				QuestionMark :: ts3 => 
					let
						val (ts4,nd4) = assignmentExpression(ts3,NOLIST,beta)
					in case ts4 of
						Colon :: ts5 =>
							let
								val (ts6,nd6) = assignmentExpression(ts5,NOLIST,beta)
							in
								(ts6,nd6)
							end
					  | _ => raise ParseError							
					end
			  | _ => 
					(ts2,nd2)
			end
		end

(*

*)

(*

NonAssignmentExpression(allowList, beta)   
    LetExpression(beta)
    YieldExpression(beta)
    LogicalOrExpression(beta)
    LogicalOrExpression(beta)  ?  NonAssignmentExpression(allowLet, beta)  
							   :  NonAssignmentExpression(allowLet, beta)
    
NonAssignmentExpression(noList, beta)    
    SimpleYieldExpression
    LogicalOrExpression(beta)
    LogicalOrExpression(beta)  ?  NonAssignmentExpression(allowLet, beta)  
							   :  NonAssignmentExpression(noLet, beta)

*)

and nonAssignmentExpression (ts,ALLOWLIST,beta) = raise ParseError
  | nonAssignmentExpression (ts,NOLIST,beta) = raise ParseError

(*
	LetExpression(beta)    
    	let  (  LetBindingList  )  ListExpression(beta)
*)

and letExpression (ts,beta) =
    let val _ = trace([">> letExpression with next=",tokenname(hd(ts))])
	in case ts of
		Let :: LeftParen :: ts1 => 
			let
				val (ts2,nd2) = letBindingList (ts1)
			in 
			case ts2 of
				RightParen :: ts3 =>
					let
				        val (ts4,nd4) = listExpression(ts3,beta)
					in
						(ts4,Ast.LetExpr{defs=nd2,body=nd4})
					end
 			  |	_ => raise ParseError
			end
	  | _ => raise ParseError
	end

(*
	LetBindingList    
    	«empty»
	    NonemptyLetBindingList

	NonemptyLetBindingList    
    	VariableBindingallowIn
	    VariableBindingallowIn  ,  NonemptyLetBindingList
*)

and letBindingList (ts) =
    let val _ = trace([">> letBindingList with next=",tokenname(hd(ts))]) 
		fun nonemptyLetBindingList ts = 
			let
				val (ts1,nd1) = variableBinding (ts,Ast.LetVar,NOLIST,ALLOWIN)
			in case ts1 of
				RightParen :: _ => (ts1,nd1::[])
			  | Comma :: ts2 =>
					let
						val (ts3,nd3) = nonemptyLetBindingList ts2
					in
						(ts3,nd1::nd3)
					end
			  | _ => raise ParseError
			end
	in case ts of 
		RightParen :: ts1 => (ts,[])
	  | _ => nonemptyLetBindingList ts
	end

(*
	YieldExpressionb    
    	yield
	    yield  [no line break]  ListExpressionb
*)

and yieldExpression (ts,beta) =
	case ts of
		Yield :: ts1 => 
			let
			in case ts1 of
				(SemiColon :: _ | RightBrace :: _ | RightParen :: _) => (ts1,Ast.YieldExpr NONE)
			  | _ => 
					let
						val (ts2,nd2) = listExpression(ts1,beta)
					in
						(ts2,Ast.YieldExpr(SOME nd2))
					end
			end
	  | _ => raise ParseError

(*
	SimpleYieldExpression    
    	yield
*)

and simpleYieldExpression ts =
	case ts of
		Yield :: ts1 => (ts1,Ast.YieldExpr NONE)
	  | _ => raise ParseError

(*

AssignmentExpression (a,b)    
    ConditionalExpression (a,b)
    PostfixExpression  =  AssignmentExpression(a,b)
    PostfixExpression  CompoundAssignment  AssignmentExpression (a,b)
    PostfixExpression  LogicalAssignment  AssignmentExpression (a,b)
    TypedPattern  =  AssignmentExpression (a,b)
    
*)

and assignmentExpression (ts,a,b) :(token list * Ast.EXPR) = 
    let val _ = trace([">> assignmentExpression with next=",tokenname(hd(ts))]) 
    in case ts of
		LeftBrace :: _ => 
			let
				val (ts1,nd1) = objectLiteral ts
			in case ts1 of
				Assign :: _ => 
					let
						val (ts2,nd2) = assignmentExpression(tl ts1,a,b)						
					in

					    (trace(["<< assignmentExpression with next=",tokenname(hd(ts2))]);
						(ts2,Ast.BinaryExpr(Ast.Assign,Ast.LiteralExpr nd1,nd2)))

					end
			  | _ => (ts1,Ast.LiteralExpr nd1)
			end
	  | _ => 
	    	let
				val (ts1,nd1) = conditionalExpression(ts,a,b)
			in case (ts1,nd1) of
				(Assign :: ts2, Ast.Ref {...}) => 
					let
						val (ts3,nd3) = assignmentExpression(ts2,a,b)						
					in case ts1 of
						Assign :: _ => (ts3,Ast.BinaryExpr(Ast.Assign,nd1,nd3))
					  | _ => 
						(trace(["<< assignmentExpression"]);
						(ts1,nd1))
					end
			  | (Assign :: ts2, _) => raise ParseError (* does not have proper lhs expression *)
			  | _ => (trace(["<< assignmentExpression with next=",tokenname(hd(ts1))]);(ts1,nd1))
			end

    end

(*
	CompoundAssignment    
    	*=
	    /=
    	%=
	    +=
    	-=
	    <<=
    	>>=
	    >>>=
    	&=
	    ^=
    	|=
*)

(*

LogicalAssignment    
    &&=
    ||=
    
*)

	and logicalAssignment ts =
    	raise ParseError

(*
	ListExpression(beta)    
    	AssignmentExpression(beta)
	    ListExpression(beta)  ,  AssignmentExpression(beta)

	right recursive:

	ListExpression(beta)    
    	AssignmentExpression(beta) ListExpressionPrime(beta)
    
	ListExpressionPrime(beta)    
    	«empty»
	    , AssignmentExpression(beta) ListExpressionPrime(beta)
*)

and listExpression (ts,beta) = 
    let
		fun listExpressionPrime (ts,nd1,beta) =
	    	let
		    in case ts of
    		    Comma :: ts1 =>
					let
            			val (ts2,nd2) = assignmentExpression(ts1,ALLOWLIST,beta)
	               		val (ts3,nd3) = listExpressionPrime(ts2,nd2,beta)
	    	      	in
    	    	     	(ts3, nd1 :: nd3)
	    	      	end
	    	  | _ => (ts, nd1 :: [])
		    end
        val (ts1,nd1) = assignmentExpression(ts,ALLOWLIST,beta)
        val (ts2,nd2) = listExpressionPrime(ts1,nd1,beta)
    in
        (ts2, Ast.ListExpr nd2)
    end

(*
	Pattern    
    	ObjectPattern
	    ArrayPattern
*)

and pattern ts =
	let
	in case ts of
		LeftBrace :: _ => objectPattern ts
	  | LeftBracket :: _ => arrayPattern ts
	  | _ => raise ParseError
	end

(*
	ObjectPattern    
    	{  DestructuringFieldList  }
*)

and objectPattern ts =
	let
	in case ts of
		LeftBrace :: ts =>
			let
				val (ts1,nd1) = destructuringFieldList ts
			in case ts1 of
				RightBrace :: _ => (tl ts1,Ast.ObjectPattern nd1)
			  | _ => raise ParseError
			end
	  | _ => raise ParseError
	end

(*
	DestructuringFieldList    
    	DestructuringField
	    DestructuringFieldList  ,  DestructuringField
*)

and destructuringFieldList ts =
	let
		fun destructuringFieldList' ts =
			let
			in case ts of
				Comma :: _ =>
					let
						val (ts1,nd1) = destructuringField (tl ts)
						val (ts2,nd2) = destructuringFieldList' (ts1)
					in case ts of
					    _ => (ts2,nd1::nd2)
					end
			  | _ => (ts,[])
			end
		val (ts1,nd1) = destructuringField ts
		val (ts2,nd2) = destructuringFieldList' ts1
	in
		(ts2,nd1::nd2)
	end

(*
	DestructuringField    
    	NonAttributeQualifiedIdentifier  :  Pattern
	    NonAttributeQualifiedIdentifier  :  PostfixExpression
*)

and destructuringField ts =
	let
		val (ts1,nd1) = nonAttributeQualifiedIdentifier ts
	in case ts1 of
		Colon :: (LeftBrace | LeftBracket) :: _ => 
			let
				val (ts2,nd2) = pattern (tl ts1)
			in
				(ts2,{name=Ast.Ref {base=NONE,ident=nd1},ptrn=nd2})
			end
	  | Colon :: _ => 
			let
				val (ts2,nd2) = postfixExpression (tl ts1,NOLIST,ALLOWIN)
			in
				(ts2,{name=Ast.Ref {base=NONE,ident=nd1},ptrn=Ast.SimplePattern nd2})
			end
	  | _ => raise ParseError
	end

(*
	ArrayPattern    
    	[  DestructuringElementList  ]
*)

and arrayPattern ts =
	let
	in case ts of
		LeftBracket :: _ => 
			let
				val (ts1,nd1) = destructuringElementList (tl ts)
			in case ts1 of
				RightBracket :: _ => (tl ts1,Ast.ArrayPattern nd1)
			  | _ => raise ParseError
			end
	  | _ => raise ParseError
	end

(*
	DestructuringElementList    
    	«empty»
	    DestructuringElement
    	, DestructuringElementList
	    DestructuringElement , DestructuringElementList
*)

and destructuringElementList (ts) =
    let val _ = trace([">> destructuringElementList with next=",tokenname(hd(ts))]) 
	in case ts of
		RightBracket :: _ => (ts,[])
	  | Comma :: _ => 
			let
				val (ts1,nd1) = destructuringElementList (tl ts)
			in
				(ts1,Ast.SimplePattern(Ast.LiteralExpr(Ast.LiteralUndefined)) :: nd1)
			end
	  | _ =>
			let
				val (ts1,nd1) = destructuringElement (ts)
			in case ts1 of
				Comma :: _ =>
					let
						val (ts2,nd2) = destructuringElementList (tl ts1)
					in
						(ts2,nd1::nd2)
					end
			  | _ => (ts1,nd1::[])
			end
	end

(*
	DestructuringElement    
    	Pattern
	    PostfixExpression
*)

and destructuringElement ts =
    let val _ = trace([">> destructuringElement with next=",tokenname(hd(ts))]) 		
	in case ts of
		(LeftBrace | LeftParen) :: _ => pattern ts
	  | _ => 
			let
				val (ts1,nd1) = postfixExpression (ts,NOLIST,ALLOWIN)
			in
				(ts1,Ast.SimplePattern(nd1))
			end
	end

(*
	TYPE EXPRESSIONS
*)

(*
	TypeExpression    
	    FunctionType
    	UnionType
	    RecordType
    	ArrayType
    	TypeIdentifier
	    TypeIdentifier  !
    	TypeIdentifier  ?
*)

and typeExpression (ts) : (token list * Ast.TYPE_EXPR) =
    let val _ = trace([">> typeExpression with next=",tokenname(hd ts)])
    in case ts of
       Function :: _ => functionType ts
     | LeftParen :: ts1 => unionType ts
     | LeftBrace :: ts1 => recordType ts
     | LeftBracket :: ts1 => arrayType ts
	 | _ => 
            let
                val (ts1,nd1) = typeIdentifier ts
				val rf = Ast.Ref {base=NONE,ident=nd1}
            in case ts1 of
				Not :: _ =>
					(tl ts1,Ast.PrimaryType{ident=nd1,kind=Ast.NotNullable}) 
			  | QuestionMark :: _ =>
					(tl ts1,Ast.PrimaryType{ident=nd1,kind=Ast.Nullable}) 
			  | _ =>
					(ts1,Ast.PrimaryType{ident=nd1,kind=Ast.Named}) 
            end
    end

(*
	TypeExpressionList    
    	TypeExpression
	    TypeExpressionList  ,  TypeExpression
*)

and typeExpressionList (ts): (token list * Ast.TYPE_EXPR list) = 
    let
		fun typeExpressionList' (ts,nd) =
	    	let
		    in case ts of
    		    Comma :: _ =>
					let
            			val (ts1,nd1) = typeExpression(tl ts)
	               		val (ts2,nd2) = typeExpressionList'(ts1,nd1)
	    	      	in
    	    	     	(ts2, nd1 :: nd2)
	    	      	end
	    	  | _ => (ts, nd :: [])
		    end
        val (ts1,nd1) = typeExpression(ts)
        val (ts2,nd2) = typeExpressionList'(ts1,nd1)
    in
        (ts2,nd2)
    end

(*
	FunctionType    
    	function  FunctionSignature
*)

and functionType (ts) : (token list * Ast.TYPE_EXPR)  =
    let val _ = trace([">> functionType with next=",tokenname(hd(ts))]) 		
	in case ts of
		Function :: _ => 
			let
				val (ts1,Ast.FunctionSignature{typeparams,params,resulttype}) = 
								functionSignature (tl ts)
				fun paramtypes (params:Ast.FORMAL list):Ast.TYPE_EXPR option list =
					case params of
						[] => []
					  | _ => 
							let
								val _ = log(["paramtypes"]);
								val {ty,... } = hd params
							in
								ty :: paramtypes (tl params)
							end
			in
				trace(["<< functionType with next=",tokenname(hd ts1)]);
				(ts1,Ast.FunctionType {paramTypes=paramtypes params,
						returnType=resulttype,boundThisType=NONE,hasRest=false})
			end
	  | _ => raise ParseError
	end

(*
	UnionType    
    	(  TypeExpressionList  )
*)

and unionType (ts) : (token list * Ast.TYPE_EXPR)  =
    let val _ = trace([">> unionType with next=",tokenname(hd(ts))]) 		
	in case ts of
		LeftParen :: _ => 
			let
				val (ts1,nd1) = typeExpressionList (tl ts)
			in case ts1 of
				RightParen :: _ =>
					(tl ts1, Ast.UnionType nd1)
			  | _ => raise ParseError
			end
	  | _ => raise ParseError
	end

(*
	RecordType    
    	{  FieldTypeList  }
*)

and recordType (ts) : (token list * Ast.TYPE_EXPR) = 
    let val _ = trace([">> recordType with next=",tokenname(hd(ts))]) 
	in case ts of
		LeftBrace :: ts1 => 
			let
				val (ts2,nd2) = fieldTypeList ts1
			in case ts2 of
			    RightBrace :: ts3 => 
					(trace(["<< recordType with next=",tokenname(hd(ts3))]);
					(ts3,Ast.RecordType nd2))
			  | _ => raise ParseError
			end
	  | _ => raise ParseError
	end

(*
	FieldTypeList    
    	«empty»
	    NonemptyFieldTypeList

	NonemptyFieldTypeList    
    	FieldType
	    FieldType  ,  NonemptyFieldTypeList
*)

and fieldTypeList ts =
    let val _ = trace([">> fieldTypeList with next=",tokenname(hd(ts))]) 
		fun nonemptyFieldTypeList (ts) =
			let
				val (ts1,nd1) = fieldType(ts)
			in case ts1 of
				Comma :: _ => 
					let
						val (ts2,nd2) = nonemptyFieldTypeList (tl ts1)
					in
						(ts2,nd1::nd2)
					end
			  | _ => (ts1,nd1::[])
			end
	in case ts of
		RightBrace :: ts1 => (ts1,[])
	  | _ => 
		let
			val (ts1,nd1) = nonemptyFieldTypeList (ts)
		in
			(ts1,nd1)
 		end
	end

(*
	FieldType    
    	FieldName  :  TypeExpression    
*)

and fieldType ts =
    let val _ = trace([">> fieldType with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = fieldName ts
	in case ts1 of
		Colon :: _ =>
			let
				val (ts2,nd2) = typeExpression (tl ts1)
			in
				(ts2,{name=nd1,ty=nd2})
			end
	  | _ => raise ParseError
	end

(*
	ArrayType    
    	[  ElementTypeList  ]
*)

and arrayType (ts) : (token list * Ast.TYPE_EXPR)  =
    let val _ = trace([">> arrayType with next=",tokenname(hd(ts))]) 
	in case ts of
		LeftBracket :: _ => 
			let
				val (ts1,nd1) = elementTypeList (tl ts)
			in case ts1 of
			    RightBracket :: _ => (tl ts1,Ast.ArrayType nd1)
			  | _ => raise ParseError
			end
	  | _ => raise ParseError
	end


(*
	ElementTypeList    
    	«empty»
	    TypeExpression
    	,  ElementTypeList
	    TypeExpression  ,  ElementTypeList
*)

and elementTypeList (ts) : token list * Ast.TYPE_EXPR list =
    let val _ = trace([">> elementTypeList with next=",tokenname(hd(ts))]) 
	in case ts of
		RightBracket :: _ => (ts,[])
	  | Comma :: _ => 
			let
				val (ts1,nd1) = elementTypeList (tl ts)
			in
				(ts1,Ast.SpecialType(Ast.Any) :: nd1)
			end
	  | _ =>
			let
				val (ts1,nd1) = typeExpression (ts)
			in case ts1 of
				Comma :: _ =>
					let
						val (ts2,nd2) = elementTypeList (tl ts1)
					in
						(ts2,nd1::nd2)
					end
			  | _ => (ts1,nd1::[])
			end
	end

(* STATEMENTS *)

(*

Statementw    
    Block
    BreakStatement Semicolonw
    ContinueStatement Semicolonw
    DefaultXMLNamespaceStatement Semicolonw
    DoStatement Semicolonw
    ExpressionStatement Semicolonw
    ForStatementw
    IfStatementw
    LabeledStatementw
    LetStatementw
    ReturnStatement Semicolon(omega)
    SuperStatement Semicolonw
    SwitchStatement
    ThrowStatement Semicolonw
    TryStatement
    WhileStatementw
    WithStatementw
    
Substatementw    
    EmptyStatement
    Statementw
    
Semicolonabbrev     
    ;
    VirtualSemicolon
    «empty»
    
SemicolonnoShortIf    
    ;
    VirtualSemicolon
    «empty»
    
Semicolonfull    
    ;
    VirtualSemicolon

*)

and statement (ts,omega) =
	let
	in case ts of
	    Return :: _ =>
			let
				val (ts1,nd1) = returnStatement (ts)
(* todo 				val (ts2,_) = semicolon (ts1,omega) *)
			in
				(ts1,nd1)
			end
	  | _ =>
			let
				val (ts1,nd1) = expressionStatement (ts)
(* todo 				val (ts2,_) = semicolon (ts1,omega) *)
			in
				(ts1,nd1)
			end
	end


(*
    
EmptyStatement     
    ;

*)

and emptyStatement ts =
    let
    in case ts of
		SemiColon :: ts1 => (ts1,Ast.EmptyStmt)
	  | _ => raise ParseError
    end

(*
    
ExpressionStatement    
    [lookahead !{ function, { }] ListExpression (allowLet,allowIn)

*)

and expressionStatement ts =
    let
        val _ = trace([">> expressionStatement with next=", tokenname(hd ts)])
        val (ts1,nd1) = listExpression(ts,ALLOWIN)
    in
        (ts1,Ast.ExprStmt(nd1))
    end

(*
    
SuperStatement    
    super Arguments
    
*)

(*

Block    
    { Directives }

*)

and block (ts) : (token list * Ast.BLOCK) =
    let val _ = trace([">> block with next=", tokenname(hd ts)])
    in case ts of
        LeftBrace :: RightBrace :: ts1 => (ts1,Ast.Block{pragmas=[],defns=[],stmts=[]})
      | LeftBrace :: ts1 =>
            let
                val (ts2,nd2) = directives ts1
            in case ts2 of
                RightBrace :: ts3 => (ts3,Ast.Block{pragmas=[],defns=[],stmts=nd2})
			  | _ => raise ParseError
            end
	  | _ => raise ParseError
    end

(*
    
LabeledStatementw    
    Identifier : Substatementw
    
IfStatementabbrev    
    if ParenListExpression Substatementabbrev
    if ParenListExpression SubstatementnoShortIf else Substatementabbrev
    
IfStatementfull    
    if ParenListExpression Substatementfull
    if ParenListExpression SubstatementnoShortIf else Substatementfull
    
IfStatementnoShortIf    
    if ParenListExpression SubstatementnoShortIf else SubstatementnoShortIf
    
SwitchStatement    
    switch ParenListExpression { CaseElements }
    switch  type  (  TypedIdentifierallowIn  =  AssignmentExpressionallowIn  )  {  TypeCaseElements  }
    
CaseElements    
    «empty»
    CaseLabel
    CaseLabel CaseElementsPrefix CaseElementabbrev
    
CaseElementsPrefix    
    «empty»
    CaseElementsPrefix CaseElementfull
    
CaseElementw    
    Directivew
    CaseLabel
    
CaseLabel    
    case ListExpressionallowIn :
    default :
    
TypeCaseElements    
    TypeCaseElement
    TypeCaseElements  TypeCaseElement
    
TypeCaseElement    
    case  (  TypedIdentifierallowIn  )  Block
    case  (  TypedPattern  )  Block
    default  Block
    
DoStatement    
    do Substatementabbrev while ParenListExpression
    
WhileStatementw    
    while ParenListExpression Substatementw
    
ForStatementw    
    for  (  ForInitialiser  ;  OptionalExpression  ;  OptionalExpression  )  Substatementw
    for  (  ForInBinding  in  ListExpressionallowIn  )  Substatementw
    for  each  ( ForInBinding  in  ListExpressionallowIn  )  Substatementw
    
ForInitialiser    
    «empty»
    ListExpressionnoIn
    VariableDefinitionnoIn
    
ForInBinding    
    PostfixExpression
    Pattern
    VariableDefinitionKind VariableBindingnoIn
    
OptionalExpression    
    ListExpressionallowIn
    «empty»
    
LetStatementw    
    let  (  LetBindingList  )  Substatementw
    
WithStatementw    
    with  (  ListExpressionallowIn  )  Substatementw
    with  (  ListExpressionallowIn  :  TypeExpression  )  Substatementw
    
ContinueStatement    
    continue
    continue [no line break] Identifier
    
BreakStatement    
    break
    break [no line break] Identifier
*)

(*    
ReturnStatement    
    return
    return [no line break] ListExpressionallowIn
*)

and returnStatement ts =
	let
	in case ts of
		Return :: (SemiColon | RightBrace) :: _ => (tl ts,Ast.ReturnStmt (Ast.LiteralExpr Ast.LiteralUndefined))
	  | Return :: _ =>
			let
				val (ts1,nd1) = listExpression(tl ts, ALLOWIN)
			in
				(ts1,Ast.ReturnStmt nd1)
			end
	  | _ => raise ParseError
	end

(*    
ThrowStatement     
    throw  ListExpressionallowIn
    
TryStatement    
    try Block CatchClauses
    try Block CatchClausesOpt finally Block
    
CatchClausesOpt    
    «empty»
    CatchClauses
    
CatchClauses    
    CatchClause
    CatchClauses CatchClause
    
CatchClause    
    catch  (  Parameter  )  Block
    
DefaultXMLNamespaceStatement    
    default  xml  namespace = NonAssignmentExpressionallowLet, allowIn

*)
    
(* DIRECTIVES *)

(*
	Directive(omega)    
    	EmptyStatement
	    Statement(omega)
    	AnnotatableDirective(omega)
	    Attributes [no line break] AnnotatableDirective(omega)
    	IncludeDirective Semicolon(omega)    
*)

and directive (ts,omega) =
    let val _ = trace([">> directive with next=", tokenname(hd ts)])
	in case ts of
		SemiColon :: _ => 
			let
				val (ts1,nd1) = emptyStatement ts
			in
				(ts1,nd1)
			end
	  | (Var | Function ) :: _ => 
			let
				val (ts1,nd1) = annotatableDirective (ts,omega)
			in
				(ts1,Ast.DefnStmt(nd1))
			end
	  | _ => 
			let
				val (ts1,nd1) = statement (ts,omega)
			in
				(ts1,nd1)
			end
	end

(*
	AnnotatableDirective(omega)
    	VariableDefinition(allowIn) Semicolon(omega)
	    FunctionDefinition
    	ClassDefinition
	    InterfaceDefinition
    	NamespaceDefinition Semicolon(omega)
	    TypeDefinition Semicolon(omega)

	Attributes    
    	Attribute
	    Attribute [no line break] Attributes
    
	Attribute    
    	SimpleTypeIdentifier
	    ReservedNamespace
    	dynamic
	    final
    	native
	    override
    	prototype
	    static
    	[  AssignmentExpression(allowLet, allowIn)  ]
*)

and annotatableDirective (ts,omega) : token list * Ast.DEFINITION list  =
    let val _ = trace([">> annotatableDirective with next=", tokenname(hd ts)])
	in case ts of
		Var :: _ => variableDefinition (ts,ALLOWIN)
	  | _ => raise ParseError
	end

(*
	Directives    
    	«empty»
	    DirectivesPrefix Directive(abbrev)
    
	DirectivesPrefix    
    	«empty»
	    Pragmas
    	DirectivesPrefix Directive(full)
*)

and directives (ts) : (token list * Ast.STMT list) =
    let val _ = trace([">> directives with next=", tokenname(hd ts)])
	in case ts of
		RightBrace :: _ => (ts,[])
	  | _ => 
			let
				val (ts1,nd1) = directive (ts,ABBREV)
			in
				(ts1,[nd1])
			end
	end

(*
IncludeDirective    
    include  [no line break]  StringLiteral
    
Pragmas    
    Pragma
    Pragmas  Pragma
    
Pragma    
    UsePragma  Semicolon(full)
    ImportPragma  Semicolon(full)
    
UsePragma    
    use  PragmaItems
    
PragmaItems    
    PragmaItem
    PragmaItems  ,  PragmaItem
    
PragmaItem    
    PragmaIdentifier
    PragmaIdentifier  PragmaArgument
    
PragmaIdentifier    
    decimal
    default namespace
    double
    int
    namespace
    Number
    rounding
    standard
    strict
    uint
    
PragmaArgument    
    true
    false
    NumberLiteral
    -  NumberLiteral
    StringLiteral
    SimpleTypeIdentifier
    
ImportPragma    
    import  ImportName
    import  Identifier  =  ImportName
    
ImportName    
    PackageIdentifier  .  PropertyIdentifier

*)
    
(* DEFINITIONS *)    

(*
    
VariableDefinitionb    
    VariableDefinitionKind  VariableBindingListallowLet, b
    
VariableDefinitionKind    
    const
    let
    let const
    var
    
VariableBindingLista, b    
    VariableBindinga, b
    VariableBindingListnoLet, b  ,  VariableBindinga, b
    
VariableBindinga, b    
    TypedIdentifier
    TypedIdentifierb VariableInitialisationa, b
    TypedPattern VariableInitialisationa, b
    
VariableInitialisationa, b    
    =  AssignmentExpressiona, b


     and varDefn =
         SimpleDefn of { tag: varDefnTag,
                         init: expr option,
                         attrs: attributes,
                         name: ident,
                         ty: tyExpr option }

       | DestructDefn of { tag: varDefnTag,
                           init: expr option,
                           attrs: attributes,
                           ptrn: pattern,
                           ty: tyExpr option }

       | DestructuringDefn of { tag: varDefnTag,
                                init: expr option,
                                attrs: attributes,
                                temp: ident,
                                postInit: expr option,
                                names: ident list,
                                ty: tyExpr option }
*)

and variableDefinition (ts,beta) =
    let val _ = trace([">> variableDefinition with next=", tokenname(hd ts)])
	in case ts of
		Const :: _ => variableBindingList (tl ts,Ast.Const,ALLOWLIST,beta)
	  | Var :: _ => variableBindingList (tl ts,Ast.Var,ALLOWLIST,beta)
	  | _ => raise ParseError
	end

and variableBindingList (ts,tag,alpha,beta) = 
    let val _ = trace([">> variableBindingList with next=", tokenname(hd ts)])
		fun variableBindingListPrime (ts,tag,alpha,beta) =
	    	let
		    in case ts of
    		    Comma :: _ =>
					let
            			val (ts1,nd1) = variableBinding(ts,tag,alpha,beta)
	               		val (ts2,nd2) = variableBindingListPrime(ts1,tag,alpha,beta)
	    	      	in
    	    	     	(ts2, Ast.VariableDefn nd1 :: nd2)
	    	      	end
	    	  | _ => (ts, [])
		    end
   			val (ts1,nd1) = variableBinding(ts,tag,alpha,beta)
       		val (ts2,nd2) = variableBindingListPrime(ts1,tag,alpha,beta)
    in
        (ts2, Ast.VariableDefn nd1 :: nd2)
    end

and variableBinding (ts,tag,alpha,beta) = 
    let val _ = trace([">> variableBinding with next=", tokenname(hd ts)])
		val defaultAttrs = 
			Ast.Attributes { 
				ns = Ast.Internal "",
		        override = false,
        		static = false,
		        final = false,
        		dynamic = false,
		        prototype = false,
        		nullable = false }

	in case ts of
		(LeftBrace | LeftBracket) :: _ =>
			let
				val (ts1,{ptrn,ty}) = typedPattern ts
			in case ts1 of
				Assign :: _ =>
					let
						val (ts2,nd2) = assignmentExpression (tl ts1,alpha,beta)
					in
						(ts2, Ast.VariableDefinition { tag = tag,
                         					   init = SOME nd2,
                         					   attrs = defaultAttrs,
                         					   pattern = ptrn ,
                         					   ty = ty } )
					end
			  | _ => raise ParseError
			end
	  | _ => 
			let
				val (ts1,{p,t}) = typedIdentifier (ts)
			in case ts1 of
				Assign :: _ =>
					let
						val (ts2,nd2) = assignmentExpression (tl ts1,alpha,beta)
					in
						(ts2, Ast.VariableDefinition { tag = tag,
                         init = SOME nd2,
                         attrs = defaultAttrs,
                         pattern = p,
                         ty = t } )
					end
			  | _ => (ts1, Ast.VariableDefinition { tag = tag,
                         init = NONE,
                         attrs = defaultAttrs,
                         pattern = p,
                         ty = t } )
			end
	end

(*

FunctionDefinition    
    function  FunctionName  FunctionCommon
    let  function  FunctionName  FunctionCommon
    
FunctionName    
    Identifier
    OperatorName
    to  [no line break]  Identifier
    get  [no line break]  PropertyIdentifier
    set  [no line break]  PropertyIdentifier
    call  [no line break]  PropertyIdentifier
    
OperatorName [one of]    
    +   -   ~   *   /   %   <   >   <=   >=   ==   <<   >>   >>>   &   |   ===   !=   !==
    
ClassDefinition    
    class  ClassName  Inheritance Block
    
Inheritance    
    «empty»
    extends TypeIdentifier
    implements TypeIdentifierList
    extends TypeIdentifier implements TypeIdentifierList
    
TypeIdentifierList    
    TypeIdentifier
    TypeIdentifier  ,  TypeIdentifierList
    
InterfaceDefinition    
    interface  ClassName  ExtendsList Block
    
ExtendsList    
    «empty»
    extends TypeIdentifierList
    
NamespaceDefinition    
    namespace NamespaceBinding
    
NamespaceBinding    
    Identifier NamespaceInitialisation
    
NamespaceInitialisation    
    «empty»
    = StringLiteral
    = SimpleTypeIdentifier
    
TypeDefinition    
    type  TypeBinding
    
TypeBinding    
    Identifier  TypeInitialisation
    
TypeInitialisation    
    =  TypeExpression
*)

(*
    
Program    
    Directives
    PackageDefinition Program

*)
    
and program ts =
    let
       val _ = trace([">> program with next=",tokenname(hd(ts))])
    in case ts of
        Package :: tr => 
			let 
			    val (tr2, pkgs) = packageDefinition tr
			in
			    (tr2, {packages=[pkgs], body=(Ast.Block {pragmas=[],
						     defns=[],
						     stmts=[]})})
			end
      | _ => 
			let
			    val (tr2, directives) = directives ts
			in
			    (tr2, {packages=[], body=(Ast.Block {stmts=directives,
					   defns=[],
					   pragmas=[]})})
			end
    end

(*

PackageDefinition    
    PackageAttributes package PackageNameOpt Block
    
PackageAttributes    
    internal
    «empty»
    
PackageNameOpt    
    «empty»
    PackageName
    
PackageName [create a lexical PackageIdentifier with the sequence of characters that make a PackageName]    
    Identifier
    PackageName  .  Identifier
    
ClassName    
    ParameterisedClassName
    ParameterisedClassName  !
    
ParameterisedClassName    
    Identifier
    Identifier  TypeParameters

*)


and packageDefinition ts = raise ParseError

fun mkReader filename = 
    let
        val stream = TextIO.openIn filename
    in
        fn _ => case TextIO.inputLine stream of
                    SOME line => (log ["read line ", line]; line)
                  | NONE => ""
    end

(*
   scan to first < or /

   the scanner state includes the reader and a token stream.
   when the token stream is empty continue scanning in the current slash context until another key character is encountered.

   switch slash contexts at the normal points in the program.
*)

fun dumpTokens (ts,lst) =
	case ts of
		[] => rev lst
      | _ => dumpTokens(tl ts, tokenname(hd ts) :: "\n  " :: lst)

fun lexFile (filename : string) : (token list) = 
    let 
        val lexer = Lexer.makeLexer (mkReader filename)
	val tokens = Lexer.UserDeclarations.token_list lexer
    in
        log ("tokens:" :: dumpTokens(tokens,[])); 
        tokens
    end

fun parse ts =
    let 
	val (residual, result) = (program ts) 
	fun check_residual (Eol :: xs) = check_residual xs
	  | check_residual [Eof] = ()
	  | check_residual _ = raise ParseError
    in
	check_residual residual;
	log ["parsed all input, pretty-printing:"];
	Pretty.ppProgram result;
	result
    end

fun parseFile filename = 
    (log ["scanning ", filename];
     let val ast = parse (lexFile filename)
     in
         log ["parsed ", filename, "\n"];
         ast
     end)

(*
 handle ParseError => (log ["parse error"]; raise ParseError)
      | Lexer.LexError => (log ["lex error"]; raise Lexer.LexError)
*)

end (* Parser *)
