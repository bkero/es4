
structure Parser = struct

open Token

exception ParseError

datatype alpha =
    ALLOWLIST
  | NOLIST

datatype beta =
    ALLOWIN
  | NOIN

datatype gamma =
	ALLOWEXPR
  | NOEXPR

datatype omega =
    ABBREV
  | NOSHORTIF
  | FULL

fun log ss = 
    (TextIO.print "log: "; 
     List.app TextIO.print ss; 
     TextIO.print "\n")

val trace_on = true

fun trace ss =
	if trace_on then log ss else ()

fun error ss =
	(log ("*syntax error: " :: ss); raise ParseError)

fun newline ts = Lexer.UserDeclarations.followsLineBreak ts


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
      | Precision :: tr => (tr,"precision")
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
      | _ => error(["expecting 'identifier' before '",tokenname(hd ts),"'"])
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
              (ts1,Ast.LexicalRef{ident=Ast.Identifier {ident=nd1, openNamespaces=ref []}})
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
                val id = Ast.Identifier {ident=nd1, openNamespaces=ref []}
          	in case ts1 of
              	DoubleColon :: _ => 
					qualifiedIdentifier'(tl ts1,Ast.LexicalRef ({ident=id}))
              | _ => 
					(trace(["<< simpleQualifiedIdentifier with next=",tokenname(hd(ts1))]);
					(ts1,id))
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
				(ts1,Ast.AttributeIdentifier (Ast.ExpressionIdentifier nd1))
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
        LeftParen :: _ => 
            let
                val (ts1,nd1) = listExpression (tl ts,ALLOWIN)
            in case ts1 of
                RightParen :: _ => 
					(trace(["<< parenListExpression with next=",tokenname(hd(ts1))]);
					(tl ts1,nd1))
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

and functionExpression (ts,a,b) =
    let val _ = trace([">> functionExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        Function :: ts1 => 
			let
			in case ts1 of
				(LeftDotAngle | LeftParen) :: _ => 
					let
						val (ts3,nd3) = functionSignature ts1
					in case (ts3,a) of
						(LeftBrace :: _,_) => 
							let
								val (ts4,nd4) = block ts3
							in
								(ts4,Ast.FunExpr {ident=NONE,sign=nd3,body=nd4})
							end
					  | (_,ALLOWLIST) => 
							let
								val (ts4,nd4) = listExpression (ts3,b)
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
					in case (ts3,a) of
						(LeftBrace :: _,_) => 
							let
								val (ts4,nd4) = block ts3
							in
								(ts4,Ast.FunExpr {ident=SOME nd2,sign=nd3,body=nd4})
							end
					  | (_,ALLOWLIST) => 
							let
								val (ts4,nd4) = listExpression (ts3,b)
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
        ParameterKind TypedIdentifier(ALLOWIN)
        ParameterKind TypedPattern

    ParameterKind    
        «empty»
        const
*)

and parameter (ts) : (token list * Ast.FORMAL) =
    let val _ = trace([">> parameter with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = parameterKind (ts)
		val (ts2,{p,t}) = typedPattern (ts1,NOLIST,ALLOWIN,NOEXPR)
	in
		(ts2,{pattern=p,ty=t,tag=nd1,isRest=false,init=NONE})
	end

and parameterKind (ts) : (token list * Ast.VAR_DEFN_TAG)  = 
    let val _ = trace([">> parameterKind with next=",tokenname(hd(ts))]) 
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
					(tl ts,{pattern=Ast.IdentifierPattern "",
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

    NonemptyFieldList(a)
        LiteralField(a)
        LiteralField(noLet)  ,  NonemptyFieldList(a)
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
				(ts1,Ast.LexicalRef{ident=nd1})
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
        [  ElementList  ]
        [  ElementList  ]  :  ArrayType
*)

and arrayLiteral (ts) =
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
						(ts2,Ast.LiteralArray {expr=nd1,ty=SOME nd2})
					end
			  | RightBracket :: _ => 
					(tl ts1,Ast.LiteralArray {expr=nd1,ty=NONE})
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
	PrimaryExpression(a,b)
    	null
	    true
    	false
	    NumberLiteral
    	StringLiteral
	    this
    	RegularExpression
	    XMLInitialiser
    	ParenListExpression
	    ArrayLiteral
    	ObjectLiteral
	    FunctionExpression(a,b)
    	AttributeIdentifier
	    TypeIdentifier
*)

and primaryExpression (ts,a,b) =
    let val _ = trace([">> primaryExpression with next=",tokenname(hd ts)])
    in case ts of
        Null :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralNull))
      | True :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean true))
      | False :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean false))
      | NumberLiteral n :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralNumber n))
      | StringLiteral s :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralString s))
      | This :: ts1 => (ts1, Ast.NullaryExpr Ast.This)
      | LeftParen :: _ => 
			parenListExpression ts
      | LeftBracket :: _ => 
			let 
				val (ts1,nd1) = arrayLiteral ts
			in 
				(ts1,Ast.LiteralExpr nd1) 
			end
      | LeftBrace :: _ => 
			let 
				val (ts1,nd1) = objectLiteral ts 
			in 
				(ts1,Ast.LiteralExpr nd1) 
			end
      | Function :: _ => functionExpression (ts,a,b)
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
(* what's this?      | Eol :: ts1 => primaryExpression (ts1,a,b) *)
      | At :: _ => 
			let
				val (ts1,nd1) = attributeIdentifier ts
			in
				(ts1,Ast.LexicalRef {ident=nd1})
			end
      | _ => 
			let
				val (ts1,nd1) = typeIdentifier ts
			in
				(ts1,Ast.LexicalRef {ident=nd1})
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

and memberExpression (ts,a,b) =
    let val _ = trace([">> memberExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        New :: _ =>
            let
                val (ts1,nd1) = memberExpression(tl ts,a,b)
                val (ts2,nd2) = arguments(ts1)
                val (ts3,nd3) = memberExpressionPrime(ts2,Ast.CallExpr {func=nd1,actuals=nd2},a,b)
            in
                (ts3,nd3)
            end
      | Super :: _ =>
            let
                val (ts2,nd2) = superExpression(ts)
                val (ts3,nd3) = propertyOperator(ts2,nd2)
                val (ts4,nd4) = memberExpressionPrime(ts3,nd3,a,b)
               in
                (ts4,nd4)
            end
      | _ =>
            let
                val (ts3,nd3) = primaryExpression(ts,a,b)
                val (ts4,nd4) = memberExpressionPrime(ts3,nd3,a,b)
               in
                (trace(["<< memberExpression with next=",tokenname(hd ts4)]);(ts4,nd4))
            end
    end

and memberExpressionPrime (ts,nd,a,b) =
    let val _ = trace([">> memberExpressionPrime with next=",tokenname(hd(ts))])
    in case ts of
        (LeftBracket :: _ | Dot :: _) =>
            let
                val (ts2,nd2) = propertyOperator(ts,nd)
            in
                memberExpressionPrime(ts2, nd2,a,b)
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

and callExpression (ts,a,b) =
    let val _ = trace([">> callExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = memberExpression(ts,a,b)
        val (ts2,nd2) = arguments(ts1)
    in 
        callExpressionPrime(ts2,Ast.CallExpr({func=nd1,actuals=nd2}),a,b)
    end

and callExpressionPrime (ts,nd,a,b) =
    let val _ = trace([">> callExpressionPrime with next=",tokenname(hd(ts))])
    in case ts of
        (LeftBracket :: _ | Dot :: _) =>
            let
                val (ts1,nd1) = propertyOperator(tl ts,nd)
            in
                memberExpressionPrime(ts1, nd1,a,b)
            end
      | LeftParen :: _ => 
            let
                val (ts1,nd1) = arguments(ts)
            in
                memberExpressionPrime(ts1,Ast.CallExpr({func=nd,actuals=nd1}),a,b)
            end
      | _ => (ts,nd)
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
		    let val _ = trace([">> argumentList' with next=",tokenname(hd(ts))])
		    in case ts of
		        Comma :: _ => 
        		    let
                		val (ts1,nd1) = assignmentExpression(tl ts,NOLIST,ALLOWIN)
		                val (ts2,nd2) = argumentList'(ts1)
        		    in
                		(ts2,nd1::nd2)
		            end
		      | RightParen :: _ => 
					(ts,[])
			  | _ => 
					(log(["*syntax error*: expect '",tokenname RightParen, "' before '",tokenname(hd ts),"'"]);
					raise ParseError)
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

	STATUS: Needs testing
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
               			        (ts4,Ast.ObjectRef {base=SOME nd,ident=Ast.ExpressionIdentifier(nd4)})
                            end
					  | DoubleColon :: ts3 => 
                            let
                                val (ts4,nd4) = reservedOrPropertyIdentifier(ts3)
                            in
                                (ts4,Ast.ObjectRef({base=SOME nd,ident=Ast.Identifier {ident=nd4,openNamespaces=ref []}}))
                            end
					  | _ => raise ParseError (* e4x filter expr *)
                    end
              | _ => 
                    let
                        val (ts4,nd4) = reservedOrPropertyIdentifier(ts1)
                    in
                        (ts4,Ast.ObjectRef({base=SOME nd,ident=Ast.Identifier {ident=nd4,openNamespaces=ref []}}))
                    end
            end
      | LeftBracket :: _ => 
            let
                val (ts4,nd4) = brackets(ts)
            in
                (ts4,Ast.ObjectRef({base=SOME nd,ident=Ast.ExpressionIdentifier(nd4)}))
            end
      | _ => raise ParseError
    end

(*
	Brackets	
		[  ListExpression(allowIn)  ]
		[  SliceExpression   ]
		
	SliceExpression	
		OptionalExpression  :  OptionalExpression
		OptionalExpression  :  OptionalExpression  :  OptionalExpression
		
	OptionalExpression	
		ListExpression(allowIn)
		«empty»

	TODO: implement SliceExpression
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
	NewExpression(a,b)
    	MemberExpression(a,b)
	    new  NewExpression(a,b)
*)

and newExpression (ts,a,b) =
    let val _ = trace([">> newExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        New :: ts1 =>
            let
                val (ts2,nd2) = newExpression(ts1,a,b)
            in
                (ts2,Ast.NewExpr({obj=nd2,actuals=[]}))
            end
      | _ => memberExpression(ts,a,b)
    end

(*
    LeftHandSideExpression(a, b)   
        NewExpression(a, b)
        CallExpression(a, b)
    
    refactored:

    LeftHandSideExpression(a, b)
        new NewExpression(a, b)
        MemberExpression(a,b) Arguments CallExpressionPrime(a, b)
        MemberExpression(a, b)
*)

and leftHandSideExpression (ts,a,b) =
    let val _ = trace([">> leftHandSideExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        New :: ts1 =>
            let
                val (ts2,nd2) = newExpression(ts1,a,b)
            in
                (ts2,Ast.NewExpr({obj=nd2,actuals=[]}))
            end
      | _ =>
            let
                val (ts2,nd2) = memberExpression(ts,a,b)
            in case ts2 of
                LeftParen :: _ =>
                    let
                        val (ts3,nd3) = arguments(ts2)
                        val (ts4,nd4) = callExpressionPrime(ts3,Ast.CallExpr {func=nd2,actuals=nd3},a,b)
                    in
						(trace(["<< leftHandSideExpression with next=",tokenname(hd(ts4))]);(ts4,nd4))
                    end
              | _ => 
					(trace(["<< leftHandSideExpression with next=",tokenname(hd(ts2))]);
					(ts2,nd2))
            end
    end

(*
	PostfixExpression(a, b)	
		LeftHandSideExpression(a, b)
		LeftHandSideExpression(a, b)  [no line break]  ++
		LeftHandSideExpression(a, b)  [no line break]  --
*)

and postfixExpression (ts,a,b) =
    let val _ = trace([">> postfixExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = leftHandSideExpression(ts,a,b)
    in case ts1 of
		PlusPlus :: ts2 => (ts2,Ast.UnaryExpr(Ast.PostIncrement,nd1))
	  | MinusMinus :: ts2 => (ts2,Ast.UnaryExpr(Ast.PostDecrement,nd1))
	  | _ => (trace(["<< postfixExpression"]);(ts1,nd1))
    end

(*
	UnaryExpression(a, b)	
		PostfixExpression(a, b)
		delete  PostfixExpression(a, b)
		void  UnaryExpression(a, b)
		typeof  UnaryExpression(a, b)
		++   PostfixExpression(a, b)
		--  PostfixExpression(a, b)
		+  UnaryExpression(a, b)
		-  UnaryExpression(a, b)
		~  UnaryExpression(a, b)
		!  UnaryExpression(a, b)
		type TypeExpression
*)

and unaryExpression (ts,a,b) =
    let val _ = trace([">> unaryExpression with next=",tokenname(hd(ts))]) 
    in case ts of
		Delete :: ts1 => 
			let 
				val (ts2,nd2) = postfixExpression (ts1,a,b) 
			in 
				(ts2,Ast.UnaryExpr(Ast.Delete,nd2)) 
			end
	  | Void :: ts1 => 
			let 
				val (ts2,nd2) = unaryExpression (ts1,a,b) 
			in 
				(ts2,Ast.UnaryExpr(Ast.Void,nd2)) 
			end
	  | TypeOf :: ts1 => 
			let 
				val (ts2,nd2) = unaryExpression (ts1,a,b) 
			in 
				(ts2,Ast.UnaryExpr(Ast.Typeof,nd2)) 
			end
	  | PlusPlus :: ts1 => 
			let 
				val (ts2,nd2) = postfixExpression (ts1,a,b) 
			in 
				(ts2,Ast.UnaryExpr(Ast.PreIncrement,nd2)) 
			end
	  | MinusMinus :: ts1 => 
			let 
				val (ts2,nd2) = postfixExpression (ts1,a,b) 
			in 
				(ts2,Ast.UnaryExpr(Ast.PreDecrement,nd2)) 
			end
	  | Plus :: ts1 => 
			let 
				val (ts2,nd2) = unaryExpression (ts1,a,b) 
			in 
				(ts2,Ast.UnaryExpr(Ast.UnaryPlus,nd2)) 
			end
	  | BitwiseNot :: ts1 => 
			let 
				val (ts2,nd2) = unaryExpression (ts1,a,b) 
			in 
				(ts2,Ast.UnaryExpr(Ast.BitwiseNot,nd2)) 
			end
	  | Not :: ts1 => 
			let 
				val (ts2,nd2) = unaryExpression (ts1,a,b) 
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
			postfixExpression (ts,a,b)
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

and multiplicativeExpression (ts,a,b) =
    let val _ = trace([">> multiplicativeExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = unaryExpression (ts,a,b)
		fun multiplicativeExpression' (ts1, nd1,a,b) =
			case ts1 of
				Mult :: ts2 => 
					let 
						val (ts3,nd3) = unaryExpression (ts2,a,b) 
					in 
						multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Times,nd1,nd3),a,b) 
					end

			  | LexBreakDiv x :: _ =>
					let	
					in case (#lex_initial x)() of
						Div :: ts2 => 
							let 
								val (ts3,nd3) = unaryExpression (ts2,a,b) 
            				in 
								multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Divide,nd1,nd3),a,b) 
							end
					  | _ => raise ParseError
					end

			  | Modulus :: ts2 => 
					let 
						val (ts3,nd3) = unaryExpression (ts2,a,b) 
					in 
						multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Remainder,nd1,nd3),a,b) 
					end
			  | _ => (trace(["<< multiplicative"]);(ts1,nd1))
    in
        multiplicativeExpression' (ts1,nd1,a,b)
    end

(*
	AdditiveExpression    
    	MultiplicativeExpression
	    AdditiveExpression  +  MultiplicativeExpression
	    AdditiveExpression  -  MultiplicativeExpression

	right recursive: (see pattern of MultiplicativeExpression)
*)

and additiveExpression (ts,a,b) =
    let val _ = trace([">> additiveExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = multiplicativeExpression (ts,a,b)
		fun additiveExpression' (ts1, nd1,a,b) =
			case ts1 of
				Plus :: ts2 => 
					let 
						val (ts3,nd3) = multiplicativeExpression (ts2,a,b) 
					in 
						additiveExpression' (ts3,Ast.BinaryExpr(Ast.Plus,nd1,nd3),a,b) 
					end
			  | Minus :: ts2 => 
					let 
						val (ts3,nd3) = multiplicativeExpression (ts2,a,b) 
					in 
						additiveExpression' (ts3,Ast.BinaryExpr(Ast.Minus,nd1,nd3),a,b) 
					end
			  | _ => 
					(trace(["<< additiveExpression"]);
					(ts1,nd1))
    in
        additiveExpression' (ts1,nd1,a,b)
    end

(*
	ShiftExpression    
	    AdditiveExpression
	    ShiftExpression  <<  AdditiveExpression
	    ShiftExpression  >>  AdditiveExpression
    	ShiftExpression  >>>  AdditiveExpression
    
	right recursive: (see pattern of MultiplicativeExpression)
*)

and shiftExpression (ts,a,b) =
    let val _ = trace([">> shiftExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = additiveExpression (ts,a,b)
		fun shiftExpression' (ts1,nd1,a,b) =
			case ts1 of
				LeftShift :: ts2 => 
					let 
						val (ts3,nd3) = additiveExpression (ts2,a,b) 
					in 
						shiftExpression' (ts3,Ast.BinaryExpr(Ast.LeftShift,nd1,nd3),a,b) 
					end
			  | RightShift :: ts2 => 
					let 
						val (ts3,nd3) = additiveExpression (ts2,a,b)
					in 
						shiftExpression' (ts3,Ast.BinaryExpr(Ast.RightShift,nd1,nd3),a,b) 
					end
			  | UnsignedRightShift :: ts2 => 
					let 
						val (ts3,nd3) = additiveExpression (ts2,a,b) 
					in 
						shiftExpression' (ts3,Ast.BinaryExpr(Ast.RightShiftUnsigned,nd1,nd3),a,b) 
					end
			  | _ => (trace(["<< shiftExpression"]);(ts1,nd1))
    in
        shiftExpression' (ts1,nd1,a,b)
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

and relationalExpression (ts,a, b)=
    let val _ = trace([">> relationalExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = shiftExpression (ts,a,b)
		fun relationalExpression' (ts1,nd1,a,b) =
			case (ts1,b) of
				(LessThan :: ts2,_) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,a,b) 
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.Less,nd1,nd3),a,ALLOWIN) 
					end
			  | (GreaterThan :: ts2,_) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,a,b)
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.Greater,nd1,nd3),a,ALLOWIN) 
					end
			  | (LessThanOrEquals :: ts2, _) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,a,b) 
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.LessOrEqual,nd1,nd3),a,ALLOWIN) 
					end
			  | (GreaterThanOrEquals :: ts2, _) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,a,b) 
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.GreaterOrEqual,nd1,nd3),a,ALLOWIN) 
					end
			  | (In :: ts2, ALLOWIN) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,a,b) 
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.In,nd1,nd3),a,ALLOWIN) 
					end
			  | (InstanceOf :: ts2, _) => 
					let 
						val (ts3,nd3) = shiftExpression (ts2,a,b) 
					in 
						relationalExpression' (ts3,Ast.BinaryExpr(Ast.InstanceOf,nd1,nd3),a,ALLOWIN) 
					end
			  | (Is :: ts2, _) => 
					let 
						val (ts3,nd3) = typeExpression (ts2) 
					in 
						relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.Is,nd1,nd3),a,ALLOWIN) 
					end
			  | (To :: ts2, _) => 
					let 
						val (ts3,nd3) = typeExpression (ts2) 
					in 
						relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.To,nd1,nd3),a,ALLOWIN) 
					end
			  | (Cast :: ts2, _) => 
					let 
						val (ts3,nd3) = typeExpression (ts2) 
					in 
						relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.Cast,nd1,nd3),a,ALLOWIN) 
					end
			  | (_,_) => 
					(trace(["<< relationalExpression"]);(ts1,nd1))
    in
        relationalExpression' (ts1,nd1,a,b)
    end

(*
	EqualityExpression(b)
    	RelationalExpression(b)
	    EqualityExpression(b)  ==  RelationalExpression(b)
	    EqualityExpression(b)  !=  RelationalExpression(b)
	    EqualityExpression(b)  ===  RelationalExpression(b)
	    EqualityExpression(b)  !==  RelationalExpression(b)

	right recursive: (see pattern of MultiplicativeExpression)

*)

and equalityExpression (ts,a,b)=
    let val _ = trace([">> equalityExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = relationalExpression (ts,a,b)
		fun equalityExpression' (ts1,nd1) =
			case ts1 of
				Equals :: ts2 => 
					let 
						val (ts3,nd3) = relationalExpression (ts2,a,b) 
					in 
						equalityExpression' (ts3,Ast.BinaryExpr(Ast.Equals,nd1,nd3)) 
					end
			  | NotEquals :: ts2 => 
					let 
						val (ts3,nd3) = relationalExpression (ts2,a,b) 
					in 
						equalityExpression' (ts3,Ast.BinaryExpr(Ast.NotEquals,nd1,nd3)) 
					end
			  | StrictEquals :: ts2 => 
					let 
						val (ts3,nd3) = relationalExpression (ts2,a,b) 
					in 
						equalityExpression' (ts3,Ast.BinaryExpr(Ast.StrictEquals,nd1,nd3)) 
					end
			  | StrictNotEquals :: ts2 => 
					let 
						val (ts3,nd3) = relationalExpression (ts2,a,b) 
					in 
						equalityExpression' (ts3,Ast.BinaryExpr(Ast.StrictNotEquals,nd1,nd3)) 
					end
			  | _ => 
					(trace(["<< equalityExpression"]);(ts1,nd1))
		
    in
        equalityExpression' (ts1,nd1)
    end

(*
	BitwiseAndExpression(b)    
    	EqualityExpression(b)
	    BitwiseAndExpressionr(b)  &  EqualityExpression(b)

	right recursive: (see pattern of MultiplicativeExpression)
*)

and bitwiseAndExpression (ts,a,b)=
    let val _ = trace([">> bitwiseAndExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = equalityExpression (ts,a,b)
		fun bitwiseAndExpression' (ts1,nd1) =
			case ts1 of
				BitwiseAnd :: ts2 => 
					let 
						val (ts3,nd3) = equalityExpression (ts2,a,b) 
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

and bitwiseXorExpression (ts,a,b)=
    let val _ = trace([">> bitwiseXorExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = bitwiseAndExpression (ts,a,b)
		fun bitwiseXorExpression' (ts1,nd1) =
			case ts1 of
				BitwiseXor :: ts2 => 
					let 
						val (ts3,nd3) = bitwiseAndExpression (ts2,a,b) 
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

and bitwiseOrExpression (ts,a,b)=
    let val _ = trace([">> bitwiseOrExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = bitwiseXorExpression (ts,a,b)
		fun bitwiseOrExpression' (ts1,nd1) =
			case ts1 of
				BitwiseOr :: ts2 => 
					let 
						val (ts3,nd3) = bitwiseXorExpression (ts2,a,b) 
					in 
						bitwiseOrExpression' (ts3,Ast.BinaryExpr(Ast.BitwiseOr,nd1,nd3)) 
					end
			  | _ => (trace(["<< bitwiseAnd"]);(ts1,nd1))
		
    in
        bitwiseOrExpression' (ts1,nd1)
    end

(*
	LogicalAndExpression(b)    
    	BitwiseOrExpression(b)
	    LogicalAndExpression(b)  &&  BitwiseOrExpression(b)
*)

and logicalAndExpression (ts,a,b)=
    let val _ = trace([">> logicalAndExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = bitwiseOrExpression (ts,a,b)
		fun logicalAndExpression' (ts1,nd1) =
			case ts1 of
				LogicalAnd :: ts2 => 
					let 
						val (ts3,nd3) = bitwiseOrExpression (ts2,a,b) 
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
	LogicalOrExpression(b)
    	LogicalAndExpression(b)
	    LogicalOrExpression(b)  ||  LogicalAndExpression(b)

*)

and logicalOrExpression (ts,a,b) =
    let val _ = trace([">> logicalOrExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = logicalAndExpression (ts,a,b)
		fun logicalOrExpression' (ts1,nd1) =
			case ts1 of
				LogicalOr :: ts2 => 
					let 
						val (ts3,nd3) = logicalAndExpression (ts2,a,b) 
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
	ConditionalExpression(ALLOWLIST, b)    
	    LetExpression(b)
    	YieldExpression(b)
	    LogicalOrExpression(b)
    	LogicalOrExpression(b)  ?  AssignmentExpression(ALLOWLIST,b)  
								   :  AssignmentExpression(ALLOWLIST,b)

	ConditionalExpression(NOLIST, b)    
    	SimpleYieldExpression
	    LogicalOrExpression(b)
    	LogicalOrExpression(b)  ?  AssignmentExpression(ALLOWLIST,b) 
								   :  AssignmentExpression(NOLIST,b)
    
*)

and conditionalExpression (ts,ALLOWLIST,b) =
    let val _ = trace([">> conditionalExpression ALLOWLIST with next=",tokenname(hd(ts))])
    in case ts of
        Let :: _ => letExpression(ts,b)
	  | Yield :: _ => yieldExpression(ts,b)
      | _ => 
			let
				val (ts2,nd2) = logicalOrExpression(ts,ALLOWLIST,b)
			in case ts2 of
				QuestionMark :: ts3 => 
					let
						val (ts4,nd4) = assignmentExpression(ts3,ALLOWLIST,b)
					in case ts4 of
						Colon :: ts5 =>
							let
								val (ts6,nd6) = assignmentExpression(ts5,ALLOWLIST,b)
							in
								(ts6,nd6)
							end
					  | _ => raise ParseError							
					end
			  | _ => 
					(trace(["<< conditionalExpression ALLOWLIST with next=",tokenname(hd(ts2))]);
					(ts2,nd2))
			end
		end
 
  | conditionalExpression (ts,NOLIST,b) =
    let val _ = trace([">> conditionalExpression NOLIST with next=",tokenname(hd(ts))])
    in case ts of
	    Yield :: _ => simpleYieldExpression ts
      | _ => 
			let
				val (ts2,nd2) = logicalOrExpression(ts,NOLIST,b)
			in case ts2 of
				QuestionMark :: ts3 => 
					let
						val (ts4,nd4) = assignmentExpression(ts3,NOLIST,b)
					in case ts4 of
						Colon :: ts5 =>
							let
								val (ts6,nd6) = assignmentExpression(ts5,NOLIST,b)
							in
								(ts6,nd6)
							end
					  | _ => raise ParseError							
					end
			  | _ => 
					(trace(["<< conditionalExpression NOLIST with next=",tokenname(hd(ts2))]);
					(ts2,nd2))
			end
		end

(*
	NonAssignmentExpression(allowList, b)   
    	LetExpression(b)
	    YieldExpression(b)
    	LogicalOrExpression(b)
	    LogicalOrExpression(b)  ?  NonAssignmentExpression(allowLet, b)  
							   :  NonAssignmentExpression(allowLet, b)
    
	NonAssignmentExpression(noList, b)    
    	SimpleYieldExpression
	    LogicalOrExpression(b)
    	LogicalOrExpression(b)  ?  NonAssignmentExpression(allowLet, b)  
							   :  NonAssignmentExpression(noLet, b)
*)

and nonAssignmentExpression (ts,ALLOWLIST,b) = raise ParseError
  | nonAssignmentExpression (ts,NOLIST,b) = raise ParseError

(*
	LetExpression(b)    
    	let  (  LetBindingList  )  ListExpression(b)
*)

and letExpression (ts,b) =
    let val _ = trace([">> letExpression with next=",tokenname(hd(ts))])
	in case ts of
		Let :: LeftParen :: ts1 => 
			let
				val (ts2,nd2) = letBindingList (ts1)
			in 
			case ts2 of
				RightParen :: ts3 =>
					let
				        val (ts4,nd4) = listExpression(ts3,b)
					in
						(trace(["<< letExpression with next=",tokenname(hd(ts4))]);
						(ts4,Ast.LetExpr{defs=nd2,body=nd4}))
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
			  | Comma :: _ =>
					let
						val (ts2,nd2) = nonemptyLetBindingList (tl ts1)
					in
						(trace(["<< nonemptyLetBindingList with next=",tokenname(hd ts2)]);
						(ts2,nd1::nd2))
					end
			  | _ => raise ParseError
			end
	in case ts of 
		RightParen :: _ => 
			(trace(["<< nonemptyLetBindingList with next=",tokenname(hd ts)]);
			(ts,[]))
	  | _ => 
			let
				val (ts1,nd1) = nonemptyLetBindingList ts
			in
				(trace(["<< letBindingList with next=",tokenname(hd ts1)]);
				(ts1,nd1))
			end
	end

(*
	YieldExpressionb    
    	yield
	    yield  [no line break]  ListExpressionb
*)

and yieldExpression (ts,b) =
    let val _ = trace([">> yieldExpression with next=",tokenname(hd(ts))]) 
	in case ts of
		Yield :: ts1 => 
			let
			in case ts1 of
				(SemiColon :: _ | RightBrace :: _ | RightParen :: _) => (ts1,Ast.YieldExpr NONE)
			  | _ => 
					let
						val (ts2,nd2) = listExpression(ts1,b)
					in
						(ts2,Ast.YieldExpr(SOME nd2))
					end
			end
	  | _ => raise ParseError
	end

(*
	SimpleYieldExpression    
    	yield
*)

and simpleYieldExpression ts =
	case ts of
		Yield :: ts1 => (ts1,Ast.YieldExpr NONE)
	  | _ => raise ParseError

(*
	AssignmentExpression(a, b)	
		ConditionalExpression(a, b)
		Pattern(a, b, allowExpr)  =  AssignmentExpression(a, b)
		Pattern(a, b, allowExpr)  CompoundAssignment  AssignmentExpression(a, b)
		Pattern(a, b, allowExpr)  LogicalAssignment  AssignmentExpression(a, b)
*)

and assignmentExpression (ts,a,b) : (token list * Ast.EXPR) = 
    let val _ = trace([">> assignmentExpression with next=",tokenname(hd(ts))])
		val (ts1,nd1) = conditionalExpression(ts,a,b)
    in case (ts1,nd1) of
	  	(Assign :: _, (Ast.ObjectRef _ | Ast.LexicalRef _ | Ast.LiteralExpr (Ast.LiteralObject _)
						| Ast.LiteralExpr (Ast.LiteralArray _)) ) => 
	    	let
			    (* todo: convert to pattern, may result in syntax error *)
				val (ts2,nd2) = assignmentExpression(tl ts1,a,b)						
			in 
				(ts2,Ast.BinaryExpr(Ast.Assign,nd1,nd2))
			end
	  | (Assign :: _, _) => 
			(error(["invalid lhs"]);raise ParseError)
	  | _ =>
			(trace(["<< assignmentExpression with next=",tokenname(hd(ts1))]);
			(ts1,nd1))
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
	ListExpression(b)    
    	AssignmentExpression(b)
	    ListExpression(b)  ,  AssignmentExpression(b)

	right recursive:

	ListExpression(b)    
    	AssignmentExpression(b) ListExpressionPrime(b)
    
	ListExpressionPrime(b)    
    	«empty»
	    , AssignmentExpression(b) ListExpressionPrime(b)
*)

and listExpression (ts,b) = 
    let
		val _ =	trace([">> listExpression with next=",tokenname(hd ts)])
		fun listExpression' (ts,b) =
	    	let
				val _ =	trace([">> listExpression' with next=",tokenname(hd ts)])
		    in case ts of
    		    Comma :: _ =>
					let
            			val (ts1,nd1) = assignmentExpression(tl ts,ALLOWLIST,b)
	               		val (ts2,nd2) = listExpression'(ts1,b)
	    	      	in
						(trace(["<< listExpression' with next=",tokenname(hd(ts2))]);
    	    	     	(ts2, nd1 :: nd2))
	    	      	end
	    	  | _ => (ts, [])
		    end
        val (ts1,nd1) = assignmentExpression(ts,ALLOWLIST,b)
        val (ts2,nd2) = listExpression'(ts1,b)
    in
		trace(["<< listExpression with next=",tokenname(hd(ts2))]);
        (ts2, Ast.ListExpr (nd1 :: nd2))
    end

(*
	Pattern(a, b, g)	
		SimplePattern(a, b, g)
		ObjectPattern(g)
		ArrayPattern(g)
*)

and pattern (ts,a,b,g) =
	let
	in case ts of
		LeftBrace :: _ => objectPattern (ts,g)
	  | LeftBracket :: _ => arrayPattern (ts,g)
	  | _ => simplePattern (ts,a,b,g)
	end

(*
	SimplePattern(a, b, noExpr)	
		Identifier
		
	SimplePattern(a, b, allowExpr)	
		PostfixExpression(a, b)
*)

and simplePattern (ts,a,b,NOEXPR) =
    let val _ = trace([">> simplePattern(a,b,NOEXPR) with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = identifier ts
	in
		(trace(["<< simplePattern(a,b,NOEXPR) with next=",tokenname(hd(ts1))]);
		(ts1,Ast.IdentifierPattern nd1))
	end

  | simplePattern (ts,a,b,ALLOWEXPR) =
    let val _ = trace([">> simplePattern(a,b,ALLOWEXPR) with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = postfixExpression (ts,a,b)
	in
		(trace(["<< simplePattern(a,b,ALLOWEXPR) with next=",tokenname(hd(ts1))]);
		(ts1,Ast.SimplePattern nd1))
	end

(*
	ObjectPattern(g)   
    	{  DestructuringFieldList(g)  }
*)

and objectPattern (ts,g) =
	let
	in case ts of
		LeftBrace :: ts =>
			let
				val (ts1,nd1) = destructuringFieldList (ts,g)
			in case ts1 of
				RightBrace :: _ => (tl ts1,Ast.ObjectPattern nd1)
			  | _ => raise ParseError
			end
	  | _ => raise ParseError
	end

(*
	DestructuringFieldList(g)
    	DestructuringField(g)
	    DestructuringFieldList(g)  ,  DestructuringField(g)
*)

and destructuringFieldList (ts,g) =
	let
		fun destructuringFieldList' (ts,g) =
			let
			in case ts of
				Comma :: _ =>
					let
						val (ts1,nd1) = destructuringField (tl ts,g)
						val (ts2,nd2) = destructuringFieldList' (ts1,g)
					in case ts of
					    _ => (ts2,nd1::nd2)
					end
			  | _ => (ts,[])
			end
		val (ts1,nd1) = destructuringField (ts,g)
		val (ts2,nd2) = destructuringFieldList' (ts1,g)
	in
		(ts2,nd1::nd2)
	end

(*
	DestructuringField(g)
    	NonAttributeQualifiedIdentifier  :  Pattern(NOLIST,ALLOWIN,g)
*)

and destructuringField (ts,g) =
	let
		val (ts1,nd1) = nonAttributeQualifiedIdentifier (ts)
	in case ts1 of
		Colon :: _ => 
			let
				val (ts2,nd2) = pattern (tl ts1,NOLIST,ALLOWIN,g)
			in
				(ts2,{name=Ast.LexicalRef {ident=nd1},ptrn=nd2})
			end
	  | _ => raise ParseError
	end

(*
	ArrayPattern(g)   
    	[  DestructuringElementList(g)  ]
*)

and arrayPattern (ts,g) =
	let
	in case ts of
		LeftBracket :: _ => 
			let
				val (ts1,nd1) = destructuringElementList (tl ts,g)
			in case ts1 of
				RightBracket :: _ => (tl ts1,Ast.ArrayPattern nd1)
			  | _ => raise ParseError
			end
	  | _ => raise ParseError
	end

(*
	DestructuringElementList(g)   
    	«empty»
	    DestructuringElement(g)
    	, DestructuringElementList(g)
	    DestructuringElement(g) , DestructuringElementList(g)
*)

and destructuringElementList (ts,g) =
    let val _ = trace([">> destructuringElementList with next=",tokenname(hd(ts))]) 
	in case ts of
		RightBracket :: _ => (ts,[])
	  | Comma :: _ => 
			let
				val (ts1,nd1) = destructuringElementList (tl ts,g)
			in
				(ts1,Ast.SimplePattern(Ast.LiteralExpr(Ast.LiteralUndefined)) :: nd1)
			end
	  | _ =>
			let
				val (ts1,nd1) = destructuringElement (ts,g)
			in case ts1 of
				Comma :: _ =>
					let
						val (ts2,nd2) = destructuringElementList (tl ts1,g)
					in
						(ts2,nd1::nd2)
					end
			  | _ => (ts1,nd1::[])
			end
	end

(*
	DestructuringElement(g)   
    	Pattern(NOLIST,ALLOWIN,g)
*)

and destructuringElement (ts,g) =
    let val _ = trace([">> destructuringElement with next=",tokenname(hd(ts))]) 		
	in
		pattern (ts,NOLIST,ALLOWIN,g)
	end

(*
	TypedIdentifier(a,b)	
		SimplePattern(a,b,noExpr)
		SimplePattern(a,b,noExpr)  :  TypeExpression
*)

and typedIdentifier (ts,a,b) =
    let val _ = trace([">> typedIdentifier with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = simplePattern (ts,a,b,NOEXPR)
	in case ts1 of
		Colon :: _ => 
			let
				val (ts2,nd2) = typeExpression (tl ts1)
			in
				(ts2, {p=nd1,t=SOME nd2})
			end
	  | _ => 
			let
			in
				(trace(["<< typedIdentifier with next=",tokenname(hd(ts1))]);
				(ts1, {p=nd1,t=NONE}))
			end
		
	end

(*		
	TypedPattern(a,b,g)
		SimplePattern(a,b,g)
		SimplePattern(a,b,g)  :  TypeExpression
		ObjectPattern
		ObjectPattern  :  RecordType
		ArrayPattern
		ArrayPattern  :  ArrayType
*)

and typedPattern (ts,a,b,g) =
    let val _ = trace([">> typedPattern with next=",tokenname(hd(ts))]) 
	in case ts of
		LeftBrace :: _ => 
			let
				val (ts1,nd1) = objectPattern (ts,g)
			in case ts1 of
				Colon :: _ =>
					let
						val (ts2,nd2) = recordType (tl ts1)
					in
						(ts2,{p=nd1,t=SOME nd2})
					end
			  | _ =>
					(ts1,{p=nd1,t=NONE})
			end
	  | LeftBracket :: _ => 
			let
				val (ts1,nd1) = arrayPattern (ts,g)
			in case ts1 of
				Colon :: _ =>
					let
						val (ts2,nd2) = arrayType (tl ts1)
					in
						(ts2,{p=nd1,t=SOME nd2})
					end
			  | _ =>
					(ts1,{p=nd1,t=NONE})
			end
	  | _ =>
			let
				val (ts1,nd1) = simplePattern (ts,a,b,g)
			in case ts1 of
				Colon :: _ =>
					let
						val (ts2,nd2) = typeExpression (tl ts1)
					in
						(ts2,{p=nd1,t=SOME nd2})
					end
			  | _ =>
					(trace(["<< typedPattern with next=",tokenname(hd ts1)]);
					(ts1,{p=nd1,t=NONE}))
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
				val rf = Ast.LexicalRef {ident=nd1}
            in case ts1 of
				Not :: _ =>
					(tl ts1,Ast.PrimaryType {ident=nd1,kind=Ast.NonNullable}) 
			  | QuestionMark :: _ =>
					(tl ts1,Ast.PrimaryType{ident=nd1,kind=Ast.Nullable}) 
			  | _ =>
					(ts1,Ast.PrimaryType{ident=nd1,kind=Ast.Named}) 
            end
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

and semicolon (ts,nd,FULL) =
    let val _ = trace([">> semicolon(FULL) with next=", tokenname(hd ts)])
	in case ts of
		SemiColon :: _ => (tl ts,nd)
	  | (Eof | RightBrace) :: _ => (ts,nd)   (* ABBREV special cases *)
	  | _ => 
			if newline ts then (log(["inserting semicolon"]);(ts,nd))
			else (error(["expecting semicolon before ",tokenname(hd ts)]); raise ParseError)
	end
  | semicolon (ts,nd,_) =
    let val _ = trace([">> semicolon(ABBREV | NOSHORTIF) with next=", tokenname(hd ts)])
	in case ts of
		SemiColon :: _ => (tl ts,nd)
	  | _ => 
          (trace(["<< semicolon(ABBREV | NOSHORTIF) with next=", tokenname(hd ts)]);
          (ts,nd))
	end

and statement (ts,omega) =
    let val _ = trace([">> statement with next=", tokenname(hd ts)])
	in case ts of
	    Return :: _ =>
			let
				val (ts1,nd1) = returnStatement (ts)
				val (ts2,nd2) = semicolon (ts1,nd1,omega)
			in
				(ts2,nd2)
			end
	  | _ =>
			let
				val (ts1,nd1) = expressionStatement (ts)
 				val (ts2,nd2) = semicolon (ts1,nd1,omega)
			in
				trace(["<< statement with next=", tokenname(hd ts2)]);
				(ts2,nd2)
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
		trace(["<< expressionStatement with next=", tokenname(hd ts1)]);
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
		Return :: (SemiColon | RightBrace) :: _ => 
			(tl ts,Ast.ReturnStmt (Ast.ListExpr []))
	  | Return :: _ =>
			if newline(tl ts) then 
				(tl ts,Ast.ReturnStmt (Ast.ListExpr []))
			else 
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

	DirectivesPrefix    
    	«empty»
	    Pragmas DirectivePrefix'

	DirectivesPrefix'
	   	«empty»
		Directive(full) DirectivesPrefix'
*)

and directives (ts) : (token list * Ast.STMT list) =
    let val _ = trace([">> directives with next=", tokenname(hd ts)])
	in case ts of
		(RightBrace | Eof) :: _ => (ts,[])
	  | _ => 
			let
				val (ts1,nd1) = directivesPrefix ts
(*				val (ts2,nd2) = directive(ts1,ABBREV)   *)
			in
				trace(["<< directives with next=", tokenname(hd ts1)]);
				(ts1,nd1)
			end
	end

and directivesPrefix (ts) : (token list * Ast.STMT list) =
    let val _ = trace([">> directivesPrefix with next=", tokenname(hd ts)])
		fun directivesPrefix' ts =
		    let val _ = trace([">> directivesPrefix' with next=", tokenname(hd ts)])
			in case ts of
				(RightBrace | Eof) :: _ => (ts,[])
			  | _ => 
					let
						val (ts1,nd1) = directive (ts,FULL)
						val (ts2,nd2) = directivesPrefix' ts1
					in
						trace(["<< directivesPrefix' with next=", tokenname(hd ts2)]);
						(ts2,nd1 :: nd2)
					end
			end
		
	in case ts of
		(RightBrace | Eof) :: _ => (ts,[])
	  | _ => 
			let
				val (ts2,nd2) = directivesPrefix' ts
			in
				trace(["<< directivesPrefix with next=", tokenname(hd ts2)]);
				(ts2,nd2)
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

and variableDefinition (ts,b) =
    let val _ = trace([">> variableDefinition with next=", tokenname(hd ts)])
	in case ts of
		Const :: _ => variableBindingList (tl ts,Ast.Const,ALLOWLIST,b)
	  | Var :: _ => variableBindingList (tl ts,Ast.Var,ALLOWLIST,b)
	  | _ => raise ParseError
	end

and variableBindingList (ts,tag,a,b) = 
    let val _ = trace([">> variableBindingList with next=", tokenname(hd ts)])
		fun variableBindingListPrime (ts,tag,a,b) =
	    	let
		    in case ts of
    		    Comma :: _ =>
					let
            			val (ts1,nd1) = variableBinding(ts,tag,a,b)
	               		val (ts2,nd2) = variableBindingListPrime(ts1,tag,a,b)
	    	      	in
    	    	     	(ts2, Ast.VariableDefn nd1 :: nd2)
	    	      	end
	    	  | _ => (ts, [])
		    end
   			val (ts1,nd1) = variableBinding(ts,tag,a,b)
       		val (ts2,nd2) = variableBindingListPrime(ts1,tag,a,b)
    in
        (ts2, Ast.VariableDefn nd1 :: nd2)
    end

and variableBinding (ts,tag,a,b) = 
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
		val (ts1,{p,t}) = typedPattern (ts,a,b,NOEXPR)
	in case (ts1,p) of
			(Assign :: _,_) =>
				let
					val _ = trace(["xx variableBinding with next=", tokenname(hd ts1)])
					val (ts2,nd2) = assignmentExpression (tl ts1,a,b)
				in
					(trace(["<< variableBinding with next=", tokenname(hd ts2)]);
					(ts2, Ast.VariableDefinition { tag = tag,
                        					   init = SOME nd2,
                        					   attrs = defaultAttrs,
                        					   pattern = p,
                        					   ty = t } ))
				end
		  | (_,Ast.IdentifierPattern _) =>
				let
				in
					(trace(["<< variableBinding with next=", tokenname(hd ts1)]);
					(ts1, Ast.VariableDefinition { tag = tag,
                         					   init = NONE,
                         					   attrs = defaultAttrs,
                         					   pattern = p,
                         					   ty = t } ))
				end
		  | (_,_) => raise ParseError
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

fun dumpLineBreaks (lbs,lst) =
	case lbs of
		[] => rev lst
      | _ => dumpLineBreaks(tl lbs, Int.toString(hd lbs) :: "\n  " :: lst)

fun lexFile (filename : string) : (token list) = 
    let 
        val lexer = Lexer.makeLexer (mkReader filename)
		val tokens = Lexer.UserDeclarations.token_list lexer
		val line_breaks = !Lexer.UserDeclarations.line_breaks
    in
        log ("tokens:" :: dumpTokens(tokens,[])); 
        log ("line breaks:" :: dumpLineBreaks(line_breaks,[])); 
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
     handle ParseError => (log ["parse error"]; raise ParseError)
          | Lexer.LexError => (log ["lex error"]; raise Lexer.LexError)

end (* Parser *)
