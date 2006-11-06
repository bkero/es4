structure Parser = struct

open Token

exception ParseError

datatype alpha =
    ALLOWLET
  | NOLET

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

(*

Identifier    
    Identifier
    ContextuallyReservedIdentifier
*)

fun identifier ts =
    let
    in case ts of
        IDENTIFIER(nm) :: tr  => (tr,nm)
      | TYPE :: tr  => (tr,"type")
      | _ => raise ParseError
    end

(*
    PropertyIdentifier ->
        Identifier
        *
*)

and propertyIdentifier ts =
    let 
        val _ = log([">> propertyIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        MULT :: tr => (tr,"*")
      | _ => 
            let
                val (ts1,nd1) = identifier ts
            in
                (log(["<< propertyIdentifier with next=",tokenname(hd(ts1))]);(ts1,nd1)) 
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
        (INTERNAL :: _ | INTRINSIC :: _ | PRIVATE :: _ | PROTECTED :: _ | PUBLIC :: _) => 
          let
              val (ts1,nd1) = reservedNamespace (ts)
          in
              (ts1,Ast.LiteralExpr(nd1))
          end
      | _ => 
          let
              val (ts1,nd1) = propertyIdentifier (ts)
          in
              (ts1,Ast.QualIdent({qual=NONE, ident=nd1, opennss=[Ast.Namespace (Ast.PUBLIC,"")]}))
          end
    end

and reservedNamespace ts =
    let val _ = log([">> reservedNamespace with next=",tokenname(hd(ts))])
    in case ts of
        INTERNAL :: tr => (tr, Ast.LiteralNamespace(Ast.Namespace (Ast.INTERNAL,"put package name here")))
      | INTRINSIC :: tr => (tr, Ast.LiteralNamespace(Ast.Namespace (Ast.INTRINSIC,"")))
      | PRIVATE :: tr => (tr, Ast.LiteralNamespace(Ast.Namespace (Ast.PRIVATE,"put class name here")))
      | PROTECTED :: tr => (tr, Ast.LiteralNamespace(Ast.Namespace (Ast.PROTECTED,"put class name here")))
      | PUBLIC :: tr => (tr, Ast.LiteralNamespace(Ast.Namespace (Ast.PUBLIC,"")))
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
    let val _ = log([">> simpleQualifiedIdentifier with next=",tokenname(hd(ts))]) 
    in 
    case ts of
        (INTERNAL :: _ | INTRINSIC :: _ | PRIVATE :: _ | PROTECTED :: _ | PUBLIC :: _) => 
          let 
              val (ts1, nd1) = reservedNamespace(ts)
          in case ts1 of
              DOUBLECOLON :: ts2 => qualifiedIdentifierPrime(ts2,Ast.LiteralExpr(nd1))
            | _ => raise ParseError
          end
      | _ => 
          let
              val (ts1, nd1) = propertyIdentifier(ts)
          in case ts1 of
              DOUBLECOLON :: ts2 => qualifiedIdentifierPrime(ts2,Ast.LiteralExpr(Ast.LiteralString(nd1)))
            | _ => (log(["<< simpleQualifiedIdentifier with next=",tokenname(hd(ts))]);(ts1, Ast.QualIdent({qual=NONE, ident=nd1, opennss=[Ast.Namespace(Ast.PUBLIC,"")]})))
          end
    end

and expressionQualifiedIdentifier ts =
    let val (ts1,nd1) = parenListExpression(ts)
    in 
    case ts1 of
        DOUBLECOLON :: ts2 => qualifiedIdentifierPrime(ts2,nd1)
      | _ => raise ParseError
    end

and reservedOrPropertyIdentifier ts =
    case isreserved(hd ts) of
        true => (tl ts, tokenname(hd ts))
      | false => propertyIdentifier(ts)

and qualifiedIdentifierPrime (ts, nd1) : (token list * Ast.expr) =
    let val _ = log([">> qualifiedIdentifierPrime with next=",tokenname(hd(ts))]) 
    in case ts of
        LEFTBRACKET :: _ => raise ParseError (* Brackets ts *)
      | tk :: ts1 =>
            let
                val (ts2,nd2) =    reservedOrPropertyIdentifier(ts)
                val (ts3,nd3) = (ts2,Ast.QualIdent({qual=(SOME(nd1)), ident=nd2, opennss=[Ast.Namespace(Ast.PUBLIC,"")]}))
            in
                (ts3,nd3)
            end
    end

(*
    NonAttributeQualifiedIdentifier    
        SimpleQualifiedIdentifier
        ExpressionQualifiedIdentifier
*)

and nonAttributeQualifiedIdentifier ts =
    let val _ = log([">> nonAttributeQualifiedIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        LEFTPAREN :: _ => expressionQualifiedIdentifier(ts)
      | _ => 
        let
            val (ts1,nd1) = simpleQualifiedIdentifier(ts)
        in
            (log(["<< nonAttributeQualifiedIdentifier with next=",tokenname(hd(ts1))]);(ts1,nd1))
        end
    end

(*
    AttributeIdentifier    
        @  Brackets
        @  NonAttributeQualifiedIdentifier
*)

and attributeIdentifier ts =
    let val _ = log([">> attributeIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        AT :: LEFTBRACKET :: _ => brackets(ts)
      | AT :: t1 => nonAttributeQualifiedIdentifier(t1)
      | _ => raise ParseError
    end

(*

    QualifiedIdentifier    
        AttributeIdentifier
        NonAttributeQualifiedIdentifier

*)

and qualifiedIdentifier ts =
    case ts of
        AT :: _ => attributeIdentifier(ts)
      | _ => nonAttributeQualifiedIdentifier(ts)

(*
    SimpleTypeIdentifier    
        PackageIdentifier  .  Identifier
        NonAttributeQualifiedIdentifier
*)

and simpleTypeIdentifier ts =
    case ts of
        PACKAGEIDENTIFIER :: DOT :: ts1 => 
            let val (ts2,nd2) = identifier(ts) in (ts2,Ast.LiteralExpr(Ast.LiteralString(nd2))) end
      | _ => nonAttributeQualifiedIdentifier(ts)

(*
    TypeIdentifier    
        SimpleTypeIdentifier
        SimpleTypeIdentifier  .<  TypeExpressionList  >
*)

and typeIdentifier ts =
    let
        val (ts1,nd1) = simpleTypeIdentifier ts
    in case ts1 of
        LEFTDOTANGLE :: ts2 => 
            let
                val (ts3,nd3) = typeExpressionList ts2
            in case ts3 of
                RIGHTANGLE :: ts4 => (ts4,nd3)
              | _ => raise ParseError
            end
      | _ => (ts1, nd1)        
    end

(*
    ParenExpression    
        (  AssignmentExpressionallowLet, allowIn  )
*)

and parenExpression ts =
    let
    in case ts of
        LEFTPAREN :: ts1 => 
            let
                val (ts2,nd2:Ast.expr) = assignmentExpression (ts1,ALLOWLET,ALLOWIN)
            in case ts2 of
                RIGHTPAREN :: ts3 => (ts3,nd2)
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    ParenListExpression    
        (  ListExpressionallowIn  )
*)

and parenListExpression ts =
    let
    in case ts of
        LEFTPAREN :: ts1 => 
            let
                val (ts2,nd2) = listExpression (ts1,ALLOWIN)
            in case ts2 of
                RIGHTPAREN :: ts3 => (ts3,nd2)
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    FunctionExpression    
        function  FunctionCommon
        function  Identifier  FunctionCommon
*)

and functionExpression ts =
    let
    in case ts of
        FUNCTION :: LEFTPAREN :: ts1 => functionCommon ts1
      | FUNCTION :: tk1 :: ts1 => functionCommon ts1
      | _ => raise ParseError
    end

(*
    FunctionCommon    
        FunctionSignature
        FunctionSignature Block
*)

and functionCommon ts =
    let
        val (ts1,nd1) = functionSignature ts
    in 
    case ts1 of
        LEFTBRACE :: _ => 
            let
                val (ts2,nd2) = block ts1
            in
                (ts2,Ast.FunExpr({sign=nd1,body=nd2}))
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
    let
    in case ts of
        LEFTDOTANGLE :: ts1 => 
            let
                val (ts2,nd2) = typeParameterList ts1
            in
                let
                in case ts2 of 
                    GREATERTHAN :: ts3 => functionSignaturePrime (ts3, nd2)
                  | _ => raise ParseError
                end
            end
      | _ => functionSignaturePrime (ts, [])
    end

and functionSignaturePrime (ts, nd1) =
    case ts of
        LEFTPAREN :: ts1 =>
           let
               val (ts2, nd2) = parameters ts1
           in case ts2 of
               RIGHTPAREN :: tsx =>
                   let
                       val (ts3,nd3) = resultType tsx
                   in
                       (ts3,Ast.FunctionSignature {typeparams=nd1,params=nd2,resulttype=nd3})
                   end
             | _ => raise ParseError
           end
      | _ => raise ParseError

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

and typeParameterList ts =
    let
(*
       val (ts1,nd1) = (ts, [Ast.PrimaryType({name=Ast.LiteralExpr(Ast.LiteralString("int")),
                                       annotation=Ast.NONNULLABLE})]) 
*)
        val (ts1,nd1) = identifier ts
        val (ts2,nd2) = typeParameterListPrime (ts1,Ast.LiteralString(nd1)::nil)
    in
        (ts2,nd2)
    end

and typeParameterListPrime (ts, lst) =
    let
    in case ts of
        COMMA :: ts1 =>
           let
               val (ts2,nd2) = identifier(ts1)
               val (ts3,nd3) = typeParameterListPrime(ts2,Ast.LiteralString(nd2)::lst)
           in
             (ts3,nd3)
           end
      | _ => (ts,lst)
     end

(*
    Parameters    
        «empty»
        NonemptyParameters(ALLOWLET)
*)

and parameters ts = raise ParseError

(*

    NonemptyParameters(alpha)    
        ParameterInit(alpha)
        ParameterInitnoLet  ,  NonemptyParameters(alpha)
        RestParameter

*)

and nonemptyParameters ts alpha = raise ParseError

(*
    ParameterInit(alpha)
        Parameter
        Parameter  =  NonAssignmentExpression(alpha,ALLOWIN)
*)

and parameterInit ts alpha = raise ParseError

(*
    Parameter    
        ParameterAttributes TypedIdentifier(ALLOWIN)
        ParameterAttributes TypedPattern
*)

and parameter ts alpha = raise ParseError

(*
    ParameterAttributes    
        «empty»
        const
*)

and parameterAttributes ts = raise ParseError

(*
    RestParameter    
        ...
        ...  ParameterAttributes TypedIdentifier
        ...  ParameterAttributes TypedPattern
*)

and restParameter ts = raise ParseError

(*
    ResultType    
        «empty»
        :  void
        :  TypeExpression
*)

and resultType ts = 
    let
    in case ts of
        VOID :: ts1 => (ts1,Ast.SpecialType(Ast.NOTYPE))
      | _ => typeExpression ts
    end

(*

    ObjectLiteral    
        {  FieldList  }
        {  FieldList  }  :  RecordType
*)

and objectLiteral ts = raise ParseError

(*
    FieldList    
        «empty»
        NonemptyFieldListallowLet
*)

and fieldList ts = raise ParseError

(*
    NonemptyFieldList(alpha)
        LiteralField(alpha)
        LiteralFieldnoLet  ,  NonemptyFieldList(alpha)
*)

and nonemptyFieldList (ts, alpha) = raise ParseError

(*
    LiteralField (alpha)    
        FieldName  :  AssignmentExpression(alpha, ALLOWIN)
*)

and literalField ts alpha = raise ParseError

(*
    FieldName    
        NonAttributeQualifiedIdentifier
        StringLiteral
        NumberLiteral
        ReservedIdentifier
        ParenExpression
*)

and fieldName ts = raise ParseError

(*
    ArrayLiteral    
        [  ElementList(ALLOWLET)  ]
        [  ElementList(ALLOWLET)  ]  :  ArrayType
*)

and arrayLiteral ts = raise ParseError

(*
    ElementList(alpha)
        «empty»
        LiteralElement(alpha)
        ,  ElementList(alpha)
        LiteralElement(NOLET)  ,  ElementList(alpha)
*)

(*
    LiteralElement(alpha)
        AssignmentExpression(alpha, ALLOWIN)
*)

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
    CastExpression    
        cast  TypeExpression  ParenListExpression
*)

and castExpression ts =
    let
    in case ts of
        CAST :: ts1 =>
            let
                val (ts2,nd2) = typeExpression(ts1)
                val (ts3,nd3) = parenListExpression(ts2)
            in
                (ts3,Ast.BinaryTypeExpr(Ast.CAST,nd3,nd2))
            end
      | _ => raise ParseError
    end

(*

PrimaryExpression    
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
    FunctionExpression
    CastExpression

*)

and regexpLiteral (ts, regex) =
    let val _ = log(["regexp=",regex])
    in case ts of 
        DIV :: tr => 
            let
            in
                (tr,Ast.LiteralExpr(Ast.LiteralRegExp{pattern=regex,global=false,multiline=false,caseInsensitive=false}))
            end
      | tk :: tr  => regexpLiteral (tr,tokenname(tk)^regex)
    end

and primaryExpression ts =
    let val _ = log([">> primaryExpression with next=",tokenname(hd ts)])
    in case ts of
        NULL :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralNull))
      | TRUE :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean true))
      | FALSE :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean false))
      | NUMBERLITERAL n :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralNumber n))
      | STRINGLITERAL s :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralString s))
      | THIS :: ts1 => (ts1, Ast.NullaryExpr Ast.THIS)
      | AT :: _ => attributeIdentifier ts
      | LEFTPAREN :: _ => parenListExpression ts
      | LEFTBRACKET :: _ => arrayLiteral ts
      | LEFTBRACE :: _ => objectLiteral ts
      | FUNCTION :: _ => functionExpression ts
      | CAST :: _ => castExpression ts
      | DIV :: ts1 => regexpLiteral (ts1,"")
            
(* todo
      | REGEXP r :: ts1 => (ts1, Ast.RegExp(r))
      | (XMLMARKUP | LESSTHAN ) :: _ => xmlInitializer ts
*)
      | EOL :: ts1 => primaryExpression ts1
      | _ => typeIdentifier ts
    end

(*

SuperExpression    
    super
    super  ParenExpression

*)

and superExpression ts =
    let val _ = log([">> superExpression with next=",tokenname(hd(ts))]) 
        val (SUPER :: ts1) = ts
    in case ts1 of
        LEFTPAREN :: _ => 
            let
                val (ts2,nd2) = parenExpression(ts1)
                val (ts3,nd3) = (ts2,Ast.SuperExpr(SOME(nd2)))
            in
                (ts3,nd3)
            end
        | _ => (ts1,Ast.SuperExpr(NONE))
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

and memberExpression ts =
    let val _ = log([">> memberExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        NEW :: ts1 =>
            let
                val (ts2,nd2) = memberExpression(ts1)
                val (ts3,nd3) = arguments(ts2,nd2)
                val (ts4,nd4) = memberExpressionPrime(ts3,nd3)
               in
                (ts4,nd4)
            end
      | SUPER :: _ =>
            let
                val (ts2,nd2) = superExpression(ts)
                val (ts3,nd3) = propertyOperator(ts2,nd2)
                val (ts4,nd4) = memberExpressionPrime(ts3,nd3)
               in
                (ts4,nd4)
            end
      | _ =>
            let
                val (ts3,nd3) = primaryExpression(ts)
                val (ts4,nd4) = memberExpressionPrime(ts3,nd3)
               in
                (log(["<< memberExpression with next=",tokenname(hd ts4)]);(ts4,nd4))
            end
    end

and memberExpressionPrime (ts,nd) =
    let val _ = log([">> memberExpressionPrime with next=",tokenname(hd(ts))])
    in case ts of
        (LEFTBRACKET :: ts1 | DOT :: ts1) =>
            let
                val (ts2,nd2) = propertyOperator(ts1,nd)
            in
                memberExpressionPrime(ts2, nd2)
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

and callExpression ts =
    let val _ = log([">> callExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = memberExpression(ts)
        val (ts2,nd2) = arguments(ts1,nd1)
    in 
        callExpressionPrime(ts2,nd2)
    end

and callExpressionPrime (ts,nd) =
    let val _ = log([">> callExpressionPrime with next=",tokenname(hd(ts))])
    in case ts of
        (LEFTBRACKET :: ts1 | DOT :: ts1) =>
            let
                val (ts2,nd2) = propertyOperator(ts1,nd)
            in
                memberExpressionPrime(ts2, nd2)
            end
      | LEFTPAREN :: _ => 
            let
                val (ts2,nd2) = arguments(ts,nd)
            in
                memberExpressionPrime(ts2,nd2)
            end
      | _ => (ts,nd)
    end

(*
    Arguments    
        (  )
        (  ArgumentList(ALLOWLET)  )
*)

and arguments (ts, nd) =
    let val _ = log([">> callExpression with next=",tokenname(hd(ts))]) 
        val (LEFTPAREN::ts1,nd1) = (ts,nd)
        val (ts2,nd2) = argumentList(ts1,ALLOWLET)
        val (RIGHTPAREN::ts3,nd3) = (ts2,nd2)
    in 
        (ts3,nd3)
    end

(*
    ArgumentList(alpha)    
        AssignmentExpression(alpha, ALLOWIN)
        ArgumentList(NOLET)  ,  AssignmentExpression(alpha, ALLOWIN)

    refactored:

    ArgumentList(alpha)
        AssignmentExpression(alpha,ALLOWIN) ArgumentListPrime(ALLOWLET)

    ArgumentListPrime(alpha)
        «empty»
        [NOLET] , AssignmentExpression(alpha,ALLOWIN) ArgumentListPrime(ALLOWLET)
*)

and argumentList (ts, alpha) =
    let val _ = log([">> argumentList with next=",tokenname(hd(ts))])
        val (ts1,nd1) = assignmentExpression(ts,alpha,ALLOWIN)
        val (ts2,nd2) = argumentListPrime(ts1,ALLOWLET,nd1)
    in
        (ts2,nd2)
    end

and argumentListPrime (ts, alpha, nd) =
    let val _ = log([">> argumentList with next=",tokenname(hd(ts))])
    in case (ts,nd) of
        (COMMA :: _, Ast.LetExpr _) => raise ParseError
      | (COMMA :: ts1, _) => 
            let
                val (ts2,nd2) = assignmentExpression(ts1,alpha,ALLOWIN)
                val (ts3,nd3) = argumentListPrime(ts2,ALLOWLET,nd2)
            in
                (ts3,nd3)
            end
      | _ => (ts,nd)
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
    let val _ = log([">> propertyOperator with next=",tokenname(hd(ts))]) 
    in case ts of
        DOT :: ts1 =>
            let
            in case ts1 of
                PAREN :: _ =>
                    let
                        val (ts2,nd2) = parenListExpression(ts1)
                        val (DOUBLECOLON :: ts3,nd3) = (ts2,nd2)
                    in case ts3 of
                        BRACKET :: _ => 
                            let
                                val (ts4,nd4) =    brackets(ts3)
                            in
                                (ts4,Ast.Property {obj=nd,field=nd4,indirect=false})
                            end
                      | _ => 
                            let
                                val (ts4,nd4) =    reservedOrPropertyIdentifier(ts3)
                            in
                                (ts4,Ast.Property({obj=nd,field=Ast.LiteralExpr(Ast.LiteralString(nd4)),indirect=false}))
                            end
                    end
              | _ => 
                    let
                        val (ts4,nd4) =    reservedOrPropertyIdentifier(ts1)
                    in
                        (ts4,Ast.Property({obj=nd,field=Ast.LiteralExpr(Ast.LiteralString(nd4)),indirect=false}))
                    end
            end
      | LEFTBRACKET :: _ => 
            let
                val (ts4,nd4) =    brackets(ts)
            in
                (ts4,Ast.Property({obj=nd,field=nd4,indirect=false}))
            end
      | _ => raise ParseError
    end

(*

Brackets    
    [  ]
    [  ListExpressionallowIn  ]
    [  ListExpressionallowIn  :  ListExpressionallowIn  ]
    
*)

and brackets (ts) =
    raise ParseError

(*

NewExpression    
    MemberExpression
    new  NewExpression
    
*)

and newExpression ts =
    let val _ = log([">> leftHandSideExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        NEW :: ts1 =>
            let
                val (ts2,nd2) = newExpression(ts1)
            in
                (ts2,Ast.NewExpr({obj=nd2,actuals=[]}))
            end
      | _ => memberExpression(ts)
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

and leftHandSideExpression ts =
    let val _ = log([">> leftHandSideExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        NEW :: ts1 =>
            let
                val (ts2,nd2) = newExpression(ts1)
            in
                (ts2,Ast.NewExpr({obj=nd2,actuals=[]}))
            end
      | _ =>
            let
                val (ts2,nd2) = memberExpression(ts)
            in case ts2 of
                LEFTPAREN :: _ =>
                    let
                        val (ts3,nd3) = arguments(ts2,nd2)
                    in
                        callExpressionPrime(ts3,nd3)
                    end
              | _ => (ts2,nd2)
            end
    end

(*

PostfixExpression    
    LeftHandSideExpression
    LeftHandSideExpression  [no line break]  ++
    LeftHandSideExpression  [no line break]  --
    
*)

and postfixExpression ts =
    let val _ = log([">> postfixExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = leftHandSideExpression(ts)
    in case ts1 of
		PLUSPLUS :: ts2 => (ts2,Ast.UnaryExpr(Ast.POST_INCREMENT,nd1))
	  | MINUSMINUS :: ts2 => (ts2,Ast.UnaryExpr(Ast.POST_DECREMENT,nd1))
	  | _ => (ts1,nd1)
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

and unaryExpression ts =
    let val _ = log([">> unaryExpression with next=",tokenname(hd(ts))]) 
    in case ts of
		DELETE :: ts1 => let val (ts2,nd2) = postfixExpression ts1 in (ts2,Ast.UnaryExpr(Ast.DELETE,nd2)) end
	  | VOID :: ts1 => let val (ts2,nd2) = unaryExpression ts1 in (ts2,Ast.UnaryExpr(Ast.VOID,nd2)) end
	  | TYPEOF :: ts1 => let val (ts2,nd2) = unaryExpression ts1 in (ts2,Ast.UnaryExpr(Ast.TYPEOF,nd2)) end
	  | PLUSPLUS :: ts1 => let val (ts2,nd2) = postfixExpression ts1 in (ts2,Ast.UnaryExpr(Ast.PRE_INCREMENT,nd2)) end
	  | MINUSMINUS :: ts1 => let val (ts2,nd2) = postfixExpression ts1 in (ts2,Ast.UnaryExpr(Ast.PRE_DECREMENT,nd2)) end
	  | PLUS :: ts1 => let val (ts2,nd2) = unaryExpression ts1 in (ts2,Ast.UnaryExpr(Ast.UNARY_PLUS,nd2)) end
	  | BITWISENOT :: ts1 => let val (ts2,nd2) = unaryExpression ts1 in (ts2,Ast.UnaryExpr(Ast.BITWISE_NOT,nd2)) end
	  | NOT :: ts1 => let val (ts2,nd2) = unaryExpression ts1 in (ts2,Ast.UnaryExpr(Ast.LOGICAL_NOT,nd2)) end
	  | TYPE :: ts1 => let val (ts2,nd2) = typeExpression ts1 in (ts2,Ast.TypeExpr(nd2)) end
	  | _ => postfixExpression ts
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

and multiplicativeExpression ts =
    let val _ = log([">> multiplicativeExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = unaryExpression ts
		fun multiplicativeExpression' (ts1, nd1) =
			case ts1 of
				MULT :: ts2 => let val (ts3,nd3) = unaryExpression ts2 in multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.TIMES,nd1,nd3)) end
			  | DIV :: ts2 => let val (ts3,nd3) = unaryExpression ts2 in multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.DIVIDE,nd1,nd3)) end
			  | MODULUS :: ts2 => let val (ts3,nd3) = unaryExpression ts2 in multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.REMAINDER,nd1,nd3)) end
			  | _ => (ts1,nd1)
    in
        multiplicativeExpression' (ts1,nd1)
    end

(*

AdditiveExpression    
    MultiplicativeExpression
    AdditiveExpression  +  MultiplicativeExpression
    AdditiveExpression  -  MultiplicativeExpression

right recursive: (see pattern of MultiplicativeExpression)

*)

and additiveExpression ts =
    let val _ = log([">> additiveExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = multiplicativeExpression ts
		fun additiveExpression' (ts1, nd1) =
			case ts1 of
				PLUS :: ts2 => let val (ts3,nd3) = multiplicativeExpression ts2 in additiveExpression' (ts3,Ast.BinaryExpr(Ast.PLUS,nd1,nd3)) end
			  | MINUS :: ts2 => let val (ts3,nd3) = multiplicativeExpression ts2 in additiveExpression' (ts3,Ast.BinaryExpr(Ast.MINUS,nd1,nd3)) end
			  | _ => (ts1,nd1)
    in
        additiveExpression' (ts1,nd1)
    end

(*

	ShiftExpression    
	    AdditiveExpression
	    ShiftExpression  <<  AdditiveExpression
	    ShiftExpression  >>  AdditiveExpression
    	ShiftExpression  >>>  AdditiveExpression
    
	right recursive: (see pattern of MultiplicativeExpression)

*)

and shiftExpression ts =
    let val _ = log([">> shiftExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = additiveExpression ts
		fun shiftExpression' (ts1, nd1) =
			case ts1 of
				LEFTSHIFT :: ts2 => let val (ts3,nd3) = additiveExpression ts2 in shiftExpression' (ts3,Ast.BinaryExpr(Ast.LEFT_SHIFT,nd1,nd3)) end
			  | RIGHTSHIFT :: ts2 => let val (ts3,nd3) = additiveExpression ts2 in shiftExpression' (ts3,Ast.BinaryExpr(Ast.RIGHT_SHIFT,nd1,nd3)) end
			  | UNSIGNEDRIGHTSHIFT :: ts2 => let val (ts3,nd3) = additiveExpression ts2 in shiftExpression' (ts3,Ast.BinaryExpr(Ast.RIGHT_SHIFT_UNSIGNED,nd1,nd3)) end
			  | _ => (ts1,nd1)
    in
        shiftExpression' (ts1,nd1)
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

	RelationalExpression(NOIN)
    	ShiftExpression
	    RelationalExpressionallowIn  <  ShiftExpression
    	RelationalExpressionallowIn  >  ShiftExpression
	    RelationalExpressionallowIn  <=  ShiftExpression
    	RelationalExpressionallowIn  >=  ShiftExpression
    	RelationalExpressionallowIn  instanceof  ShiftExpression
	    RelationalExpressionallowIn  is  TypeExpression
	    RelationalExpressionallowIn  to  TypeExpression

	right recursive: (see pattern of MultiplicativeExpression)

*)

and relationalExpression (ts, beta)=
    let val _ = log([">> relationalExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = shiftExpression ts

(* fixme: reuse common code here *)

		fun relationalExpression' (ts1, nd1, ALLOWIN) =
			case ts1 of
				LESSTHAN :: ts2 => let val (ts3,nd3) = shiftExpression ts2 in relationalExpression' (ts3,Ast.BinaryExpr(Ast.LESS,nd1,nd3),ALLOWIN) end
			  | GREATERTHAN :: ts2 => let val (ts3,nd3) = shiftExpression ts2 in relationalExpression' (ts3,Ast.BinaryExpr(Ast.GREATER,nd1,nd3),ALLOWIN) end
			  | LESSTHANOREQUAL :: ts2 => let val (ts3,nd3) = shiftExpression ts2 in relationalExpression' (ts3,Ast.BinaryExpr(Ast.LESS_OR_EQUAL,nd1,nd3),ALLOWIN) end
			  | GREATERTHANOREQUAL :: ts2 => let val (ts3,nd3) = shiftExpression ts2 in relationalExpression' (ts3,Ast.BinaryExpr(Ast.GREATER_OR_EQUAL,nd1,nd3),ALLOWIN) end
			  | IN :: ts2 => let val (ts3,nd3) = shiftExpression ts2 in relationalExpression' (ts3,Ast.BinaryExpr(Ast.IN,nd1,nd3),ALLOWIN) end
			  | INSTANCEOF :: ts2 => let val (ts3,nd3) = shiftExpression ts2 in relationalExpression' (ts3,Ast.BinaryExpr(Ast.INSTANCEOF,nd1,nd3),ALLOWIN) end
			  | IS :: ts2 => let val (ts3,nd3) = typeExpression ts2 in relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.IS,nd1,nd3),ALLOWIN) end
			  | TO :: ts2 => let val (ts3,nd3) = typeExpression ts2 in relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.TO,nd1,nd3),ALLOWIN) end
			  | _ => (ts1,nd1)
		
		fun relationalExpression' (ts1, nd1, NOIN) =
			case ts1 of
				LESSTHAN :: ts2 => let val (ts3,nd3) = shiftExpression ts2 in relationalExpression' (ts3,Ast.BinaryExpr(Ast.LESS,nd1,nd3),NOIN) end
			  | GREATERTHAN :: ts2 => let val (ts3,nd3) = shiftExpression ts2 in relationalExpression' (ts3,Ast.BinaryExpr(Ast.GREATER,nd1,nd3),NOIN) end
			  | LESSTHANOREQUAL :: ts2 => let val (ts3,nd3) = shiftExpression ts2 in relationalExpression' (ts3,Ast.BinaryExpr(Ast.LESS_OR_EQUAL,nd1,nd3),NOIN) end
			  | GREATERTHANOREQUAL :: ts2 => let val (ts3,nd3) = shiftExpression ts2 in relationalExpression' (ts3,Ast.BinaryExpr(Ast.GREATER_OR_EQUAL,nd1,nd3),NOIN) end
			  | INSTANCEOF :: ts2 => let val (ts3,nd3) = shiftExpression ts2 in relationalExpression' (ts3,Ast.BinaryExpr(Ast.INSTANCEOF,nd1,nd3),NOIN) end
			  | IS :: ts2 => let val (ts3,nd3) = typeExpression ts2 in relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.IS,nd1,nd3),NOIN) end
			  | TO :: ts2 => let val (ts3,nd3) = typeExpression ts2 in relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.TO,nd1,nd3),NOIN) end
			  | _ => (ts1,nd1)
    in
        relationalExpression' (ts1,nd1,beta)
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

and equalityExpression (ts, beta)=
    let val _ = log([">> equalityExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = relationalExpression (ts,beta)
		fun equalityExpression' (ts1,nd1) =
			case ts1 of
				EQUALS :: ts2 => let val (ts3,nd3) = relationalExpression (ts2,beta) in equalityExpression' (ts3,Ast.BinaryExpr(Ast.EQUALS,nd1,nd3)) end
			  | NOTEQUALS :: ts2 => let val (ts3,nd3) = relationalExpression (ts2,beta) in equalityExpression' (ts3,Ast.BinaryExpr(Ast.NOT_EQUALS,nd1,nd3)) end
			  | STRICTEQUALS :: ts2 => let val (ts3,nd3) = relationalExpression (ts2,beta) in equalityExpression' (ts3,Ast.BinaryExpr(Ast.STRICT_EQUALS,nd1,nd3)) end
			  | STRICTNOTEQUALS :: ts2 => let val (ts3,nd3) = relationalExpression (ts2,beta) in equalityExpression' (ts3,Ast.BinaryExpr(Ast.STRICT_NOT_EQUALS,nd1,nd3)) end
			  | _ => (ts1,nd1)
		
    in
        equalityExpression' (ts1,nd1)
    end

(*
	BitwiseAndExpression(beta)    
    	EqualityExpression(beta)
	    BitwiseAndExpressionr(beta)  &  EqualityExpression(beta)

	right recursive: (see pattern of MultiplicativeExpression)
*)

and bitwiseAndExpression (ts, beta)=
    let val _ = log([">> bitwiseAndExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = equalityExpression (ts,beta)
		fun bitwiseAndExpression' (ts1,nd1) =
			case ts1 of
				AMPERSAND :: ts2 => let val (ts3,nd3) = equalityExpression (ts2,beta) in bitwiseAndExpression' (ts3,Ast.BinaryExpr(Ast.BITWISE_AND,nd1,nd3)) end
			  | _ => (ts1,nd1)
		
    in
        bitwiseAndExpression' (ts1,nd1)
    end

(*
	BitwiseXorExpressionb    
    	BitwiseAndExpressionb
	    BitwiseXorExpressionb  ^  BitwiseAndExpressionb

	right recursive: (see pattern of MultiplicativeExpression)
*)

and bitwiseXorExpression (ts, beta)=
    let val _ = log([">> bitwiseXorExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = bitwiseAndExpression (ts,beta)
		fun bitwiseXorExpression' (ts1,nd1) =
			case ts1 of
				BITWISEXOR :: ts2 => let val (ts3,nd3) = bitwiseAndExpression (ts2,beta) in bitwiseXorExpression' (ts3,Ast.BinaryExpr(Ast.BITWISE_XOR,nd1,nd3)) end
			  | _ => (ts1,nd1)
		
    in
        bitwiseXorExpression' (ts1,nd1)
    end

(*
	BitwiseOrExpressionb    
    	BitwiseXorExpressionb
	    BitwiseOrExpressionb  |  BitwiseXorExpressionb
*)

and bitwiseOrExpression (ts, beta)=
    let val _ = log([">> bitwiseOrExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = bitwiseXorExpression (ts,beta)
		fun bitwiseOrExpression' (ts1,nd1) =
			case ts1 of
				BITWISEOR :: ts2 => let val (ts3,nd3) = bitwiseXorExpression (ts2,beta) in bitwiseOrExpression' (ts3,Ast.BinaryExpr(Ast.BITWISE_OR,nd1,nd3)) end
			  | _ => (ts1,nd1)
		
    in
        bitwiseOrExpression' (ts1,nd1)
    end

(*
	LogicalAndExpressionb    
    	BitwiseOrExpressionb
	    LogicalAndExpressionb  &&  BitwiseOrExpressionb
*)

and logicalAndExpression (ts, beta)=
    let val _ = log([">> logicalAndExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = bitwiseOrExpression (ts,beta)
		fun logicalAndExpression' (ts1,nd1) =
			case ts1 of
				LOGICALAND :: ts2 => let val (ts3,nd3) = bitwiseOrExpression (ts2,beta) in logicalAndExpression' (ts3,Ast.BinaryExpr(Ast.LOGICAL_AND,nd1,nd3)) end
			  | _ => (ts1,nd1)
		
    in
        logicalAndExpression' (ts1,nd1)
    end

(*
	LogicalXorExpressionb    
    	LogicalAndExpressionb
	    LogicalXorExpressionb  ^^  LogicalAndExpressionb
*)

and logicalXorExpression (ts, beta) =
    let val _ = log([">> logicalXorExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = logicalAndExpression (ts,beta)
		fun logicalXorExpression' (ts1,nd1) =
			case ts1 of
				LOGICALXOR :: ts2 => let val (ts3,nd3) = logicalAndExpression (ts2,beta) in logicalXorExpression' (ts3,Ast.BinaryExpr(Ast.LOGICAL_XOR,nd1,nd3)) end
			  | _ => (ts1,nd1)
		
    in
        logicalXorExpression' (ts1,nd1)
    end

(*
	LogicalOrExpressionb    
    	LogicalXorExpressionb
	    LogicalOrExpressionb  ||  LogicalXorExpressionb

*)

and logicalOrExpression (ts, beta) =
    let val _ = log([">> logicalOrExpression with next=",tokenname(hd(ts))]) 
		val (ts1,nd1) = logicalXorExpression (ts,beta)
		fun logicalOrExpression' (ts1,nd1) =
			case ts1 of
				LOGICALXOR :: ts2 => let val (ts3,nd3) = logicalXorExpression (ts2,beta) in logicalOrExpression' (ts3,Ast.BinaryExpr(Ast.LOGICAL_XOR,nd1,nd3)) end
			  | _ => (ts1,nd1)
		
    in
        logicalOrExpression' (ts1,nd1)
    end

(*
	ConditionalExpression(ALLOWLET, beta)    
    	ExpressionClosure(beta)
	    LetExpression(beta)
    	YieldExpression(beta)
	    LogicalOrExpression(beta)
    	LogicalOrExpression(beta)  ?  AssignmentExpression(ALLOWLET,beta)  :  AssignmentExpression(ALLOWLET,beta)


	ConditionalExpression(NOLET, beta)    
    	SimpleYieldExpression
	    LogicalOrExpression(beta)
    	LogicalOrExpression(beta)  ?  AssignmentExpression(ALLOWLET,beta) :  AssignmentExpression(NOLET,beta)
    
*)

and conditionalExpression (ts,ALLOWLET,beta) =
    let val _ = log([">> conditionalExpression with next=",tokenname(hd(ts))])
    in case ts of
        FUNCTION :: _ => expressionClosure(ts,beta)
      | LET :: _ => letExpression(ts,beta)
	  | YIELD :: _ => yieldExpression(ts,beta)
      | _ => 
			let
				val (ts2,nd2) = logicalOrExpression(ts,beta)
			in case ts2 of
				QUESTIONMARK :: ts3 => 
					let
						val (ts4,nd4) = assignmentExpression(ts3,ALLOWLET,beta)
						val (COLON::ts5,nd5) = (ts4,nd4)
						val (ts6,nd6) = assignmentExpression(ts5,ALLOWLET,beta)
					in
						(ts6,nd6)
					end
			  | _ => (ts2,nd2)
			end
    end

  | conditionalExpression (ts,NOLET,beta) =
    let val _ = log([">> conditionalExpression with next=",tokenname(hd(ts))])
    in case ts of
		YIELD :: _ => simpleYieldExpression ts
      | _ => 
			let
				val (ts1,nd1) = logicalOrExpression(ts,beta)
			in case ts1 of
				QUESTIONMARK :: ts2 => 
					let
						val (ts3,nd3) = assignmentExpression(ts2,ALLOWLET,beta)
						val (COLON::ts4,nd4) = (ts3,nd3)
						val (ts5,nd5) = assignmentExpression(ts4,NOLET,beta)
					in
						(ts5,nd5)
					end
			  | _ => (ts1,nd1)
			end
    end

(*

*)

(*

NonAssignmentExpressionallowLet, b    
    ExpressionClosureb
    LetExpressionb
    YieldExpressionb
    LogicalOrExpressionb
    LogicalOrExpressionb  ?  NonAssignmentExpressionallowLet, b  :  NonAssignmentExpressionallowLet, b
    
*)

(*

NonAssignmentExpressionnoLet, b    
    SimpleYieldExpression
    LogicalOrExpressionb
    LogicalOrExpressionb  ?  NonAssignmentExpressionallowLet, b  :  NonAssignmentExpressionnoLet, b
    
*)

(*

LetExpression(beta)    
    let  (  LetBindingList  )  ListExpressionb
    
*)

and letExpression (ts,beta) = raise ParseError

(*

LetBindingList    
    «empty»
    NonemptyLetBindingList
    
*)

(*

NonemptyLetBindingList    
    VariableBindingallowIn
    VariableBindingallowIn  ,  NonemptyLetBindingList
    
*)

(*

ExpressionClosure(beta)   
    function  FunctionSignature  ListExpression(beta)
    
*)

and expressionClosure (ts,beta) = raise ParseError

(*

YieldExpressionb    
    yield
    yield  [no line break]  ListExpressionb
    
*)

and yieldExpression (ts,beta) =
	case ts of
		YIELD :: ts1 => 
			let
			in case ts1 of
				(SEMICOLON :: _ | RIGHTBRACE :: _ | RIGHTPAREN :: _) => (ts1,Ast.YieldExpr NONE)
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
		YIELD :: ts1 => (ts1,Ast.YieldExpr NONE)
	  | _ => raise ParseError

(*

AssignmentExpressiona, b    
    ConditionalExpressiona, b
    PostfixExpression  =  AssignmentExpressiona, b
    PostfixExpression  CompoundAssignment  AssignmentExpressiona, b
    PostfixExpression  LogicalAssignment  AssignmentExpressiona, b
    TypedPattern  =  AssignmentExpressiona, b
    
*)

and assignmentExpression (ts, alpha, beta):(token list * Ast.expr) = 
    let val _ = log([">> assignmentExpression with next=",tokenname(hd(ts))]) 
    in
        multiplicativeExpression (ts)
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
    ^^=
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
        val (ts1,nd1) = assignmentExpression(ts,ALLOWLET,beta)
        val (ts2,nd2) = listExpressionPrime(ts1,nd1,beta)
    in
        (ts2, Ast.ListExpr nd2)
    end

and listExpressionPrime (ts,nd1,beta) =
    let
    in case ts of
        COMMA :: ts1 =>
           let
               val (ts2,nd2) = assignmentExpression(ts1,ALLOWLET,beta)
               val (ts3,nd3) = listExpressionPrime(ts2,nd2,beta)
           in
             (ts3, nd1 :: nd3)
           end
      | _ => (ts, nd1 :: [])
     end


(*

Pattern    
    ObjectPattern
    ArrayPattern
    
*)

and pattern ts =
    raise ParseError

(*

ObjectPattern    
    {  DestructuringFieldList  }
    
*)

(*

DestructuringFieldList    
    DestructuringField
    DestructuringFieldList  ,  DestructuringField
    
*)

(*

DestructuringField    
    NonAttributeQualifiedIdentifier  :  Pattern
    NonAttributeQualifiedIdentifier  :  PostfixExpression
    
*)

(*

ArrayPattern    
    [  DestructuringElementList  ]
    
*)

(*

DestructuringElementList    
    «empty»
    DestructuringElement
    , DestructuringElementList
    DestructuringElement , DestructuringElementList
    
*)

(*

DestructuringElement    
    Pattern
    PostfixExpression
    
*)

(*

TYPE EXPRESSIONS    
    
*)

(*

FunctionType    
    function  FunctionSignature
    
*)

(*

UnionType    
    (  TypeExpressionList  )
    
*)

(*

RecordType    
    {  FieldTypeList  }
    
*)

(*

FieldTypeList    
    «empty»
    NonemptyFieldTypeList
    
*)

(*

NonemptyFieldTypeList    
    FieldType
    FieldType  ,  NonemptyFieldTypeList
    
*)

(*

FieldType    
    FieldName  :  TypeExpression
    
*)

(*

ArrayType    
    [  ElementTypeList  ]
    
*)

(*

ElementTypeList    
    «empty»
    TypeExpression
    ,  ElementTypeList
    TypeExpression  ,  ElementTypeList
    
*)

(*

TypeExpression    
    TypeIdentifier
    TypeIdentifier  !
    TypeIdentifier  ?
    FunctionType
    UnionType
    RecordType
    ArrayType
    
*)

and typeExpression ts =
    let
    in case ts of
(*
       FUNCTION :: ts1 => functionType ts
       LEFTPAREN :: ts1 => unionType ts
       LEFTBRACE :: ts1 => recordType ts
       LEFTBRACKET :: ts1 => arrayType ts
*)
        _ => 
            let
                val (ts1,nd1) = typeIdentifier ts
            in
                (ts1,Ast.UnresolvedType(nd1)) (* Ast.PrimaryType {name=nd1,annotation=Ast.NAMED}) *)
            end
    end

(*

TypeExpressionList    
    TypeExpression
    TypeExpressionList  ,  TypeExpression

*)



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
    ReturnStatement Semicolonw
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

(*
    
EmptyStatement     
    ;

*)

and emptyStatement ts =
    let
    in
        (ts,Ast.EmptyStmt)
    end

(*
    
ExpressionStatement    
    [lookahead !{ function, { }] ListExpression (allowLet,allowIn)

*)

and expressionStatement ts =
    let
        val _ = log([">> expressionStatement with next=", tokenname(hd ts)])
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

and block ts =
    let
    in case ts of
        LEFTBRACE :: ts1 =>
            let
                val (ts2,nd2) = expressionStatement (ts1)  (* todo: direcitves *)
            in case ts2 of
                RIGHTBRACE :: ts3 => (ts3,Ast.Block{directives=[],defns=[],stmts=[nd2]})
            end
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
    
ReturnStatement    
    return
    return [no line break] ListExpressionallowIn
    
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
    
Directivew    
    EmptyStatement
    Statementw
    AnnotatableDirectivew
    Attributes [no line break] AnnotatableDirectivew
    IncludeDirective Semicolonw
    
AnnotatableDirectivew    
    VariableDefinitionallowIn Semicolonw
    FunctionDefinition
    ClassDefinition
    InterfaceDefinition
    NamespaceDefinition Semicolonw
    TypeDefinition Semicolonw
    
Directives    
    «empty»
    DirectivesPrefix Directiveabbrev
    
DirectivesPrefix    
    «empty»
    Pragmas
    DirectivesPrefix Directivefull
    
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
    [  AssignmentExpressionallowLet, allowIn  ]
    
IncludeDirective    
    include  [no line break]  StringLiteral
    
Pragmas    
    Pragma
    Pragmas  Pragma
    
Pragma    
    UsePragma  Semicolonfull
    ImportPragma  Semicolonfull
    
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
    TypedIdentifierb
    TypedIdentifierb VariableInitialisationa, b
    TypedPattern VariableInitialisationa, b
    
VariableInitialisationa, b    
    =  AssignmentExpressiona, b
    
TypedIdentifierb    
    Identifier
    Identifier  :  TypeExpressionb
    
TypedPattern    
    Pattern
    Pattern  :  TypeExpression
    
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
       val _ = log([">> program with next=",tokenname(hd(ts))])
    in case ts of
        PACKAGE :: tr => 
	let 
	    val (tr2, pkgs) = packageDefinition tr
	in
	    (tr2, {packages=[pkgs], body=(Ast.Block {directives=[],
						     defns=[],
						     stmts=[]})})
	end
      | _             => 
	let
	    val (tr2, stmts) = expressionStatement ts
	in
	    (tr2, {packages=[], body=(Ast.Block {directives=[],
						 defns=[],
						 stmts=[stmts]})})
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


and typeExpressionList ts = raise ParseError

(*

*)

and packageDefinition ts = raise ParseError

(*

*)

and directives ts = raise ParseError



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

fun lexFile (filename : string) : (token list) = 
    let 
        val lexer = Lexer.makeLexer (mkReader filename)
	val tokens = Lexer.UserDeclarations.token_list lexer
    in
        tokens
    end



fun parse ts =
    let 
	val (residual, result) = (program ts) 
	fun check_residual (EOL :: xs) = check_residual xs
	  | check_residual [EOF] = ()
	  | check_residual _ = raise ParseError
    in
	check_residual residual;
	log ["parsed all input, pretty-printing:"];
	Pretty.ppProgram result;
	result
    end

fun parseFile filename = 
    (log ["scanning ", filename];
     (parse (lexFile filename)
      handle ParseError => (log ["parse error"]; 
			    raise ParseError)
	   | Lexer.LexError => (log ["lex error"];
				raise Lexer.LexError));
     log ["parsed ", filename, "\n"])


end (* Parser *)
