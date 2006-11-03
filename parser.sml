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
                val (ts2,nd2) = listExpression (ts1,ALLOWLET,ALLOWIN)
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
        �empty�
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
        �empty�
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
        �empty�
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
        �empty�
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
        �empty�
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
        �empty�
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
        �empty�
    
    XMLAttribute    
        XMLWhitespace  XMLName  XMLWhitespaceopt  =  XMLWhitespaceopt  {  ListExpressionallowIn  }
        XMLWhitespace  XMLName  XMLWhitespaceopt  =  XMLWhitespaceopt  XMLAttributeValue
    
    XMLElementContent    
        {  ListExpressionallowIn  }  XMLElementContent
        XMLMarkup  XMLElementContent
        XMLText  XMLElementContent
        XMLElement  XMLElementContent
        �empty�
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
        �empty�

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
        �empty�
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
        �empty�
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

(*

MultiplicativeExpression    
    UnaryExpression
    MultiplicativeExpression  *  UnaryExpression
    MultiplicativeExpression  /  UnaryExpression
    MultiplicativeExpression  %  UnaryExpression
    
*)

(*

AdditiveExpression    
    MultiplicativeExpression
    AdditiveExpression  +  MultiplicativeExpression
    AdditiveExpression  -  MultiplicativeExpression
    
*)

(*

ShiftExpression    
    AdditiveExpression
    ShiftExpression  <<  AdditiveExpression
    ShiftExpression  >>  AdditiveExpression
    ShiftExpression  >>>  AdditiveExpression
    
*)

(*

RelationalExpressionallowIn    
    ShiftExpression
    RelationalExpressionallowIn  <  ShiftExpression
    RelationalExpressionallowIn  >  ShiftExpression
    RelationalExpressionallowIn  <=  ShiftExpression
    RelationalExpressionallowIn  >=  ShiftExpression
    RelationalExpressionallowIn  in  ShiftExpression
    RelationalExpressionallowIn  instanceof  ShiftExpression
    RelationalExpressionallowIn  is  TypeExpression
    RelationalExpressionallowIn  to  TypeExpression
    
*)

(*

RelationalExpressionnoIn    
    ShiftExpression
    RelationalExpressionnoIn  <  ShiftExpression
    RelationalExpressionnoIn  >  ShiftExpression
    RelationalExpressionnoIn  <=  ShiftExpression
    RelationalExpressionnoIn  >=  ShiftExpression
    RelationalExpressionnoIn  instanceof  ShiftExpression
    RelationalExpressionnoIn  is  TypeExpression
    RelationalExpressionnoIn  to  TypeExpression
    
*)

(*

EqualityExpressionb    
    RelationalExpressionb
    EqualityExpressionb  ==  RelationalExpressionb
    EqualityExpressionb  !=  RelationalExpressionb
    EqualityExpressionb  ===  RelationalExpressionb
    EqualityExpressionb  !==  RelationalExpressionb
    
*)

(*

BitwiseAndExpressionb    
    EqualityExpressionb
    BitwiseAndExpressionrb  &  EqualityExpressionb
    
*)

(*

BitwiseXorExpressionb    
    BitwiseAndExpressionb
    BitwiseXorExpressionb  ^  BitwiseAndExpressionb
    
*)

(*

BitwiseOrExpressionb    
    BitwiseXorExpressionb
    BitwiseOrExpressionb  |  BitwiseXorExpressionb
    
*)

(*

LogicalAndExpressionb    
    BitwiseOrExpressionb
    LogicalAndExpressionb  &&  BitwiseOrExpressionb
    
*)

(*

LogicalXorExpressionb    
    LogicalAndExpressionb
    LogicalXorExpressionb  ^^  LogicalAndExpressionb
    
*)

(*

LogicalOrExpressionb    
    LogicalXorExpressionb
    LogicalOrExpressionb  ||  LogicalXorExpressionb
    
*)

(*

ConditionalExpressionallowLet, b    
    ExpressionClosureb
    LetExpressionb
    YieldExpressionb
    LogicalOrExpressionb
    LogicalOrExpressionb  ?  AssignmentExpressionallowLet, b  :  AssignmentExpressionallowLet, b
    
*)

(*

ConditionalExpressionnoLet, b    
    SimpleYieldExpression
    LogicalOrExpressionb
    LogicalOrExpressionb  ?  AssignmentExpressionallowLet, b  :  AssignmentExpressionnoLet, b
    
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

LetExpressionb    
    let  (  LetBindingList  )  ListExpressionb
    
*)

(*

LetBindingList    
    �empty�
    NonemptyLetBindingList
    
*)

(*

NonemptyLetBindingList    
    VariableBindingallowIn
    VariableBindingallowIn  ,  NonemptyLetBindingList
    
*)

(*

ExpressionClosureb    
    function  FunctionSignature  ListExpressionb
    
*)

(*

YieldExpressionb    
    yield
    yield  [no line break]  ListExpressionb
    
*)

(*

SimpleYieldExpression    
    yield
    
*)

and simpleYieldExpression ts =
    raise ParseError

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
        leftHandSideExpression (ts)
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
    �empty�
    , AssignmentExpression(beta) ListExpressionPrime(beta)

*)

and listExpression (ts, alpha, beta) = 
    let
        val (ts1,nd1) = assignmentExpression(ts,alpha,beta)
        val (ts2,nd2) = listExpressionPrime(ts1,nd1,alpha,beta)
    in
        (ts2, Ast.ListExpr nd2)
    end

and listExpressionPrime (ts, nd1, alpha, beta) =
    let
    in case ts of
        COMMA :: ts1 =>
           let
               val (ts2,nd2) = assignmentExpression(ts1,alpha,beta)
               val (ts3,nd3) = listExpressionPrime(ts2,nd2,alpha,beta)
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
    �empty�
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
    �empty�
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
    �empty�
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
    �empty�
    
SemicolonnoShortIf    
    ;
    VirtualSemicolon
    �empty�
    
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
        val (ts1,nd1) = listExpression(ts, ALLOWLET, ALLOWIN)
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
    �empty�
    CaseLabel
    CaseLabel CaseElementsPrefix CaseElementabbrev
    
CaseElementsPrefix    
    �empty�
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
    �empty�
    ListExpressionnoIn
    VariableDefinitionnoIn
    
ForInBinding    
    PostfixExpression
    Pattern
    VariableDefinitionKind VariableBindingnoIn
    
OptionalExpression    
    ListExpressionallowIn
    �empty�
    
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
    �empty�
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
    �empty�
    DirectivesPrefix Directiveabbrev
    
DirectivesPrefix    
    �empty�
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
    �empty�
    extends TypeIdentifier
    implements TypeIdentifierList
    extends TypeIdentifier implements TypeIdentifierList
    
TypeIdentifierList    
    TypeIdentifier
    TypeIdentifier  ,  TypeIdentifierList
    
InterfaceDefinition    
    interface  ClassName  ExtendsList Block
    
ExtendsList    
    �empty�
    extends TypeIdentifierList
    
NamespaceDefinition    
    namespace NamespaceBinding
    
NamespaceBinding    
    Identifier NamespaceInitialisation
    
NamespaceInitialisation    
    �empty�
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
        PACKAGE :: tr => packageDefinition tr
      | _             => expressionStatement ts
    end

(*

PackageDefinition    
    PackageAttributes package PackageNameOpt Block
    
PackageAttributes    
    internal
    �empty�
    
PackageNameOpt    
    �empty�
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
        val tmp = ref []
        val lexer = Lexer.makeLexer (mkReader filename)
        fun step _ = 
            case lexer () of 
                EMPTY => (tmp := EMPTY :: (!tmp); List.rev (!tmp))
              | other => (tmp := other :: (!tmp); step ())
        val result = step ()
             handle Lexer.LexError => (log ["lex error"]; 
                           raise Lexer.LexError)
    in
        log ["lexed ", filename];
        result
    end

fun parse ts =
    case (program ts) of
        ([EOL, EMPTY],result) => result
      | _  => raise ParseError

fun parseFile filename = 
    (log ["scanning ", filename];
     (parse (lexFile filename)
      handle ParseError => (log ["parse error"]; 
                raise ParseError));
     log ["parsed ", filename, "\n"])


end (* Parser *)
