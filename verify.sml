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
structure Verify = struct

(* ENV contains all type checking context, including ribs (variable-type bindings),
 * strict vs standard mode, and the expected return type, if any.
 *)


type STD_TYPES = { 
     AnyNumberType: Ast.TYPE_EXPRESSION,
     doubleType:    Ast.TYPE_EXPRESSION,
     decimalType:   Ast.TYPE_EXPRESSION,

     AnyStringType: Ast.TYPE_EXPRESSION,
     stringType:    Ast.TYPE_EXPRESSION,

     AnyBooleanType: Ast.TYPE_EXPRESSION,
     booleanType:   Ast.TYPE_EXPRESSION,

     RegExpType:    Ast.TYPE_EXPRESSION,

     NamespaceType: Ast.TYPE_EXPRESSION,
     TypeType:      Ast.TYPE_EXPRESSION
}

type ENV = { returnType: Ast.TYPE_EXPRESSION option,
             strict: bool,
             prog: Fixture.PROGRAM,
             ribs: Ast.RIBS,
             stdTypes: STD_TYPES }

fun withReturnType { returnType=_, strict, prog, ribs, stdTypes } returnType =
    { returnType=returnType, strict=strict, prog=prog, ribs=ribs, stdTypes=stdTypes }

fun withRibs { returnType, strict, prog, ribs=_, stdTypes } ribs =
    { returnType=returnType, strict=strict, prog=prog, ribs=ribs, stdTypes=stdTypes }

fun withStrict { returnType, strict=_, prog, ribs, stdTypes } strict =
    { returnType=returnType, strict=strict, prog=prog, ribs=ribs, stdTypes=stdTypes }

fun withRib { returnType, strict, prog, ribs, stdTypes} extn =
    { returnType=returnType, strict=strict, prog=prog, ribs=(extn :: ribs), stdTypes=stdTypes }

fun withRibOpt { returnType, strict, prog, ribs, stdTypes} extn =
    { returnType=returnType, 
      strict=strict, 
      prog=prog, 
      ribs=case extn of 
               NONE => ribs 
             | SOME e => (e :: ribs), 
      stdTypes=stdTypes }

(* Local tracing machinery *)

val warningsAreFailures = ref false
val traceWarnings = ref false
val doTrace = ref false
val doTraceFrag = ref false
val Any =  Ast.SpecialType Ast.Any
fun log ss = LogErr.log ("[verify] " :: ss)
fun trace ss = if (!doTrace) then log ("trace: "::ss) else ()
fun error ss = LogErr.verifyError ss
fun warning ss = 
    if !warningsAreFailures 
    then error ss
    else 
        if !traceWarnings
        then LogErr.log("STRICT-MODE WARNING: " :: ss)
        else ()

fun logType ty = (Pretty.ppType ty; TextIO.print "\n")
fun traceType ty = if (!doTrace) then logType ty else ()
fun fmtType ty = if !doTrace
                 then LogErr.ty ty
                 else ""

fun liftOption (f: 'a -> 'b) (x:'a option) (y:'b) : 'b =
    case x of
        NONE => y
      | SOME xx => f xx

(****************************** standard types *************************)

val undefinedType   = Ast.SpecialType Ast.Undefined
val nullType        = Ast.SpecialType Ast.Null
val anyType         = Ast.SpecialType Ast.Any
                                           
fun newEnv (prog:Fixture.PROGRAM) 
           (strict:bool) 
    : ENV = 
    { 
     returnType = NONE,
     strict = strict,
     prog = prog, 
     ribs = [Fixture.getRootRib prog],
     
     stdTypes = 
     {      
      AnyNumberType = Type.getNamedGroundType prog Name.ES4_AnyNumber,
      doubleType    = Type.getNamedGroundType prog Name.ES4_double,
      decimalType   = Type.getNamedGroundType prog Name.ES4_decimal,

      AnyStringType = Type.getNamedGroundType prog Name.ES4_AnyString,
      stringType    = Type.getNamedGroundType prog Name.ES4_string,

      AnyBooleanType= Type.getNamedGroundType prog Name.ES4_AnyBoolean,
      booleanType   = Type.getNamedGroundType prog Name.ES4_boolean,

      RegExpType    = Type.getNamedGroundType prog Name.public_RegExp,

      NamespaceType = Type.getNamedGroundType prog Name.ES4_Namespace,

      TypeType      = Type.getNamedGroundType prog Name.intrinsic_Type
     }
    }


(******************* Subtyping and Compatibility *************************)

(* src and dst are normalized *)
fun checkMatch (env:ENV)
               (src:Ast.TYPE_EXPRESSION) (* assignment src *)
		       (dst:Ast.TYPE_EXPRESSION) (* assignment dst *)
    : unit =
    let in
        trace ["checkMatch ", LogErr.ty src, " vs. ", LogErr.ty dst]; 
        if not (#strict env) orelse Type.groundMatches src dst
        then ()
        else warning 
                 ["checkMatch: incompatible types in assignment: a value of type\n    ", 
                  LogErr.ty src, 
                  "\ncannot be put into a slot of type\n    ", 
                  LogErr.ty dst]
    end

(* t1 and t2 are normalized *)
fun leastUpperBound (t1:Ast.TYPE_EXPRESSION)
                    (t2:Ast.TYPE_EXPRESSION)
    : Ast.TYPE_EXPRESSION =
    let
    in
        if      Type.groundIsCompatibleSubtype t1 t2 then t2
        else if Type.groundIsCompatibleSubtype t2 t1 then t1
        else Ast.UnionType [t1, t2]
    end

(******************* Utilities for resolving IDENT_EXPRESSIONs *********************)

(* Resolves the given expr to a namespace, or to NONE *)

fun resolveExprToNamespace (env:ENV)
                           (expr:Ast.EXPRESSION)
    : Ast.NAMESPACE option =
    case expr of
        Ast.LiteralExpr (Ast.LiteralNamespace ns) => 
        SOME ns
      | Ast.LexicalRef {ident= Ast.QualifiedIdentifier { qual=expr, ident }, loc } =>
        let
        in
            LogErr.setLoc loc;
            case resolveExprToNamespace env expr of
                NONE => NONE
              | SOME ns => 
                resolveExprToNamespace env  
                                       (Ast.LexicalRef {ident=(Ast.Identifier { openNamespaces=[[ns]], 
                                                                                ident=ident, 
                                                                                rootRib=NONE }), 
                                                        loc=loc })
        end
      | Ast.LexicalRef {ident = Ast.Identifier { openNamespaces, ident, rootRib }, loc} =>
        let
         in
            LogErr.setLoc loc;
            case Fixture.findName ((#ribs env), ident, openNamespaces, rootRib) of 
                NONE => NONE (* no occurrence in ribs *)
              | SOME (ribs, name) =>
                case Fixture.getFixture (List.hd ribs) (Ast.PropName name) of
                    Ast.NamespaceFixture ns => SOME ns
                  | _ => NONE
        end
      | _ => NONE

(* Returns the type of the given fixture. The result is not yet normalized,
 * and so only makes sense in the environment the fixture was defined in. *)

fun typeOfFixture (env:ENV)
			      (fixture:Ast.FIXTURE)
    : Ast.TYPE_EXPRESSION = 
    case fixture of 	
	    (* 
	     * FIXME: classtypes should be turned into instancetypes of 
	     * the static-class type, so we can look up static props on them
	     * correctly. Abusing object types like this is no good.
	     *)
	    (Ast.ClassFixture (Ast.Cls {classType, ...})) => classType	
      | (Ast.ValFixture { ty, ... }) => ty	
      | (Ast.VirtualValFixture { ty, ... }) => ty        
      | (Ast.TypeFixture _) => (#TypeType (#stdTypes env))
      | _ => anyType

(* Resolves an IDENT_EXPRESSION in the given RIBS, and returns the type of
 * that IDENT_EXPRESSION, or NONE. The returned type has been verified.
 *)

(******************** Verification **************************************************)

fun verifyIdentExpr (env:ENV)
                    (ribs:Ast.RIBS)
                    (idexpr:Ast.IDENT_EXPRESSION)
    : Ast.TYPE_EXPRESSION option =
    let in
        case idexpr of
            Ast.QualifiedIdentifier { qual=expr, ident } => 
            let in
                case resolveExprToNamespace env expr of
                    SOME ns =>
                    verifyIdentExpr env ribs
                                    (Ast.Identifier { openNamespaces = [[ns]], ident = ident, rootRib=NONE})
                  | NONE => NONE
            end
          | Ast.Identifier { openNamespaces, ident, rootRib } =>
            let 
            in
                case Fixture.findName (ribs, ident, openNamespaces, rootRib) of 
                    NONE => NONE (* no occurrence in ribs *)
                  | SOME (ribs, name) =>
                    let val fixture = Fixture.getFixture (List.hd ribs) (Ast.PropName name)
                        val ty = typeOfFixture env fixture
                        val ty = verifyType (withRibs env ribs) ty
                    in
                        SOME ty
                    end
            end
          | _ => NONE (* verifier does not handle these kinds of references *)
    end

(* Verification (aka normalization) converts a (non-closed) TYPE_EXPRESSION into a 
 * (closed, aka grounded) TYPE_EXPRESSION. 
 *)

and verifyType (env:ENV)
               (ty:Ast.TYPE_EXPRESSION)
    : Ast.TYPE_EXPRESSION =
    let
        val _ = trace ["verifyType: calling normalize ", LogErr.ty ty]
        val norm : Ast.TYPE_EXPRESSION = 
            (* FIXME: it is *super wrong* to just be using the root rib here. 
            Type.normalize [Fixture.getRootRib (#prog env)] ty *)
            Type.normalize (#ribs env) ty
            handle LogErr.TypeError e => 
                   let in
                       if (#strict env) 
                       then 
                           warning [e, " while normalizing ", LogErr.ty ty]
                       else ();
                       ty
                   end
        val _ = trace ["verifyType: back from normalize ", LogErr.ty ty]
    in
        norm 
    end

and verifyFixtureName (env:ENV) 
                      (ribs:Ast.RIBS)
                      (fname:Ast.FIXTURE_NAME)
    : Ast.TYPE_EXPRESSION option =
    case ribs of
        [] => NONE
      | rib::ribs' =>
        if Fixture.hasFixture rib fname 
        then
            let val fixture = Fixture.getFixture rib fname
                val ty = typeOfFixture env fixture
                val ty = verifyType (withRibs env ribs) ty
            in
                SOME ty
            end
        else
            verifyFixtureName env ribs' fname

and verifyInits (env:ENV) (inits:Ast.INITS)
    : unit =
    let in
        List.map 
         (fn (fname, expr) => 
             case (verifyFixtureName env (#ribs env) fname) of
                 SOME ty => checkMatch env (verifyExpr env expr) ty
               | NONE => warning ["Unbound fixture name ", LogErr.fname fname, " in inits"])
         inits; 
        ()
    end

(* verifyHead returns an extended environment *)

and verifyHead (env:ENV) 
               (head:Ast.HEAD)
    : ENV =
    let
        val Ast.Head (rib, inits) = head
        val env' = withRib env rib
    in        
        trace ["verifying head with rib ", LogErr.rib rib, " in ribs ", LogErr.ribs (#ribs env)];
        verifyRib env rib;
        verifyInits env' inits;
        trace ["done with verifying head"];
        env'
    end

and verifyLvalue (env:ENV)
                 (expr : Ast.EXPRESSION) 
    : Ast.TYPE_EXPRESSION = 
    let
    in
        case expr of
            (* FIXME: check for final fields *)
            Ast.ObjectRef { base, ident, loc } => 
            verifyExpr env expr
          | Ast.LexicalRef { ident, loc } => 
            verifyExpr env expr
          | _ =>
            (warning ["Not an lvalue"]; anyType)
    end

and verifyExpr (env:ENV)
               (expr:Ast.EXPRESSION)
    : Ast.TYPE_EXPRESSION =
    let val _ = trace [">>> Verifying expr "]
        val _ = if !doTrace then Pretty.ppExpr expr else ()
        val r = verifyExpr2 env expr
        val _ = trace ["<<< Verifying expr ", LogErr.ty r]
    in
        r
    end

and verifyExpr2 (env:ENV)
               (expr:Ast.EXPRESSION)
    : Ast.TYPE_EXPRESSION =
    let
        val { prog, 
              strict, 
              stdTypes = 
              { AnyNumberType, 
                doubleType,
                decimalType,

                AnyStringType,
                stringType,

                AnyBooleanType, 
                booleanType, 

                RegExpType,

                TypeType,
                NamespaceType }, ... } = env
        fun verifySub (e:Ast.EXPRESSION) : Ast.TYPE_EXPRESSION = verifyExpr env e
        fun verifySubList (es:Ast.EXPRESSION list) : Ast.TYPE_EXPRESSION list = map (verifyExpr env) es
        fun verifySubOption (eo:Ast.EXPRESSION option) : Ast.TYPE_EXPRESSION option = Option.map verifySub eo
        fun binaryOpType (b:Ast.BINOP) t1 t2 : Ast.TYPE_EXPRESSION =
            let
                (* FIXME: these are way wrong. For the time being, just jam in star everywhere.
                 * Fix when we know how numbers work. 
                 *)
                val AdditionType = anyType
                val CompareType = anyType
                val LogicalType = anyType
                val (expectedType1, expectedType2, resultType) =
                    case b of
                         Ast.Plus       => (AdditionType,  AdditionType,  AdditionType)
                       | Ast.Minus      => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.Times      => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.Divide     => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.Remainder  => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.LeftShift  => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.RightShift => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.RightShiftUnsigned => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.BitwiseAnd => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.BitwiseOr  => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.BitwiseXor => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.LogicalAnd => (booleanType,   LogicalType,   LogicalType)
                       | Ast.LogicalOr  => (booleanType,   LogicalType,   LogicalType)
                       | Ast.InstanceOf => (anyType,       anyType,       booleanType)
                       | Ast.In         => (anyType,       anyType,       booleanType)
                       | Ast.Equals     => (anyType,       anyType,       booleanType)
                       | Ast.NotEquals  => (anyType,       anyType,       booleanType)
                       | Ast.StrictEquals => (anyType,     anyType,       booleanType)
                       | Ast.StrictNotEquals => (anyType,  anyType,       booleanType)
                       | Ast.Less       => (CompareType,  CompareType,    booleanType)
                       | Ast.LessOrEqual => (CompareType, CompareType,    booleanType)
                       | Ast.Greater    => (CompareType,  CompareType,    booleanType)
                       | Ast.GreaterOrEqual => (CompareType, CompareType, booleanType)
                       | Ast.Comma      => (anyType,      anyType,        t2)
            in
                checkMatch env t1 expectedType1;
                checkMatch env t2 expectedType2;
                resultType
            end
    in
        case expr of
            Ast.TernaryExpr (e1, e2, e3) =>
            let
                val t1:Ast.TYPE_EXPRESSION = verifySub e1
                val t2:Ast.TYPE_EXPRESSION = verifySub e2
                val t3:Ast.TYPE_EXPRESSION = verifySub e3
            in
                checkMatch env t1 booleanType;
                leastUpperBound t2 t3
            end

          | Ast.BinaryExpr (b, e1, e2) =>
            let
                val t1 = verifySub e1
                val t2 = verifySub e2
                val resultType = binaryOpType b t1 t2
            in
                resultType
            end

          | Ast.BinaryTypeExpr (b, e, ty) =>
            let
                val t1 = verifySub e
                val t2 = verifyType env ty
                val resultType = case b of
                                     Ast.Is => booleanType
                                   | _ => t2
            in
                case b of
                        Ast.Is => checkMatch env t1 t2
                  | Ast.Cast => checkMatch env t1 t2;
                resultType
            end

          | Ast.UnaryExpr (u, e) =>
            let
                val t = verifySub e
                val resultType = 
                    case u of
                        (* FIXME: these are probably mostly wrong *)
                        Ast.Delete        => booleanType
                      | Ast.Void          => undefinedType
                      | Ast.Typeof        => stringType
                      | Ast.PreIncrement  => AnyNumberType
                      | Ast.PreDecrement  => AnyNumberType
                      | Ast.PostIncrement => AnyNumberType
                      | Ast.PostDecrement => AnyNumberType
                      | Ast.UnaryPlus     => AnyNumberType
                      | Ast.UnaryMinus    => AnyNumberType
                      | Ast.BitwiseNot    => AnyNumberType
                      | Ast.LogicalNot    => booleanType
                      | Ast.Spread        => Ast.ArrayType [anyType]
                      (* TODO: isn't this supposed to be the prefix of a type expression? *)
                      | Ast.Type          => TypeType
            in
                case u of
                    (* FIXME: these are probably wrong *)
                    Ast.Delete        => ()
                  | Ast.PreIncrement  => checkMatch env t AnyNumberType
                  | Ast.PostIncrement => checkMatch env t AnyNumberType
                  | Ast.PreDecrement  => checkMatch env t AnyNumberType
                  | Ast.PostDecrement => checkMatch env t AnyNumberType
                  | Ast.UnaryPlus     => checkMatch env t AnyNumberType
                  | Ast.UnaryMinus    => checkMatch env t AnyNumberType
                  | Ast.BitwiseNot    => checkMatch env t AnyNumberType
                  | Ast.LogicalNot    => checkMatch env t booleanType
                  (* TODO: Ast.Type? *)
                  | _ => ();
                resultType
            end
            
          | Ast.TypeExpr t =>
            let
            in
                verifyType env t;
                TypeType
            end

          | Ast.ThisExpr k =>
            (* FIXME: type of current class, if any... also Self type? *)
            (* FIXME: handle function and generator this *)
            let
            in
                anyType
            end

          | Ast.YieldExpr eo =>
            let
                val t = verifySubOption eo
            in
                (* FIXME: strict check that returnType is Generator.<t> *)
                anyType
            end

          | Ast.SuperExpr eo =>
            let
                val t = verifySubOption eo
            in
                (* FIXME: what is this AST form again? *)
                anyType
            end

          | Ast.LiteralExpr le =>
            let
            in
                case le of
                    Ast.LiteralFunction func => 
                    verifyFunc env func   

                  | Ast.LiteralObject { expr, ty } =>
                    let
                        fun verifyField { kind, name, init } =
                            (verifyExpr env init; ())
                    in
                        List.app verifyField expr;
                        liftOption (verifyType env) ty anyType
                    end                    

                  (* FIXME handle comprehensions *)
                  | Ast.LiteralArray { exprs=Ast.ListExpr exprs, ty } => 
                    let                        
                    in
                        List.map (verifyExpr env) exprs;
                        liftOption (verifyType env) ty (Ast.ArrayType [anyType])
                    end

                  | Ast.LiteralNull        => nullType
                  | Ast.LiteralUndefined   => undefinedType
                  | Ast.LiteralDouble _    => doubleType
                  | Ast.LiteralDecimal _   => decimalType
                  | Ast.LiteralBoolean _   => booleanType
                  | Ast.LiteralString _    => stringType
                  | Ast.LiteralXML _       => anyType
                  | Ast.LiteralNamespace _ => NamespaceType
                  | Ast.LiteralRegExp _    => RegExpType
            end

          | Ast.CallExpr {func, actuals} =>
            let
                val t = verifySub func
                val args = verifySubList actuals
            in
                case t of
                    Ast.FunctionType { params, result, thisType, hasRest, minArgs } =>
                    
                    let fun checkargs args params =
                            case (args,params,hasRest) of
                                ([],[],false) => ()
                              | ([], [restType], true) => ()
                              | (args, [restType], true) =>
                                checkMatch env (Ast.ArrayType args) restType
                              | (a::ar, p::pr, _) =>
                                let in
                                    checkMatch env a p;
                                    checkargs ar pr
                                end
                              | _ => warning ["too many actuals"]
                    in
                        checkargs args params;
                        result
                    end
                  | Ast.SpecialType Ast.Any => anyType
                                               
		      (* 
		       * FIXME: Actually have to handle instance types here, and hook into
		       * their meta::invoke slot as well.
               * do not print error msgs for now, too noisy
		       *)
                  | _ => (warning ["ill-typed call to type ", LogErr.ty t]; anyType)
            end

            (* FIXME: what is this? *)
          | Ast.ApplyTypeExpr { expr, actuals } =>
            let
                val t = verifySub expr
                val actuals' = List.map (verifyType env) actuals
            in
                anyType
            end

          | Ast.LetExpr { defs=_, head as SOME (Ast.Head (rib, inits)), body } =>
            let
                val _ = verifyRib env rib
                val env' = withRib env rib
            in
                verifyInits env' inits;
                verifyExpr env' body
            end

          | Ast.NewExpr { obj, actuals } =>
            let
                val t = verifySub obj
                val ts = verifySubList actuals
            in
                (* FIXME: implement *)
                anyType
            end

          | Ast.ObjectRef { base, ident=idexpr, loc } =>
            let
                val _ = LogErr.setLoc loc
                val t = verifySub base
            in
                case t of
                    Ast.SpecialType Ast.Any => anyType
                  | Ast.ObjectType fields =>
                    let in
                        case List.find
                                 (fn {name, ty} => 
                                     case idexpr of
                                         (* FIXME: ignoring namespaces here *)
                                         Ast.Identifier { ident, ... } => ident=name
                                       | _ => false)
                                 fields
                         of
                         SOME {name, ty} => ty
                       | NONE => (warning ["Unknown field name ", LogErr.identExpr idexpr,
                                           " in object type ", LogErr.ty t];
                                  anyType)
                    end
(*
                  | Ast.InstanceType 
*)
                  | _ => (warning ["ObjectRef on non-object type: ", LogErr.ty t]; 
                          anyType)
            end

(*


     and INSTANCE_TYPE =
          {  name: NAME,
             typeParams: IDENT list,      
             typeArgs: TYPE_EXPRESSION list,
             nonnullable: bool,           (* redundant, ignored in verify.sml *)
             superTypes: TYPE_EXPRESSION list,  (* redundant, ignored in verify.sml *)
             ty: TYPE_EXPRESSION,               (* redundant, ignored in verify.sml *)
             dynamic: bool }              (* redundant, ignored in verify.sml *)


>> class d{var y;}
>> var x:d
>> x.y
STRICT-MODE WARNING: ObjectRef on non-object type: (d|null)

>> var w:d!;
>> w.y
STRICT-MODE WARNING: ObjectRef on non-object type: d



     and FIELD_TYPE =
           { name: IDENT,
             ty: TYPE_EXPRESSION }


     and IDENT_EXPRESSION =
         Identifier of
           { ident : IDENT,
             openNamespaces : NAMESPACE list list }
(* CF: the above should be unified with
        type MULTINAME = { nss: NAMESPACE list list, id: IDENT }
   Perhaps Identifier should be Multiname
*)
       | QualifiedExpression of  (* type * *)
           { qual : EXPRESSION,
             expr : EXPRESSION }
       | AttributeIdentifier of IDENT_EXPRESSION
       (* for bracket exprs: o[x] and @[x] *)
       | ExpressionIdentifier of
         { expr: EXPRESSION,
           openNamespaces : NAMESPACE list list }
       | QualifiedIdentifier of
           { qual : EXPRESSION,
             ident : Ustring.STRING }
       | UnresolvedPath of (IDENT list * IDENT_EXPRESSION) (* QualifiedIdentifier or ObjectRef *)
       | WildcardIdentifier            (* CF: not really an identifier, should be part of T *)

*)

          | Ast.LexicalRef { ident, loc } =>
            let in
                trace [ "lexicalref ", if strict then "strict" else "non-strict"];
                LogErr.setLoc loc;
                case verifyIdentExpr env (#ribs env) ident of
                    NONE => (warning ["unbound IDENT_EXPRESSION ", LogErr.identExpr ident]; anyType)
                  | SOME t => t
            end
                
          | Ast.SetExpr (a, le, re) =>
            let
                val t1 = verifyLvalue env le
                val t2 = verifySub re
                val resultType =
                    case a of
                        Ast.Assign => t2
                      | _ =>
                        let val binop = 
                                case a of
                                    Ast.AssignPlus => Ast.Plus
                                  | Ast.AssignMinus => Ast.Minus
                                  | Ast.AssignTimes => Ast.Times
                                  | Ast.AssignDivide  => Ast.Divide
                                  | Ast.AssignRemainder => Ast.Remainder
                                  | Ast.AssignLeftShift => Ast.LeftShift
                                  | Ast.AssignRightShift => Ast.RightShift
                                  | Ast.AssignRightShiftUnsigned => Ast.RightShiftUnsigned
                                  | Ast.AssignBitwiseAnd => Ast.BitwiseAnd
                                  | Ast.AssignBitwiseOr => Ast.BitwiseOr
                                  | Ast.AssignBitwiseXor => Ast.BitwiseXor
                                  | Ast.AssignLogicalAnd => Ast.LogicalAnd
                                  | Ast.AssignLogicalOr => Ast.LogicalOr
                                  | Ast.Assign => Ast.LogicalOr (* unreachable *)
                        in 
                            binaryOpType binop t1 t2
                        end
            in
                checkMatch env resultType t1;
                t1
            end

          | Ast.GetTemp n =>
            (* FIXME: these only occur on the RHS of compiled destructuring assignments. how to type-check? *)
            anyType

          | Ast.GetParam n =>
            LogErr.internalError ["GetParam not eliminated by Defn"]

          | Ast.ListExpr es =>
            let
                val ts = verifySubList es
            in
                case ts of 
                    [] => undefinedType
                  | _ => List.last ts
            end

          | Ast.InitExpr (it, head, inits) =>
            let
            in
                verifyInits (verifyHead env head) inits;
                anyType
            end

    end


and verifyExprAndCheck (env:ENV)
                       (expr:Ast.EXPRESSION)
                       (expectedType:Ast.TYPE_EXPRESSION)
    : unit =
    let 
        val ty = verifyExpr env expr
    in
       checkMatch env ty expectedType
    end

(*
    STATEMENT
*)

and verifyStmt (env:ENV)
               (stmt:Ast.STATEMENT)
    : unit =
    let 
        fun verifySub s = verifyStmt env s
        val { prog, 
              strict, 
              returnType,
              stdTypes = 
              { AnyNumberType, 
                doubleType,
                decimalType,

                AnyStringType,
                stringType,

                AnyBooleanType, 
                booleanType, 

                RegExpType,

                TypeType,
                NamespaceType }, ... } = env
    in
        case stmt of

            Ast.EmptyStmt => ()
          | Ast.BreakStmt i => ()
          | Ast.ContinueStmt i => ()

          | Ast.ExprStmt e => (verifyExpr env e; ())

          | Ast.ForInStmt {isEach, defn, obj, rib, next, labels, body} =>
            let
                val newEnv = withRibOpt env rib
            in
                verifyExpr env obj;
                verifyStmt newEnv next;
                verifyStmt newEnv body
            end

          | Ast.ThrowStmt es => (verifyExpr env es; ())

          | Ast.ReturnStmt es =>
            let 
                val ty = verifyExpr env es
            in
                    (* FIXME: this does not work yet. Nothing sets returnType *)
                    (*
	                 case returnType of
	                     NONE => error ["return not allowed here"]
                       | SOME retTy => checkMatch ty retTy
                     *)
                    ()
            end


          | Ast.BlockStmt block => verifyBlock env block
          | Ast.ClassBlock { block, ... } => verifyBlock env block
          | Ast.LabeledStmt (_, s) => verifySub s
          | Ast.LetStmt block => verifyBlock env block

          | Ast.WhileStmt { cond, body, rib, ... } =>
            let
                val newEnv = withRibOpt env rib
            in
                verifyExprAndCheck env cond booleanType;
                verifyStmt newEnv body
            end

          | Ast.DoWhileStmt { cond, body, rib, ... } =>
            let
                val newEnv = withRibOpt env rib
            in
                verifyExprAndCheck env cond booleanType;
                verifyStmt newEnv body
            end

          | Ast.ForStmt  { rib, init, cond, update, body, ... } =>
            let 
                val newEnv = withRibOpt env rib
            in
                Option.app (verifyRib env) rib;
                List.app (verifyStmt newEnv) init;
                verifyExprAndCheck newEnv cond booleanType;
                verifyExpr newEnv update;
                verifyStmt newEnv body
            end

          | Ast.IfStmt {cnd, thn, els } =>
            let
            in
                verifyExprAndCheck env cnd booleanType;
                verifySub thn;
                verifySub els
            end

          | Ast.WithStmt {obj, ty, body} => (* FIXME: implement *)
            ()

          | Ast.TryStmt {block, catches, finally} =>
            let
            in
                verifyBlock env block;
                List.app (verifyCatchClause env) catches;
                Option.app (verifyBlock env) finally
            end

          | Ast.SwitchStmt { cond, cases, ... } =>
            let
                fun verifyCase { label, inits, body } =
                    let
                    in
                        Option.map (verifyExpr env) label;
                        Option.app (verifyInits env) inits;
                        verifyBlock env body
                    end
            in
                verifyExpr env cond;
                List.app verifyCase cases
            end

          | Ast.SwitchTypeStmt { cond, ty, cases } =>
            let
            in
                verifyExpr env cond;
                verifyType env ty;
                List.app (verifyCatchClause env) cases
            end

          | Ast.DXNStmt x => (* FIXME: implement *)
            ()

          | _ => error ["Shouldn't happen: failed to match in Verify.verifyStmt"]

    end


and verifyCatchClause (env:ENV)
                      (clause:Ast.CATCH_CLAUSE)
    : unit =
    let
        val {bindings, ty, rib, inits, block} = clause
        val blockEnv = withRibOpt env rib
    in
        verifyType env ty;
        Option.app (verifyRib env) rib;
        Option.map (verifyInits blockEnv) inits;
        verifyBlock blockEnv block
    end

and strictness (curr:bool) [] = curr
  | strictness (curr:bool) ((x:Ast.PRAGMA)::xs) = 
    case x of 
	Ast.UseStrict => strictness true xs
      | Ast.UseStandard => strictness false xs
      | _ => strictness curr xs
	     
and verifyBlock (env:ENV)
                (b:Ast.BLOCK)
    : unit =
    let
       val Ast.Block { head as SOME head', body, loc, pragmas, ... } = b
	    val env = withStrict env (strictness (#strict env) pragmas)
        val _ = LogErr.setLoc loc
        val env' = verifyHead env head'
    in
        List.app (verifyStmt env') body
    end

(* returns the normalized type of this function *)
and verifyFunc (env:ENV)
               (func:Ast.FUNC)
    : Ast.TYPE_EXPRESSION =
    let
        val Ast.Func { name, fsig=Ast.FunctionSignature { typeParams, ...}, 
                       native, generator, block, param, defaults, ty, loc } = func
        (* FIXME: use public as namespace of type variables? *)
        val rib = map (fn id => (Ast.PropName {ns=Name.publicNS, id=id},
                                 Ast.TypeVarFixture (Parser.nextAstNonce ())))
                  typeParams
        val env' = withRib env rib
        val blockEnv = verifyHead env' param
    in
        LogErr.setLoc loc;
        map (verifyExpr env') defaults;
        Option.app (verifyBlock blockEnv) block;
        verifyType env' ty
    end


and verifyFixture (env:ENV)
		          (f:Ast.FIXTURE)
    : unit =
    case f of
     
        Ast.ClassFixture (Ast.Cls {name, privateNS, protectedNS, parentProtectedNSs, 
                                   typeParams, nonnullable, 
                                   dynamic, extends, implements, 
                                   classRib, instanceRib, instanceInits, 
                                   constructor, classType, instanceType }) =>
         let
             val classEnv = withRib env classRib
             val instanceEnv = withRib classEnv instanceRib
         in
             verifyRib env classRib;
             verifyRib classEnv instanceRib;
             verifyHead instanceEnv instanceInits;
             case constructor of
                 NONE => ()
               | SOME (Ast.Ctor {settings, superArgs, func}) =>
                 let
                 in
                     (* FIXME: need to construct a sort of odd env for settings verification. *)
                     (* verifyHead instanceEnv settings; *)
                     map (verifyExpr instanceEnv) superArgs;
                     verifyFunc instanceEnv func;
                     ()
                 end
         end

      (* FIXME: verify interfaces *)

      | Ast.TypeFixture ty => (verifyType env ty; ())
      | Ast.ValFixture { ty, readOnly } => (verifyType env ty; ())
      | Ast.MethodFixture { func, ty, ... } =>
        let
        in
            verifyFunc env func;
            verifyType env ty;
            ()
        end

      | Ast.VirtualValFixture { ty, getter, setter} =>
        let
        in
            verifyType env ty;
            Option.map (verifyFunc env) getter;
            Option.map (verifyFunc env) setter;
            ()
        end

      | _ => ()

(* The env does not yet include this rib *)
and verifyRib (env:ENV)
              (rib:Ast.RIB)
    : unit =
    let
        val env = withRib env rib
        fun doFixture (name, fixture) =
            (trace ["verifying fixture: ", LogErr.fname name];
             verifyFixture env fixture)
    in
        (* FIXME: should we check for duplicate bindings? *)
        List.app doFixture rib
    end


and verifyFragment (env:ENV)
                   (frag:Ast.FRAGMENT) 
  : unit = 
    case frag of 
        Ast.Anon block => verifyBlock env block


and verifyTopRib (prog:Fixture.PROGRAM)
                 (strict:bool)
                 (rib:Ast.RIB)
    : unit =
    let
        val env = newEnv prog strict
    in
        verifyRib env rib
    end


and verifyTopFragment (prog:Fixture.PROGRAM)
                      (strict:bool) 
                      (frag:Ast.FRAGMENT) 
  : Ast.FRAGMENT =
    if strict 
    then
        let 
            val env = newEnv prog strict
        in
            trace ["verifyTopFragment"];
            if !doTraceFrag then
                let in
                    print "verifyTopFragment:printing\n";
                    Pretty.ppFragment frag;
                    TextIO.print "\n"
                end
            else ();
            verifyFragment env frag;
            trace ["verification complete ",
                   (if strict then "strict " else "nonstrict ")];
            frag
        end
    else
        frag
end


