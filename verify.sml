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
     AnyNumberType: Ast.TYPE,
     doubleType:    Ast.TYPE,
     decimalType:   Ast.TYPE,

     AnyStringType: Ast.TYPE,
     stringType:    Ast.TYPE,

     AnyBooleanType: Ast.TYPE,
     booleanType:   Ast.TYPE,

     RegExpType:    Ast.TYPE,

     NamespaceType: Ast.TYPE,
     TypeType:      Ast.TYPE
}

type ENV = { returnType: Ast.TYPE option,
             strict: bool,
             rootRib: Ast.RIB,
             ribs: Ast.RIBS,
             stdTypes: STD_TYPES,
             thisType: Ast.TYPE
           }

fun withReturnType { returnType=_, strict, rootRib, ribs, stdTypes, thisType } returnType =
    { returnType=returnType, strict=strict, rootRib=rootRib, ribs=ribs, stdTypes=stdTypes, thisType = thisType }

fun withRibs { returnType, strict, rootRib, ribs=_, stdTypes, thisType } ribs =
    { returnType=returnType, strict=strict, rootRib=rootRib, ribs=ribs, stdTypes=stdTypes, thisType = thisType }

fun withStrict { returnType, strict=_, rootRib, ribs, stdTypes, thisType } strict =
    { returnType=returnType, strict=strict, rootRib=rootRib, ribs=ribs, stdTypes=stdTypes, thisType = thisType }

fun withRib { returnType, strict, rootRib, ribs, stdTypes, thisType} extn =
    { returnType=returnType, strict=strict, rootRib=rootRib, ribs=(extn :: ribs), stdTypes=stdTypes, thisType = thisType }

fun withRibOpt { returnType, strict, rootRib, ribs, stdTypes, thisType } extn =
    { returnType=returnType, 
      strict=strict, 
      rootRib=rootRib, 
      ribs=case extn of 
               NONE => ribs 
             | SOME e => (e :: ribs), 
      stdTypes=stdTypes,
      thisType = thisType }

(* Local tracing machinery *)

val warningsAreFailures = ref false
val traceWarnings = ref false
val doTrace = ref false
val doTraceProg = ref false
val Any =  Ast.AnyType
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

val undefinedType   = Ast.UndefinedType
val nullType        = Ast.NullType
val anyType         = Ast.AnyType
                                           
fun newEnv (rootRib:Ast.RIB) 
           (strict:bool) 
    : ENV = 
    { 
     returnType = NONE,
     strict = strict,
     rootRib = rootRib, 
     ribs = [rootRib],
     thisType = Ast.AnyType,
     
     stdTypes = 
     {      
      AnyNumberType = Type.getNamedGroundType rootRib Name.ES4_AnyNumber,
      doubleType    = Type.getNamedGroundType rootRib Name.ES4_double,
      decimalType   = Type.getNamedGroundType rootRib Name.ES4_decimal,

      AnyStringType = Type.getNamedGroundType rootRib Name.ES4_AnyString,
      stringType    = Type.getNamedGroundType rootRib Name.ES4_string,

      AnyBooleanType= Type.getNamedGroundType rootRib Name.ES4_AnyBoolean,
      booleanType   = Type.getNamedGroundType rootRib Name.ES4_boolean,

      RegExpType    = Type.getNamedGroundType rootRib Name.public_RegExp,

      NamespaceType = Type.getNamedGroundType rootRib Name.ES4_Namespace,

      TypeType      = Type.getNamedGroundType rootRib Name.intrinsic_Type
     }
    }


(******************* Subtyping and Compatibility *************************)

(* src and dst are normalized *)
fun checkMatch (env:ENV)
               (src:Ast.TYPE) (* assignment src *)
		       (dst:Ast.TYPE) (* assignment dst *)
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
fun leastUpperBound (t1:Ast.TYPE)
                    (t2:Ast.TYPE)
    : Ast.TYPE =
    let
    in
        if      Type.compatibleSubtype t1 t2 then t2
        else if Type.compatibleSubtype t2 t1 then t1
        else Ast.UnionType [t1, t2]
    end

(******************* Utilities for resolving IDENTIFIER_EXPRESSIONs *********************)

(* Returns the type of the given fixture. The result is not yet normalized,
 * and so only makes sense in the environment the fixture was defined in. *)

fun typeOfFixture (env:ENV)
			      (fixture:Ast.FIXTURE)
    : Ast.TYPE = 
    case fixture of 	
	    (* 
	     * FIXME: classtypes should be turned into instancetypes of 
	     * the static-class type, so we can look up static props on them
	     * correctly. Abusing object types like this is no good.
	     *)
	    (Ast.ClassFixture (Ast.Class {classType, ...})) => classType	
      | (Ast.ValFixture { ty, ... }) => ty	
      | (Ast.VirtualValFixture { ty, ... }) => ty        
      | (Ast.TypeFixture _) => (#TypeType (#stdTypes env))
      | _ => anyType

(******************** Verification **************************************************)

(*
fun verifyNameExpr (env:ENV)
                   (ribs:Ast.RIBS)
                   (nameExpr:Ast.NAME_EXPRESSION)
    : Ast.TYPE =
    let
        val (_, _, fix) = Fixture.resolveNameExpr ribs nameExpr
        val ty = typeOfFixture env fix
        val ty = verifyType (withRibs env ribs) ty
    in
        ty
    end
*)
(* Verification (aka normalization) converts a (non-closed) TYPE into a 
 * (closed, aka grounded) TYPE. 
 *)

and verifyType (env:ENV)
               (ty:Ast.TYPE)
    : Ast.TYPE =
    let
        val _ = trace ["verifyType: calling normalize ", LogErr.ty ty]
        val norm : Ast.TYPE = 
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
    : Ast.TYPE option =
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
    : Ast.TYPE = 
    let
    in
        case expr of
            (* FIXME: check for final fields *)
            Ast.ObjectNameReference _ => 
            verifyExpr env expr
          | Ast.ObjectIndexReference _ => 
            verifyExpr env expr
          | Ast.LexicalReference _ => 
            verifyExpr env expr
          | _ =>
            (warning ["Not an lvalue"]; anyType)
    end

and verifyExpr (env:ENV)
               (expr:Ast.EXPRESSION)
    : Ast.TYPE =
    let val _ = trace [">>> Verifying expr "]
        val _ = if !doTrace then Pretty.ppExpr expr else ()
        val r = verifyExpr2 env expr
        val _ = trace ["<<< Verifying expr ", LogErr.ty r]
    in
        r
    end

and verifyExpr2 (env:ENV)
               (expr:Ast.EXPRESSION)
    : Ast.TYPE =
    let
        val { rootRib, 
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
        fun verifySub (e:Ast.EXPRESSION) : Ast.TYPE = verifyExpr env e
        fun verifySubList (es:Ast.EXPRESSION list) : Ast.TYPE list = map (verifyExpr env) es
        fun verifySubOption (eo:Ast.EXPRESSION option) : Ast.TYPE option = Option.map verifySub eo
        fun binaryOpType (b:Ast.BINOP) t1 t2 : Ast.TYPE =
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
            Ast.ConditionalExpression (e1, e2, e3) =>
            let
                val t1:Ast.TYPE = verifySub e1
                val t2:Ast.TYPE = verifySub e2
                val t3:Ast.TYPE = verifySub e3
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
                      | Ast.Spread        => Ast.ArrayType ([], SOME anyType)
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
            (#thisType env)

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

          | Ast.LiteralFunction func => 
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
                liftOption (verifyType env) ty (Ast.ArrayType ([anyType],NONE)
)
            end

          | Ast.LiteralNull        => nullType
          | Ast.LiteralUndefined   => undefinedType
          | Ast.LiteralDouble _    => 
            (trace ["doubleType=", LogErr.ty doubleType]; doubleType)
          | Ast.LiteralDecimal _   => decimalType
          | Ast.LiteralBoolean _   => booleanType
          | Ast.LiteralString _    => stringType
          | Ast.LiteralNamespace _ => NamespaceType
          | Ast.LiteralRegExp _    => RegExpType
          
          | Ast.CallExpr {func, actuals} => Ast.AnyType
(* FIXME: get calls working
            let
                val t = verifySub func
                val args = verifySubList actuals
            in
                case t of
                    Ast.FunctionType { typeParams=[], params, result=SOME result, thisType, hasRest, minArgs } =>
                    
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
                  | Ast.AnyType => (warning ["ill-typed call to type ", LogErr.ty t]; anyType)
                                               
		      (* 
		       * FIXME: Actually have to handle instance types here, and hook into
		       * their meta::invoke slot as well.
               * do not print error msgs for now, too noisy
		       *)
                  | _ => (warning ["ill-typed call to type ", LogErr.ty t]; anyType)
            end
*)
            (* FIXME: what is this? *)
          | Ast.ApplyTypeExpression { expr, actuals } =>
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

          | Ast.ObjectIndexReference { object, index, loc } =>
            (verifySub object; 
             verifySub index)
          | Ast.ObjectNameReference { object, name, loc } => Ast.AnyType
(* FIXME: get working
            let
                val _ = LogErr.setLoc loc
                val t = verifySub object
                val refName = Type.nameExprToFieldName (#ribs env) name
            in
                case t of
                    Ast.AnyType => anyType
                  | Ast.RecordType fields =>
                    let in
                        case List.find
                                 (fn {name, ty} => refName = (Type.nameExprToFieldName (#ribs env) name))
                                 fields
                         of
                            SOME {name, ty} => ty
                          | NONE => (warning ["Unknown field name ", LogErr.name refName,
                                              " in object type ", LogErr.ty t];
                                     anyType)
                    end
(*
                  | Ast.InstanceType 
*)
                  | _ => (warning ["ObjectNameReference on non-object type: ", LogErr.ty t]; 
                          anyType)
            end
*)
(*


     and INSTANCE_TYPE =
          {  name: NAME,
             typeParams: IDENTIFIER list,      
             typeArgs: TYPE list,
             nonnullable: bool,           (* redundant, ignored in verify.sml *)
             superTypes: TYPE list,  (* redundant, ignored in verify.sml *)
             ty: TYPE,               (* redundant, ignored in verify.sml *)
             dynamic: bool }              (* redundant, ignored in verify.sml *)


>> class d{var y;}
>> var x:d
>> x.y
STRICT-MODE WARNING: ObjectRef on non-object type: (d|null)

>> var w:d!;
>> w.y
STRICT-MODE WARNING: ObjectRef on non-object type: d



     and FIELD_TYPE =
           { name: IDENTIFIER,
             ty: TYPE }


     and IDENTIFIER_EXPRESSION =
         Identifier of
           { ident : IDENTIFIER,
             openNamespaces : NAMESPACE list list }
(* CF: the above should be unified with
        type MULTINAME = { nss: NAMESPACE list list, id: IDENTIFIER }
   Perhaps Identifier should be Multiname
*)
       | QualifiedExpression of  (* type * *)
           { qual : EXPRESSION,
             expr : EXPRESSION }
       | AttributeIdentifier of IDENTIFIER_EXPRESSION
       (* for bracket exprs: o[x] and @[x] *)
       | ExpressionIdentifier of
         { expr: EXPRESSION,
           openNamespaces : NAMESPACE list list }
       | QualifiedIdentifier of
           { qual : EXPRESSION,
             ident : Ustring.STRING }
       | UnresolvedPath of (IDENTIFIER list * IDENTIFIER_EXPRESSION) (* QualifiedIdentifier or ObjectRef *)
       | WildcardIdentifier            (* CF: not really an identifier, should be part of T *)

*)

          | Ast.LexicalReference { name, loc } =>
            let in
                trace [ "lexicalref ", if strict then "strict" else "non-strict"];
                LogErr.setLoc loc;
                if strict 
                then Ast.AnyType (* FIXME: verifyNameExpr env (#ribs env) name *)
                else Ast.AnyType (* try/catch, or reformulate the lookup to have a fail-soft mode. *)
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
                       (expectedType:Ast.TYPE)
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
        val { rootRib, 
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

and paramsToTypeVars (typeParams:Ast.IDENTIFIER list)
    : Ast.RIB = 
    
    map (fn id => (Ast.PropName (Name.public id),
                   Ast.TypeVarFixture (Parser.nextAstNonce ())))
        typeParams
        
and verifyFunc (env:ENV)
               (func:Ast.FUNC)
    : Ast.TYPE =
    let
        val Ast.Func { name, fsig=Ast.FunctionSignature { typeParams, ...}, 
                       native, generator, block, param, defaults, ty, loc } = func
        (* FIXME: use public as namespace of type variables? *)
        val rib = paramsToTypeVars typeParams
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
     
        Ast.ClassFixture (Ast.Class {name, privateNS, protectedNS, parentProtectedNSs, 
                                   typeParams, nonnullable, 
                                   dynamic, extends, implements, 
                                   classRib, instanceRib, instanceInits, 
                                   constructor, classType }) =>
         let
             val typeVarRib = paramsToTypeVars typeParams
             val typeEnv = withRib env typeVarRib
             val classEnv = withRib typeEnv classRib
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

      | Ast.TypeFixture (typeParams,ty) => (verifyType env ty; ())   (* FIXME: extend env with typeParams *)
      | Ast.ValFixture { ty, writable } => (verifyType env ty; ())
      | Ast.MethodFixture { func, ty, ... } =>
        let
        in
            verifyFunc env func;
            verifyType env ty;
            ()
        end

      | Ast.VirtualValFixture { ty, getter, setter} =>
        let
            fun fst (a,_) = a
        in
            verifyType env ty;
            Option.map ((verifyFunc env) o fst) getter;
            Option.map ((verifyFunc env) o fst) setter;
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


and verifyTopRib (rootRib:Ast.RIB)
                 (strict:bool)
                 (rib:Ast.RIB)
    : unit =
    let
        val env = newEnv rootRib strict
    in
        verifyRib env rib
    end


and verifyProgram (rootRib:Ast.RIB)
                  (strict:bool) 
                  (prog:Ast.PROGRAM) 
  : Ast.PROGRAM =
    if strict 
    then
        let 
            val env = newEnv rootRib strict
            val Ast.Program block = prog
        in
            trace ["verifyProgram"];
            if !doTraceProg then
                let in
                    print "verifyProgram:printing\n";
                    Pretty.ppProgram prog;
                    TextIO.print "\n"
                end
            else ();
            verifyBlock env block;
            trace ["verification complete ",
                   (if strict then "strict " else "nonstrict ")];
            prog
        end
    else
        prog
end


