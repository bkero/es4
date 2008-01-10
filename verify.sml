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
     AnyNumberType: Ast.TYPE_EXPR,
     doubleType: Ast.TYPE_EXPR,
     decimalType: Ast.TYPE_EXPR,
     intType: Ast.TYPE_EXPR,
     uintType: Ast.TYPE_EXPR,
     byteType: Ast.TYPE_EXPR,

     AnyStringType: Ast.TYPE_EXPR,
     stringType: Ast.TYPE_EXPR,

     AnyBooleanType: Ast.TYPE_EXPR,
     booleanType: Ast.TYPE_EXPR,

     RegExpType: Ast.TYPE_EXPR,

     NamespaceType: Ast.TYPE_EXPR,
     TypeType: Ast.TYPE_EXPR

}

type ENV = { returnType: Ast.TYPE_EXPR option,
             strict: bool,
             prog: Fixture.PROGRAM,
             ribs: Ast.RIB list,
             stdTypes: STD_TYPES }

fun withReturnType { returnType=_, strict, prog, ribs, stdTypes } returnType =
    { returnType=returnType, strict=strict, prog=prog, ribs=ribs, stdTypes=stdTypes }

fun withRibs { returnType, strict, prog, ribs=_, stdTypes } ribs =
    { returnType=returnType, strict=strict, ribs=ribs, stdTypes=stdTypes }

fun withStrict { returnType, strict=_, prog, ribs, stdTypes } strict =
    { returnType=returnType, strict=strict, prog=prog, ribs=ribs, stdTypes=stdTypes }

fun withRib { returnType, strict, prog, ribs, stdTypes} extn =
    { returnType=returnType, strict=strict, prog=prog, ribs=(extn :: ribs), stdTypes=stdTypes }

(* Local tracing machinery *)

val doTrace = ref false
val doTraceFrag = ref false
fun log ss = LogErr.log ("[verify] " :: ss)
fun trace ss = if (!doTrace) then log ss else ()
fun error ss = LogErr.verifyError ss

fun logType ty = (Pretty.ppType ty; TextIO.print "\n")
fun traceType ty = if (!doTrace) then logType ty else ()

(*
fun typeToString ty = ...
*)

fun fmtType ty = if !doTrace
                 then LogErr.ty ty
                 else ""

(****************************** standard types *************************)

val undefinedType   = Ast.SpecialType Ast.Undefined
val nullType        = Ast.SpecialType Ast.Null
val anyType         = Ast.SpecialType Ast.Any

fun normalize (prog:Fixture.PROGRAM)
              (strict:bool)              
              (ty:Ast.TY)
    : Ast.TY = 
    Type.normalize prog [] ty
    handle LogErr.TypeError e => 
           if strict 
           then error [e, " while normalizing ", 
                       LogErr.ty (AstQuery.typeExprOf ty)]
           else ty
                                
fun makeTy (tyExpr:Ast.TYPE_EXPR) 
    : Ast.TY =
    Ast.Ty { expr = tyExpr,
             ribId = NONE }

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
      doubleType = Type.getNamedGroundType prog Name.ES4_double,
      decimalType = Type.getNamedGroundType prog Name.ES4_decimal,
      intType = Type.getNamedGroundType prog Name.ES4_int,
      uintType = Type.getNamedGroundType prog Name.ES4_uint,
      byteType = Type.getNamedGroundType prog Name.ES4_byte,

      AnyStringType = Type.getNamedGroundType prog Name.ES4_AnyString,
      stringType = Type.getNamedGroundType prog Name.ES4_string,

      AnyBooleanType = Type.getNamedGroundType prog Name.ES4_AnyBoolean,
      booleanType = Type.getNamedGroundType prog Name.ES4_boolean,

      RegExpType = Type.getNamedGroundType prog Name.nons_RegExp,

      NamespaceType = Type.getNamedGroundType prog Name.ES4_Namespace,

      TypeType = Type.getNamedGroundType prog Name.intrinsic_Type
     }
    }

(****************************** misc auxiliary functions *************************)

fun assert b s = if b then () else (raise error [s])

fun checkForDuplicates [] = ()
  | checkForDuplicates (x::xs) =
    if List.exists (fn y => x = y) xs
    then error ["concurrent definition"]
    else checkForDuplicates xs


(******************* Subtyping and Compatibility *************************)

infix 4 <:;

fun (tsub <: tsup) =
    Type.groundIsSubtype tsub tsup

infix 4 ~:;

fun (tleft ~: tright) =
    Type.groundIsCompatible tleft tright

fun checkCompatible (t1:Ast.TYPE_EXPR) (* assignment src *)
		            (t2:Ast.TYPE_EXPR) (* assignment dst *)
    : unit =
    if t1 ~: t2
    then ()
    else error ["checkCompatible failed: ", LogErr.ty t1, " vs. ", LogErr.ty t2]

fun checkBicompatible (ty1:Ast.TYPE_EXPR)
		              (ty2:Ast.TYPE_EXPR)
    : unit =
    let in
	    checkCompatible ty1 ty2;
	    checkCompatible ty2 ty1
    end

fun checkConvertible (ty1:Ast.TYPE_EXPR)
                     (ty2:Ast.TYPE_EXPR)
    : unit =
    (* TODO: int to float, etc, and to() methods *)
    checkCompatible ty1 ty2

fun leastUpperBound (t1:Ast.TYPE_EXPR)
                    (t2:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR =
    let
    in
        if t1 <: t2 then
            t2
        else if t2 <: t1 then
            t1
        else
            Ast.UnionType [t1, t2]
    end


(******************** Verification **************************************************)

fun verifyType (env:ENV)
               (ty:Ast.TY)
    : (Ast.TY * Ast.TYPE_EXPR) =
(* 
 * Verification, if it runs, is obliged to come up with a best guess
 * ground type expression for every type it sees. It does this because it performs 
 * some static reasoning and leaves behind a bunch of runtime checks.
 * 
 * As such, when we hit a TY, we try to reduce it to a TYPE_EXPR. If we get a non-ground
 * type, we produce the special TYPE_EXPR "anyType", representing *, because it's
 * the "best guess" at this stage. 
 * 
 * We also return the normalized TY, as many of our callers are rewriting their 
 * type expressions and it gives us a chance to optimize away future
 * runtime calculations of the same normal form.
 *)
    let
        val norm = normalize (#prog env) (#strict env) ty
    in
        (norm, 
         if Type.isGroundTy norm
         then (AstQuery.typeExprOf norm)
         else anyType)
    end

fun verifyTy (env:ENV)
             (ty:Ast.TY)
    : Ast.TY =
    
    let
        val (ty,te) = verifyType env ty
    in
        ty
    end

fun verifyTypeExpr (env:ENV)
                   (ty:Ast.TY)
    : Ast.TYPE_EXPR =
    
    let
        val (ty,te) = verifyType env ty
    in
        te
    end


(*
    HEAD
*)

and verifyInits (env:ENV) (inits:Ast.INITS)
    : Ast.INITS =
    List.map (fn (name, expr) => (name, verifyExprOnly env expr))
             inits

and verifyInitsOption (env:ENV)
		              (inits:Ast.INITS option)
    : Ast.INITS =
    case inits of
        SOME inits => verifyInits env inits
      | _ => LogErr.internalError ["missing inits"]


and verifyHead (env:ENV) ((Ast.Head (rib, inits)):Ast.HEAD)
    : Ast.HEAD =
    (trace ["verifying head with ", Int.toString (length rib), " entry rib"];
     (Ast.Head (verifyRib env rib, verifyInits env inits)))

(*
    EXPR
*)

and verifyExpr (env:ENV)
               (expr:Ast.EXPR)
    : (Ast.EXPR * Ast.TYPE_EXPR) =
    let
        val { prog, 
              strict, 
              stdTypes = 
              { AnyNumberType, 
                doubleType,
                decimalType,
                intType,
                uintType,
                byteType,

                AnyStringType,
                stringType,

                AnyBooleanType, 
                booleanType, 

                RegExpType,

                TypeType,
                NamespaceType }, ... } = env

        fun verifySub e = verifyExpr env e
        fun verifySubList es = verifyExprs env es
        fun verifySubOption NONE = (NONE, NONE)
          | verifySubOption (SOME e) =
            let
                val (e', t) = verifySub e
            in
                (SOME e', SOME t)
            end

        fun return (e, t:Ast.TYPE_EXPR) =
            (Ast.ExpectedTypeExpr (t, e), t)

        fun whenStrict (thunk:unit -> unit) : unit =
            if strict
            then thunk ()
            else ()
    in
        case expr of
            Ast.TernaryExpr (e1, e2, e3) =>
            let
                val (e1', t1) = verifySub e1
                val (e2', t2) = verifySub e2
                val (e3', t3) = verifySub e3
            in
                whenStrict (fn () => checkConvertible t1 booleanType);
                (* FIXME: this produces a union type. is that right? *)
                return (Ast.TernaryExpr (e1', e2', e3'), leastUpperBound t2 t3)
            end

          | Ast.BinaryExpr (b, e1, e2) =>
            let
                val (e1', t1) = verifySub e1
                val (e2', t2) = verifySub e2
                (* FIXME: these are way wrong. For the time being, just jam in star everywhere
                 * we know we don't know enough *)
                val AdditionType = anyType
                val CompareType = anyType
                (* FIXME: need to deal with operator overloading *)
                val (expectedType1, expectedType2, resultType) =
                    case b of
                         Ast.Plus => (AdditionType, AdditionType, AdditionType)
                       | Ast.Minus => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.Times => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.Divide => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.Remainder => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.LeftShift => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.RightShift => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.RightShiftUnsigned => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.BitwiseAnd => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.BitwiseOr => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.BitwiseXor => (AnyNumberType, AnyNumberType, AnyNumberType)
                       | Ast.LogicalAnd => (booleanType, booleanType, booleanType)
                       | Ast.LogicalOr => (booleanType, booleanType, booleanType)
                       | Ast.InstanceOf => (anyType, anyType, booleanType)
                       | Ast.In => (anyType, anyType, booleanType)
                       | Ast.Equals => (anyType, anyType, booleanType)
                       | Ast.NotEquals => (anyType, anyType, booleanType)
                       | Ast.StrictEquals => (anyType, anyType, booleanType)
                       | Ast.StrictNotEquals => (anyType, anyType, booleanType)
                       | Ast.Less => (CompareType, CompareType, booleanType)
                       | Ast.LessOrEqual => (CompareType, CompareType, booleanType)
                       | Ast.Greater => (CompareType, CompareType, booleanType)
                       | Ast.GreaterOrEqual => (CompareType, CompareType, booleanType)
                       | Ast.Comma => (anyType, anyType, t2)
            in
                whenStrict (fn () =>
                               let
                               in
                                   checkCompatible t1 expectedType1;
                                   checkCompatible t2 expectedType2
                               end);
                return (Ast.BinaryExpr (b, e1', e2'), resultType)
            end

          | Ast.ExpectedTypeExpr (t, e) =>
            (*
             * If we got here, we're re-verifying an already-verified expression.
             * This means we've already put expectations on all subexpressions, and
             * we don't want to re-wrap any subexpressions, so we just bottom out here.
             *)
            (Ast.ExpectedTypeExpr (t, e), t)

          | Ast.BinaryTypeExpr (b, e, ty) =>
            let
                val (e', t1) = verifySub e
                val (ty, t2) = verifyType env ty
                val resultType = case b of
                                      Ast.Is => booleanType
                                    | _ => t2
            in
                whenStrict (fn () => case b of
                                          Ast.To => checkConvertible t1 t2
                                        | Ast.Is => ()
                                        | _ => checkCompatible t1 t2);
                return (Ast.BinaryTypeExpr (b, e', ty), resultType)
            end

          | Ast.UnaryExpr (u, e) =>
            let
                val (e', t) = verifySub e
                val resultType = case u of
                                      (* FIXME: these are probably mostly wrong *)
                                      Ast.Delete => booleanType
                                    | Ast.Void => undefinedType
                                    | Ast.Typeof => stringType
                                    | Ast.PreIncrement => AnyNumberType
                                    | Ast.PreDecrement => AnyNumberType
                                    | Ast.PostIncrement => AnyNumberType
                                    | Ast.PostDecrement => AnyNumberType
                                    | Ast.UnaryPlus => AnyNumberType
                                    | Ast.UnaryMinus => AnyNumberType
                                    | Ast.BitwiseNot => AnyNumberType
                                    | Ast.LogicalNot => booleanType
                                    (* TODO: isn't this supposed to be the prefix of a type expression? *)
                                    | Ast.Type => TypeType
            in
                whenStrict (fn () =>
                               case u of
                                    (* FIXME: these are probably wrong *)
                                    Ast.Delete => ()
                                  | Ast.PreIncrement => checkCompatible t AnyNumberType
                                  | Ast.PostIncrement => checkCompatible t AnyNumberType
                                  | Ast.PreDecrement => checkCompatible t AnyNumberType
                                  | Ast.PostDecrement => checkCompatible t AnyNumberType
                                  | Ast.UnaryPlus => checkCompatible t AnyNumberType
                                  | Ast.UnaryMinus => checkCompatible t AnyNumberType
                                  | Ast.BitwiseNot => checkCompatible t AnyNumberType
                                  | Ast.LogicalNot => checkConvertible t booleanType
                                  (* TODO: Ast.Type? *)
                                  | _ => ());
                return (Ast.UnaryExpr (u, e'), resultType)
            end

          | Ast.TypeExpr t =>
            return (Ast.TypeExpr (verifyTy env t), TypeType)

          | Ast.ThisExpr k =>
            (* FIXME: type of current class, if any... also Self type? *)
            (* FIXME: handle function and generator this *)
            return (Ast.ThisExpr k, anyType)

          | Ast.YieldExpr eo =>
            let
                val (eo', t) = verifySubOption eo
            in
                (* TODO: strict check that returnType is Generator.<t> *)
                return (Ast.YieldExpr eo', anyType)
            end

          | Ast.SuperExpr eo =>
            let
                val (eo', t) = verifySubOption eo
            in
                (* TODO: what is this AST form again? *)
                return (Ast.SuperExpr eo', anyType)
            end

          | Ast.LiteralExpr le =>
            let
                val resultType = case le of
                                      Ast.LiteralNull => nullType
                                    | Ast.LiteralUndefined => undefinedType
                                    | Ast.LiteralDouble _ => doubleType
                                    | Ast.LiteralDecimal _ => decimalType
                                    | Ast.LiteralInt _ => intType
                                    | Ast.LiteralUInt _ => uintType
                                    | Ast.LiteralBoolean _ => booleanType
                                    | Ast.LiteralString _ => stringType
                                    | Ast.LiteralArray { ty=SOME ty, ... } => verifyTypeExpr env ty
                                    (* FIXME: how do we want to represent [*] ? *)
                                    | Ast.LiteralArray { ty=NONE, ... } => Ast.ArrayType [anyType]
                                    (* TODO: define this *)
                                    | Ast.LiteralXML _ => anyType
                                    | Ast.LiteralNamespace _ => NamespaceType
                                    | Ast.LiteralObject { ty=SOME ty, ... } => verifyTypeExpr env ty
                                    (* FIXME: how do we want to represent {*} ? *)
                                    | Ast.LiteralObject { ty=NONE, ... } => anyType
                                    | Ast.LiteralFunction (Ast.Func { ty, ... }) => verifyTypeExpr env ty
                                    | Ast.LiteralRegExp _ => RegExpType
                fun verifyField { kind, name, init } =
                    { kind = kind,
                      name = name,
                      init = verifyExprOnly env init }

                val le = case le of
                             Ast.LiteralFunction func =>
                             Ast.LiteralFunction (verifyFunc env func)

                           | Ast.LiteralObject { expr, ty } =>
                             Ast.LiteralObject { expr = map verifyField expr,
                                                 ty = case ty of
                                                          NONE => NONE
                                                        | SOME ty' => SOME (verifyTy env ty') }

                           | Ast.LiteralArray { exprs=Ast.ListExpr exprs, ty } => (* FIXME handle comprehensions *)
                             Ast.LiteralArray { exprs = Ast.ListExpr (map (verifyExprOnly env) exprs),
                                                ty = case ty of
                                                         NONE => NONE
                                                       | SOME ty' => SOME (verifyTy env ty') }
                           | x => x
            in
                return (Ast.LiteralExpr le, resultType)
            end

          | Ast.CallExpr {func, actuals} =>
            let
                val (func', t) = verifySub func
                val (actuals', ts) = verifySubList actuals
                val resultType = case t of
                                      Ast.FunctionType { result, ... } => result
                                    | _ => anyType
            in
                whenStrict (fn () =>
                               case t of
                                    Ast.FunctionType { params, result, thisType, hasRest, minArgs } =>
                                    if (List.length actuals) < minArgs then
                                        error ["too few actuals"]
                                    else if (not hasRest) andalso
                                            ((List.length actuals) > (List.length params)) then
                                        error ["too many actuals"]
                                    else
                                        List.app (fn (formal, actual) => checkCompatible actual formal)
                                                 (ListPair.zip (params, ts))
                                  | Ast.SpecialType Ast.Any => ()
                                  | _ => error ["ill-typed call"]);
                return (Ast.CallExpr { func = func', actuals = actuals' }, resultType)
            end

            (* TODO: what is this? *)
          | Ast.ApplyTypeExpr { expr, actuals } =>
            let
                val (expr', t) = verifySub expr
                val actuals' = List.map (verifyTy env) actuals
            in
                return (Ast.ApplyTypeExpr { expr = expr',
                                            actuals = actuals' }, anyType)
            end

          (* TODO: ---------- left off here ---------- *)

          | Ast.LetExpr { defs, body, head } =>
            let
                val defs' = defs (* TODO *)
                val head' = Option.map (verifyHead env) head
                (* TODO: verify body with `head' rib in env *)
                val (body', t) = verifySub body
            in
                return (Ast.LetExpr { defs = defs',
                                      body = body',
                                      head = head' }, anyType)
            end

          | Ast.NewExpr { obj, actuals } =>
            let
                val (obj', t) = verifySub obj
                val (actuals', ts) = verifySubList actuals
            in
                return (Ast.NewExpr { obj = obj',
                                      actuals = actuals' }, anyType)
            end

          | Ast.ObjectRef { base, ident, loc } =>
            let
                val _ = LogErr.setLoc loc
                val (base', t) = verifySub base
                val _ = LogErr.setLoc loc
                val ident' = ident (* TODO *)
            in
                return (Ast.ObjectRef { base=base', ident=ident', loc=loc }, anyType)
            end

          | Ast.LexicalRef { ident, loc } =>
            let
                val _ = LogErr.setLoc loc
                val ident' = ident (* TODO *)
                val _ = LogErr.setLoc loc
            in
                return (Ast.LexicalRef { ident=ident', loc=loc }, anyType)
            end

          | Ast.SetExpr (a, le, re) =>
            let
                val (le', t1) = verifySub le
                val (re', t2) = verifySub re
            in
                return (Ast.SetExpr (a, le', re'), anyType)
            end

          | Ast.GetTemp n =>
            (* TODO: these only occur on the RHS of compiled destructuring assignments. how to type-check? *)
            return (Ast.GetTemp n, anyType)

          | Ast.GetParam n =>
            LogErr.internalError ["GetParam not eliminated by Defn"]

          | Ast.ListExpr es =>
            let
                val (es', ts) = verifySubList es
            in
                return (Ast.ListExpr es', anyType)
            end

          | Ast.InitExpr (it, head, inits) =>
            let
                val it' = it (* TODO *)
                val head' = verifyHead env head
                val inits' = verifyInits env inits
            in
                return (Ast.InitExpr (it', head', inits'), anyType)
            end

    end


and verifyExprOnly (env:ENV)
                   (expr:Ast.EXPR)
    : Ast.EXPR =
    let
        val (e, _) = verifyExpr env expr
    in
        e
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
                       (expectedType:Ast.TYPE_EXPR)
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
    let 
        fun verifySub s = verifyStmt env s
        val { prog, 
              strict, 
              returnType,
              stdTypes = 
              { AnyNumberType, 
                doubleType,
                decimalType,
                intType,
                uintType,
                byteType,

                AnyStringType,
                stringType,

                AnyBooleanType, 
                booleanType, 

                RegExpType,

                TypeType,
                NamespaceType }, ... } = env
    in
        case stmt of
            Ast.EmptyStmt =>
            Ast.EmptyStmt

          | Ast.ExprStmt e =>
            Ast.ExprStmt (verifyExprOnly env e)

          | Ast.ForInStmt {isEach, defn, obj, rib, next, labels, body} =>
            let
                val obj = verifyExprOnly env obj
                val rib = valOf rib
                val rib = verifyRib env rib
                val env = withRib env rib
                val next = verifyStmt env next
                val body = verifyStmt env body
            in
                Ast.ForInStmt { isEach = isEach,
                                defn = defn,
                                obj = obj,
                                rib = SOME rib,
                                next = next,
                                labels = labels,
                                body = body }
            end

          | Ast.ThrowStmt es =>
            Ast.ThrowStmt (verifyExprOnly env es)

          | Ast.ReturnStmt es =>
            let 
                val (es',ty) = verifyExpr env es
            in
                if strict
                then
                    (* FIXME: this does not work yet. Nothing sets returnType *)
                    (*
	                 case returnType of
	                     NONE => error ["return not allowed here"]
                       | SOME retTy => checkCompatible ty retTy
                     *)
                    ()
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

          | Ast.WhileStmt {cond,body,labels,rib=NONE} =>
            Ast.WhileStmt {cond=verifyExprAndCheck env cond booleanType,
                           body=verifySub body,
                           labels=labels,
                           rib=NONE}

          | Ast.DoWhileStmt {cond,body,labels,rib=NONE} =>
            Ast.DoWhileStmt {cond=verifyExprAndCheck env cond booleanType,
                             body=verifySub body,
                             labels=labels,
                             rib=NONE}

          | Ast.ForStmt  { defn=_, rib, init, cond, update, labels, body } =>
            let 
                val rib' = verifyRibOption env rib
                val env' = withRib env rib'
                val init' = verifyStmts env' init
                val cond' = verifyExprAndCheck env' cond booleanType
                val update' = verifyExprOnly env' update
                val body' = verifyStmt env' body
            in
                Ast.ForStmt  { defn=NONE, rib=SOME rib', init=init', cond=cond',
                               update=update', labels=labels, body=body' }
            end
          | Ast.IfStmt {cnd, els, thn} =>
            Ast.IfStmt {cnd=verifyExprAndCheck env cnd booleanType,
                        els=verifySub els,
                        thn=verifySub thn}

          | Ast.WithStmt {obj, ty, body} => (*TODO*)
            Ast.WithStmt {obj=obj, ty=ty, body=body}

          | Ast.TryStmt {block, catches, finally} =>
            Ast.TryStmt {block=verifyBlock env block,
                         catches=List.map (verifyCatchClause env) catches,
                         finally=Option.map (verifyBlock env) finally }

          | Ast.SwitchStmt {cond, cases, labels} =>
            let
                fun verifyCase { label, inits, body } =
                    { label = Option.map (verifyExprOnly env) label,
                      inits = Option.map (verifyInits env) inits,
                      body = verifyBlock env body }
            in
                Ast.SwitchStmt {cond = verifyExprOnly env cond,
                                cases = map verifyCase cases,
                                labels = labels}
            end

          | Ast.SwitchTypeStmt {cond, ty, cases} =>
            Ast.SwitchTypeStmt {cond = verifyExprOnly env cond,
                                ty = verifyTy env ty,
                                cases = List.map (verifyCatchClause env) cases}

          | Ast.DXNStmt x => (*TODO*)
            Ast.DXNStmt x

          | _ => error ["Shouldn't happen: failed to match in Verify.verifyStmt"]

    end

and verifyCatchClause (env:ENV)
                      ({bindings, ty, rib, inits, block}:Ast.CATCH_CLAUSE)
    : Ast.CATCH_CLAUSE =
    let
        val ty = verifyTy env ty
        val rib' = verifyRibOption env rib
        val inits' = verifyInitsOption env inits
        val env' = withRib env rib'
        val block' = verifyBlock env' block
    in
        {bindings=bindings, ty=ty,
         rib=SOME rib', inits=SOME inits', block=block'}
    end

and verifyStmts (env) (stmts:Ast.STMT list)
    : Ast.STMT list =
    List.map (verifyStmt env) stmts

and verifyBlock (env:ENV)
                (b:Ast.BLOCK)
    : Ast.BLOCK =
    let
    in case b of
        Ast.Block { head, body, loc, pragmas=pragmas, defns=defns } =>
            let
                val _ = LogErr.setLoc loc
                val (Ast.Head (rib, _)) = valOf head
                val env = withRib env rib
                val head = Option.map (verifyHead env) head
                val body = verifyStmts env body
            in
                Ast.Block { pragmas = pragmas,
                            defns = defns,
                            body = body,
                            head = head,
                            loc = loc }
            end
    end

(*
    RIB
*)

and verifyFunc (env:ENV)
               (func:Ast.FUNC)
    : Ast.FUNC =
    let
        val Ast.Func { name, fsig, native, block, param, defaults, ty, loc } = func
        val ty = verifyTy env ty
        val param = verifyHead env param
        val (defaults,_) = verifyExprs env defaults
        val (Ast.Head (paramRib, _)) = param
        val env = withRib env paramRib
        val block = Option.map (verifyBlock env) block
    in
        Ast.Func { name=name, fsig=fsig, native=native, block=block, param=param, defaults=defaults, ty=ty, loc=loc }
    end

and verifyFixture (env:ENV)
		          (f:Ast.FIXTURE)
    : Ast.FIXTURE = (*TODO*)
    let in
        case f of
         Ast.NamespaceFixture ns =>
         Ast.NamespaceFixture ns
       | Ast.ClassFixture (Ast.Cls {name, typeParams, nonnullable, dynamic, extends, implements, classRib, instanceRib,
                                    instanceInits, constructor, classType, instanceType }) =>
         let
             val classRib = verifyRib env classRib
             val env = withRib env classRib
             val instanceRib = verifyRib env instanceRib
             val instanceInits = verifyHead env instanceInits
             val constructor = case constructor of
                                   NONE => NONE
                                 | SOME (Ast.Ctor {settings, superArgs, func}) =>
                                   let
                                       val settings = verifyHead env settings
                                       val (superArgs, _) = verifyExprs env superArgs
                                       val func = verifyFunc env func
                                   in
                                       SOME (Ast.Ctor { settings = settings,
                                                        superArgs = superArgs,
                                                        func = func })
                                   end
         in
             Ast.ClassFixture (Ast.Cls {name=name, typeParams=typeParams, nonnullable=nonnullable, dynamic=dynamic, 
                                        extends=extends, implements=implements, classRib=classRib,
                                        instanceRib=instanceRib, instanceInits=instanceInits,                                        
                                        constructor=constructor, classType=classType, instanceType=instanceType })
         end
       | Ast.TypeVarFixture x =>
         Ast.TypeVarFixture x
       | Ast.TypeFixture ty =>
         Ast.TypeFixture (verifyTy env ty)
       | Ast.ValFixture {ty, readOnly} =>
         Ast.ValFixture {ty=verifyTy env ty, readOnly=readOnly}
       | Ast.MethodFixture { func, ty, readOnly, override, final } =>
         let
             val func = verifyFunc env func
             val ty = verifyTy env ty
         in
             Ast.MethodFixture { func=func, ty=ty, readOnly=readOnly, override=override, final=final }
         end
       | Ast.VirtualValFixture { ty, getter, setter} =>
         let
             val ty = verifyTy env ty
             val getter = Option.map (verifyFunc env) getter
             val setter = Option.map (verifyFunc env) setter
         in
             Ast.VirtualValFixture { ty = ty, getter = getter, setter = setter }
         end

       | _ => f
    (*
       | _ => unimplError ["in verifyFixture"]
     *)
    end

and verifyRib (env:ENV)
              (rib:Ast.RIB)
    : Ast.RIB =
    let
        val env = withRib env rib
        fun doFixture (name, fixture) =
            (trace ["verifying fixture: ", LogErr.fname name];
             (name,verifyFixture env fixture))
    in
        map doFixture rib
    end

and verifyRibOption (env:ENV)
		            (fs:Ast.RIB option)
    : Ast.RIB =
    case fs of
        SOME rib => verifyRib env rib

      | _ => LogErr.internalError ["missing rib"]

(*
    PROGRAM
*)

and verifyFragment (env:ENV)
                   (frag:Ast.FRAGMENT) 
  : Ast.FRAGMENT = 
    case frag of 
        Ast.Unit { name, fragments } => 
        Ast.Unit { name = name,
                   fragments = map (verifyFragment env) fragments }

      | Ast.Package { name, fragments } => 
        Ast.Package { name = name,
                      fragments = map (verifyFragment env) fragments }
        
      | Ast.Anon block => 
        Ast.Anon (verifyBlock env block)

and verifyTopRib (prog:Fixture.PROGRAM)
                 (strict:bool)
                 (rib:Ast.RIB)
    : Ast.RIB =
    let
        val env = newEnv prog strict
    in
        verifyRib env rib
    end

and verifyTopFragment (prog:Fixture.PROGRAM)
                      (strict:bool) 
                      (frag:Ast.FRAGMENT) 
  : Ast.FRAGMENT = 
    let 
        val env = newEnv prog strict
        val res = verifyFragment env frag
    in
        trace ["verification complete"];
        (if !doTrace orelse !doTraceFrag
         then Pretty.ppFragment res
         else ());
        res
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

and verifyIdentExpr (env as {env,this,...}:Ast.RIB)
		    (ide:Ast.IDENT_EXPR)
    : Ast.TYPE_EXPR =
    let
    in
	case ide of
	    Ast.QualifiedIdentifier { qual, ident } =>
	    let in
		checkCompatible (verifyExpr env qual) NamespaceType;
		anyType
	    end
	  | Ast.QualifiedExpression { qual, expr } =>
	    let in
		checkCompatible (verifyExpr env qual) NamespaceType;
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

and verifyExpr (env as {env,this,...}:Ast.RIB)
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
      | LiteralExpr (LiteralBoolean _) => booleanType
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
      | LiteralExpr (LiteralFunction (Func { param=(rib,inits), block, ty, ... }))
	=>
	 let
	     val env1 = verifyFunctionType env ty
	     val extensions = verifyRib env1 rib
	     val env2 = withEnvExtn env1 extensions
	 in
	     checkForDuplicateExtension extensions;
(** FIXME: inits are now settings and are BINDINGS
	     verifyStmts env2 inits;
*)
	     verifyBlock env2 block;
	     Ast.FunctionType ty
         end
      | LexicalRef { ident, loc } =>
	verifyIdentExpr env ident
      | ListExpr l => List.last (List.map (verifyExpr env) l)
      | LetExpr {defs=_, body, head=SOME (rib,inits) } =>  (* FIXME: inits added *)
          let val extensions = verifyRib env rib
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


and verifyCallExpr  (env as {env,this,...}:Ast.RIB)
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
and verifyPattern (env:Ast.RIB) (Ast.IdentifierPattern name) = (
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

(* deprecated due to rib
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
		checkBicompatible booleanType argType;
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
	    let in checkConvertible argType booleanType; booleanType end

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
		booleanType
	    end
    end


and verifyTrinaryExpr (env:RIB) (triop:TRIOP, a:EXPR, b:EXPR, c:EXPR) =
    case triop of
	Cond =>
	let val aty = verifyExpr env a
	    val bty = verifyExpr env b
	    val cty = verifyExpr env c
	in
	    checkConvertible aty booleanType;
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
	checkCompatible (verifyExpr env cnd) booleanType;
	verifyStmt env thn;
	verifyStmt env els
    end

  | (DoWhileStmt {cond,body,labels,rib} | WhileStmt {cond,body,labels,rib}) =>
    let in
	checkCompatible (verifyExpr env cond) booleanType;
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

(* deprecated due to rib
  | LetStmt (defns, body) =>
    let val extensions = List.concat (List.map (fn d => verifyBinding env d) defns)
    in
        checkForDuplicateExtension extensions;
        verifyStmt (withEnv (env, foldl extendEnv env extensions)) body
    end
*)

  | ForStmt { defn=_, rib, init, cond, update, labels, body } =>
    let val extensions = verifyRibOption env rib
        val env' = withEnvExtn env extensions
(* NOT USED
	fun verifyExprs exprs =
	    let in
		if List.length exprs = 0
		then booleanType
		else List.last (List.map (fn e => verifyExpr env' e) exprs)
	    end
*)

    in
  	verifyStmts env' init;
	checkCompatible (verifyExpr env' cond) booleanType;
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
	(fn {bindings, ty, rib, block} =>
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
	    let val Func {name, fsig, rib, inits, body } = func
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

(******************** Rib **************************************************)

(* rib at the block level *)

(******************** Blocks **************************************************)




and verifyBlock (env as {env,...})

                (Block {pragmas,defns=_,body,head,loc}) =
let
     val SOME (rib,inits) = head
        val extensions = verifyRib env rib
        val env' = withEnvExtn env extensions
    in
        verifyStmts env' body
    end

fun verifyProgram (prog as { packages, rib, block }) =
    let
    in
      TextIO.print ("type checking program\n");
      Pretty.ppRib (!Defn.topRib);
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
