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
structure Type = struct

val doTrace = ref false
fun log ss = LogErr.log ("[type] " :: ss) 
fun trace ss = if (!doTrace) then log ss else ()
fun error ss = LogErr.typeError ss

fun logType ty = (Pretty.ppType ty; TextIO.print "\n")
fun traceType ty = if (!doTrace) then logType ty else ()

val undefinedType   = Ast.SpecialType Ast.Undefined
val nullType        = Ast.SpecialType Ast.Null
val anyType         = Ast.SpecialType Ast.Any

fun assert b s = if b then () else (raise Fail s)

fun fmtName n = if !doTrace
                then LogErr.name n  
                else ""

fun fmtMname n = if !doTrace
                 then LogErr.multiname n
                 else ""

fun fmtType t = if !doTrace
                 then LogErr.ty t
                 else ""

fun fmtTy t = if !doTrace
              then LogErr.ty (AstQuery.typeExprOf t)
              else ""


(* -----------------------------------------------------------------------------
 * Normalized types
 * ----------------------------------------------------------------------------- *)

(* Unused
type TYPE_VALUE = Ast.TYPE_EXPR  (* Invariant: normalized *)
*)

fun findNamespace (prog:Fixture.PROGRAM)
                  (ribId:Ast.RIB_ID option)
                  (expr:Ast.EXPR)
    : Ast.NAMESPACE option =
    let
        fun withMname (mname:Ast.MULTINAME) 
            : Ast.NAMESPACE option = 
            case Fixture.resolveToFixture prog mname ribId of
                NONE => NONE
              | SOME (n, Ast.NamespaceFixture ns) => SOME ns
              | _ => error ["namespace expression resolved ",
                            "to non-namespace fixture"]
    in    
        case expr of
            Ast.LiteralExpr (Ast.LiteralNamespace ns) => SOME ns

          | Ast.LexicalRef {ident = Ast.Identifier {ident, openNamespaces}, loc } =>
            (LogErr.setLoc loc;
             withMname {id = ident, nss = openNamespaces})

          | Ast.LexicalRef {ident = Ast.QualifiedIdentifier {qual, ident}, loc } =>
            (LogErr.setLoc loc;
             case findNamespace prog ribId qual of
                 NONE => NONE
               | SOME ns => withMname {id = ident, nss = [[ns]]})

          | Ast.LexicalRef _ => error ["dynamic name in namespace"]
                                
          | _ => error ["dynamic expr in namespace"]
    end

fun liftOption (f:'a -> 'a option) 
               (ls:'a list) 
    : (('a list) option) = 
    let
        val (res:(('a option) list)) = map f ls
    in
        if List.all Option.isSome res
        then SOME (map Option.valOf res)
        else NONE
    end

(* 
 * While normalizing a TYPE_EXPR, we are doing two things: basic structural 
 * simplification (flattening unions, collapsing / shifting nulls, dereferencing
 * array elements, etc); and looking up names / evaluating closures as we hit
 * them. The latter is subject to some restrictions:
 * 
 *   - No cyclic terms are allowed
 *   - No partial applications are allowed
 *   - No passing of closures, only "ground" terms.
 *
 * The key points are:
 *
 *   - Normalization always terminates (all recursion is an error)
 *   - Normalization is a partial evaluator that respects missing information: 
 *     if a term is not known, it is returned "as-is". 
 *
 * A type *should* evaluate to a "ground" term if its unit is closed. We leave
 * that fact up to the various callers of normalization, though.
 *
 * A "ground type" is one that is closed, but, for example,
 *  function.<X>(X):X  is still considered a ground type.
 *)

fun isGroundTypeEnv (e:Ast.IDENT list) (t:Ast.TYPE_EXPR) 
    : bool = 
    let
        fun isGroundType t = isGroundTypeEnv e t
        fun isGroundField { name, ty } = isGroundType ty
        fun isGroundOption NONE = true
          | isGroundOption (SOME t) = isGroundType t
    in
        case t of 
            Ast.SpecialType _ => true
          | Ast.InstanceType it => 
            (length (#typeParams it) = length (#typeArgs it))
          | Ast.TypeName (Ast.Identifier {ident, openNamespaces}) => 
            if     List.exists (fn i => i = ident) e (* ground if ident in environment *)                
            then (trace ["bound var in ground type"]; true)
            else (trace ["free var in ground type"]; false)
          | Ast.TypeName _ => false
          | Ast.ElementTypeRef _ => false
          | Ast.FieldTypeRef _ => false
          | Ast.AppType _ => false
          | Ast.LamType {params, body} => isGroundTypeEnv (params @ e) body
          | Ast.NullableType { expr, ... } => isGroundType expr
          | Ast.ObjectType fields => List.all isGroundField fields
          | Ast.LikeType t => isGroundTypeEnv e t
          | Ast.UnionType tys => List.all isGroundType tys
          | Ast.ArrayType tys =>List.all isGroundType tys
              | Ast.FunctionType { params, result, thisType, ... } => 
                List.all isGroundType params andalso
                isGroundType result andalso
                isGroundOption thisType
    end

fun isGroundType  (t:Ast.TYPE_EXPR) 
    : bool = 
    isGroundTypeEnv [] t

fun isGroundTy (ty:Ast.TY) 
    : bool =
    isGroundType (AstQuery.typeExprOf ty)

fun groundExpr (ty:Ast.TY) 
    : Ast.TYPE_EXPR = 
    if isGroundTy ty
    then AstQuery.typeExprOf ty
    else (Pretty.ppType (AstQuery.typeExprOf ty);
          error ["extracting ground type expr from non-ground ty"])
             
(* 
 * During normalization we work with a "slighly unpacked" form of type
 * closures: type TY_NORM
 *
 * It's similar to Ast.TY -- can map back and forth with Ast.TY -- but
 * it explicitly models the flattening of unions and the 
 * collapse-and-shift-to-end-of-union implied with nullability.
 *)

type TY_NORM = { exprs: Ast.TYPE_EXPR list,
                 nullable: bool,
                 ribId: Ast.RIB_ID option }

fun normalize (prog:Fixture.PROGRAM) 
              (locals:Ast.RIBS)
              (t:Ast.TY) 
    : Ast.TY =
    let
        val _ = trace [">>> normalize: ", fmtTy t]
        val res = (norm2ty (ty2norm prog t locals []))
        val _ = trace ["<<< normalize: ", fmtTy t, " ---> ", fmtTy res]
    in
        res
    end

and repackage (t:Ast.TY) 
    : TY_NORM = 
    let
        val Ast.Ty { expr, ribId } = t
    in
        { exprs = [expr],
          nullable = false,
          ribId = ribId }
    end


and fix2norm (prog:Fixture.PROGRAM)
             (originalt:Ast.TY)
             (mname:Ast.MULTINAME)
             (fixOpt:(Ast.NAME * Ast.FIXTURE) option)
             (seen:((Ast.RIB_ID option) * Ast.MULTINAME) list)
    : TY_NORM = 
    let
        val _ = trace ["examined type multiname ", fmtMname mname]
        val Ast.Ty { expr, ribId } = originalt
        fun subIty ity = 
            let
                val subnorm = ty2norm prog ity [] seen
                val nullable = case (#exprs subnorm) of 
                                   [Ast.InstanceType { nonnullable, ... }] => not nonnullable
                                 | [Ast.LamType { body = Ast.InstanceType { nonnullable, ... }, ... }] => not nonnullable
                                 | _ => error ["expected instance type for multiname ", 
                                               LogErr.multiname mname]
            in
                { exprs = (#exprs subnorm), nullable = nullable, ribId = (#ribId subnorm) }
            end
    in
        case fixOpt of 
            NONE => (trace ["failed to resolve"];
                     if Fixture.ribIsClosed prog ribId
                     then error ["type multiname ", LogErr.multiname mname, 
                                 " failed to resolve in closed unit "]
                     else repackage originalt)
          | SOME (n, fix) => 
            (trace ["resolved"];
             case fix of
                 Ast.TypeFixture ty => ty2norm prog ty [] seen
               | Ast.ClassFixture (Ast.Cls cls) => subIty (#instanceType cls)
               | Ast.InterfaceFixture (Ast.Iface iface) => subIty (#instanceType iface)
               | Ast.TypeVarFixture _ => repackage originalt
               | _ => error ["expected type fixture for: ", LogErr.multiname mname])
    end

    
and maybeNamedSlow (prog:Fixture.PROGRAM)
                   (originalt:Ast.TY) 
                   (locals:Ast.RIBS)
                   (mname:Ast.MULTINAME) 
                   (seen:((Ast.RIB_ID option) * Ast.MULTINAME) list)
    : TY_NORM =
    let
        val (ribs, closed) = Fixture.getRibsForTy prog originalt 
        val fullRibs = locals @ ribs
    in       
        case Multiname.resolveInRibs mname fullRibs of 
            NONE => fix2norm prog originalt mname NONE seen
          | SOME (ribs, n) =>
            let 
                val fix = Fixture.getFixture (List.hd ribs) (Ast.PropName n)
            in
                fix2norm prog originalt mname (SOME (n, fix)) seen
            end
    end


and maybeNamed (prog:Fixture.PROGRAM)
               (originalt:Ast.TY) 
               (locals:Ast.RIBS)
               (mname:Ast.MULTINAME) 
               (seen:((Ast.RIB_ID option) * Ast.MULTINAME) list)
    : TY_NORM =
    let
        val Ast.Ty { ribId, expr } = originalt
        val newSeen = if List.exists (fn x => x = (ribId, mname)) seen
                      then (error ["Cyclical type term detected in ", LogErr.ty expr]; [])
                      else (ribId, mname) :: seen
    in
        case locals of 
            x::xs => maybeNamedSlow prog originalt locals mname newSeen
          | [] => 
            let
                val Ast.Ty { ribId, expr } = originalt
                val fixOpt = Fixture.resolveToFixture prog mname ribId 
                val norm = fix2norm prog originalt mname fixOpt newSeen
                val ty = norm2ty norm
            in
                (* 
                 * Optionally: if the name resolved to a *type abbreviation*,
                 * write back through to the fixture cache to save the 
                 * normalized form. This will cause future normalization
                 * of the same type name to accelerate.
                 *)
                case fixOpt of
                    SOME (n, Ast.TypeFixture _) =>
                    if (isGroundTy ty)                   
                    then Fixture.updateFixtureCache prog ribId mname n (Ast.TypeFixture ty)
                    else ()
                  | _ => ();
                norm
            end
    end

and norm2ty (norm:TY_NORM) 
    : Ast.TY = 
    let
        val { exprs, nullable, ribId } = norm 
        val _ = trace ["norm2ty on type with nullable=", Bool.toString nullable]
        val null = Ast.SpecialType Ast.Null
        val expr = case exprs of 
                       [] => if nullable
                             then null
                             else Ast.UnionType []
                     | [x] => if nullable
                              then Ast.UnionType [x, null]
                              else x
                     | union => if nullable
                                then Ast.UnionType (union @ [null])
                                else Ast.UnionType union
    in
        Ast.Ty { expr = expr,
                 ribId = ribId }
    end

and ty2norm (prog:Fixture.PROGRAM) 
            (ty:Ast.TY) 
            (locals:Ast.RIBS) (* Local bindings that extend the frame referenced in ty. *)
            (seen:((Ast.RIB_ID option) * Ast.MULTINAME) list)
    : TY_NORM =
    let
        val Ast.Ty { ribId, expr } = ty

        (* Package up a 1-term type in the current TY environment. *)
        fun simple (e:Ast.TYPE_EXPR)
                   (nullable:bool)
            : TY_NORM = 
            if isGroundType e 
            then { exprs = [e], nullable = nullable, ribId = ribId }
            else error ["internal error: simple type is not ground: ", LogErr.ty e]
                 

        (* Use 'subTerm2Norm' to evaluate a TYPE_EXPR in the same environment
         * as the current TY, and get back a new TY_NORM. *)
        fun subTerm2Norm (e:Ast.TYPE_EXPR) 
            : TY_NORM = 
            ty2norm prog (Ast.Ty { expr = e, ribId = ribId }) locals seen

        (* 
         * Use 'subTerm' to evaluate a TYPE_EXPR in the same environment
         * as the current TY, and get back "SOME TYPE_EXPR", if the subterm
         * normalized to a ground term. NONE if there's a non-ground term. 
         *)
        fun subTerm (e:Ast.TYPE_EXPR) 
            : Ast.TYPE_EXPR option = 
            let 
                val t = norm2ty (subTerm2Norm e)
            in
                if isGroundType (AstQuery.typeExprOf t)
                then SOME (AstQuery.typeExprOf t)
                else NONE
            end

        (* Same as above, but fails if any member of its list fails. *)
        val subTerms = liftOption subTerm 

        (* 
         * Likewise, but with options. The confusing part, alas, is that
         * the outer option means "evaluation did/did-not succeed" and the
         * inner option is structural. 
         *)
        fun subTermOption (e:Ast.TYPE_EXPR option)
            : (Ast.TYPE_EXPR option) option =             
            case e of 
                NONE => SOME (NONE)
              | SOME te => 
                let
                    val t = norm2ty (subTerm2Norm te)
                in
                    if isGroundType (AstQuery.typeExprOf t)
                    then SOME (SOME (AstQuery.typeExprOf t))
                    else NONE
                end

        val { exprs, ribId, nullable } = 
            case expr of              
                Ast.TypeName (Ast.Identifier { ident, openNamespaces }) => 
                maybeNamed prog ty locals { id = ident, nss = openNamespaces } seen
                
              | Ast.TypeName (Ast.QualifiedIdentifier { qual, ident }) =>
                (case findNamespace prog ribId qual of
                     SOME ns => maybeNamed prog ty locals { nss = [[ns]], id = ident } seen
                   | NONE => repackage ty)
                
              | Ast.TypeName Ast.WildcardIdentifier => 
                subTerm2Norm (Ast.SpecialType Ast.Any)

              | Ast.TypeName _ =>
                error ["dynamic name in type expression: ", LogErr.ty expr]
                
              | Ast.InstanceType it => 
                (* 
                 * We want to bind any typeargs at this point, if we can resolve them.
                 * 
                 * If we do not, we will not be able to compare parametric instance types.
                 *)
                let
                    val { typeParams, typeArgs, ... } = it
                    fun withArgs args = Ast.InstanceType { name = (#name it),
                                                           typeParams = typeParams,
                                                           typeArgs = args,
                                                           nonnullable = (#nonnullable it),
                                                           superTypes = (#superTypes it),
                                                           ty = (#ty it),
                                                           dynamic = (#dynamic it) }                                                       
                in
                    if (length typeParams) = (length typeArgs)
                    then simple expr false
                    else 
                        case (subTerms (map (fn id => Name.typename (Name.nons id)) typeParams)) of 
                            NONE => simple expr false
                          | SOME args => simple (withArgs args) false
                end

              (* 
               * NB: We used to go up through the supertypes and into all the subterms
               * normalizing as we went. This was both very painful at runtime *and* 
               * at least *somewhat* pointless, since the definer ensures all these
               * types are ground  before it *creates* an InstanceType in the first place.
               * 
               * Keep that in mind! You can't just throw together an InstanceType with 
               * non-ground terms and expect "some later stage" of the system to catch it.
               * 
               * It won't. 
               * 
               * Possibly in the future we'll consider making ground terms a special type of
               * type expressions (though it will require some interconversions), such that 
               * this is semantically impossible. For now it's just something you will have 
               * to ensure manually.
               * 
               * Old code follows:
               *)

              (* 
               let
                   val t0 = ty
                   val { name, typeArgs, superTypes, ty, 
                         nonnullable, dynamic } = it
                   val typeArgs' = subTerms typeArgs
                   val superTypes' = subTerms superTypes
                   val ty' = subTerm ty
               in
                   case (typeArgs', superTypes', ty') of
                       (SOME ta, SOME st, SOME t) => 
                       simple (Ast.InstanceType 
                                   { name = name,
                                     typeArgs = ta,
                                     superTypes = st,
                                     ty = t,
                                     nonnullable = nonnullable,
                                     dynamic = dynamic }) (not nonnullable)
                     | _ => repackage t0
               end
               *)
                
              | Ast.AppType { base, args } => 
                let
                    val baseNorm = subTerm2Norm base
                    val baseTy = norm2ty baseNorm
                    val (args':Ast.TY list) = map (norm2ty o subTerm2Norm) args
                in                
                    case baseNorm of 
                        { exprs=[Ast.LamType {params, body}], nullable, ribId } => 
                        let
                            val _ = if length params = length args'
                                    then ()
                                    else error ["applying wrong number of type arguments"]
                        in
                            (* 
                             * FIXME: we may wish to have non-ground terms -- say, type variable fixtures -- 
                             * propagate through lambdas. For now we do not: we simply suspend normalization
                             * any time we lack ground terms. 
                             *
                             * Probably the thing to do here is to consider a separate concept -- "closed" --
                             * which differs from ground in that a typename in an env where the typename resolves
                             * to a typeVarFixture is closed, but not ground. In those cases, after all, we can 
                             * propagate the binding through transparent ty lams. We just can't instantiate the
                             * resulting type.
                             *
                             * Maybe. Revisit.
                             *)
                            if not (List.all isGroundTy args')
                            then repackage ty
                            else 
                                let
                                    val names = List.map Name.nons params
                                    val bindings = ListPair.zip (names, args')
                                    fun bind ((n,t),rib) = (Ast.PropName n, Ast.TypeFixture t)::rib
                                    val rib = List.foldl bind [] bindings
                                    val appNorm = ty2norm prog (Ast.Ty { expr=body, ribId=ribId }) (rib::locals) seen
                                in
                                    { exprs = (#exprs appNorm), ribId = (#ribId appNorm), 
                                      nullable = nullable orelse (#nullable appNorm) }
                                end               
                        end 
                      | _ => repackage ty
                end
                
              | (Ast.UnionType tys) => 
                let 
                    fun extract {exprs, nullable, ribId} = (exprs, nullable) 
                    val sub = extract o subTerm2Norm
                    val (listOfLists, listOfNulls) = ListPair.unzip (map sub tys)
                    val exprs' = List.concat listOfLists
                    val nullable = List.exists (fn x => x) listOfNulls
                in
                    {exprs=exprs', nullable=nullable, ribId=ribId }
                end
                
              | (Ast.SpecialType Ast.Null) => 
                {exprs=[], nullable=true, ribId=ribId }
                
              | (Ast.ArrayType []) => 
                simple (Ast.ArrayType [Ast.SpecialType Ast.Any]) false
                
              | (Ast.ArrayType tys) => 
                (case subTerms tys of
                     NONE => repackage ty
                   | SOME tys' => simple (Ast.ArrayType tys') false)
                
              | (Ast.FunctionType { params, result, 
                                    thisType, hasRest, minArgs }) =>
                let
                    val params' = subTerms params
                    val result' = subTerm result
                    val this' = subTermOption thisType
                in
                    case (params', result', this') of 
                        (SOME p, SOME r, SOME t) => 
                        simple (Ast.FunctionType { params = p,
                                                   result = r,
                                                   thisType = t,
                                                   hasRest = hasRest,
                                                   minArgs = minArgs }) false
                      | _ => repackage ty
                end
                
              | Ast.ObjectType fields => 
                let
                    fun unpack { name, ty } = (name, ty)
                    fun pack (name, ty) = { name=name, ty=ty }
                    val (names, tys) = ListPair.unzip (map unpack fields)
                in
                    case subTerms tys of
                        NONE => repackage ty
                      | SOME tys'=> 
                        simple 
                            (Ast.ObjectType (map pack (ListPair.zip (names, tys')))) 
                            false
                end

              | Ast.LikeType t => 
                let
                    val { exprs, ribId, nullable } = subTerm2Norm t
                in
                    { exprs = map (fn x => Ast.LikeType x) exprs,
                      ribId = ribId,
                      nullable = nullable }
                end
                
                
              | Ast.NullableType { expr, nullable } => 
                let
                    val _ = trace [">>> normalizing nullableType with nullable=", Bool.toString nullable]
                    val {exprs, ribId=ribId', ...} = subTerm2Norm expr
                    val _ = trace ["<<< normalizing nullableType with nullable=", Bool.toString nullable]
                in                
                    if not (List.all isGroundType exprs)
                    then repackage ty
                    else { exprs=exprs, ribId=ribId', nullable=nullable }
                end


              | Ast.ElementTypeRef (t, idx) =>                 
                let
                    val baseNorm = subTerm2Norm t
                in
                    case baseNorm of 
                        { exprs=[Ast.ArrayType fields], nullable, ribId } => 
                        let
                            val t = if idx < length fields 
                                    then List.nth (fields, idx)
                                    else 
                                        if length fields > 0
                                        then List.last fields
                                        else Ast.SpecialType Ast.Any
                        in
                            subTerm2Norm t
                        end
                      | { exprs=[Ast.SpecialType Ast.Any], nullable, ribId } => 
                          baseNorm
                      | _ => error ["ElementTypeRef on non-ArrayType: ", LogErr.ty t]
                end
                
                
              | Ast.FieldTypeRef (t, ident) => 
                let
                    val baseNorm = subTerm2Norm t
                in
                    case baseNorm of 
                        { exprs=[Ast.ObjectType fields], nullable, ribId } =>
                        let                            
                            fun f [] = error ["FieldTypeRef on unknown field: ", Ustring.toAscii ident]
                              | f ({name,ty}::fields) = if name = ident 
                                                        then ty
                                                        else f fields
                            val t = f fields
                        in
                            subTerm2Norm t
                        end
                      | { exprs = [Ast.SpecialType Ast.Any], nullable, ribId } => 
                        baseNorm
                      | _ => error ["FieldTypeRef on non-ObjectType: ", LogErr.ty t]
                end
(*
              | Ast.LamType { params, body } =>
                (* FIXME: add params to env, may shadow other vars *)
                let in
                    case subTerm body of
                        SOME body' =>
                        { exprs = [ Ast.LamType { params=params, body=body' } ],
                          nullable = false,
                          ribId = ribId }

                      | _ => repackage ty 
                end
  *)              
              | _ => repackage ty

        fun nonNull (Ast.SpecialType Ast.Null) = false
          | nonNull _ = true

        val exprs = List.filter nonNull exprs
    in
        { exprs = exprs, ribId = ribId, nullable = nullable }        
    end


fun isNamedField (name:Ast.IDENT) (field:Ast.FIELD_TYPE) = 
    Ustring.stringEquals name (#name field) 
    
fun extractFieldType (name:Ast.IDENT) 
                     (fields:Ast.FIELD_TYPE list)
    : (Ast.TYPE_EXPR * Ast.FIELD_TYPE list) option = 
    case List.partition (isNamedField name) fields of
        ([field], rest) => SOME ((#ty field), rest)
      | _ => NONE

fun arrayPairWise predicate xs ys = 
    ((length xs) = (length ys)) andalso
    ListPair.all (fn (t1,t2) => predicate t1 t2) (xs, ys)
    
fun fieldPairWise predicate [] [] = true
  | fieldPairWise predicate ({ name, ty }::ts1) ts2 =
    (case extractFieldType name ts2 of
         NONE => false
       | SOME (ty2, rest) => predicate ty ty2 andalso fieldPairWise predicate ts1 rest)                             
  | fieldPairWise _ _ _ = false

fun fieldPairWiseSuperset predicate _ [] = true
  | fieldPairWiseSuperset predicate ts1 ({ name, ty }::ts2) =
    (case extractFieldType name ts1 of
         NONE => false
       | SOME (ty1, rest) => predicate ty1 ty andalso fieldPairWise predicate rest ts2)

fun optionWise predicate (SOME a) (SOME b) = predicate a b
  | optionWise _ NONE NONE = true
  | optionWise _ _ _ = false

             
(* -----------------------------------------------------------------------------
 * Equality
 * ----------------------------------------------------------------------------- *)

fun normalizingPredicate groundPredicate 
                         nonNormalizableDefault
                         (prog:Fixture.PROGRAM)
                         (locals:Ast.RIBS)
                         (t1:Ast.TY)
                         (t2:Ast.TY)
  =
  let
      val norm1 = normalize prog locals t1
      val norm2 = normalize prog locals t2
  in
      if isGroundTy norm1 andalso
         isGroundTy norm2 
      then 
          groundPredicate
              (AstQuery.typeExprOf norm1) 
              (AstQuery.typeExprOf norm2)
      else
          nonNormalizableDefault
    end

(* -----------------------------------------------------------------------------
 * Generic matching algorithm
 * ----------------------------------------------------------------------------- *)

datatype BICOMPAT = Bicompat | Compat
datatype VARIANCE = Covariant | Invariant

fun groundMatchesGeneric (b:BICOMPAT)
                  (v:VARIANCE)
                  (ty1:Ast.TYPE_EXPR)
                  (ty2:Ast.TYPE_EXPR)

    : bool = 
    if b=Bicompat andalso
       (case findSpecialConversion ty1 ty2 of 
              SOME _ => true
            | NONE => false)
    then
        let in
            trace ["findSpecialConversion ", LogErr.ty ty1, " vs. ", LogErr.ty ty2];
            true
        end
    else
    case (b, v, ty1, ty2) of 

      (* A-LIKE-COV *)
        (_, _, Ast.LikeType lt1, Ast.LikeType lt2) => 
        groundMatchesGeneric b v lt2 lt2

      (* A-LIKE *)
      | (_, Covariant, _, Ast.LikeType lt2) => 
        groundMatchesGeneric b v ty1 lt2

      (* A-GENERIC *)
      (* FIXME: need to alpha-rename so have consistent parameters *)
      | (_, _, Ast.LamType lt1, Ast.LamType lt2) => 
        groundMatchesGeneric b v (#body lt1) (#body lt2)

      (* A-OBJ *)
      | (_, _, Ast.ObjectType fields1, Ast.ObjectType fields2) => 
        fieldPairWiseSuperset (groundMatchesGeneric b Invariant) fields1 fields2
        
      (* A-ARROW *)
      | (_, _, 
         Ast.FunctionType
		     {params  =params1,
		      result  =result1,
		      thisType=thisType1,
		      hasRest =hasRest1,
	          minArgs=minArgs1},
	     Ast.FunctionType
		     {params=params2,
		      result=result2,
		      thisType=thisType2,
		      hasRest=hasRest2,
		      minArgs=minArgs2}) => 
        (arrayPairWise (groundMatchesGeneric b Invariant) params1 params2) andalso
        groundMatchesGeneric b v result1 result2 andalso
        (optionWise (groundMatchesGeneric b Invariant) thisType1 thisType2) andalso
        hasRest1 = hasRest2 andalso
        minArgs1 = minArgs2

      (* A-DYN1 *)
      | (_, _, _, Ast.SpecialType Ast.Any) => true

      (* A-DYN2 *)
      | (Bicompat, _, Ast.SpecialType Ast.Any, _) => true

      (* A-INSTANCE -- generalized from A-INT *)
      | (_, _, Ast.InstanceType it1, Ast.InstanceType it2) =>
        (Mach.nameEq (#name it1) (#name it2) andalso
         (arrayPairWise (groundMatchesGeneric b Invariant) (#typeArgs it1) (#typeArgs it2)))
        orelse 
        (List.exists (fn sup => groundMatchesGeneric b v sup ty2) 
                     (#superTypes it1))
        
      (* Extra rules covering nullable, array and union types. *)

      | (_, _, Ast.SpecialType x, Ast.SpecialType y) => 
        x = y
        
      | (_, _, Ast.NullableType nt1, Ast.NullableType nt2) =>
        (#nullable nt1) = (#nullable nt2) andalso
        groundMatchesGeneric b v (#expr nt1) (#expr nt2)
        
      | (_, _, Ast.ArrayType tys1, Ast.ArrayType tys2) => 
        arrayPairWise (groundMatchesGeneric b Invariant) tys1 tys2
        
      | (_, _, Ast.UnionType tys1, _) => 
        List.all (fn t => groundMatchesGeneric b v t ty2) tys1

      | (_, _, _, Ast.UnionType tys2) => 
        List.exists (groundMatchesGeneric b v ty1) tys2

      (* A-STRUCTURAL -- knit the structural types on the end of the nominal lattice. *)

      | (_, _, Ast.ArrayType _, Ast.InstanceType { name, ... }) => 
	List.exists (Mach.nameEq name) [ Name.nons_Array,
					 Name.nons_Object ]

      | (_, _, Ast.ObjectType _, Ast.InstanceType { name, ... }) => 
	List.exists (Mach.nameEq name) [ Name.nons_Object ]

      | (_, _, Ast.FunctionType _, Ast.InstanceType { name, ... }) => 
	List.exists (Mach.nameEq name) [ Name.nons_Function, 
					 Name.nons_Object ]

    | _ => false

    

and findSpecialConversion (tyExpr1:Ast.TYPE_EXPR)
                          (tyExpr2:Ast.TYPE_EXPR) 
    : Ast.TYPE_EXPR option = 
    let
        fun extract (Ast.UnionType [Ast.InstanceType t, Ast.SpecialType Ast.Null]) = SOME t
          | extract (Ast.UnionType [Ast.InstanceType t]) = SOME t
          | extract (Ast.InstanceType t) = SOME t
          | extract _ = NONE
        val srcInstance = extract tyExpr1
        val dstInstance = extract tyExpr2
        fun isNumericType n = 
            List.exists (Mach.nameEq n) [ Name.ES4_double, 
                                          Name.ES4_decimal,
                                          Name.nons_Number ]
        fun isStringType n = 
            List.exists (Mach.nameEq n) [ Name.ES4_string,
                                          Name.nons_String ]

        fun isBooleanType n = 
            List.exists (Mach.nameEq n) [ Name.ES4_boolean,
                                          Name.nons_Boolean ]
    in
        case (srcInstance, dstInstance) of
            ((SOME src), (SOME dst)) => 
            let
                val {name=srcName, ...} = src
                val {name=dstName, ...} = dst
            in
                if 
                    (isBooleanType dstName)
                    orelse
                    (isNumericType srcName andalso isNumericType dstName)
                    orelse
                    (isStringType srcName andalso isStringType dstName)
                then SOME (Ast.InstanceType dst)
                else NONE
            end

          | (_, (SOME dst)) => 
            let
                val {name=dstName, ...} = dst
            in
                if 
                    (isBooleanType dstName)
                then SOME (Ast.InstanceType dst)
                else NONE
            end

          | _ => NONE
    end

(* -----------------------------------------------------------------------------
 * Compatible-subtyping:  <*
 * ----------------------------------------------------------------------------- *)

val groundIsCompatibleSubtype = groundMatchesGeneric Compat Covariant
val isCompatibleSubtype = normalizingPredicate groundIsCompatibleSubtype false

(* -----------------------------------------------------------------------------
 * Matching: ~<
 * ----------------------------------------------------------------------------- *)

val groundMatches = groundMatchesGeneric Bicompat Covariant
val matches = normalizingPredicate groundMatches false



(* 
 * Small helper for finding instance types by name.
 *)

fun instanceTy (prog:Fixture.PROGRAM)
               (n:Ast.NAME)
    : Ast.TY =
    case Fixture.resolveToFixture prog { nss = [[(#ns n)]], id=(#id n) } NONE of
        SOME (_, Ast.ClassFixture (Ast.Cls cls)) => (#instanceType cls)
      | SOME (_, Ast.InterfaceFixture (Ast.Iface iface)) => (#instanceType iface)
      | _ => error [LogErr.name n, " does not resolve to an instance type"]

fun groundType (prog:Fixture.PROGRAM)
               (ty:Ast.TY) 
    : Ast.TYPE_EXPR = 
    let
        val norm = normalize prog [] ty
        val expr = AstQuery.typeExprOf norm
    in
        if isGroundTy norm
        then expr
        else error ["Unable to ground type closure for ", 
                    LogErr.ty expr]
    end    

fun makeTy (tyExpr:Ast.TYPE_EXPR) 
    : Ast.TY =
    Ast.Ty { expr = tyExpr,
             ribId = NONE }

fun getNamedGroundType (prog:Fixture.PROGRAM)
                       (name:Ast.NAME)
   : Ast.TYPE_EXPR = 
    groundType prog (makeTy (Name.typename name))
            

end
