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
 *)

fun isGroundType (t:Ast.TYPE_EXPR) 
    : bool = 
    let
        fun isGroundField { name, ty } = isGroundType ty
        fun isGroundOption NONE = true
          | isGroundOption (SOME t) = isGroundType t
    in
        case t of 
            Ast.SpecialType _ => true
          | Ast.InstanceType it => 
            (length (#typeParams it) = length (#typeArgs it))

          | Ast.TypeName _ => false
          | Ast.ElementTypeRef _ => false
          | Ast.FieldTypeRef _ => false
          | Ast.AppType _ => false
          | Ast.LamType _ => false
                             
          | Ast.NullableType { expr, ... } => isGroundType expr
          | Ast.ObjectType fields => List.all isGroundField fields
          | Ast.UnionType tys => List.all isGroundType tys
          | Ast.ArrayType tys =>List.all isGroundType tys
          | Ast.FunctionType { params, result, thisType, ... } => 
            List.all isGroundType params andalso
            isGroundType result andalso
            isGroundOption thisType
    end

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
    : Ast.TY = (norm2ty (ty2norm prog t locals []))

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
               | Ast.ClassFixture (Ast.Cls cls) => ty2norm prog (#instanceType cls) [] seen
               | Ast.InterfaceFixture (Ast.Iface iface) => ty2norm prog (#instanceType iface) [] seen
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
            else error ["internal error: non-ground term is not simple"]
                 

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
    in
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
                then simple expr (not (#nonnullable it))
                else 
                    case (subTerms (map (fn id => Name.typename (Name.nons id)) typeParams)) of 
                        NONE => simple expr (not (#nonnullable it))
                      | SOME args => simple (withArgs args) (not (#nonnullable it))
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
                val sub = norm2ty o subTerm2Norm
                val (base':Ast.TY) = sub base
                val (args':Ast.TY list) = map sub args
            in
                case base' of 
                    Ast.Ty {expr=Ast.LamType {params, body}, ribId=ribId' } => 
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
                            in
                                ty2norm prog (Ast.Ty { expr=body, ribId=ribId' }) (rib::locals) seen
                            end               
                    end 
                  | Ast.Ty {expr=Ast.TypeName _, ...} => repackage ty
                  | _ => error ["applying bad type: ", LogErr.ty (AstQuery.typeExprOf base')]
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
            
          | Ast.NullableType { expr, nullable } => 
            let
                val {exprs, ribId=ribId', ...} = subTerm2Norm expr
            in
                { exprs=exprs, ribId=ribId', nullable=nullable }
            end
            
          | Ast.ElementTypeRef ((Ast.ArrayType fields), idx) => 
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
            
          | Ast.ElementTypeRef (t, _) => 
            error ["ElementTypeRef on non-ArrayType: ", LogErr.ty t]
            
          | Ast.FieldTypeRef ((Ast.ObjectType fields), ident) => 
            let
                fun f [] = error ["FieldTypeRef on unknown field: ", Ustring.toAscii ident]
                  | f ({name,ty}::fields) = if name = ident 
                                            then ty
                                            else f fields
                val t = f fields
            in
                subTerm2Norm t
            end
            
          | Ast.FieldTypeRef (Ast.SpecialType Ast.Any, _) =>
            simple (Ast.SpecialType Ast.Any) false
            
          | Ast.FieldTypeRef (t, _) => 
            error ["FieldTypeRef on non-ObjectType: ", LogErr.ty t]
            
          | _ => repackage ty
    end

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
    

fun groundEquals (t1:Ast.TYPE_EXPR)
                 (t2:Ast.TYPE_EXPR)
    : bool =
    let
        val _ = trace [">>> groundEquals: ", fmtType t1, " == ", fmtType t2 ];

        val _ = if not (isGroundType t1 andalso isGroundType t2)
                then error ["groundEquals on non-ground terms"]
                else ()

                     
        fun allEqual [] [] = true
          | allEqual (x::xs) (y::ys) = groundEquals x y 
                                       andalso  allEqual xs ys
          | allEqual _ _ = false


        fun isNamedField (name:Ast.IDENT) (field:Ast.FIELD_TYPE) = 
            Ustring.stringEquals name (#name field) 
            
        fun extractFieldType (name:Ast.IDENT) 
                             (fields:Ast.FIELD_TYPE list)
            : (Ast.TYPE_EXPR * Ast.FIELD_TYPE list) option = 
            case List.partition (isNamedField name) fields of
                ([field], rest) => SOME ((#ty field), rest)
              | _ => NONE

        fun fieldTypesEqual [] [] = true
          | fieldTypesEqual ({ name, ty }::ts1) ts2 =
            (case extractFieldType name ts2 of
                 NONE => false
               | SOME (ty2, rest) => groundEquals ty ty2
                                     andalso fieldTypesEqual ts1 rest)
          | fieldTypesEqual _ _ = false
                                      
        fun optionEqual NONE NONE = true
          | optionEqual (SOME t1) (SOME t2) = groundEquals t1 t2
          | optionEqual _ _ = false

        val res = 
            case (t1, t2) of
                (Ast.SpecialType st1, Ast.SpecialType st2) => st1 = st2

              | (Ast.UnionType ts1, Ast.UnionType ts2) => allEqual ts1 ts2
                                                          
              | (Ast.UnionType [t1], _) => groundEquals t1 t2
              | (_, Ast.UnionType [t2]) => groundEquals t1 t2

              | (Ast.ArrayType ts1, Ast.ArrayType ts2)
                => allEqual ts1 ts2

              | (Ast.FunctionType { params=params1,
                                    result=result1,
                                    thisType=thisType1,
                                    hasRest=hasRest1,
                                    minArgs=minArgs1, ... },
                 Ast.FunctionType { params=params2,
                                    result=result2,
                                    thisType=thisType2,
                                    hasRest=hasRest2,
                                    minArgs=minArgs2, ... })

                => (allEqual params1 params2) andalso
                   groundEquals result1 result2 andalso
                   (optionEqual thisType1 thisType2) andalso
                   hasRest1 = hasRest2 andalso
                   minArgs1 = minArgs2

              | (Ast.ObjectType fts1, Ast.ObjectType fts2)
                => fieldTypesEqual fts1 fts2

              | (Ast.InstanceType ity1, Ast.InstanceType ity2)
                => Mach.nameEq (#name ity1) (#name ity2) andalso
                   allEqual (#typeArgs ity1) (#typeArgs ity2)
                   
              | _ => false                     
    in
        trace ["<<< groundEquals: ", fmtType t1, " == ", fmtType t2, " = ", Bool.toString res ];
        res
    end

val equals = normalizingPredicate groundEquals false

(* -----------------------------------------------------------------------------
 * Subtyping
 * ----------------------------------------------------------------------------- *)

fun groundIsSubtype (t1:Ast.TYPE_EXPR) (* derived *)
                    (t2:Ast.TYPE_EXPR) (* base *)
    : bool = 
    let
        val _ = trace [">>> groundIsSubtype: ", fmtType t1, " <: ", fmtType t2 ];
        val _ = if not (isGroundType t1 andalso isGroundType t2)
                then error ["groundIsSubtype on non-ground terms"]
                else ()

        val res = if groundEquals t1 t2 then
                      true
                  else
                      case (t1,t2) of
                          (* FIXME: nullability is a bit of a mess. *)
                          
                          ((Ast.SpecialType Ast.Null), Ast.NullableType { nullable, ... }) =>
                          nullable

                        | (_, Ast.NullableType { nullable=false, expr }) =>
                          groundIsSubtype t1 expr

                        | ((Ast.SpecialType Ast.Null), Ast.InstanceType {nonnullable,...}) =>
                          not nonnullable

                        | (Ast.SpecialType _, _) => false

	                    | (Ast.UnionType types1,_) =>
	                      List.all (fn t => groundIsSubtype t t2) types1

	                    | (_, Ast.UnionType types2) =>
	                      List.exists (fn t => groundIsSubtype t1 t) types2

                        | (Ast.ArrayType _, Ast.InstanceType it2) =>
                          (#name it2) = Name.nons_Array orelse
                          (#name it2) = Name.nons_Object
                          
                        | (Ast.ObjectType _, Ast.InstanceType it2) =>
                          (#name it2) = Name.nons_Object

                        | (Ast.FunctionType _, Ast.InstanceType it2) =>
                          (#name it2) = Name.nons_Function orelse
                          (#name it2) = Name.nons_Object

                        | (Ast.InstanceType it1, Ast.InstanceType it2) =>
                          List.exists (fn sup => groundIsSubtype sup t2) 
                                      (#superTypes it1)
                          
                        | _ => false
    in
        trace ["<<< groundIsSubtype: ", fmtType t1, " <: ", fmtType t2, " = ", Bool.toString res ];
        res
    end

val isSubtype = normalizingPredicate groundIsSubtype false


(* -----------------------------------------------------------------------------
 * Compatibility
 * ----------------------------------------------------------------------------- *)

fun groundIsCompatible (t1:Ast.TYPE_EXPR)
                       (t2:Ast.TYPE_EXPR)
    : bool = 
    let
        val _ = trace [">>> groundIsCompatible: ", fmtType t1, " ~: ", fmtType t2 ]
        val res = (groundIsSubtype t1 t2) orelse
	              (groundEquals t1 anyType) orelse
	              (groundEquals t2 anyType) orelse
	              case (t1,t2) of
	                  (Ast.UnionType types1,_) =>
	                  List.all (fn t => groundIsCompatible t t2) types1
	                | (_, Ast.UnionType types2) =>
	                  (* t1 must exist in types2 *)
	                  List.exists (fn t => groundIsCompatible t1 t) types2
	                | (Ast.ArrayType types1, Ast.ArrayType types2) =>
	                  (* arrays are invariant, every entry should be compatible in both directions *)
	                  let fun check (h1::t1) (h2::t2) =
		                      (groundIsCompatible h1 h2)
		                      andalso
		                      (groundIsCompatible h2 h1)
		                      andalso
		                      (case (t1,t2) of
			                       ([],[]) => true
		                         | ([],_::_) => check [h1] t2
		                         | (_::_,[]) => check t1 [h2]
		                         | (_::_,_::_) => check t1 t2)
                            | check _ _ =
                              error ["unexpected array types: ",
                                     LogErr.ty t1, " vs. ", LogErr.ty t2]
	                  in
		                  check types1 types2
	                  end
                      
	                | (Ast.ObjectType fields1, Ast.ObjectType fields2) =>
	                  false

	                | (Ast.FunctionType
		                   {params  =params1,
		                    result  =result1,
		                    thisType=thisType1,
		                    hasRest =hasRest1,
	                        minArgs=minArgs},
	                   Ast.FunctionType
		                   {params=params2,
		                    result=result2,
		                    thisType=thisType2,
		                    hasRest=hasRest2,
		                    minArgs=minArgs2}) =>
	                  let
	                  in
		                  (* TODO: Assume for now that functions are not polymorphic *)
		                  assert (not hasRest1 andalso not hasRest2) "cannot handle rest args";

		                  ListPair.all (fn (t1,t2) => groundIsCompatible t1 t2) (params1,params2)
		                  andalso
		                  groundIsCompatible result1 result2
	                  end

                    | _ => false
    in
        trace ["<<< isCompatible: ", fmtType t1, " ~: ", fmtType t2, " = ", Bool.toString res ];
        res
    end

val isCompatible = normalizingPredicate groundIsCompatible false


(* -----------------------------------------------------------------------------
 * Convertibility
 * ----------------------------------------------------------------------------- *)

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


(*
 * When investigating ty1 ~~> ty2, call this.
 *
 * It checks a matrix of predefined conversions and returns an instanceType
 * expression if one exists for a defined target conversion form of ty2.
 * 
 * The return value *should* be a type you can look up, find a class C for,
 * and call C.meta::invoke(x:ty1).
 *
 * If no such conversion exists, it returns NONE.      
 *)

fun groundFindConversion (prog:Fixture.PROGRAM)
                         (tyExpr1:Ast.TYPE_EXPR)
                         (tyExpr2:Ast.TYPE_EXPR) 
    : Ast.TYPE_EXPR option = 
    let
        (* 
         * FIXME: push this up to fixture and initialize it only once.
         * The verifier these types too, after all!
         *)
        val AnyNumberType = getNamedGroundType prog Name.ES4_AnyNumber
        val AnyStringType = getNamedGroundType prog Name.ES4_AnyString
        val AnyBooleanType = getNamedGroundType prog Name.ES4_AnyBoolean

        val AnyType = Ast.SpecialType Ast.Any
        val undefinedType = Ast.SpecialType Ast.Undefined
        val nullType = Ast.SpecialType Ast.Null

        val decimalType = getNamedGroundType prog Name.ES4_decimal
        val doubleType = getNamedGroundType prog Name.ES4_double
        val intType = getNamedGroundType prog Name.ES4_int
        val uintType = getNamedGroundType prog Name.ES4_uint
        val byteType = getNamedGroundType prog Name.ES4_byte
        val NumberType = getNamedGroundType prog Name.nons_Number

        val stringType = getNamedGroundType prog Name.ES4_string
        val StringType = getNamedGroundType prog Name.nons_String

        val booleanType = getNamedGroundType prog Name.ES4_boolean
        val BooleanType = getNamedGroundType prog Name.nons_Boolean

        val AnyScalarType = Ast.UnionType [ AnyNumberType, 
                                            AnyStringType, 
                                            AnyBooleanType,
                                            undefinedType, 
                                            nullType ]

        fun flattenUnion (Ast.UnionType tys) = List.concat (map flattenUnion tys)
          | flattenUnion t = [t]

    (* 
     * We have a matrix of pairs-of-conversion-paths that work, for
     * example, any kind of number can be converted to any kind of string.
     *
     * First we flatten the tyExpr2 (possible) union into a list of
     * possible target conversion types.
     * 
     * For each type target in the flattened target list, we check to see
     * if there's a (src,dst) pair in the conversion matrix where dst = target
     * and tyExpr1 ~: src.
     * 
     * If we find zero, there is no conversion.
     * 
     * If we find one or more, we return the dst of the first pair we
     * find. The caller then looks up this type and calls it via
     * meta::invoke.
     *
     * NB: the order of this matrix is a *priority order* for conversions.
     * It returns the first matching pair.
     * 
     * Also keep in mind that if you're trying to convert to a target
     * union type, you are going to get the first *union member* that
     * matches *any* dst in the matrix, not necessarily the first
     * matrix-entry. The priority scheme is only 1-dimensional.
     *
     * So it's best to do one of two things: sort your union by
     * best-to-worst in the first place, or simply don't try to
     * convert into a union, as it's likely to surprise you!
     *)

        val matrix = [ (AnyScalarType, decimalType),
                       (AnyScalarType, doubleType),
                       (AnyScalarType, intType),
                       (AnyScalarType, uintType),
                       (AnyScalarType, byteType),

                       (AnyScalarType, booleanType),
                       (AnyScalarType, BooleanType),                       

                       (AnyScalarType, stringType),
                       (AnyScalarType, StringType) ]

        fun pairMatches target (src,dst) = (groundEquals target dst andalso
                                            groundIsCompatible tyExpr1 src)
        fun searchTargs [] = NONE
          | searchTargs (t::ts) = 
            case List.find (pairMatches t) matrix of
                SOME (src,dst) => SOME dst
              | NONE => searchTargs ts
        
        val res = searchTargs (flattenUnion tyExpr2)         
    in
        case res of 
            NONE => trace ["determined that ", fmtType tyExpr1, 
                           " does not convert to ", fmtType tyExpr2]
          | SOME t2 => 
            (* 
             * FIXME: possibly insert a sanity check here that the 
             * meta::invoke of this instance type has a single-arg 
             * form that's tyExpr1 is compatible with?
             *)
            trace ["determined that ", fmtType tyExpr1, 
                   " does convert to ", fmtType tyExpr2, 
                   ", specifically to ", fmtType t2];
        
        res
    end

fun findConversion (prog:Fixture.PROGRAM)
                   (locals:Ast.RIBS)
                   (t1:Ast.TY)
                   (t2:Ast.TY) = 
    normalizingPredicate (groundFindConversion prog) NONE prog locals t1 t2


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
             

end
