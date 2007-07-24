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
fun trace ss = if (!doTrace) then LogErr.log ("[type] " :: ss) else ()
fun error ss = LogErr.typeError ss

fun logType ty = (Pretty.ppType ty; TextIO.print "\n")
fun traceType ty = if (!doTrace) then logType ty else ()

val undefinedType   = Ast.SpecialType Ast.Undefined
val nullType        = Ast.SpecialType Ast.Null
val anyType         = Ast.SpecialType Ast.Any

fun assert b s = if b then () else (raise Fail s)

fun toString ty =
    let
        fun nsExprToString e =
            case e of
                Ast.LiteralExpr (Ast.LiteralNamespace ns) => LogErr.namespace ns
              | Ast.LexicalRef {ident = Ast.Identifier {ident, ...}, ... } => Ustring.toAscii ident
              | _ => error ["unexpected expression type in namespace context"]
        fun nssToString nss =
            LogErr.join ", " (map LogErr.namespace nss)
        fun nsssToString nsss =
            LogErr.join ", " (map (fn nss => "(" ^ (nssToString nss) ^ ")") nsss)
        fun typeList tys =
            LogErr.join ", " (map toString tys)
        fun fieldToString {name, ty} = (Ustring.toAscii name) ^ ": " ^ (toString ty)
        fun fieldList fields =
            LogErr.join ", " (map fieldToString fields)
    in
        case ty of
            Ast.SpecialType Ast.Any => "*"
          | Ast.SpecialType Ast.Null => "null"
          | Ast.SpecialType Ast.Undefined => "undefined"
          | Ast.SpecialType Ast.VoidType => "<VoidType>"
          | Ast.UnionType tys => "(" ^ (typeList tys) ^ ")"
          | Ast.ArrayType tys => "[" ^ (typeList tys) ^ "]"
          | Ast.TypeName (Ast.Identifier {ident, openNamespaces}) => "<TypeName: {" ^ (nsssToString openNamespaces) ^ "}::" ^ (Ustring.toAscii ident) ^ ">"
          | Ast.TypeName (Ast.QualifiedIdentifier { qual, ident }) => "<TypeName: " ^ (nsExprToString qual) ^ "::" ^ (Ustring.toAscii ident) ^ ">"
          | Ast.TypeName _ => "<TypeName: ...>"
          | Ast.ElementTypeRef _ => "<ElementTypeRef: ...>"
          | Ast.FieldTypeRef _ => "<FieldTypeRef: ...>"
          | Ast.FunctionType {params, result, ...} => "<function (" ^ (typeList params) ^ ") -> " ^ (toString result) ^ ">"
          | Ast.ObjectType fields => "{" ^ fieldList fields ^ "}"
          | Ast.AppType {base, args} => (toString base) ^ ".<" ^ (typeList args) ^ ">"
          | Ast.NullableType { expr, nullable } => (toString expr) ^ (if nullable then "?" else "!")
          | Ast.InstanceType { name, ... } => LogErr.name name
    end

fun fmtName n = if !doTrace
                then LogErr.name n
                else ""

fun fmtType ty = if !doTrace
                 then toString ty
                 else ""

(* -----------------------------------------------------------------------------
 * Normalized types
 * ----------------------------------------------------------------------------- *)

type TYPE_VALUE = Ast.TYPE_EXPR  (* Invariant: normalized *)

(*  A normalized type is one of

type TYPE_EXPR =
         SpecialType of SPECIAL_TY
       | UnionType of TYPE_EXPR list
       | ArrayType of TYPE_EXPR list
       | FunctionType of FUNC_TYPE
       | ObjectType of FIELD_TYPE list
       | AppType of
           { base: TYPE_EXPR,  -- not a function type
             args: TYPE_EXPR list }
       | NullableType of
           { expr:TYPE_EXPR,
             nullable:bool }
       | InstanceType of
           { name: NAME,
             typeParams: IDENT list,
             ty: TYPE_EXPR,
             dynamic: bool }
       | NominalType of NAME

and excludes
       | TypeName of IDENT_EXPR
       | ElementTypeRef of (TYPE_EXPR * int)
       | FieldTypeRef of (TYPE_EXPR * IDENT)
*)

fun findNamespace (env:Ast.RIBS)
                  (expr:Ast.EXPR)
    : Ast.NAMESPACE option =
    let
        fun withMname (mname:Ast.MULTINAME) 
            : Ast.NAMESPACE option = 
            case Multiname.resolveInRibs mname env of 
                NONE => NONE
              | SOME (ribs, n) => 
                (case Fixture.getFixture (List.hd ribs) (Ast.PropName n) of
                     Ast.NamespaceFixture ns => SOME ns
                   | _ => error ["namespace expression resolved ",
                                 "to non-namespace fixture"])
    in    
        case expr of
            Ast.LiteralExpr (Ast.LiteralNamespace ns) => SOME ns

          | Ast.LexicalRef {ident = Ast.Identifier {ident, openNamespaces}, loc } =>
            (LogErr.setLoc loc;
             withMname {id = ident, nss = openNamespaces})

          | Ast.LexicalRef {ident = Ast.QualifiedIdentifier {qual, ident}, loc } =>
            (LogErr.setLoc loc;
             case findNamespace env qual of
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
          | Ast.InstanceType _ => true

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

fun envOf (ty:Ast.TY) : Ast.RIBS = 
    let
        val Ast.Ty { env, ... } = ty
    in
        env
    end

fun exprOf (ty:Ast.TY) : Ast.TYPE_EXPR = 
    let
        val Ast.Ty { expr, ... } = ty
    in
        expr
    end


fun isGroundTy (ty:Ast.TY) 
    : bool =
    isGroundType (exprOf ty)

             
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
                 env: Ast.RIBS,
                 unit: Ast.UNIT_NAME option }

fun normalize (prog:Fixture.PROGRAM) 
              (t:Ast.TY) 
    : Ast.TY = (norm2ty (ty2norm prog t))

and repackage (t:Ast.TY) 
    : TY_NORM = 
    let
        val Ast.Ty { expr, env, unit, ... } = t
    in
        { exprs = [expr],
          nullable = false,
          env = env,
          unit = unit }
    end
                
and maybeNamed (prog:Fixture.PROGRAM)
               (originalt:Ast.TY) 
               (mname:Ast.MULTINAME) 
    : TY_NORM =
    let
        val Ast.Ty { env, unit, ... } = originalt
        val (topRib, closed) = 
            case unit of 
                NONE => (Fixture.getTopRib prog, false)
              | SOME u => (case Fixture.getTopRibForUnit prog u of
                               NONE => (Fixture.getTopRib prog, false)
                             | SOME closedRib => (closedRib, true))
        val ribs = env @ [topRib]
    in
        case Multiname.resolveInRibs mname ribs of 
            NONE => if closed 
                    then error ["type multiname ", LogErr.multiname mname, 
                                " failed to resolve in closed unit "]
                    else repackage originalt
          | SOME (ribs, n) => 
            let 
                val (defn:Ast.TY) = 
                    case Fixture.getFixture (List.hd ribs) (Ast.PropName n) of
                        Ast.TypeFixture ty => ty
                      | Ast.ClassFixture (Ast.Cls cls) => (#instanceType cls)
                      | Ast.InterfaceFixture (Ast.Iface iface) => (#instanceType iface)
                      | _ => error ["expected type fixture for: ", LogErr.name n]
            in
                ty2norm prog defn
            end
    end

and norm2ty (norm:TY_NORM) 
    : Ast.TY = 
    let
        val { exprs, nullable, env, unit } = norm 
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
                 unit = unit,
                 env = env }
    end

and ty2norm (prog:Fixture.PROGRAM) 
            (ty:Ast.TY) 
    : TY_NORM =
    let
        val Ast.Ty { env, unit, expr } = ty

        (* Package up a 1-term type in the current TY environment. *)
        fun simple (e:Ast.TYPE_EXPR)
                   (nullable:bool)
            : TY_NORM = 
            if isGroundType e 
            then { exprs = [e], nullable = nullable, env = env, unit = unit }
            else error ["internal error: non-ground term is not simple"]
                 

        (* Use 'subTerm2Norm' to evaluate a TYPE_EXPR in the same environment
         * as the current TY, and get back a new TY_NORM. *)
        fun subTerm2Norm (e:Ast.TYPE_EXPR) 
            : TY_NORM = 
            ty2norm prog (Ast.Ty { expr = e, env = env, unit = unit })

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
                if isGroundType (exprOf t)
                then SOME (exprOf t)
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
                    if isGroundType (exprOf t)
                    then SOME (SOME (exprOf t))
                    else NONE
                end
    in
        case expr of              
            Ast.TypeName (Ast.Identifier { ident, openNamespaces }) => 
            maybeNamed prog ty { id = ident, nss = openNamespaces }
            
          | Ast.TypeName (Ast.QualifiedIdentifier { qual, ident }) =>
            (case findNamespace (envOf ty) qual of
                 SOME ns => maybeNamed prog ty { nss = [[ns]], id = ident }
               | NONE => repackage ty)
            
          | Ast.TypeName _ => error ["dynamic name in type expression"]
                              
          | Ast.InstanceType it => 
            let
                val t0 = ty
                val { name, typeArgs, superTypes, ty, 
                      conversionTy, nonnullable, dynamic } = it
                val typeArgs' = subTerms typeArgs
                val superTypes' = subTerms superTypes
                val ty' = subTerm ty
                val conversionTy' = subTermOption conversionTy
            in
                case (typeArgs', superTypes', ty', conversionTy') of
                    (SOME ta, SOME st, SOME t, SOME c) => 
                    simple (Ast.InstanceType 
                                { name = name,
                                  typeArgs = ta,
                                  superTypes = st,
                                  ty = t,
                                  conversionTy = c,
                                  nonnullable = nonnullable,
                                  dynamic = dynamic }) (not nonnullable)
                  | _ => repackage t0
            end
                                   
          | Ast.AppType { base, args } => 
            let
                val sub = norm2ty o subTerm2Norm
                val (base':Ast.TY) = sub base
                val (args':Ast.TY list) = map sub args
            in
                case base' of 
                    Ast.Ty {expr=Ast.LamType {params, body}, env=env', unit=unit'} => 
                    let
                        val _ = if length params = length args'
                                then ()
                                else error ["applying wrong number of type arguments"]
                        val _ = if List.all isGroundTy args'
                                then () 
                                else error ["passing non-ground type arguments"]
                        (* FIXME: need some extra power here to detect recursion. *)
                        val names = List.map Name.nons params
                        val bindings = ListPair.zip (names, args')
                        fun bind ((n,t),rib) = (Ast.PropName n, Ast.TypeFixture t)::rib
                        val rib = List.foldl bind [] bindings
                        val env'' = rib :: env'
                    in
                        ty2norm prog (Ast.Ty { expr=base, env=env'', unit=unit' })
                    end                
                  | Ast.Ty {expr=Ast.TypeName _, ...} => repackage ty
                  | _ => error ["applying bad type"]
            end
            
          | (Ast.UnionType tys) => 
            let 
                fun extract {exprs, nullable, unit, env} = (exprs, nullable) 
                val sub = extract o subTerm2Norm
                val (listOfLists, listOfNulls) = ListPair.unzip (map sub tys)
                val exprs' = List.concat listOfLists
                val nullable = List.exists (fn x => x) listOfNulls
            in
                {exprs=exprs', nullable=nullable, env=env, unit=unit}
            end
            
          | (Ast.SpecialType Ast.Null) => 
            {exprs=[], nullable=true, env=env, unit=unit}
            
          | (Ast.ArrayType []) => 
            simple (Ast.ArrayType [Ast.SpecialType Ast.Any]) true
            
          | (Ast.ArrayType tys) => 
            (case subTerms tys of
                 NONE => repackage ty
               | SOME tys' => simple (Ast.ArrayType tys') true)
            
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
                                               minArgs = minArgs }) true
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
                        true
            end
            
          | Ast.NullableType { expr, nullable } => 
            let
                val {exprs, env=env', unit=unit', ...} = subTerm2Norm expr
            in
                { exprs=exprs, env=env', unit=unit', nullable=nullable }
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
            error ["ElementTypeRef on non-ArrayType: ", toString t]
            
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
            error ["FieldTypeRef on non-ObjectType: ", toString t]
            
          | _ => repackage ty
    end
                 
fun serializeGroundType (a:Ast.TYPE_EXPR) 
    : Ustring.STRING list =
    let 
        val fromBool = Ustring.fromString o Bool.toString
        val cc = Ustring.fromCharCode
        fun prefixList ls = 
            (cc 1) ::
            (Ustring.fromInt (length ls)) ::
            (Ustring.fromString ":") :: ls
        fun serializeGroundTypeList tys = 
            prefixList (List.concat (map serializeGroundType tys))
        fun serializeGroundTypeOption tyo = 
            case tyo of
                NONE => [cc 2]
              | SOME ty => (cc 3) :: (serializeGroundType ty)
    in
        case a of 
            Ast.SpecialType Ast.Any => [cc 4]
          | Ast.SpecialType Ast.Null => [cc 5]
          | Ast.SpecialType Ast.Undefined => [cc 6]
          | Ast.SpecialType Ast.VoidType => [cc 7]
                                            
          | Ast.UnionType tys => 
            (cc 8) :: (serializeGroundTypeList tys)

          | Ast.ArrayType tys => 
            (cc 9) :: (serializeGroundTypeList tys)

          | Ast.FunctionType { params, result, thisType, hasRest, minArgs } => 
            (cc 10) :: 
            ((serializeGroundTypeList params) @
             [cc 11] @ (serializeGroundType result) @
             [cc 12] @ (serializeGroundTypeOption thisType) @
             [cc 13] @ (serializeGroundTypeOption thisType) @
             [cc 14] @ [fromBool hasRest] @ [Ustring.fromInt minArgs])

          | Ast.ObjectType fields => 
            let
                fun serializeField {name, ty} = 
                    ((cc 15) :: name :: 
                     (cc 16) :: (serializeGroundType ty))
                val fields' = List.concat (map serializeField fields)
            in
                (cc 17) :: (prefixList fields')
            end

          | Ast.NullableType { expr, nullable } => 
            (cc 18) :: (fromBool nullable) ::
            (cc 19) :: (serializeGroundType expr)
            
          | Ast.InstanceType { name, typeArgs, ... } => 
            (cc 22) :: (#id name) :: 
            (cc 23) :: ((NameKey.decomposeNS (#ns name)) @
                        [cc 24] @ (serializeGroundTypeList typeArgs))

          | _ => error ["serializing non-ground type"]
    end

(* -----------------------------------------------------------------------------
 * ****************** WIP line: code below here needs rewriting ****************
 * ----------------------------------------------------------------------------- *)


(* -----------------------------------------------------------------------------
 * Equality
 * ----------------------------------------------------------------------------- *)

fun equals (t1:Ast.TY)
           (t2:Ast.TY)
    : bool =
true
(*
    let
        fun pairEqual (t1, t2) = equals t1 t2
        fun allEqual ts1 ts2 = List.all pairEqual (ListPair.zip (ts1, ts2))
        fun fieldTypesEqual [] [] = true
          | fieldTypesEqual ({ name=id1, ty=t1 }::ts1) ts2 =
            (case (List.partition (fn ({ name=id, ty }) => Ustring.stringEquals id id1) ts2) of
                 ([{ ty=ty2, name }], ts2) => (equals t1 t2) andalso fieldTypesEqual ts1 ts2
               | _ => false)
          | fieldTypesEqual _ _ = false
        fun namedEqual [] [] = true
          | namedEqual ((id1,t1)::ts1) ts2 =
            (case (List.partition (fn (id,_) => Ustring.stringEquals id id1) ts2) of
                 ([(_,t2)], ts2) => (equals t1 t2) andalso namedEqual ts1 ts2
               | _ => false)
          | namedEqual _ _ = false
        fun optionEqual NONE NONE = true
          | optionEqual (SOME t1) (SOME t2) = equals t1 t2
          | optionEqual _ _ = false
        (* TODO: should this be a library function in ast.sml? *)
        fun namespaceEquals (Ast.Intrinsic, Ast.Intrinsic) = true
          | namespaceEquals (Ast.OperatorNamespace, Ast.OperatorNamespace) = true
          | namespaceEquals (Ast.Private i1, Ast.Private i2) = Ustring.stringEquals i1 i2
          | namespaceEquals (Ast.Protected i1, Ast.Protected i2) = Ustring.stringEquals i1 i2
          | namespaceEquals (Ast.Public i1, Ast.Public i2) = Ustring.stringEquals i1 i2
          | namespaceEquals (Ast.Internal i1, Ast.Internal i2) = Ustring.stringEquals i1 i2
          | namespaceEquals (Ast.UserNamespace s1, Ast.UserNamespace s2) = Ustring.stringEquals s1 s2
          | namespaceEquals (Ast.AnonUserNamespace i1, Ast.AnonUserNamespace i2) = i1 = i2
          | namespaceEquals (Ast.LimitedNamespace (i1, ns1), Ast.LimitedNamespace (i2, ns2)) =
            (Ustring.stringEquals i1 i2) andalso (namespaceEquals (ns1, ns2))
          | namespaceEquals (_, _) = false
        (* FIXME: totally bogus *)
        fun identExprEquals (Ast.Identifier { ident=ident1, openNamespaces=openNamespaces1 })
                            (Ast.Identifier { ident=ident2, openNamespaces=openNamespaces2 }) =
            (Ustring.stringEquals ident1 ident2) andalso
            (List.all (fn (ls1, ls2) => List.all namespaceEquals (ListPair.zip (ls1, ls2)))
                      (ListPair.zip (openNamespaces1, openNamespaces2)))
          | identExprEquals _ _ = false
    in
        case (t1, t2) of
            (Ast.SpecialType st1, Ast.SpecialType st2) => st1 = st2
          | (Ast.UnionType ts1, Ast.UnionType ts2)
            => allEqual ts1 ts2
          | (Ast.UnionType [t1], _) => equals t1 t2
          | (_, Ast.UnionType [t2]) => equals t1 t2
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
               equals result1 result2 andalso
               (optionEqual thisType1 thisType2) andalso
               hasRest1 = hasRest2 andalso
               minArgs1 = minArgs2
          | (Ast.ObjectType fts1, Ast.ObjectType fts2)
            => fieldTypesEqual fts1 fts2
(*
       | AppType of
           { base: TYPE_EXPR,  -- not a function type
             args: TYPE_EXPR list }
       | NullableType of
           { expr:TYPE_EXPR,
             nullable:bool }
*)
          | (Ast.InstanceType {  name=name1,
                                 nonnullable=nonnullable1,
                                 superTypes=superTypes1,
                                 ty=ty1,
                                 conversionTy=conversionTy1,
                                 dynamic=dynamic1, ... },
             Ast.InstanceType {  name=name2,
                                 nonnullable=nonnullable2,
                                 superTypes=superTypes2,
                                 ty=ty2,
                                 conversionTy=conversionTy2,
                                 dynamic=dynamic2, ... })
            => (Mach.nameEq name1 name2) andalso
               (nonnullable1 = nonnullable2) andalso
               (* FIXME: order shouldn't matter *)
               (List.all (fn (nm1, nm2) => Mach.nameEq nm1 nm2)
                         (ListPair.zip (superTypes1, superTypes2))) andalso
               (equals ty1 ty2) andalso
               (optionEqual conversionTy1 conversionTy2) andalso
               dynamic1 = dynamic2
          (* FIXME: I thought this shouldn't happen? disallowed in normalized types...? it is being used, though *)
          | (Ast.TypeName ie1, Ast.TypeName ie2)
            => identExprEquals ie1 ie2
          | _ => false
    end
*)

(* -----------------------------------------------------------------------------
 * Subtyping
 * ----------------------------------------------------------------------------- *)

fun isSubtype (prog:Fixture.PROGRAM)
              (t1:Ast.TY) (* derived *)
              (t2:Ast.TY) (* base *)
    : bool =
    true
(*
    let
        val _ = trace [">>> isSubtype: ", fmtType t1, " <: ", fmtType t2 ];
        val t1 = normalize t1
        val t2 = normalize t2
        val res = if equals t1 t2 then
                      true
                  else
                      case (t1,t2) of
                          (* FIXME: nullability is a bit of a mess. *)

                          ((Ast.SpecialType Ast.Null), Ast.NullableType { nullable, ... }) =>
                          nullable

                        | (_, Ast.NullableType { nullable=false, expr }) =>
                          isSubtype tf t1 expr

                        | ((Ast.SpecialType Ast.Null), Ast.InstanceType {nonnullable,...}) =>
                          not nonnullable

                        | (Ast.SpecialType _, _) => false

	                    | (Ast.UnionType types1,_) =>
	                      List.all (fn t => isSubtype tf t t2) types1

	                    | (_, Ast.UnionType types2) =>
	                      List.exists (fn t => isSubtype tf t1 t) types2

                        (*
                        | (Ast.UnionType ts1, Ast.UnionType ts2) =>
                          unimplError ["isSubtype 1"]
                        | (Ast.ArrayType ts1, Ast.ArrayType ts2) =>
                          unimplError ["isSubtype 2"]
                        | (Ast.FunctionType ft1, Ast.FunctionType ft2) =>
                          unimplError ["isSubtype 3"]
                        | (Ast.ObjectType fts1, Ast.ObjectType fts2) =>
                          unimplError ["isSubtype 4"]
                         *)
                        | (Ast.ArrayType _, Ast.InstanceType it2) =>
                          (#name it2) = Name.nons_Array

                        | (Ast.ObjectType _, Ast.InstanceType it2) =>
                          (#name it2) = Name.nons_Object

                        | (Ast.FunctionType _, Ast.InstanceType it2) =>
                          (#name it2) = Name.nons_Function

                        | (Ast.InstanceType it1, Ast.InstanceType it2) =>
                          Fixture.isClass tf (#name it1)
                          andalso Fixture.instanceOf tf (#name it1) (#name it2)
                        | _ => false
    in
        trace ["<<< isSubtype: ", fmtType t1, " <: ", fmtType t2, " = ", Bool.toString res ];
        res
    end
*)

(* -----------------------------------------------------------------------------
 * Compatibility
 * ----------------------------------------------------------------------------- *)

and isCompatible (prog:Fixture.PROGRAM)
                 (t1:Ast.TY)
                 (t2:Ast.TY)
    : bool =
true
(*
    let
        val t1 = normalize t1
        val t2 = normalize t2
        val _ = trace [">>> isCompatible: ", fmtType t1, " ~: ", fmtType t2 ];
        val res = (isSubtype tf t1 t2) orelse
	              (equals t1 anyType) orelse
	              (equals t2 anyType) orelse
	              case (t1,t2) of
	                  (Ast.UnionType types1,_) =>
	                  List.all (fn t => isCompatible tf t t2) types1
	                | (_, Ast.UnionType types2) =>
	                  (* t1 must exist in types2 *)
	                  List.exists (fn t => isCompatible tf t1 t) types2
	                | (Ast.ArrayType types1, Ast.ArrayType types2) =>
	                  (* arrays are invariant, every entry should be compatible in both directions *)
	                  let fun check (h1::t1) (h2::t2) =
		                      (isCompatible tf h1 h2)
		                      andalso
		                      (isCompatible tf h2 h1)
		                      andalso
		                      (case (t1,t2) of
			                       ([],[]) => true
		                         | ([],_::_) => check [h1] t2
		                         | (_::_,[]) => check t1 [h2]
		                         | (_::_,_::_) => check t1 t2)
                            | check _ _ =
                              error ["unexpected array types: ",
                                     toString t1, " vs. ", toString t2]
	                  in
		                  check types1 types2
	                  end

	                | (Ast.AppType {base=base1,args=args1}, Ast.AppType {base=base2,args=args2}) =>
	                  (* We keep types normalized wrt beta-reduction,
	                   * so base1 and base2 must be class or interface types.
	                   * Type arguments are covariant, and so must be intra-compatible - CHECK
	                   *)
	                  false

	                | (Ast.ObjectType fields1, Ast.ObjectType fields2) =>
	                  false

	                | (Ast.FunctionType
		                   {typeParams=typeParams1,
		                    params  =params1,
		                    result  =result1,
		                    thisType=thisType1,
		                    hasRest =hasRest1,
	                        minArgs=minArgs},
	                   Ast.FunctionType
		                   {typeParams=typeParams2,
		                    params=params2,
		                    result=result2,
		                    thisType=thisType2,
		                    hasRest=hasRest2,
		                    minArgs=minArgs2}) =>
	                  let
	                  in
		                  (* TODO: Assume for now that functions are not polymorphic *)
		                  assert (typeParams1 = [] andalso typeParams2=[]) "cannot handle polymorphic fns";
		                  assert (not hasRest1 andalso not hasRest2) "cannot handle rest args";

		                  ListPair.all (fn (t1,t2) => isCompatible tf t1 t2) (params1,params2)
		                  andalso
		                  isCompatible tf result1 result2
	                  end

                    | _ => false
	(* catch all *)
	(* | _ => unimplError ["isCompatible"] *)
    in
        trace ["<<< isCompatible: ", fmtType t1, " ~: ", fmtType t2, " = ", Bool.toString res ];
        res
    end
*)

(* -----------------------------------------------------------------------------
 * Convertibility
 * ----------------------------------------------------------------------------- *)

(*
 * When investigating ty1 ~~> ty2, call this.
 * It returns the name of the class of ty2 containing
 * meta static function convert(x:pt) where ty1 ~: pt,
 * or NONE if there is no such converter.
 *)

fun findConversion (prog:Fixture.PROGRAM)
                   (ty1:Ast.TY)
                   (ty2:Ast.TY)
    : Ast.NAME option =
    NONE
(*
    let
        val _ = trace ["searching for converter from ", fmtType ty1,
                       " ~~> ", fmtType ty2];
        val ty1 = normalize ty1
        val ty2 = normalize ty2
        fun tryToConvertTo (target:Ast.TYPE_EXPR) =
            case target of
                Ast.InstanceType { name, conversionTy=SOME c, ... } =>
                if isCompatible tf ty1 c
                then SOME name
                else NONE
              | Ast.UnionType [] => NONE
              | (Ast.UnionType (t::ts)) =>
                (case tryToConvertTo t of
                     NONE => tryToConvertTo (Ast.UnionType ts)
                   | found => found)
              | _ => NONE
    in
        tryToConvertTo ty2
    end
*)
end
