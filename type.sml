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

(* -----------------------------------------------------------------------------
 * General overview of types and type bindings
 * ----------------------------------------------------------------------------- 
 * 
 * There are two kinds of type variables. 
 * - Defined type variables are introduced via type definitions "type X = T".
 * - Generic type variables are introduced via  function.<X>() {...}, 
 *   class C.<X> {...} and interface I.<X> { ... }
 * 
 * An environment (aka RIBS) that may contain two kinds of bindings for type variable.
 * - TypeFixture, where the environment associated a type variable with a corresponding type,
 *   which are introduced at verify time for defined type variables, 
 *   and also at eval time for all type variables.
 * - TypeVarFixture just contain a type variable, without a corresponding type, 
 *   and are introduced at verify time for generic type variables.
 *   TypeVarFixtures do not exist at run-time
 *
 * ----------------------------------------------------------------------------- 
 *
 * A "type constructor" is a type that is not quite a proper type, in that it needs some number
 * of type arguments to become a proper type.  
 * Unparameterized references to generic typedefs,  classes, and interfaces are type constructors.
 *
 * Consider the two type definitions:
 *   type f.<X> = function(X):X
 *   type g = function.<X>(X):X
 * 
 * Traditionally, these are very different; 
 *   g is a type that describes some kinds of generic functions,
 *   whereas f is a type constructor, and must be applied to a type argument to yield a type,
 *   eg f.<int> yields function(int):int
 *
 * But we only have one ast node (LamType) to describe both kinds,
 * so f and g are identical. That is, f is a types, and it is also a unary type constructor,
 * in that f.<int> is also a type. Ditto for g.
 * Thus function.<X>(X):X is both a type constructor and a proper type.
 *
 * ----------------------------------------------------------------------------- 
 *
 * Normalization converts a TYPE (in the context of given RIBS) 
 * into a normalized TYPE.
 * It is an error if a type cannot be normalized; 
 * that may be a static or dynamic error,
 * since normalization runs both at verify-time and eval-time.
 * 
 * Normalized types satisfy the following properties:
 *
 * - If two types are equivalent (in that they are both subtypes of each other),
 *   then normalization maps those types to the same type. This property in necessary
 *   to implement C#-style semantics for static fields of generic classes.
 *
 * - Normalized types are closed, no free TypeNames without nonces 
 *   In particular:
 *     references to TypeFixtures are replaced by the corresponding type
 *     references TypeVarFixtures (which has a nonce) give a TypeName with that nonce.
 *   At evaluation time, all normalized types should be closed, so no TypeNames.
 *
 * - Normalized types are in beta-normal form, in that any applications of LamTypes 
 *   have been reduced. 
 *
 * - Normalized types are proper types, in that they contain no type constructors (see below)
 *   ie no LamTypes, and no references to generic classes or interfaces
 *   without the appropriate number of type parameters.
 *
 * ----------------------------------------------------------------------------- 
 * 
 * Tricky example of shadowing, etc, where *2 = nonce 2, etc


 class C.<Y - *1> {
   type X = Y
   function f.<Y - *2>() {
         type W.<Z - *3> = {y:Y,z:Z}        <-- normalized to  {y:*2, z:*3 }
         function g.<Y>() {
              (W.<X>).z        = (W.<*1>).z = ({y:*2, z:*1}).z = *1
   }
 }

need to inline, to beta-reduce, to eval refs, etc

 *)


structure Type = struct

val doTrace = ref false
fun log ss = LogErr.log ("[type] " :: ss)  
fun trace ss = if (!doTrace) then log ss else ()
fun error ss = LogErr.typeError ss
fun traceTy ss ty = if (!doTrace) 
                    then let in trace [ss, LogErr.ty ty]; TextIO.print "\n" end
                    else ()

fun logType ty = (Pretty.ppType ty; TextIO.print "\n")
fun traceType ty = if (!doTrace) then logType ty else ()

val undefinedType   = Ast.UndefinedType
val nullType        = Ast.NullType
val anyType         = Ast.AnyType

fun assert b s = if b then () else (raise Fail s)

fun fmtName n = if !doTrace
                then LogErr.name n  
                else ""

fun fmtType t = if !doTrace
                 then LogErr.ty t
                 else ""

fun nameEq (a:Ast.NAME) (b:Ast.NAME) = ((#id a) = (#id b) andalso (#ns a) = (#ns b))

(* BEGIN SPEED HACK *)
val cacheLoad : (((int -> Ast.TYPE option) option) ref) = ref NONE
val cacheSave : (((int -> Ast.TYPE -> unit) option) ref) = ref NONE
(* END SPEED HACK *)


(* -----------------------------------------------------------------------------
 * Name equality, post defn phase
 * ----------------------------------------------------------------------------- *)

fun nameExpressionEqual (name1 : Ast.NAME_EXPRESSION)
                        (name2 : Ast.NAME_EXPRESSION)
    : bool =
    case (name1, name2) of
        (Ast.QualifiedName { namespace=Ast.Namespace n1, identifier = i1 },
         Ast.QualifiedName { namespace=Ast.Namespace n2, identifier = i2 } ) =>
        n1 = n2 andalso i1 = i2

     (* other match cases cannot appear post defn *)

(* -----------------------------------------------------------------------------
 * Normalization
 * ----------------------------------------------------------------------------- *)

fun mapFuncTy (f:(Ast.TYPE -> Ast.TYPE))
              (fty:Ast.FUNCTION_TYPE)
    : Ast.FUNCTION_TYPE = 
    let
        val { typeParams, params, result, thisType, hasRest, minArgs } = fty
    in
        { typeParams = typeParams,
          params = map f params,
          result = Option.map f result,
          thisType = f thisType,
          hasRest = hasRest,
          minArgs = minArgs }    
    end

fun mapObjTy (f:(Ast.TYPE -> Ast.TYPE))
             (fields:Ast.FIELD_TYPE list)
    : Ast.FIELD_TYPE list =
    let
        fun mapField ( name, ty ) = ( name, f ty )
    in
        map mapField fields
    end
        
(* Generic mapping helper. *)
fun mapTyExpr (f:(Ast.TYPE -> Ast.TYPE)) 
              (ty:Ast.TYPE)
    : Ast.TYPE =
    case ty of 
        Ast.AnyType => ty
      | Ast.NullType => ty
    (*  | Ast.VoidType => ty *)
      | Ast.UndefinedType => ty
      | Ast.TypeName _ => ty
      | Ast.AppType ( base, args ) => 
        Ast.AppType ( f base, map f args )
(*
      | Ast.LamType { params, body } => 
        Ast.LamType { params = params, 
                      body = f body }
*)
      | Ast.NonNullType t => 
        Ast.NonNullType (f t)
      | Ast.RecordType fields => 
        Ast.RecordType (mapObjTy f fields)
      | Ast.UnionType tys =>
        Ast.UnionType (map f tys)
      | Ast.ArrayType (tys, to) => 
        Ast.ArrayType (map f tys, Option.map f to)
      | Ast.FunctionType fty => 
        Ast.FunctionType (mapFuncTy f fty)
      | Ast.TypeIndexReferenceType (t, idx) => 
        Ast.TypeIndexReferenceType (f t, idx)
      | Ast.TypeNameReferenceType (t, id) =>
        Ast.TypeNameReferenceType (f t, id)
      | Ast.InstanceType { name, typeParams, typeArgs, 
                           nonnullable, superTypes, ty, dynamic } =>
        Ast.InstanceType { name=name, 
                           typeParams=typeParams,
                           typeArgs = map f typeArgs,
                           nonnullable=nonnullable, 
                           superTypes=superTypes, ty=ty, dynamic=dynamic }
(*      | Ast.TypeVarFixtureRef _ => ty *)
(*      | _ => (error ["Unknown type ", LogErr.ty ty]; anyType)
*)

fun foreachTyExpr (f:Ast.TYPE -> unit) (ty:Ast.TYPE) : unit =
    let in
        mapTyExpr (fn t => let in f t; t end) ty;
        ()
    end

(* FIXME: this is dubious: unlike many other contexts, when it comes to fields we 
 * consider an unqualified name to be in public::, not in default-declaration-namespace.
 * Possibly fix/change this. It's subtle.
 * CF: Post-defn, no UnqualifiedName, so a non-issue, I believe.
 *
fun nameExprToFieldName (env:Ast.RIBS) (Ast.QualifiedName {namespace, identifier}) = 
    { ns = Fixture.resolveNamespaceExpr env namespace, id = identifier }

  | nameExprToFieldName (env:Ast.RIBS) (Ast.UnqualifiedName { identifier, ... }) =
    { ns = Name.publicNS, id = identifier }
*)        

(* ----------------------------------------------------------------------------- *)

(* 
 * FIXME: Cormac: sorry, I've made this ref-evaluation thing depend on
 * envs like normalizeNames does, because it has to look up namespace
 * qualifiers on field labels.
 * 
 * Unfortunately I have left it in a broken state of not handling
 * shadowing under lambda binding forms yet. Either re-order it to run
 * after the normalizeLambdas pass, or add in the same
 * shadowing-handling stuff you do in normalizeNames. Up to you. -Graydon
 *)
fun normalizeRefs (env:Ast.RIBS) 
                  (ty:Ast.TYPE)
    : Ast.TYPE =
    case ty of 
        Ast.TypeIndexReferenceType (Ast.ArrayType (arr,to), idx) => 
        let
            val t = if idx < length arr 
                    then List.nth (arr, idx)
                    else    
                        if length arr > 0
                        then List.last arr (* FIXME: use to *)
                        else Ast.AnyType (* FIXME: do we allow 0-length array types? *)
        in
            normalizeRefs env t
        end
      | Ast.TypeIndexReferenceType (t, _) => error ["TypeIndexReferenceType on non-ArrayType: ", LogErr.ty t]
      | Ast.TypeNameReferenceType (Ast.RecordType fields, nameExpr) => 
        (case List.find
                  (* FIXME: nameExpr is *not* resolved at defn time *)
                  (fn ( name, ty ) => nameExpressionEqual name nameExpr)
                  fields of
             NONE => error ["TypeNameReferenceType on unknown field: ", LogErr.nameExpr nameExpr]
           | SOME ( name, ty ) => normalizeRefs env ty)
      | Ast.TypeNameReferenceType (t, _) => error ["TypeNameReferenceType on non-RecordType: ", LogErr.ty t]
      | x => mapTyExpr (normalizeRefs env) x
                                   
(* ----------------------------------------------------------------------------- *)

fun normalizeNullsInner (ty:Ast.TYPE)
    : Ast.TYPE =
    let
        val nullTy = Ast.NullType
        fun containsNull ty = 
            case ty of 
                Ast.NullType => true
              | Ast.NonNullType _ => false
              | Ast.UnionType tys => List.exists containsNull tys
              | _ => false

        fun stripNulls ty = 
            case ty of 
               Ast.NullType => NONE
              | Ast.NonNullType t => stripNulls t 
              | Ast.UnionType tys => 
                (case List.mapPartial stripNulls tys of
                     [] => NONE
                   | tys1 => SOME (Ast.UnionType tys1))
              | _ => SOME ty
    in
        if containsNull ty
        then 
            case stripNulls ty of 
                SOME (Ast.UnionType tys) => Ast.UnionType (tys @ [nullTy])
              | SOME t => Ast.UnionType [t, nullTy]
              | NONE => nullTy
        else 
            case stripNulls ty of 
                SOME t => t
              | NONE => Ast.UnionType []
    end

fun normalizeNulls (ty:Ast.TYPE)
    : Ast.TYPE = 
    mapTyExpr normalizeNulls (normalizeNullsInner ty)


(* ----------------------------------------------------------------------------- *)
(* FIXME: also need to normalize (C|D) and (D|C) to the same type.
 * We also need to normalize ...
 *
 * function.<X>(X):X
 * function.<Y>(Y):Y 
 *)
                                   
fun normalizeUnions (ty:Ast.TYPE)
    : Ast.TYPE =
    let
        fun unUnion (Ast.UnionType tys) = tys
          | unUnion t = [t]
    in
        case ty of 
            Ast.UnionType tys => 
            (case List.concat (map unUnion tys) of 
                 [x] => x
               | tys => Ast.UnionType tys)                                     
          | x => mapTyExpr normalizeUnions x
    end


fun normalizeArrays (ty:Ast.TYPE)
    : Ast.TYPE =
    case ty of 
(*        Ast.ArrayType [] => Ast.ArrayType [Ast.AnyType]
      | *) x => mapTyExpr normalizeArrays x


(* ----------------------------------------------------------------------------- *)
(* Checks that the given type does not contain any type constructors,
 * unless they are immediately applied to an appropriate number of arguments.
 * Assumes no beta redexes.
 *)

fun checkProperType (ty:Ast.TYPE) : unit = 
    let fun check ty2 = 
            case ty2 of
(* a LamType could be a generic function type, which is a type
                Ast.LamType { params, body } => 
                error ["Improper occurrence of type constructor ", LogErr.ty ty2, 
                       " in normalized type ", LogErr.ty ty]
*)
                Ast.AppType ( base, args ) =>
                error ["Improper occurrence of type application ", LogErr.ty ty2, 
                       " in normalized type ", LogErr.ty ty]

              | Ast.InstanceType { name, typeParams, typeArgs, ... } =>
                if length typeParams = length typeArgs
                then (map (foreachTyExpr check) typeArgs; ())
                else
                error ["Improper occurrence of generic class reference ", LogErr.ty ty2, 
                       " in normalized type ", LogErr.ty ty]

              | _ => foreachTyExpr check ty2
    in 
        check ty
    end
    
(* ----------------------------------------------------------------------------- *)
(* normalizeNames: replace all references to TypeFixtures by the corresponding type. *)

fun makeNameExpression (id:Ast.IDENTIFIER) : Ast.NAME_EXPRESSION 
  = Ast.QualifiedName { namespace = Ast.Namespace (Name.publicNS),
                        identifier = id }
    

fun normalizeNames (useCache:bool)
                   (env:Ast.RIBS)
                   (ids:Ast.NAME_EXPRESSION list)
                   (ty:Ast.TYPE)                    
  : Ast.TYPE = 
    let
        fun getType (nameExpr : Ast.NAME_EXPRESSION) 
            : Ast.TYPE = 
            case Fixture.resolveNameExpr env nameExpr of 
                (env', _,  Ast.TypeFixture (ids,ty')) => 
                let in
                    if ids = []
                    then ()
                    else error ["References to generic typedefs not supported yet!"];
                    (* Pulling ty out of env', need to normalize first, in the right environment *)
                    normalizeNames useCache env' [] ty'
                end
 (*               
              | (env', n, Ast.TypeVarFixture nonce) =>
                Ast.TypeVarFixtureRef nonce
*)
                (* FIXME: not sure about the following, and generic classes ... *)
              | (_, _, Ast.ClassFixture (Ast.Cls { instanceType, nonnullable, ... })) => 
                if nonnullable
                then instanceType
                else Ast.UnionType [ instanceType, Ast.NullType]

              | (_, _, Ast.InterfaceFixture (Ast.Iface { instanceType, nonnullable, ... })) => 
                if nonnullable
                then instanceType
                else Ast.UnionType [ instanceType, Ast.NullType ]
                         
              | (_, n, _) => error ["name ", LogErr.name  n, 
                                    " in type expression ", LogErr.ty ty, 
                                    " is not a type"]

        fun doResolve _ =         
            case ty of 
                Ast.TypeName (nameExpr, _) => 
                     if List.exists (nameExpressionEqual nameExpr) ids
                     then ty (* local binding, don't replace *)
                     else getType nameExpr
(*
              | Ast.LamType { params, body } => 
                Ast.LamType { params = params, 
                              body = normalizeNames false env (ids@params) body }
*)
              | t => mapTyExpr (normalizeNames useCache env ids) t
    in
        (* BEGIN SPEED HACK *)
        case (useCache, !cacheLoad, !cacheSave, ty) of 
            (true, SOME load, SOME save, Ast.TypeName (_, SOME id)) => 
            (case load id of 
                 NONE => 
                 let
                     val r = doResolve ()
                 in
                     save id r;
                     r
                 end
               | SOME r => r)
        (* END SPEED HACK *)

          | _ => doResolve ()        
    end
(* ----------------------------------------------------------------------------- *)
(* uniqueIdent maps an IDENTIFIER to a unique variant of that IDENTIFIER that has not been used before.
 * This unique-ification is used for alpha-renaming with capture-free substitution.
 *)

val uniqueIdentPostfix = ref 0

fun uniqueIdent (id:Ast.IDENTIFIER) : Ast.IDENTIFIER =
    let in
        uniqueIdentPostfix := !uniqueIdentPostfix +1;
        Ustring.stringAppend id (Ustring.fromInt (!uniqueIdentPostfix))
    end

fun makeTypeName (id:Ast.IDENTIFIER) : Ast.TYPE = 
    Ast.TypeName (makeNameExpression id, NONE)


fun nameExpressionNonceEqual (name1, nonce1) (name2, nonce2) =
    nameExpressionEqual name1 name1 andalso nonce1 = nonce2

(* 
 * FIXME: Cormac, I am pretty sure the use of unqualified names here
 * is not quite right; please discuss details with me sometime -Graydon
 *)

(* Perform capture-free substitution of "args" for all free occurrences of "params" in ty".
 *)

type SUBST = (Ast.NAME_EXPRESSION * Ast.NONCE option) * Ast.TYPE

fun substTypes (s : SUBST list) 
               (ty: Ast.TYPE) 
    : Ast.TYPE =
    case ty of
        Ast.TypeName tn =>
        (case List.find (fn (tn', ty) => nameExpressionNonceEqual tn tn') s of
             NONE => ty
           | SOME (_,ty) => ty)
      (* FIXME: think about funny name collisions, and alpha-renaming given nonces *)
      | _ => mapTyExpr (substTypes s) ty

(*

and FUNCTION_TYPE =
    { typeParams : IDENTIFIER list,
      thisType   : TYPE,
      params  : TYPE list,
      minArgs : int,
      hasRest : bool,
      result  : TYPE option    (* NONE indicates void return type *)
    }


fun substTypes (typenames: (NAME_EXPRESSION * NONCE option) list) 
               (args:Ast.TYPE list) 
               (ty:Ast.TYPE) 
    : Ast.TYPE =
    case ty of
(*
        Ast.LamType { params, body } =>
        let val uniqParams    = map uniqueIdent  params
            val refUniqParams = map makeTypeName uniqParams
            val body'  = substTypes uniqParams refUniqParams body
            val body'' = substTypes params args body'
        in
            Ast.LamType { params=uniqParams, body=body'' }
        end
        *)
        Ast.TypeName tn =>
        case List.find (nameExpressionEquals tn) 
        let fun lookup ids args =
                case (ids, args) of
                    ([],[]) => ty
                  | (id::idRest, arg::argRest) =>
                    if id = id'
                    then arg
                    else lookup idRest argRest
                  | _ => error ["substTypes: parameter / argument length mismatch"]
        in 
            lookup ids args
        end
      | _ => mapTyExpr (substTypes ids args) ty

*)
(* ----------------------------------------------------------------------------- *)
(* Perform beta-reduction of all AppTypes applied to a LamType.
 *)

fun normalizeLambdas (ty:Ast.TYPE) : Ast.TYPE = 
    (* first, normalizeLambdas in subterms *)
    let val ty = mapTyExpr normalizeLambdas ty
    in
        case ty of
(*
            Ast.AppType { base=(Ast.LamType {params, body}), args } =>
            (* a beta-redex *)
            let val _ =
                    if length params = length args
                    then ()
                    else error ["incorrect number of type arguments ", LogErr.ty ty]
                val ty = substTypes params args body
            in
                (* normalizeLambdas already run on body above, 
                 * but substitution may have exposed more redexes
                 *)
                normalizeLambdas ty
            end
*)
            (* cf: fix the rep for typeArgs so substitution will work *)
            Ast.InstanceType { name, typeParams, typeArgs, 
                               nonnullable, superTypes, ty, dynamic } =>
            Ast.InstanceType { name=name, 
                               typeParams=typeParams,
                               typeArgs = if List.null typeArgs
                                          then map makeTypeName typeParams 
                                          else typeArgs,
                               nonnullable=nonnullable, 
                               superTypes=superTypes, ty=ty, dynamic=dynamic } 

          | _ => ty
    end

(* ----------------------------------------------------------------------------- *)

fun normalize (ribs:Ast.RIB list)
              (ty:Ast.TYPE)               
    : Ast.TYPE =
    let
        val _ = traceTy "normalize1: " ty
        val ty = normalizeNames true ribs [] ty     (* inline TypeFixtures and TypeVarFixture nonces *)

        val _ = traceTy "normalize2: " ty
        val ty = normalizeRefs ribs ty

        val _ = traceTy "normalize3: " ty
        val ty = normalizeLambdas ty

        val _ = traceTy "normalize4: " ty
        val ty = normalizeNulls ty

        val _ = traceTy "normalize5: " ty
        val ty = normalizeUnions ty

        val _ = traceTy "normalize6: " ty
        val ty = normalizeArrays ty

        val _ = traceTy "normalize7: " ty
        val _  = checkProperType ty

        val _ = traceTy "normalize8: " ty
    in
        ty
    end
             

(* -----------------------------------------------------------------------------
 * Matching helpers
 * ----------------------------------------------------------------------------- *)

(* 
 * FIXME: Cormac, as far as I know -- presently! -- we now permit namespace-qualified
 * fields, thus *all* the field-pairwise stuff becomes sensitive to environment, and 
 * needs to have a static env propagated into it. I'll confirm this unfortunate situation
 * before we fix this fully, but for now assume all the reasoning about plain identifiers
 * here is incorrect. Sadly. Remove this comment when we know better. -Graydon
 *)

fun arrayPairWise predicate xs ys = 
    ((length xs) = (length ys)) andalso
    ListPair.all (fn (t1,t2) => predicate t1 t2) (xs, ys)
    
fun optionWise predicate (SOME a) (SOME b) = predicate a b
  | optionWise _ NONE NONE = true
  | optionWise _ _ _ = false

(* -----------------------------------------------------------------------------
 * Generic matching algorithm
 * ----------------------------------------------------------------------------- *)

fun findSpecialConversion (tyExpr1:Ast.TYPE)
                          (tyExpr2:Ast.TYPE) 
    : Ast.TYPE option = 
    let
        fun extract (Ast.UnionType [Ast.InstanceType t, Ast.NullType]) = SOME t
          | extract (Ast.UnionType [Ast.InstanceType t]) = SOME t
          | extract (Ast.InstanceType t) = SOME t
          | extract _ = NONE
        val srcInstance = extract tyExpr1
        val dstInstance = extract tyExpr2
        fun isNumericType n = 
            List.exists (nameEq n) [ Name.ES4_double, 
                                          Name.ES4_decimal,
                                          Name.public_Number ]
        fun isStringType n = 
            List.exists (nameEq n) [ Name.ES4_string,
                                          Name.public_String ]

        fun isBooleanType n = 
            List.exists (nameEq n) [ Name.ES4_boolean,
                                          Name.public_Boolean ]
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


fun subType (extra : Ast.TYPE -> Ast.TYPE -> bool) 
            (type1 : Ast.TYPE)
            (type2 : Ast.TYPE)
    : bool = 
    (type1 = type2)   (* reflexivity *) orelse
    (subTypeFunction extra type1 type2) orelse
    (subTypeRecord   extra type1 type2) orelse
    (subTypeUnion    extra type1 type2) orelse
    (subTypeArray    extra type1 type2) orelse
    (subTypeNonNull  extra type1 type2) orelse
    (subTypeNominal  extra type1 type2) orelse
    (subTypeStructuralNominal extra type1 type2) orelse
    (extra type1 type2) 

and subTypeStructuralNominal extra type1 type2 =
    case (type1, type2) of


      (* relating structural and nominal types *)

        (Ast.ArrayType _, Ast.InstanceType { name, ... }) => 
        (* FIXME: allow for supertypes of Array other than Object *)
        List.exists (nameEq name) [ Name.public_Array,
                                    Name.public_Object ]
        
      | (Ast.RecordType _, Ast.InstanceType { name, ... }) => 
        List.exists (nameEq name) [ Name.public_Object ]
        
      | (Ast.FunctionType _, Ast.InstanceType { name, ... }) => 
        (* FIXME: allow for supertypes of Function other than Object *)
        List.exists (nameEq name) [ Name.public_Function, 
                                    Name.public_Object ]

      | _ => false


and subTypeNominal extra type1 type2 =
    case (type1, type2) of

        ( Ast.InstanceType { name = name1, typeArgs = typeArgs1, superTypes = superTypes1, ...}, 
          Ast.InstanceType { name = name2, typeArgs = typeArgs2, ...} ) 
        =>
        (nameEq name1 name2 andalso
         length typeArgs1 = length typeArgs2 andalso
         ListPair.all (fn (type1, type2) => equivType extra type1 type2)
                      ( typeArgs1, typeArgs2 ))
        orelse 
        (List.exists (fn superType1 => subType extra superType1 type2) 
                     superTypes1)

      | _ => false

and subTypeNonNull extra type1 type2 =
    case (type1, type2) of

        (Ast.NonNullType type1, type2) =>
        subType extra type1 (Ast.UnionType [type2, Ast.NullType])

      | (type1, Ast.NonNullType type2) =>
        subType extra type1 type2 andalso
        not (subType extra Ast.NullType type1)
   
      | _ => false

and subTypeRecord extra type1 type2 = 
    case (type1, type2) of

        (Ast.RecordType fields1, Ast.RecordType fields2) => 
        List.all (fn ( name1, type1 ) =>
                     List.exists (fn ( name2, type2 ) =>
                                     nameExpressionEqual name1 name2 andalso
                                     equivType extra type1 type2)
                                 fields2)
                 fields1
        
      | _ => false


and subTypeUnion extra type1 type2 =
    case (type1, type2) of

        (Ast.UnionType types1, type2) => 
        List.all    (fn type1 => subType extra type1 type2) types1
        
      | (type1, Ast.UnionType types2) => 
        List.exists (fn type2 => subType extra type1 type2) types2
        
      | _ => false

and subTypeArray extra type1 type2 =
    case (type1, type2) of

        (Ast.ArrayType (types1, rest1), 
         Ast.ArrayType (types2, rest2)) =>  

        let
            val min = Int.min( length types1, length types2 ) 
        in
            ListPair.all (fn (type1, type2) => equivType extra type1 type2)  
                         ( List.take(types1, min),
                           List.take(types2, min) )
            andalso
            (case (rest1, rest2) of
                 (NONE,    NONE   ) => length types1 = length types2
               | (NONE,    SOME _ ) => false
               | (SOME t1, NONE   ) => length types1 >= length types2
               | (SOME t1, SOME t2) =>
                 length types1 >= length types2 andalso            
                 equivType extra t1 t2 andalso
                 List.all (fn t => equivType extra t t2)
                          (List.drop(types1, length types2)))
        end

      | _ => false

and subTypeFunction extra type1 type2 =
    case (type1, type2) of

        (Ast.FunctionType
             { typeParams = typeParams1,   params  = params1,     result  = result1,
               thisType   = thisType1,     hasRest = hasRest1,    minArgs = minArgs1 },
         Ast.FunctionType 
             { typeParams = typeParams2,   params  = params2,     result  = result2,
               thisType   = thisType2,     hasRest = hasRest2,    minArgs = minArgs2 }) 
        => 
        (* set up a substitution to alpha-rename typeParams to be identical *)
        let
            val subst = substTypes 
                            (ListPair.map 
                                 (fn (typeParam1, typeParam2) =>
                                     ( (makeNameExpression typeParam1, NONE),
                                       Ast.TypeName (makeNameExpression typeParam2, NONE) ))
                                 (typeParams1, typeParams2))
            val min = Int.min( length params1, length params2 ) 
        in
            length typeParams1 = length typeParams2
          andalso
            (case (result1, result2) of
                 (SOME type1, SOME type2) => subType extra type1 (subst type2)
               | (NONE,       NONE)       => true)
          andalso
            equivType extra thisType1 (subst thisType2)
          andalso    
            minArgs1 <= minArgs2 
          andalso
            ListPair.all (fn (type1, type2) => equivType extra type1 (subst type2))  
                         (List.take(params1, min),
                          List.take(params2, min))
          andalso
            (case (hasRest1, hasRest2) of
                 (false, false) => length params2 <= length params1
               | (true,  false) => true
               | (false, true ) => false
               | (true,  true ) =>
                     List.all (fn t => equivType extra t Ast.AnyType)
                              (List.drop(params1, min)))
        end

      | _ => false
 

and equivType (extra : Ast.TYPE -> Ast.TYPE -> bool)
              (type1 : Ast.TYPE)
              (type2 : Ast.TYPE)
    : bool = 
      (subType extra type1 type2) andalso
      (subType (fn type1 => fn type2 => extra type2 type1)
               type2 type1)

(* -----------------------------------------------------------------------------
 * Compatible-subtyping:  <*
 * ----------------------------------------------------------------------------- *)

(* cannot use findSpecialConversion here, or Boolean(1) != true *)

fun compatibleSubtype (type1 : Ast.TYPE) (type2 : Ast.TYPE) : bool = 
    subType
        (fn type1 => fn type2 => type2 = anyType)   
        type1 type2

(*
fun compatibleSubtype type1 type2 = 
    let in
        traceTy "compatibleSubtype:type1 " type1;
        traceTy "compatibleSubtype:type2 " type2;
        compareTypes
            (fn type1 => fn type2 => type2 = anyType)
            SubType
            type1 type2
    end
*)

(* val isCompatibleSubtype = normalizingPredicate compatibleSubtype  *)

(* -----------------------------------------------------------------------------
 * Matching: ~<
 * ----------------------------------------------------------------------------- *)

fun groundMatches type1 type2
  = subType 
        (fn type1 => fn type2 =>
                      type1 = anyType orelse
                      type2 = anyType orelse
                      findSpecialConversion type1 type2 <> NONE)
        type1 type2

(*
fun groundMatches type1 type2
  = compareTypes 
        (fn type1 => fn type2 =>
                      type1 = anyType orelse
                      type2 = anyType orelse
                      findSpecialConversion type1 type2 <> NONE)
        SubType
        type1 type2
*)

fun matches (prog:Fixture.PROGRAM)
            (locals:Ast.RIBS)
            (type1:Ast.TYPE)
            (type2:Ast.TYPE)
  =
  let
      (* FIXME: it is *super wrong* to just be using the root rib here. *)
      val norm1 = normalize (locals @ [Fixture.getRootRib prog]) type1
      val norm2 = normalize (locals @ [Fixture.getRootRib prog]) type2
  in
      groundMatches norm1 norm2
  end


(* 
 * Small helper for finding instance types by name.
 *)

fun instanceTy (prog:Fixture.PROGRAM)
               (n:Ast.NAME)
    : Ast.TYPE =
    case Fixture.getFixture (Fixture.getRootRib prog) (Ast.PropName n) of
        (Ast.ClassFixture (Ast.Cls cls)) => (#instanceType cls)
      | (Ast.InterfaceFixture (Ast.Iface iface)) => (#instanceType iface)
      | _ => error [LogErr.name n, " does not resolve to an instance type"]

fun groundType (prog:Fixture.PROGRAM)
               (ty:Ast.TYPE) 
    : Ast.TYPE = 
    let
        (* FIXME: it is *super wrong* to just be using the root rib here. *)
        val norm = normalize [Fixture.getRootRib prog] ty
    in
        norm
    end    

fun isGroundType (ty:Ast.TYPE) : bool = true

fun groundExpr (ty:Ast.TYPE)  (* or "groundType" *)
    : Ast.TYPE = ty (* FIXME: remove *)

fun getNamedGroundType (prog:Fixture.PROGRAM)
                       (name:Ast.NAME)
   : Ast.TYPE = 
    groundType prog (Name.typename name)


end



