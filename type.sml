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
 * Normalization converts a TYPE_EXPR (in the context of given RIBS) 
 * into a normalized TYPE_EXPR.
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
 * - Normalized types are closed, no free TypeNames. 
 *   In particular:
 *     references to TypeFixtures are replaced by the corresponding type
 *     references TypeVarFixtures (which has a nonce) are replaced by a TypeVarFixtureRef
 *     containing that nonce.
 *   At evaluation time, all normalized types should be closed, so no TypeVarFixtureRefs.
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


EXPR = 
       | CallExpr of {
             func: EXPR,
             actuals: EXPR list }
       | ApplyTypeExpr of {                  // ONLY generic fn instantiation
             expr: EXPR,  (* apply expr to type list *)
             actuals: TYPE_EXPR list }

     and TYPE_EXPR =
       | FunctionType of FUNC_TYPE
       | AppType of                         // apply type constructor
         { base: TYPE_EXPR,
           args: TYPE_EXPR list }
       | LamType of                         // 
         { params: IDENT list,
           body: TYPE_EXPR }


AppType
                                           { base = TypeName
                                                      Identifier
                                                        { ident = "C",
                                                          openNamespaces = []},
                                             args = [TypeName
                                                       Identifier
                                                         { ident = "int",
                                                           openNamespaces = []}]}}],



 *)


structure Type = struct

val doTrace = ref false
fun log ss = LogErr.log ("[type] " :: ss)  
fun trace ss = if (!doTrace) then log ss else ()
fun error ss = LogErr.typeError ss
fun traceTy ss ty = if (!doTrace) then let in trace [ss, LogErr.ty ty]; Pretty.ppType ty; TextIO.print "\n" end else ()

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
 * Normalization
 * ----------------------------------------------------------------------------- *)

fun mapFuncTy (f:(Ast.TYPE_EXPR -> Ast.TYPE_EXPR))
              (fty:Ast.FUNC_TYPE)
    : Ast.FUNC_TYPE = 
    let
        val { params, result, thisType, hasRest, minArgs } = fty
    in
        { params = map f params,
          result = f result,
          thisType = Option.map f thisType,
          hasRest = hasRest,
          minArgs = minArgs }    
    end

fun mapObjTy (f:(Ast.TYPE_EXPR -> Ast.TYPE_EXPR))
             (fields:Ast.FIELD_TYPE list)
    : Ast.FIELD_TYPE list =
    let
        fun mapField { name, ty } = { name = name, ty = f ty }
    in
        map mapField fields
    end
        
(* Generic mapping helper. *)
fun mapTyExpr (f:(Ast.TYPE_EXPR -> Ast.TYPE_EXPR)) 
              (ty:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR =
    case ty of 
        Ast.SpecialType _ => ty
      | Ast.TypeName _ => ty
      | Ast.AppType { base, args } => 
        Ast.AppType { base = f base,
                      args = map f args}
      | Ast.LamType { params, body } => 
        Ast.LamType { params = params, 
                      body = f body }
      | Ast.NullableType { expr, nullable } => 
        Ast.NullableType { expr = f expr,
                           nullable = nullable }
      | Ast.ObjectType fields => 
        Ast.ObjectType (mapObjTy f fields)
      | Ast.LikeType t => 
        Ast.LikeType (f t)
      | Ast.WrapType t => 
        Ast.WrapType (f t)
      | Ast.UnionType tys =>
        Ast.UnionType (map f tys)
      | Ast.ArrayType tys => 
        Ast.ArrayType (map f tys)
      | Ast.FunctionType fty => 
        Ast.FunctionType (mapFuncTy f fty)
      | Ast.ElementTypeRef (t, idx) => 
        Ast.ElementTypeRef (f t, idx)
      | Ast.FieldTypeRef (t, id) =>
        Ast.FieldTypeRef (f t, id)
      | Ast.InstanceType { name, typeParams, typeArgs, 
                           nonnullable, superTypes, ty, dynamic } =>
        Ast.InstanceType { name=name, 
                           typeParams=typeParams,
                           typeArgs = map f typeArgs,
                           nonnullable=nonnullable, 
                           superTypes=superTypes, ty=ty, dynamic=dynamic } 

fun foreachTyExpr (f:Ast.TYPE_EXPR -> unit) (ty:Ast.TYPE_EXPR) : unit =
    let in
        mapTyExpr (fn t => let in f t; t end) ty;
        ()
    end

(* ----------------------------------------------------------------------------- *)

fun normalizeRefs (ty:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR =
    case ty of 
        Ast.ElementTypeRef (Ast.ArrayType arr, idx) => 
        let
            val t = if idx < length arr 
                    then List.nth (arr, idx)
                    else 
                        if length arr > 0
                        then List.last arr
                        else Ast.SpecialType Ast.Any
        in
            normalizeRefs t
        end
      | Ast.ElementTypeRef (t, _) => error ["ElementTypeRef on non-ArrayType: ", LogErr.ty t]
      | Ast.FieldTypeRef (Ast.ObjectType fields, ident) => 
        let                            
            fun f [] = error ["FieldTypeRef on unknown field: ", Ustring.toAscii ident]
              | f ({name,ty}::fields) = if name = ident 
                                        then ty
                                        else f fields
            val t = f fields
        in
            normalizeRefs t
        end
      | Ast.FieldTypeRef (t, _) => error ["FieldTypeRef on non-ObjectType: ", LogErr.ty t]
      | x => mapTyExpr normalizeRefs x
                                   
(* ----------------------------------------------------------------------------- *)

fun normalizeNulls (ty:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR =
    let
        val nullTy = Ast.SpecialType Ast.Null
        fun containsNull ty = 
            case ty of 
                Ast.SpecialType Ast.Null => true
              | Ast.SpecialType _ => false
              | Ast.InstanceType _ => false
              | Ast.NullableType { expr, nullable } => nullable
              | Ast.LikeType t => containsNull t
              | Ast.ObjectType _ => false
              | Ast.UnionType tys => List.exists containsNull tys
              | Ast.ArrayType tys => false
              | Ast.FunctionType _ => false
              | t => error ["Unexpected type while normalizing nulls: ", LogErr.ty t]

        fun stripNulls ty = 
            case ty of 
                Ast.SpecialType Ast.Null => NONE
              | Ast.SpecialType t => SOME (Ast.SpecialType t)
              | Ast.InstanceType t => SOME (Ast.InstanceType t)
              | Ast.NullableType { expr, nullable } => stripNulls expr 
              | Ast.LikeType t => (case stripNulls t of 
                                       NONE => NONE
                                     | SOME t1 => SOME (Ast.LikeType t1))
              | Ast.UnionType tys => 
                (case List.mapPartial stripNulls tys of
                     [] => NONE
                   | tys1 => SOME (Ast.UnionType tys1))
              | Ast.ObjectType fields => SOME (Ast.ObjectType (mapObjTy normalizeNulls fields))
              | Ast.ArrayType tys => SOME (Ast.ArrayType (map normalizeNulls tys))
              | Ast.FunctionType fty => SOME (Ast.FunctionType (mapFuncTy normalizeNulls fty))
              | t => error ["Unexpected type while normalizing nulls: ", LogErr.ty t]
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

(* ----------------------------------------------------------------------------- *)
(* FIXME: also need to normalize (C|D) and (D|C) to the same type.
 * We also need to normalize ...
 *
 * function.<X>(X):X
 * function.<Y>(Y):Y 
 *)
                                   
fun normalizeUnions (ty:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR =
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


(* ----------------------------------------------------------------------------- *)
(* Checks that the given type does not contain any type constructors,
 * unless they are immediately applied to an appropriate number of arguments.
 * Assumes no beta redexes.
 *)

fun checkProperType (ty:Ast.TYPE_EXPR) : unit = 
    let fun check ty2 = 
            case ty2 of
                Ast.LamType { params, body } => 
                error ["Improper occurrence of type constructor ", LogErr.ty ty2, 
                       " in normalized type ", LogErr.ty ty]

              | Ast.AppType { base, args } =>
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

fun normalizeNames (env:Ast.RIBS)
                   (ids:Ast.IDENT list)
                   (ty:Ast.TYPE_EXPR)                    
  : Ast.TYPE_EXPR = 
    let
        fun getFixture (mname : Ast.MULTINAME) : (Ast.RIBS * Ast.NAME * Ast.FIXTURE) = 
            case Multiname.resolveInRibs mname env of 
                SOME (ribs, name) => 
                let val (rib::_) = ribs in
                    (ribs, name, Fixture.getFixture rib (Ast.PropName name))
                end
              | _ => error ["failed to resolve multiname ", LogErr.multiname mname, 
                            " in type expression ", LogErr.ty ty]

        fun getType (mname : Ast.MULTINAME) : Ast.TYPE_EXPR = 
            case getFixture mname of 
                (env', _,  Ast.TypeFixture ty') => 
                (* Pulling ty out of env', need to normalize first, in the right environment *)
                normalizeNames env' [] ty'
              | (env', n, Ast.TypeVarFixture nonce) =>
                Ast.TypeVarFixtureRef nonce

                (* FIXME: not sure about the following, and generic classes ... *)
              | (_, _, Ast.ClassFixture (Ast.Cls { instanceType, ... })) => instanceType
              | (_, _, Ast.InterfaceFixture (Ast.Iface { instanceType, ... })) => instanceType
              | (_, n, _) => error ["name ", LogErr.name  n, 
                                    " in type expression ", LogErr.ty ty, 
                                    " is not a type"]

        fun getNamespace (mname : Ast.MULTINAME) : Ast.NAMESPACE = 
            case getFixture mname of 
                (_, _, Ast.NamespaceFixture ns) => ns
              | (_, n, _) => error ["name ", LogErr.name  n, 
                                 " in qualifier of type expression ", LogErr.ty ty, 
                                 " is not a namespace"]

        fun getNamespaceForExpr (expr : Ast.EXPR) : Ast.NAMESPACE = 
            case expr of
                Ast.LiteralExpr (Ast.LiteralNamespace ns) => ns
                                                             
              | Ast.LexicalRef {ident = Ast.Identifier {ident, openNamespaces}, loc } =>
                (LogErr.setLoc loc;
                 getNamespace {id = ident, nss = openNamespaces})
                
              | Ast.LexicalRef {ident = Ast.QualifiedIdentifier {qual, ident}, loc } =>
                (LogErr.setLoc loc; 
                 getNamespace { id = ident, nss = [[ getNamespaceForExpr qual ]] })
                
              | _ => error ["dynamic qualifier in namespace of type expression ", LogErr.ty ty]
    in
        case ty of 
            Ast.TypeName (Ast.Identifier { ident, openNamespaces }) => 
            if List.exists (fn i => i=ident) ids
            then ty (* local binding, don't replace *)
            else getType { id = ident, nss = openNamespaces }
            
          | Ast.TypeName (Ast.QualifiedIdentifier { qual, ident }) =>
            (* FIXME: sure this is not a reference to a type var? *)
            getType { id = ident, nss = [[ getNamespaceForExpr qual ]] }
            
          | Ast.TypeName Ast.WildcardIdentifier => Ast.SpecialType Ast.Any
          | Ast.TypeName _ => error ["dynamic name in type expression ", LogErr.ty ty]
          | Ast.LamType { params, body } =>
            Ast.LamType { params = params,
                          body   = normalizeNames env (ids @ params) body }
          | t => mapTyExpr (normalizeNames env ids) t
    end

(* ----------------------------------------------------------------------------- *)
(* uniqueIdent maps an IDENT to a unique variant of that IDENT that has not been used before.
 * This unique-ification is used for alpha-renaming with capture-free substitution.
 *)

val uniqueIdentPostfix = ref 0

fun uniqueIdent (id:Ast.IDENT) : Ast.IDENT =
    let in
        uniqueIdentPostfix := !uniqueIdentPostfix +1;
        Ustring.stringAppend id (Ustring.fromInt (!uniqueIdentPostfix))
    end

fun makeTypeName (id:Ast.IDENT) : Ast.TYPE_EXPR = 
    Ast.TypeName (Ast.Identifier {ident=id, openNamespaces=[] })


(* Perform capture-free substitution of "args" for all free occurrences of "params" in ty".
 * All are normalized, so no TypeNames in args or ty, just TypeVarFixtureRefs.
 *)

fun substTypes (ids:Ast.IDENT list) (args:Ast.TYPE_EXPR list) (ty:Ast.TYPE_EXPR) : Ast.TYPE_EXPR =
    case ty of
        Ast.LamType { params, body } =>
        let val uniqParams    = map uniqueIdent  params
            val refUniqParams = map makeTypeName uniqParams
            val body'  = substTypes uniqParams refUniqParams body
            val body'' = substTypes params args body'
        in
            Ast.LamType { params=uniqParams, body=body'' }
        end
      | Ast.TypeName (Ast.Identifier { ident=id', ... }) =>
        let fun lookup ids args =
                case (ids, args) of
                    ([],[]) => ty
                  | (id::idRest, arg::argRest) =>
                    if id = id'
                    then arg
                    else lookup idRest argRest
        in 
            lookup ids args
        end
      | _ => mapTyExpr (substTypes ids args) ty


(* ----------------------------------------------------------------------------- *)
(* Perform beta-reduction of all AppTypes applied to a LamType.
 *)

fun normalizeLambdas (ty:Ast.TYPE_EXPR) : Ast.TYPE_EXPR = 
    (* first, normalizeLambdas in subterms *)
    let val ty = mapTyExpr normalizeLambdas ty
    in
        case ty of
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

            (* cf: fix the rep for typeArgs so substitution will work *)
          | Ast.InstanceType { name, typeParams, typeArgs, 
                               nonnullable, superTypes, ty, dynamic } =>
            Ast.InstanceType { name=name, 
                               typeParams=typeParams,
                               typeArgs = if typeArgs=[] 
                                          then map makeTypeName typeParams 
                                          else typeArgs,
                               nonnullable=nonnullable, 
                               superTypes=superTypes, ty=ty, dynamic=dynamic } 

          | _ => ty
    end

(* ----------------------------------------------------------------------------- *)

fun normalize (ribs:Ast.RIB list)
              (ty:Ast.TYPE_EXPR)               
    : Ast.TYPE_EXPR =
    let
        val _ = traceTy "normalize1: " ty
        val ty = normalizeNames ribs [] ty     (* inline TypeFixtures and TypeVarFixture nonces *)

        val _ = traceTy "normalize2: " ty
        val ty = normalizeRefs ty

        val _ = traceTy "normalize3: " ty
        val ty = normalizeLambdas ty

        val _ = traceTy "normalize4: " ty
        val ty = normalizeNulls ty

        val _ = traceTy "normalize5: " ty
        val ty = normalizeUnions ty

        val _ = traceTy "normalize6: " ty
        val _  = checkProperType ty

        val _ = traceTy "normalize7: " ty
    in
        ty
    end
             

(* -----------------------------------------------------------------------------
 * Matching helpers
 * ----------------------------------------------------------------------------- *)

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

fun normalizingPredicate groundPredicate 
                         (prog:Fixture.PROGRAM)
                         (locals:Ast.RIBS)
                         (t1:Ast.TYPE_EXPR)
                         (t2:Ast.TYPE_EXPR)
  =
  let
      (* FIXME: it is *super wrong* to just be using the root rib here. *)
      val norm1 = normalize (locals @ [Fixture.getRootRib prog]) t1
      val norm2 = normalize (locals @ [Fixture.getRootRib prog]) t2
  in
      groundPredicate norm1 norm2
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

        (* A-WRAP-COV *)
        (_, _, Ast.WrapType wt1, Ast.WrapType wt2) => 
        groundMatchesGeneric b v wt1 wt2
        
      (* A-WRAP *)
      | (_, Covariant, Ast.WrapType wt1, _) => 
        groundMatchesGeneric b v wt1 ty2

      (* A-LIKE-COV *)
      | (_, _, Ast.LikeType lt1, Ast.LikeType lt2) => 
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
        
      | (_, _, Ast.TypeVarFixtureRef nonce1, Ast.TypeVarFixtureRef nonce2) => 
        (nonce1 = nonce2)
      
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
                                          Name.ES4_int,
                                          Name.ES4_uint,
                                          Name.ES4_decimal,
                                          Name.ES4_double,
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
val isCompatibleSubtype = normalizingPredicate groundIsCompatibleSubtype 

(* -----------------------------------------------------------------------------
 * Matching: ~<
 * ----------------------------------------------------------------------------- *)

val groundMatches = groundMatchesGeneric Bicompat Covariant
val matches = normalizingPredicate groundMatches 


(* 
 * Small helper for finding instance types by name.
 *)

fun instanceTy (prog:Fixture.PROGRAM)
               (n:Ast.NAME)
    : Ast.TYPE_EXPR =
    case Fixture.getFixture (Fixture.getRootRib prog) (Ast.PropName n) of
        (Ast.ClassFixture (Ast.Cls cls)) => (#instanceType cls)
      | (Ast.InterfaceFixture (Ast.Iface iface)) => (#instanceType iface)
      | _ => error [LogErr.name n, " does not resolve to an instance type"]

fun groundType (prog:Fixture.PROGRAM)
               (ty:Ast.TYPE_EXPR) 
    : Ast.TYPE_EXPR = 
    let
        (* FIXME: it is *super wrong* to just be using the root rib here. *)
        val norm = normalize [Fixture.getRootRib prog] ty
    in
        norm
    end    

fun isGroundType (ty:Ast.TYPE_EXPR) : bool = true

fun groundExpr (ty:Ast.TYPE_EXPR)  (* or "groundType" *)
    : Ast.TYPE_EXPR = ty (* FIXME: remove *)

fun getNamedGroundType (prog:Fixture.PROGRAM)
                       (name:Ast.NAME)
   : Ast.TYPE_EXPR = 
    groundType prog (Name.typename name)


end



