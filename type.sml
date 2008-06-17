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
 *
 * - TypeFixture, where the environment associates a type variable
 *   with a corresponding type, which are introduced at verify time
 *   for defined type variables, and also at eval time for all type
 *   variables.
 *
 * - TypeVarFixture just contain a type variable, without a
 *   corresponding type, and are introduced at verify time for generic
 *   type variables.  TypeVarFixtures do not exist at run-time
 *
 * ----------------------------------------------------------------------------- 
 *
 * A "type constructor" is a type that is not quite a proper type, in
 * that it needs some number of type arguments to become a proper
 * type.  Unparameterized references to generic typedefs, classes, and
 * interfaces are type constructors.
 *
 * Consider the two type definitions:
 *   type f.<X> = function(X):X
 *   type g = function.<X>(X):X
 * 
 * These are very different; 
 *   g is a type that describes some kinds of generic functions,
 *   whereas f is a type constructor, and must be applied to a type argument to yield a type,
 *   eg f.<int> yields function(int):int
 *
 * ----------------------------------------------------------------------------- 
 *
 * Normalization converts a TYPE (in the context of given RIBS) into a
 * normalized TYPE.  It is an error if a type cannot be normalized;
 * that may be a static or dynamic error, since normalization runs
 * both at verify-time and eval-time.
 * 
 * Normalized types satisfy the following properties:
 *
 * - If two types are equivalent (in that they are both subtypes of
 *   each other), then normalization maps those types to the same
 *   type. This property in necessary to implement C#-style semantics
 *   for static fields of generic classes.
 *
 * - Normalized types are closed, no free TypeNames without nonces 
 *   In particular:
 *     references to TypeFixtures are replaced by the corresponding type
 *     references TypeVarFixtures (which has a nonce) give a TypeName with that nonce.
 *   At evaluation time, all normalized types should be closed, so no TypeNames.
 *
 * - Normalized types are proper types, in that they contain no type
 *   constructors (see below) and no references to generic classes or
 *   interfaces without the appropriate number of type parameters.
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

open Ast

val doTrace = ref false
fun log ss = LogErr.log ("[type] " :: ss)  
fun trace ss = if (!doTrace) then log ss else ()
fun error ss = LogErr.typeError ss
fun traceTy ss ty = if (!doTrace) 
                    then let in trace [ss, LogErr.ty ty]; TextIO.print "\n" end
                    else ()

fun logType ty = (Pretty.ppType ty; TextIO.print "\n")
fun traceType ty = if (!doTrace) then logType ty else ()

fun assert b s = if b then () else (raise Fail s)

fun fmtName n = if !doTrace
                then LogErr.name n  
                else ""

fun fmtType t = if !doTrace
                 then LogErr.ty t
                 else ""

fun nameEq (a:NAME) (b:NAME) = ((#id a) = (#id b) andalso (#ns a) = (#ns b))

(* BEGIN SPEED HACK *)
val cacheLoad : (((int -> TYPE option) option) ref) = ref NONE
val cacheSave : (((int -> TYPE -> unit) option) ref) = ref NONE
(* END SPEED HACK *)


(* -----------------------------------------------------------------------------
 * Name equality, post defn phase
 * ----------------------------------------------------------------------------- *)

fun nameExpressionEqual (name1 : NAME_EXPRESSION)
                        (name2 : NAME_EXPRESSION)
    : bool 
  = (* LDOTS *)
    case (name1, name2) of
        (QualifiedName { namespace=Namespace n1, identifier = i1 },
         QualifiedName { namespace=Namespace n2, identifier = i2 } ) =>
        n1 = n2 andalso i1 = i2

     (* other match cases cannot appear post defn *)

(* -----------------------------------------------------------------------------
 * Generic mapping helper
 * ----------------------------------------------------------------------------- *)

fun mapType (f : TYPE -> TYPE) 
            (ty: TYPE)
    : TYPE =
    case ty of 
        RecordType fields => 
        RecordType (map (fn (name, ty) => (name, f ty)) fields)
      | UnionType types =>
        UnionType (map f types)
      | ArrayType (types, restType) => 
        ArrayType (map f types, Option.map f restType)
      | FunctionType { typeParams, params, result, thisType, hasRest, minArgs } => 
        FunctionType { typeParams = typeParams,
                       params = map f params,
                       result = Option.map f result,
                       thisType = f thisType,
                       hasRest = hasRest,
                       minArgs = minArgs } 
      | NonNullType ty => 
        NonNullType (f ty)
      | AppType ( base, args ) => 
        AppType ( f base, map f args )
      | TypeIndexReferenceType (t, idx) =>  TypeIndexReferenceType (f t, idx) (* INFORMATIVE *)
      | TypeNameReferenceType  (t, id)  =>  TypeNameReferenceType  (f t, id)  (* INFORMATIVE *)
      | _ => ty

fun foreachTyExpr (f:TYPE -> unit) (ty:TYPE) : unit =
    let in
        mapType (fn t => let in f t; t end) ty;
        ()
    end

(* -----------------------------------------------------------------------------
 * Substitution and alpha-renaming
 * ----------------------------------------------------------------------------- *)

fun makeNameExpression (id:IDENTIFIER) : NAME_EXPRESSION 
  = QualifiedName { namespace = Namespace (Name.publicNS),
                    identifier = id }
    
fun nameExpressionNonceEqual (name1, nonce1) (name2, nonce2) =
    nameExpressionEqual name1 name1 andalso nonce1 = nonce2

(* Perform capture-free substitution of "args" for all free 
 * occurrences of "params" in ty".
 *)

type SUBST = (NAME_EXPRESSION * NONCE option) * TYPE

fun substTypesInternal (s : SUBST list) 
                       (ty: TYPE) 
    : TYPE =
    case ty of
        TypeName tn =>
        (case List.find (fn (tn', ty) => nameExpressionNonceEqual tn tn') s of
             NONE => ty
           | SOME (_,ty) => ty)
      (* FIXME: think about funny name collisions, and alpha-renaming given nonces *)
      | _ => mapType (substTypesInternal s) ty

fun substTypes (typeParams : IDENTIFIER list) 
               (typeArgs   : TYPE list) 
               (ty         : TYPE)
    : TYPE 
  = (* LDOTS *)
    substTypesInternal 
        (ListPair.map 
             (fn (typeParam, typeArg) =>
                 ( (makeNameExpression typeParam, NONE), typeArg ))
             (typeParams, typeArgs))
        ty
    
fun rename (typeParams1 : IDENTIFIER list)
           (typeParams2 : IDENTIFIER list)
           (ty : TYPE)
    :  TYPE 
  = (* LDOTS *)
    substTypesInternal 
        (ListPair.map 
             (fn (typeParam1, typeParam2) =>
                 ( (makeNameExpression typeParam1, NONE),
                   TypeName (makeNameExpression typeParam2, NONE) ))
             (typeParams1, typeParams2))
        ty

(* -----------------------------------------------------------------------------
 * Normalization
 * ----------------------------------------------------------------------------- *)
    
fun normalizeRefs (ty:TYPE)
    : TYPE =
    case ty of 
        TypeIndexReferenceType (ArrayType (types,rest), idx) => 
        let
            val t = if idx < length types 
                    then List.nth (types, idx)
                    else case rest of
                             NONE => AnyType  (* was error ["TypeIndexReferenceType out of bounds", LogErr.ty ty] *)
                           | SOME restType => restType
        in
            normalizeRefs t
        end
      | TypeIndexReferenceType (AnyType, _) => AnyType
      | TypeIndexReferenceType (t, _) => error ["TypeIndexReferenceType on non-ArrayType: ", LogErr.ty t]
      | TypeNameReferenceType (RecordType fields, nameExpr) => 
        (case List.find
                  (* FIXME: nameExpr is *not* resolved at defn time *)
                  (fn (name, ty) => nameExpressionEqual name nameExpr)
                  fields of
             NONE => error ["TypeNameReferenceType on unknown field: ", LogErr.nameExpr nameExpr]
           | SOME ( name, ty ) => normalizeRefs ty)
      | TypeNameReferenceType (AnyType, _) => AnyType
      | TypeNameReferenceType (t, _) => error ["TypeNameReferenceType on non-RecordType: ", LogErr.ty t]
      | x => mapType normalizeRefs x
                                   
(* ----------------------------------------------------------------------------- *)
(* FIXME: Not clear this does the right thing now that TypeNames can implicitly contain null,
 * but we have much more serious problems with unions and normalization.
 *)

fun normalizeNullsInner (ty:TYPE)
    : TYPE =
    let
        fun containsNull ty = 
            case ty of 
                NullType => true
              | NonNullType _ => false
              | UnionType tys => List.exists containsNull tys
              | _ => false

        fun stripNulls ty = 
            case ty of 
                NullType => NONE
              | NonNullType t => stripNulls t 
              | UnionType tys => 
                (case List.mapPartial stripNulls tys of
                     [] => NONE
                   | tys1 => SOME (UnionType tys1))
              | _ => SOME ty
    in
        if containsNull ty
        then 
            case stripNulls ty of 
                SOME (UnionType tys) => UnionType (tys @ [NullType])
              | SOME t => UnionType [t, NullType]
              | NONE => NullType
        else 
            case stripNulls ty of 
                SOME t => t
              | NONE => UnionType []
    end

fun normalizeNulls (ty:TYPE)
    : TYPE = 
    mapType normalizeNulls (normalizeNullsInner ty)


(* ----------------------------------------------------------------------------- *)
(* FIXME: also need to normalize (C|D) and (D|C) to the same type.
 * We also need to normalize ...
 *
 * function.<X>(X):X
 * function.<Y>(Y):Y 
 *)
                                   
fun normalizeUnions (ty:TYPE)
    : TYPE =
    let
        fun unUnion (UnionType tys) = tys
          | unUnion t = [t]
    in
        case ty of 
            UnionType tys => 
            (case List.concat (map unUnion tys) of 
                 [x] => x
               | tys => UnionType tys)                                     
          | x => mapType normalizeUnions x
    end


fun normalizeArrays (ty:TYPE)
    : TYPE =
    case ty of 
        ArrayType ([],NONE) => ArrayType ([], SOME AnyType)
      | x => mapType normalizeArrays x


(* ----------------------------------------------------------------------------- *)
(* Checks that the given type does not contain any type constructors,
 * unless they are immediately applied to an appropriate number of arguments.
 * Assumes no beta redexes.
 *)

fun checkProperType (ty:TYPE) : unit = 
    let fun check ty2 = 
            case ty2 of
(* a LamType could be a generic function type, which is a type
                LamType { params, body } => 
                error ["Improper occurrence of type constructor ", LogErr.ty ty2, 
                       " in normalized type ", LogErr.ty ty]
*)
                AppType ( base, args ) =>
                error ["Improper occurrence of type application ", LogErr.ty ty2, 
                       " in normalized type ", LogErr.ty ty]

              | _ => foreachTyExpr check ty2
    in 
        check ty
    end
  
(* ----------------------------------------------------------------------------- *)
(* normalizeNames: replace all references to TypeFixtures by the corresponding type. *)


fun resolveTypeNames (env : RIBS)
                     (ty  : TYPE)                    
    : TYPE = 
    case ty of 

        TypeName (nameExpr, _) => 
        let in
            case (Fixture.resolveNameExpr env nameExpr) of 

                (envOfDefn, _,  TypeFixture ([], typeBody)) => 
                resolveTypeNames envOfDefn typeBody

              | (_, _, ClassFixture (c as Class {nonnullable, typeParams=[], ...})   ) => 
                if nonnullable then
                    ClassType c
                else
                    UnionType [ClassType c, NullType]

              | (_, _, InterfaceFixture (i as Interface {nonnullable, typeParams=[], ...})) => 
                if nonnullable then
                    InterfaceType i
                else
                    UnionType [InterfaceType i, NullType]
                         
              | (_, n, _) => error ["name ", LogErr.name  n, " in type expression ", 
                                    LogErr.ty ty, " is not a proper type"]
        end

      | AppType (TypeName (nameExpr, _), typeArgs) =>
        let fun checkArgs typeParams =
            if length typeArgs = length typeParams then 
                ()
            else 
                error ["Incorrect no of arguments to parametric typedefn"];
        in
            case Fixture.resolveNameExpr env nameExpr of 
                (envOfDefn, _,  TypeFixture (typeParams, typeBody)) => 
                let in
                    checkArgs typeParams;
                    resolveTypeNames envOfDefn
                                     (substTypes typeParams
                                                 (map (resolveTypeNames env) 
                                                      typeArgs)
                                                 typeBody)
                end

              | (_, _, ClassFixture (c as Class {nonnullable, typeParams, ...})) =>
                let in
                    checkArgs typeParams;
                    if nonnullable then
                        AppType (ClassType c, typeArgs)
                    else
                        UnionType [AppType (ClassType c, typeArgs), NullType]
                end

              | (_, _, InterfaceFixture (i as Interface {nonnullable, typeParams, ...})) => 
                let in
                    checkArgs typeParams;
                    if nonnullable then
                        AppType (InterfaceType i, typeArgs)
                    else
                        UnionType [AppType (InterfaceType i, typeArgs), NullType]
                end
                         
              | _ => mapType (resolveTypeNames env) ty
        end

      | _ => mapType (resolveTypeNames env) ty

fun normalizeNames (useCache:bool)
                   (env:RIBS)
                   (ty:TYPE)                    
  : TYPE = 
    let
    in
        (* BEGIN SPEED HACK *)
        case (useCache, !cacheLoad, !cacheSave, ty) of 
            (true, SOME load, SOME save, TypeName (_, SOME id)) => 
            (case load id of 
                 NONE => 
                 let
                     val r = resolveTypeNames env ty
                 in
                     save id r;
                     r
                 end
               | SOME r => r)
        (* END SPEED HACK *)

          | _ => resolveTypeNames env ty        
    end


(* ----------------------------------------------------------------------------- *)

fun normalize (ribs:RIB list)
              (ty:TYPE)               
    : TYPE =
    let
        val _ = traceTy "normalize1: " ty
        val ty = normalizeNames true ribs ty     (* inline TypeFixtures and TypeVarFixture nonces *)

        val _ = traceTy "normalize2: " ty
        val ty = normalizeRefs ty

        val _ = traceTy "normalize4: " ty
        val ty = normalizeNulls ty

        val _ = traceTy "normalize5: " ty
        val ty = normalizeUnions ty

        val _ = traceTy "normalize7: " ty
        val _  = checkProperType ty

        val _ = traceTy "normalize8: " ty
    in
        ty
    end
             

(* -----------------------------------------------------------------------------
 * Subtype algorithm
 * ----------------------------------------------------------------------------- *)

fun findSpecialConversion (tyExpr1:TYPE)
                          (tyExpr2:TYPE) 
    : TYPE option = 
    let
        fun extract (UnionType [ClassType t, NullType]) = SOME t
          | extract (UnionType [ClassType t]) = SOME t
          | extract (ClassType t) = SOME t
          | extract _ = NONE
        val srcClass = extract tyExpr1
        val dstClass = extract tyExpr2
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
        case (srcClass, dstClass) of
            ((SOME src), (SOME dst)) => 
            let
                val Class {name=srcName, ...} = src
                val Class {name=dstName, ...} = dst
            in
                if 
                    (isBooleanType dstName)
                    orelse
                    (isNumericType srcName andalso isNumericType dstName)
                    orelse
                    (isStringType srcName andalso isStringType dstName)
                then SOME (ClassType dst)
                else NONE
            end

          | (_, (SOME dst)) => 
            let
                val Class {name=dstName, ...} = dst
            in
                if 
                    (isBooleanType dstName)
                then SOME (ClassType dst)
                else NONE
            end

          | _ => NONE
    end

fun subType (extra : TYPE -> TYPE -> bool) 
            (type1 : TYPE)
            (type2 : TYPE)
    : bool = 
    (type1 = type2)   (* reflexivity *) orelse
    (subTypeRecord   extra type1 type2) orelse
    (subTypeArray    extra type1 type2) orelse
    (subTypeUnion    extra type1 type2) orelse
    (subTypeFunction extra type1 type2) orelse
    (subTypeNonNull  extra type1 type2) orelse
    (subTypeNullable extra type1 type2) orelse
    (subTypeNominal  extra type1 type2) orelse
    (subTypeStructuralNominal extra type1 type2) orelse
    (extra type1 type2) 

and subTypeStructuralNominal extra type1 type2 =
    case (type1, type2) of

        (RecordType _,  ClassType (Class { name, ... })) 
        => nameEq name Name.public_Object 
           
      | (ArrayType _, ClassType (Class { name, ... })) 
        => nameEq name Name.public_Array orelse
           nameEq name Name.public_Object 
           
      | (FunctionType _, ClassType (Class { name, ... })) 
        => nameEq name Name.public_Function orelse
           nameEq name Name.public_Object 
           
      | _ => false

and subTypeNominal extra type1 type2 =
    case (type1, type2) of

        ( ClassType (Class { typeParams = [], extends, implements, ...}), _ )
        => (case extends of 
                NONE => false 
              | SOME extends => subType extra extends type2)
           orelse
           List.exists 
               (fn iface => subType extra iface type2) 
               implements
               
      | ( AppType 
              (ClassType (Class { typeParams, extends, implements, ...}),
               typeArgs),
          _ )
        => (case extends of 
                NONE => false
              | SOME extends => subType extra 
                                        (substTypes typeParams typeArgs extends)
                                        type2)
           orelse
           List.exists 
               (fn iface => subType extra 
                                    (substTypes typeParams typeArgs iface) 
                                    type2)
               implements
               
      | ( InterfaceType (Interface { typeParams = [], extends, ...}), _ )
        => List.exists 
               (fn iface => subType extra iface type2) 
               extends

      | ( AppType 
              (InterfaceType (Interface { typeParams, extends, ...}),
               typeArgs),
          _ )
        => List.exists  
               (fn iface => subType extra 
                                    (substTypes typeParams typeArgs iface) 
                                    type2) 
               extends


      | ( AppType (typeConstructor1, typeArgs1),
          AppType (typeConstructor2, typeArgs2) )
        => 
        typeConstructor1 = typeConstructor2 andalso
        length typeArgs1 = length typeArgs2 andalso
        ListPair.all
            (fn (type1, type2) => equivType extra type1 type2)
            (typeArgs1, typeArgs2)

      | _ => false

and subTypeNullable extra type1 type2 =
    case (type1, type2) of

        (NullType, 
         ClassType (Class { nonnullable = false, ... }))
        => true

      | (NullType, 
         AppType (ClassType (Class { nonnullable = false, ... }), typeArgs))
        => true

      | (NullType, 
         InterfaceType (Interface { nonnullable = false, ... }))
        => true

      | (NullType, 
         AppType (InterfaceType (Interface { nonnullable = false, ...}), typeArgs))
        => true
               
      | _ => false

and subTypeNonNull extra type1 type2 =
    case (type1, type2) of

        (* 
         * FIXME, this rule is odd; it's required to let the following code function:
         *
         *   var v : Object!
         *   v = new Array();
         *
         * Under the interpretation that the array has allocated type Array!, the latter
         * two rules here will first check that Array is a subtype of [Object!, null]
         * -- via the first rule -- and then check that Array is a subtype of Object
         * and, crucually, that null is not a subtype of Array. Unfortunately for us, 
         * null -is- a subtype of Array, it is only not-a-subtype of Array!.
         *
         * So we add one more rule here. It's possibly going to make matters worse.
         *)
        (NonNullType type1, NonNullType type2) => 
        subType extra type1 type2
        
      | (NonNullType type1, type2) =>
        subType extra type1 (UnionType [type2, NullType])
        
      | (type1, NonNullType type2) =>
        subType extra type1 type2 
        andalso not (subType extra NullType type1)


      | _ => false

and subTypeRecord extra type1 type2 = 
    case (type1, type2) of

        (RecordType fields1, RecordType fields2) => 
        List.all (fn ( name2, type2 ) =>
                     List.exists (fn ( name1, type1 ) =>
                                     nameExpressionEqual name2 name1 andalso
                                     equivType extra type2 type1)
                                 fields1)
                 fields2
        
      | _ => false


and subTypeUnion extra type1 type2 =
    case (type1, type2) of

        (UnionType types1, type2) 
        => List.all    (fn type1 => subType extra type1 type2) types1
           
      | (type1, UnionType types2) 
        => List.exists (fn type2 => subType extra type1 type2) types2
           
      | _ => false

and subTypeArray extra type1 type2 =
    case (type1, type2) of

        (ArrayType (types1, rest1), 
         ArrayType (types2, rest2)) 
        =>  
        let
            val min = Int.min( length types1, length types2 ) 
        in
            ListPair.all (fn (type1, type2) => equivType extra type1 type2)  
                         (List.take(types1, min),
                          List.take(types2, min))
            andalso
            (case (rest1, rest2) of
                 (NONE,    NONE   ) => length types1 >= length types2
               | (NONE,    SOME _ ) => false
               | (SOME _,  NONE   ) => false
               | (SOME t1, SOME t2) =>
                 length types1 >= length types2 andalso            
                 equivType extra t1 t2 andalso
                 List.all (fn types1 => equivType extra type1 t2)
                          (List.drop(types1, length types2)))
        end

      | _ => false

and subTypeFunction extra type1 type2 =
    case (type1, type2) of

        (FunctionType
             { typeParams = typeParams1,  params     = params1,     
               result     = result1,      thisType   = thisType1,
               hasRest    = hasRest1,     minArgs    = minArgs1 },
         FunctionType 
             { typeParams = typeParams2,  params     = params2,
               result     = result2,      thisType   = thisType2,
               hasRest    = hasRest2,     minArgs    = minArgs2 }) 
        => 
        (* set up a substitution to alpha-rename typeParams to be identical *)
        let
            val subst = rename typeParams1 typeParams2 
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
                     List.all (fn t => equivType extra t AnyType)
                              (List.drop(params1, min)))
        end

      | _ => false
 

and equivType (extra : TYPE -> TYPE -> bool)
              (type1 : TYPE)
              (type2 : TYPE)
    : bool = 
      (subType extra type1 type2) andalso
      (subType (fn type1 => fn type2 => extra type2 type1)
               type2 type1)

(* -----------------------------------------------------------------------------
 * Compatible-subtyping:  <*
 * ----------------------------------------------------------------------------- *)

(* cannot use findSpecialConversion here, or Boolean(1) != true *)

fun compatibleSubtype (type1 : TYPE) (type2 : TYPE) : bool = 
    subType
        (fn type1 => fn type2 => type2 = AnyType)   
        type1 type2

(* -----------------------------------------------------------------------------
 * Matching: ~<
 * ----------------------------------------------------------------------------- *)

fun groundMatches type1 type2
  = subType 
        (fn type1 => fn type2 =>
                      type1 = AnyType orelse
                      type2 = AnyType orelse
                      findSpecialConversion type1 type2 <> NONE)
        type1 type2


fun matches (rootRib:RIB)
            (locals:RIBS)
            (type1:TYPE)
            (type2:TYPE)
  =
  let
      (* FIXME: it is *super wrong* to just be using the root rib here. *)
      val norm1 = normalize (locals @ [rootRib]) type1
      val norm2 = normalize (locals @ [rootRib]) type2
  in
      groundMatches norm1 norm2
  end



(* 
 * Small helper for finding instance types by name.
 *)

fun instanceTy (rootRib:RIB)
               (n:NAME)
    : TYPE =
    case Fixture.getFixture rootRib (PropName n) of
        (ClassFixture c) => ClassType c
      | (InterfaceFixture i) => InterfaceType i
      | _ => error [LogErr.name n, " does not resolve to an instance type"]

fun groundType (rootRib:RIB)
               (ty:TYPE) 
    : TYPE = 
    let
        (* FIXME: it is *super wrong* to just be using the root rib here. *)
        val norm = normalize [rootRib] ty
    in
        norm
    end    

fun isGroundType (ty:TYPE) : bool = true   (* FIXME: deprecated *)

fun groundExpr (ty:TYPE)  (* or "groundType" *)
    : TYPE = ty (* FIXME: deprecated *)

fun getNamedGroundType (rootRib:RIB)
                       (name:NAME)
   : TYPE = 
    groundType rootRib (Name.typename name)


end



