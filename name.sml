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
structure Name = struct

val (anonNsCounter:int ref) = ref 0
fun newAnonNS _ =
    (* safe: will raise Overflow when we run out of namespaces. *)
    (anonNsCounter := (!anonNsCounter) + 1;
     (Ast.AnonUserNamespace (!anonNsCounter)))

val metaNS = newAnonNS ()
val magicNS = newAnonNS ()
val informativeNS = newAnonNS ()
val noNS = Ast.Public Ustring.empty
val ES4NS = Ast.Public Ustring.ES4_
val intrinsicNS = Ast.Intrinsic
val uint32opsNS = Ast.Public Ustring.uint32ops_

(*
 * FIXME: mangling a name into a string is bad form. We really want to
 * have derived namespaces like Private and Protected refer to their
 * containing namespace-qualified name of their containing class, rather
 * than the mangled form of their containing class.
 *)
fun mangle (n:Ast.NAME) : Ast.IDENT =
    Ustring.fromString (LogErr.fullName n)

fun make (id:Ast.IDENT) (ns:Ast.NAMESPACE) : Ast.NAME = { id = id, ns = ns }
fun meta (id:Ast.IDENT) : Ast.NAME = { id = id, ns = metaNS }
fun informative (id:Ast.IDENT) : Ast.NAME = { id = id, ns = informativeNS }
fun magic (id:Ast.IDENT) : Ast.NAME = { id = id, ns = magicNS }
fun intrinsic (id:Ast.IDENT) : Ast.NAME = { id = id, ns = intrinsicNS }
fun nons (id:Ast.IDENT) : Ast.NAME = { id = id, ns = noNS }
fun public (cls:Ast.NAME) (id:Ast.IDENT) : Ast.NAME = { id = id, ns = Ast.Public (mangle cls) }
fun private (cls:Ast.NAME) (id:Ast.IDENT) : Ast.NAME = { id = id, ns = Ast.Private (mangle cls) }
fun ES4 (id:Ast.IDENT) : Ast.NAME = { id = id, ns = ES4NS }
fun uint32ops (id:Ast.IDENT) : Ast.NAME = { id = id, ns = uint32opsNS }

(*
 * To reference a name as a type expression, you need
 * a complicated structure.
 *)

fun typename (n:Ast.NAME) =
    Ast.TypeName (Ast.QualifiedIdentifier
		      { ident = (#id n),
			qual = Ast.LiteralExpr
				   (Ast.LiteralNamespace (#ns n)) })

(*
 * Names that are supposed to be present in the global scope
 * once we finish booting.
 *)

(* From MetaObjects.es *)
val intrinsic_Type = intrinsic Ustring.Type_

(* From Interface.es -- going away. *)
val intrinsic_Interface = intrinsic Ustring.Interface_

(* From Class.es -- going away. *)
val intrinsic_Class = intrinsic Ustring.Class_

(* From Namespace.es *)
val ES4_Namespace = ES4 Ustring.Namespace_

(* From Name.es *)
val ES4_Name = ES4 Ustring.Name_
val nons_qualifier = nons Ustring.qualifier_
val nons_identifier = nons Ustring.identifier_

(* From Object.es *)
val nons_Object = nons Ustring.Object_

(* From Error.es *)
val nons_Error = nons Ustring.Error_
val nons_EvalError = nons Ustring.EvalError_
val nons_RangeError = nons Ustring.RangeError_
val nons_ReferenceError = nons Ustring.ReferenceError_
val nons_SyntaxError = nons Ustring.SyntaxError_
val nons_TypeError = nons Ustring.TypeError_

(* From Conversions.es *)
val intrinsic_ToPrimitive = intrinsic Ustring.ToPrimitive_

(* From Global.es *)
val intrinsic_NaN = intrinsic Ustring.NaN_
val intrinsic_Infinity = intrinsic Ustring.Infinity_
val intrinsic_undefined = intrinsic Ustring.undefined_
val intrinsic_eval = intrinsic Ustring.eval_
val intrinsic_parseInt = intrinsic Ustring.parseInt_
val intrinsic_parseFloat = intrinsic Ustring.parseFloat_
val intrinsic_isNaN = intrinsic Ustring.isNaN_
val intrinsic_isFinite = intrinsic Ustring.isFinite_
val intrinsic_decodeURI = intrinsic Ustring.decodeURI_
val intrinsic_decodeURIComponent = intrinsic Ustring.decodeURIComponent_
val intrinsic_encodeURI = intrinsic Ustring.encodeURI_
val intrinsic_encodeURIComponent = intrinsic Ustring.encodeURIComponent_

val nons_NaN = nons Ustring.NaN_
val nons_Infinity = nons Ustring.Infinity_
val nons_undefined = nons Ustring.undefined_
val nons_eval = nons Ustring.eval_
val nons_parseInt = nons Ustring.parseInt_
val nons_parseFloat = nons Ustring.parseFloat_
val nons_isNaN = nons Ustring.isNaN_
val nons_isFinite = nons Ustring.isFinite_
val nons_decodeURI = nons Ustring.decodeURI_
val nons_decodeURIComponent = nons Ustring.decodeURIComponent_
val nons_encodeURI = nons Ustring.encodeURI_
val nons_encodeURIComponent = nons Ustring.encodeURIComponent_

val intrinsic_print = intrinsic Ustring.print_
val intrinsic_load = intrinsic Ustring.load_
val intrinsic_assert = intrinsic Ustring.assert_
val intrinsic_typename = intrinsic Ustring.typename_
val intrinsic_readFile = intrinsic Ustring.readFile_
val intrinsic_writeFile = intrinsic Ustring.writeFile_
val intrinsic_readHTTP = intrinsic Ustring.readHTTP_
val intrinsic_explodeDouble = intrinsic Ustring.explodeDouble_

(* From Function.es *)
val nons_Function = nons Ustring.Function_

(* From Boolean.es *)
val nons_Boolean = nons Ustring.Boolean_
val ES4_AnyBoolean = ES4 Ustring.AnyBoolean_

(* From boolean_primitive.es *)
val ES4_boolean = ES4 Ustring.boolean_

(* From Number.es *)
val nons_Number = nons Ustring.Number_
val ES4_AnyNumber = ES4 Ustring.AnyNumber_

(* From double.es *)
val ES4_double = ES4 Ustring.double_

(* From int.es *)
val ES4_int = ES4 Ustring.int_

(* From uint.es *)
val ES4_uint = ES4 Ustring.uint_

(* From decimal.es *)
val ES4_decimal = ES4 Ustring.decimal_

(* From String.es *)
val nons_String = nons Ustring.String_
val ES4_AnyString = ES4 Ustring.AnyString_

(* From string_primitive.es *)
val ES4_string = ES4 Ustring.string_

(* From Array.es *)
val nons_Array = nons Ustring.Array_

(* From Date.es *)
val nons_Date = nons Ustring.Date_

(* From RegExp.es *)
val nons_RegExp = nons Ustring.RegExp_

(* From JSON.es *)
val JSON_emit = { ns = Ast.Public Ustring.JSON_, id = Ustring.emit_ }
val JSON_parse = { ns = Ast.Public Ustring.JSON_, id = Ustring.parse_ }

(* From DecimalContext.es *)
val ES4_DecimalContext = ES4 Ustring.DecimalContext_
val nons_precision = nons Ustring.precision_
val nons_mode = nons Ustring.mode_


(*
 * Natives
 *)

val intrinsic_id = intrinsic Ustring.id_
val intrinsic_proto = intrinsic Ustring.proto_
val intrinsic_dumpFunc = intrinsic Ustring.dumpFunc_
val intrinsic_inspect = intrinsic Ustring.inspect_
val intrinsic_random = intrinsic Ustring.random_
val informative_acosDouble = informative Ustring.acosDouble_
val informative_acosDecimal = informative Ustring.acosDecimal_
val informative_asinDouble = informative Ustring.asinDouble_
val informative_asinDecimal = informative Ustring.asinDecimal_
val informative_atanDouble = informative Ustring.atanDouble_
val informative_atanDecimal = informative Ustring.atanDecimal_
val informative_atan2Double = informative Ustring.atan2Double_
val informative_atan2Decimal = informative Ustring.atan2Decimal_
val informative_ceilDouble = informative Ustring.ceilDouble_
val informative_ceilDecimal = informative Ustring.ceilDecimal_
val informative_cosDouble = informative Ustring.cosDouble_
val informative_cosDecimal = informative Ustring.cosDecimal_
val informative_expDouble = informative Ustring.expDouble_
val informative_expDecimal = informative Ustring.expDecimal_
val informative_floorDouble = informative Ustring.floorDouble_
val informative_floorDecimal = informative Ustring.floorDecimal_
val informative_logDouble = informative Ustring.logDouble_
val informative_logDecimal = informative Ustring.logDecimal_
val informative_powDouble = informative Ustring.powDouble_
val informative_powDecimal = informative Ustring.powDecimal_
val informative_roundDouble = informative Ustring.roundDouble_
val informative_roundDecimal = informative Ustring.roundDecimal_
val informative_sinDouble = informative Ustring.sinDouble_
val informative_sinDecimal = informative Ustring.sinDecimal_
val informative_sqrtDouble = informative Ustring.sqrtDouble_
val informative_sqrtDecimal = informative Ustring.sqrtDecimal_
val informative_tanDouble = informative Ustring.tanDouble_
val informative_tanDecimal = informative Ustring.tanDecimal_
val informative_objectHash = informative Ustring.objectHash_
val intrinsic_now = intrinsic Ustring.now_
val intrinsic_toPrecision = intrinsic Ustring.toPrecision_
val intrinsic_toExponential = intrinsic Ustring.toExponential_
val intrinsic_toFixedStep10 = intrinsic Ustring.toFixedStep10_
val intrinsic_set = intrinsic Ustring.set_
val intrinsic_get = intrinsic Ustring.get_

val intrinsic_DaylightSavingsTA = intrinsic Ustring.DaylightSavingsTA_
val intrinsic_LocalTZA = intrinsic Ustring.LocalTZA_

val magic_stringAppend = magic Ustring.stringAppend_
val magic_stringLength = magic Ustring.stringLength_
val magic_fromCharCode = magic Ustring.fromCharCode_
val magic_charCodeAt = magic Ustring.charCodeAt_
val magic_fnLength = magic Ustring.fnLength_
val magic_apply = magic Ustring.apply_

val magic_toPrimitive = magic Ustring.toPrimitive_
val magic_isPrimitive = magic Ustring.isPrimitive_
val magic_defaultValue = magic Ustring.defaultValue_
val magic_setPropertyIsDontEnum = magic Ustring.setPropertyIsDontEnum_
val magic_getPropertyIsDontDelete = magic Ustring.getPropertyIsDontDelete_
val magic_getPropertyIsDontEnum = magic Ustring.getPropertyIsDontEnum_
val magic_hasOwnProperty = magic Ustring.hasOwnProperty_
val magic_getPrototype = magic Ustring.getPrototype_
val magic_getSuperClass = magic Ustring.getSuperClass_
val magic_getSuperInterface = magic Ustring.getSuperInterface_
val magic_getImplementedInterface = magic Ustring.getImplementedInterface_
val magic_getClassOfObject = magic Ustring.getClassOfObject_
val magic_getClassName = magic Ustring.getClassName_
val magic_construct = magic Ustring.construct_

val uint32ops_add = uint32ops Ustring.add_
val uint32ops_sub = uint32ops Ustring.sub_
val uint32ops_mul = uint32ops Ustring.mul_
val uint32ops_div = uint32ops Ustring.div_
val uint32ops_mod = uint32ops Ustring.mod_
val uint32ops_and = uint32ops Ustring.and_
val uint32ops_or = uint32ops Ustring.or_
val uint32ops_xor = uint32ops Ustring.xor_
val uint32ops_not = uint32ops Ustring.not_
val uint32ops_sar = uint32ops Ustring.sar_
val uint32ops_slr = uint32ops Ustring.slr_
val uint32ops_sll = uint32ops Ustring.sll_
val uint32ops_ror = uint32ops Ustring.ror_
val uint32ops_rol = uint32ops Ustring.rol_

(*
 * Property names that have special meanings to the interpreter.
 *)

val nons_constructor = nons Ustring.constructor_
val nons_length = nons Ustring.length_
val nons_cursor = nons Ustring.cursor_
val private_Array__length = private nons_Array (Ustring.fromString "_length")
val nons_source = nons Ustring.source_
val nons_prototype = nons Ustring.prototype_
val nons_toString = nons Ustring.toString_
val nons_valueOf = nons Ustring.valueOf_
val nons_global = nons Ustring.global_
val meta_invoke = meta Ustring.invoke_
val meta_get = meta Ustring.get_
val meta_set = meta Ustring.set_
val meta_has = meta Ustring.has_
val meta_call = meta Ustring.call_

val arguments = nons Ustring.arguments_
val empty = nons Ustring.empty

val meta_ = nons Ustring.meta_
val magic_ = nons Ustring.magic_
val informative_ = nons Ustring.informative_
val ES4_ = nons Ustring.ES4_

end
