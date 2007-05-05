structure Name = struct

val (anonNsCounter:int ref) = ref 0
fun newAnonNS _ = 
    (* safe: will raise Overflow when we run out of namespaces. *)
    (anonNsCounter := (!anonNsCounter) + 1;
     (Ast.AnonUserNamespace (!anonNsCounter)))
    
val metaNS = newAnonNS ()
val magicNS = newAnonNS ()
val publicNS = Ast.Public (Ustring.empty)
val internalNS = Ast.Internal (Ustring.empty)
val intrinsicNS = Ast.Intrinsic
(* FIXME: private is incomplete. *)
val privateNS = Ast.Private (Ustring.fromString "class name here")

fun meta (id:Ast.IDENT) : Ast.NAME = { id = id, ns = metaNS }
fun magic (id:Ast.IDENT) : Ast.NAME = { id = id, ns = magicNS }
fun public (id:Ast.IDENT) : Ast.NAME = { id = id, ns = publicNS }
fun internal (id:Ast.IDENT) : Ast.NAME= { id = id, ns = internalNS }
fun intrinsic (id:Ast.IDENT) : Ast.NAME = { id = id, ns = intrinsicNS }
fun private cls (id:Ast.IDENT) : Ast.NAME = { id = id, ns = privateNS }

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
 * Property names that have special meanings to the interpreter.
 *)

val public_constructor = public Ustring.constructor_
val public_length = public Ustring.length_
val public_cursor = public Ustring.cursor_
val private_Array__length = private (Ustring.fromString "Array") (Ustring.fromString "_length")
val public_source = public Ustring.source_
val public_prototype = public Ustring.prototype_
val public_toString = public Ustring.toString_
val public_valueOf = public Ustring.valueOf_
val public_global = public Ustring.global_
val meta_invoke = meta Ustring.invoke_
val meta_get = meta Ustring.get_
val meta_set = meta Ustring.set_
val meta_has = meta Ustring.has_
val meta_call = meta Ustring.call_

val this = internal Ustring.this_
val arguments = internal Ustring.arguments_

(* 
 * Names that are supposed to be present in the global scope 
 * once we finish booting.
 *)

(* From Type.es *)
val intrinsic_Type = intrinsic Ustring.Type_

(* From Interface.es *)
val intrinsic_Interface = intrinsic Ustring.Interface_

(* From Class.es *)
val intrinsic_Class = intrinsic Ustring.Class_

(* From Namespace.es *)
val intrinsic_Namespace = intrinsic Ustring.Namespace_

(* From Object.es *)
val public_Object = public Ustring.Object_

(* From Error.es *)
val public_Error = public Ustring.Error_
val public_EvalError = public Ustring.EvalError_
val public_RangeError = public Ustring.RangeError_
val public_ReferenceError = public Ustring.ReferenceError_
val public_SyntaxError = public Ustring.SyntaxError_
val public_TypeError = public Ustring.TypeError_

(* From Conversions.es *)
val intrinsic_ToPrimitive = intrinsic Ustring.ToPrimitive_
val intrinsic_ToBoolean = intrinsic Ustring.ToBoolean_
val intrinsic_ToInteger = intrinsic Ustring.ToInteger_
val intrinsic_ToInt = intrinsic Ustring.ToInt_
val intrinsic_ToUint = intrinsic Ustring.ToUint_
val intrinsic_ToDouble = intrinsic Ustring.ToDouble_
val intrinsic_ToDecimal = intrinsic Ustring.ToDecimal_
val intrinsic_ToString = intrinsic Ustring.ToString_
val intrinsic_ToObject = intrinsic Ustring.ToObject_
val intrinsic_ToNumeric = intrinsic Ustring.ToNumeric_

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

val public_NaN = public Ustring.NaN_
val public_Infinity = public Ustring.Infinity_
val public_undefined = public Ustring.undefined_
val public_eval = public Ustring.eval_
val public_parseInt = public Ustring.parseInt_
val public_parseFloat = public Ustring.parseFloat_
val public_isNaN = public Ustring.isNaN_
val public_isFinite = public Ustring.isFinite_
val public_decodeURI = public Ustring.decodeURI_
val public_decodeURIComponent = public Ustring.decodeURIComponent_
val public_encodeURI = public Ustring.encodeURI_
val public_encodeURIComponent = public Ustring.encodeURIComponent_

val intrinsic_print = intrinsic Ustring.print_
val intrinsic_load = intrinsic Ustring.load_
val intrinsic_assert = intrinsic Ustring.assert_
val intrinsic_typename = intrinsic Ustring.typename_

(* From Function.es *)
val public_Function = public Ustring.Function_

(* From Boolean.es *)
val public_Boolean = public Ustring.Boolean_

(* From boolean_primitive.es *) 
val intrinsic_boolean = intrinsic Ustring.boolean_

(* From Number.es *)
val public_Number = public Ustring.Number_

(* From double.es *)
val intrinsic_double = intrinsic Ustring.double_

(* From int.es *)
val intrinsic_int = intrinsic Ustring.int_

(* From uint.es *)
val intrinsic_uint = intrinsic Ustring.uint_

(* From decimal.es *)
val intrinsic_decimal = intrinsic Ustring.decimal_

(* From Numeric.es *)
val intrinsic_Numeric = intrinsic Ustring.Numeric_

(* From String.es *)
val public_String = public Ustring.String_

(* From string_primitive.es *)
val intrinsic_string = intrinsic Ustring.string_

(* From Array.es *)
val public_Array = public Ustring.Array_

(* From ByteArray.es *)
val intrinsic_ByteArray = intrinsic Ustring.ByteArray_

(* From Date.es *)
val public_Date = public Ustring.Date_

(* From RegExp.es *)
val public_RegExp = public Ustring.RegExp_

(* From JSON.es *)
val JSON_emit = { ns = Ast.Public Ustring.JSON_, id = Ustring.emit_ }
val JSON_parse = { ns = Ast.Public Ustring.JSON_, id = Ustring.parse_ }


(*
 * Natives
 *)

val intrinsic_proto = intrinsic Ustring.proto_
val intrinsic_inspect = intrinsic Ustring.inspect_
val intrinsic_tan = intrinsic Ustring.tan_
val intrinsic_sqrt = intrinsic Ustring.sqrt_
val intrinsic_sin = intrinsic Ustring.sin_
val intrinsic_round = intrinsic Ustring.round_
val intrinsic_random = intrinsic Ustring.random_
val intrinsic_pow = intrinsic Ustring.pow_
val intrinsic_log = intrinsic Ustring.log_
val intrinsic_floor = intrinsic Ustring.floor_
val intrinsic_exp = intrinsic Ustring.exp_
val intrinsic_cos = intrinsic Ustring.cos_
val intrinsic_ceil = intrinsic Ustring.ceil_
val intrinsic_atan2 = intrinsic Ustring.atan2_
val intrinsic_atan = intrinsic Ustring.atan_
val intrinsic_asin = intrinsic Ustring.asin_
val intrinsic_acos = intrinsic Ustring.acos_
val intrinsic_abs = intrinsic Ustring.abs_
val intrinsic_now = intrinsic Ustring.now_
val intrinsic_toPrecision = intrinsic Ustring.toPrecision_
val intrinsic_toExponential = intrinsic Ustring.toExponential_
val intrinsic_toFixedStep10 = intrinsic Ustring.toFixedStep10_
val intrinsic_set = intrinsic Ustring.set_
val intrinsic_get = intrinsic Ustring.get_

val public_DaylightSavingsTA = public Ustring.DaylightSavingsTA_
val public_LocalTZA = public Ustring.LocalTZA_

val magic_setByteArrayByte = magic Ustring.setByteArrayByte_
val magic_getByteArrayByte = magic Ustring.getByteArrayByte_
val magic_stringAppend = magic Ustring.stringAppend_
val magic_stringLength = magic Ustring.stringLength_
val magic_fromCharCode = magic Ustring.fromCharCode_
val magic_charCodeAt = magic Ustring.charCodeAt_
val magic_fnLength = magic Ustring.fnLength_
val magic_apply = magic Ustring.apply_
val magic_bindString = magic Ustring.bindString_
val magic_bindBoolean = magic Ustring.bindBoolean_
val magic_bindDecimal = magic Ustring.bindDecimal_
val magic_bindDouble = magic Ustring.bindDouble_
val magic_bindUInt = magic Ustring.bindUInt_
val magic_bindInt = magic Ustring.bindInt_
val magic_setPropertyIsDontEnum = magic Ustring.setPropertyIsDontEnum_
val magic_getPropertyIsDontDelete = magic Ustring.getPropertyIsDontDelete_
val magic_getPropertyIsDontEnum = magic Ustring.getPropertyIsDontEnum_
val magic_hasOwnProperty = magic Ustring.hasOwnProperty_
val magic_getPrototype = magic Ustring.getPrototype_
val magic_getClassName = magic Ustring.getClassName_
val magic_construct = magic Ustring.construct_

end
