structure Name = struct

val (anonNsCounter:int ref) = ref 0
fun newAnonNS _ = 
    (* safe: will raise Overflow when we run out of namespaces. *)
    (anonNsCounter := (!anonNsCounter) + 1;
     (Ast.AnonUserNamespace (!anonNsCounter)))
    
val metaNS = newAnonNS ()
val magicNS = newAnonNS ()
val noNS = Ast.Public (Ustring.empty)
val intrinsicNS = Ast.Intrinsic

(* 
 * FIXME: mangling a name into a string is bad form. We really want to 
 * have derived namespaces like Private and Protected refer to their
 * containing namespace-qualified name of their containing class, rather 
 * than the mangled form of their containing class.
 *)
fun mangle (n:Ast.NAME) : Ast.IDENT = 
    Ustring.fromString (LogErr.name n)

fun make (id:Ast.IDENT) (ns:Ast.NAMESPACE) : Ast.NAME = { id = id, ns = ns }
fun meta (id:Ast.IDENT) : Ast.NAME = { id = id, ns = metaNS }
fun magic (id:Ast.IDENT) : Ast.NAME = { id = id, ns = magicNS }
fun intrinsic (id:Ast.IDENT) : Ast.NAME = { id = id, ns = intrinsicNS }
fun nons (id:Ast.IDENT) : Ast.NAME = { id = id, ns = noNS }
fun public (cls:Ast.NAME) (id:Ast.IDENT) : Ast.NAME = { id = id, ns = Ast.Public (mangle cls) }
fun private (cls:Ast.NAME) (id:Ast.IDENT) : Ast.NAME = { id = id, ns = Ast.Private (mangle cls) }

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

(* From Type.es *)
val intrinsic_Type = intrinsic Ustring.Type_

(* From Interface.es *)
val intrinsic_Interface = intrinsic Ustring.Interface_

(* From Class.es *)
val intrinsic_Class = intrinsic Ustring.Class_

(* From Namespace.es *)
val intrinsic_Namespace = intrinsic Ustring.Namespace_

(* From Name.es *)
val intrinsic_Name = intrinsic Ustring.Name_
val public_qualifier = public Ustring.qualifier_
val public_identifier = public Ustring.identifier_

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

(* From Function.es *)
val nons_Function = nons Ustring.Function_

(* From Boolean.es *)
val nons_Boolean = nons Ustring.Boolean_

(* From boolean_primitive.es *) 
val intrinsic_boolean = intrinsic Ustring.boolean_

(* From Number.es *)
val nons_Number = nons Ustring.Number_

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
val nons_String = nons Ustring.String_

(* From string_primitive.es *)
val intrinsic_string = intrinsic Ustring.string_

(* From Array.es *)
val nons_Array = nons Ustring.Array_

(* From ByteArray.es *)
val intrinsic_ByteArray = intrinsic Ustring.ByteArray_

(* From Date.es *)
val nons_Date = nons Ustring.Date_

(* From RegExp.es *)
val nons_RegExp = nons Ustring.RegExp_

(* From JSON.es *)
val JSON_emit = { ns = Ast.Public Ustring.JSON_, id = Ustring.emit_ }
val JSON_parse = { ns = Ast.Public Ustring.JSON_, id = Ustring.parse_ }


(*
 * Natives
 *)

val intrinsic_proto = intrinsic Ustring.proto_
val intrinsic_dumpFunc = intrinsic Ustring.dumpFunc_
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

val nons_DaylightSavingsTA = nons Ustring.DaylightSavingsTA_
val nons_LocalTZA = nons Ustring.LocalTZA_

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

val magic_newInt = magic Ustring.newInt_
val magic_newUInt = magic Ustring.newUInt_
val magic_newDouble = magic Ustring.newDouble_

val magic_toPrimitive = magic Ustring.toPrimitive_
val magic_isPrimitive = magic Ustring.isPrimitive_
val magic_defaultValue = magic Ustring.defaultValue_
val magic_setPropertyIsDontEnum = magic Ustring.setPropertyIsDontEnum_
val magic_getPropertyIsDontDelete = magic Ustring.getPropertyIsDontDelete_
val magic_getPropertyIsDontEnum = magic Ustring.getPropertyIsDontEnum_
val magic_hasOwnProperty = magic Ustring.hasOwnProperty_
val magic_getPrototype = magic Ustring.getPrototype_
val magic_getClassName = magic Ustring.getClassName_
val magic_construct = magic Ustring.construct_

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
val meta_convert = meta Ustring.convert_

val this = nons Ustring.this_
val arguments = nons Ustring.arguments_
val empty = nons Ustring.empty

val meta_ = nons Ustring.meta_
val magic_ = nons Ustring.magic_

end
