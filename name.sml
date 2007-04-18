structure Name = struct

val (anonNsCounter:int ref) = ref 0
fun newAnonNS _ = 
    (* safe: will raise Overflow when we run out of namespaces. *)
    (anonNsCounter := (!anonNsCounter) + 1;
     (Ast.AnonUserNamespace (!anonNsCounter)))
    
val metaNS = newAnonNS ()
val magicNS = newAnonNS ()
val publicNS = Ast.Public ""
val internalNS = Ast.Internal ""
val intrinsicNS = Ast.Intrinsic

fun meta id = { id = id, ns = metaNS }
fun magic id = { id = id, ns = magicNS }
fun public id = { id = id, ns = publicNS }
fun internal id = { id = id, ns = internalNS }
fun intrinsic id = { id = id, ns = internalNS }

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

val public_constructor = public "constructor"
val public_length = public "length"
val public_source = public "source"
val public_prototype = public "prototype"
val public_toString = public "toString"
val public_valueOf = public "valueOf"
val public_global = public "global"
val meta_invoke = meta "invoke"
val meta_get = meta "get"
val meta_set = meta "set"
val meta_has = meta "has"
val meta_call = meta "call"

val this = internal "this"
val arguments = internal "arguments"

(* 
 * Names that are supposed to be present in the global scope 
 * once we finish booting.
 *)

(* From Type.es *)
val public_Type = public "Type"

(* From Interface.es *)
val public_Interface = public "Interface"

(* From Class.es *)
val public_Class = public "Class"

(* From Namespace.es *)
val public_Namespace = public "Namespace"

(* From Object.es *)
val public_Object = public "Object"

(* From Error.es *)
val public_Error = public "Error"
val public_EvalError = public "EvalError"
val public_RangeError = public "RangeError"
val public_ReferenceError = public "ReferenceError"
val public_SyntaxError = public "SyntaxError"
val public_TypeError = public "TypeError"

(* From Conversions.es *)
val intrinsic_ToPrimitive = intrinsic "ToPrimitive"
val intrinsic_ToBoolean = intrinsic "ToBoolean"
val intrinsic_ToInteger = intrinsic "ToInteger"
val intrinsic_ToInt = intrinsic "ToInt"
val intrinsic_ToUint = intrinsic "ToUint"
val intrinsic_ToDouble = intrinsic "ToDouble"
val intrinsic_ToDecimal = intrinsic "ToDecimal"
val intrinsic_ToString = intrinsic "ToString"
val intrinsic_ToObject = intrinsic "ToObject"

(* From Global.es *)
val intrinsic_NaN = intrinsic "NaN"
val intrinsic_Infinity = intrinsic "Infinity"
val intrinsic_undefined = intrinsic "undefined"
val intrinsic_eval = intrinsic "eval"
val intrinsic_parseInt = intrinsic "parseInt"
val intrinsic_parseFloat = intrinsic "parseFloat"
val intrinsic_isNaN = intrinsic "isNaN"
val intrinsic_isFinite = intrinsic "isFinite"
val intrinsic_decodeURI = intrinsic "decodeURI"
val intrinsic_decodeURIComponent = intrinsic "decodeURIComponent"
val intrinsic_encodeURI = intrinsic "encodeURI"
val intrinsic_encodeURIComponent = intrinsic "encodeURIComponent"

val public_NaN = public "NaN"
val public_Infinity = public "Infinity"
val public_undefined = public "undefined"
val public_eval = public "eval"
val public_parseInt = public "parseInt"
val public_parseFloat = public "parseFloat"
val public_isNaN = public "isNaN"
val public_isFinite = public "isFinite"
val public_decodeURI = public "decodeURI"
val public_decodeURIComponent = public "decodeURIComponent"
val public_encodeURI = public "encodeURI"
val public_encodeURIComponent = public "encodeURIComponent"

val intrinsic_print = intrinsic "print"
val intrinsic_assert = intrinsic "assert"
val intrinsic_typename = intrinsic "typename"

(* From Function.es *)
val public_Function = public "Function"
          
(* From Boolean.es *)
val public_Boolean = public "Boolean"

(* From boolean_primitive.es *) 
val public_boolean = public "boolean"

(* From Number.es *)
val public_Number = public "Number"

(* From double.es *)
val public_double = public "double"

(* From int.es *)
val public_int = public "int"

(* From uint.es *)
val public_uint = public "uint"

(* From decimal.es *)
val public_decimal = public "decimal"

(* From Numeric.es *)
val public_Numeric = public "Numeric"

(* From String.es *)
val public_String = public "String"

(* From string_primitive.es *)
val public_string = public "string"

(* From Array.es *)
val public_Array = public "Array"

(* From ByteArray.es *)
val public_ByteArray = public "ByteArray"

(* From Date.es *)
val public_Date = public "Date"

(* From JSON.es *)
val JSON_emit = { ns = Ast.Public "JSON", id = "emit" }
val JSON_parse = { ns = Ast.Public "JSON", id = "parse" }

end
