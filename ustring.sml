(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 * 
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 * 
 *    2. All liability and responsibility for the implementation or other
 * use of this Reference Implementation rests with the implementor, and
 * not with any of the parties who contribute to, or who own or hold any
 * copyright in, this Reference Implementation.
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

structure Ustring = struct

type WCHAR  = Word.word
type STRING = WCHAR vector
type SOURCE = WCHAR list


(*
 * Internal functions with intentionally ugly names
 * (only these guys know what a STRING really is)
 *)


exception BadUTF8Encoding

(*
 *  Unicode value             1st byte   2nd byte   3rd byte   4th byte
 *  -----------------------   --------   --------   --------   --------
 *  00000 00000000 0xxxxxxx   0xxxxxxx
 *  00000 00000yyy yyxxxxxx   110yyyyy   10xxxxxx
 *  00000 zzzzyyyy yyxxxxxx   1110zzzz   10yyyyyy   10xxxxxx
 *  uuuzz zzzzyyyy yyxxxxxx   11110uuu   10zzzzzz   10yyyyyy   10xxxxxx
 *)
fun internal_explode s =
let
    fun grabSixBits (w, [   ]) = raise BadUTF8Encoding
      | grabSixBits (w, c::cs) =
    let
        val b = Word.fromInt (Char.ord c)
    in
        if   (Word.andb (0wxC0, b) = 0wx80)
        then (Word.orb (Word.<< (w, 0wx06), Word.andb (0wx3F, b)), cs)
        else raise BadUTF8Encoding
    end
    
    fun convert ([   ], acc) = acc
      | convert (c::cs, acc) =
    let
        val w = Word.fromInt (Char.ord c)
        val (w_, cs_) =
            if   w < 0wx80
            then (w, cs)
            else if Word.andb (0wxE0, w) = 0wxC0
            then                           grabSixBits (Word.andb (0wx1F, w), cs)
            else if Word.andb (0wxF0, w) = 0wxE0
            then              grabSixBits (grabSixBits (Word.andb (0wx0F, w), cs))
            else if Word.andb (0wxF8, w) = 0wxF0
            then grabSixBits (grabSixBits (grabSixBits (Word.andb (0wx07, w), cs)))
            else raise BadUTF8Encoding
    in
        convert (cs_, w_::acc)
    end
in
    rev (convert (explode s, []))
end


fun internal_wcharFromChar c = Word.andb(0wx7f, Word.fromInt(Char.ord c))

fun internal_wcharIsChar c = c < 0wx80

fun internal_wcharToChar c = Char.chr(Word.toInt(Word.andb(0wx7f, c)))

fun internal_fromString s = Vector.fromList (internal_explode s)

fun internal_fromSource s = internal_explode s

fun internal_wcharToString c = if internal_wcharIsChar c
                               then Char.toCString(internal_wcharToChar c)
                               else "\\u" ^ (StringCvt.padLeft #"0" 4 (Word.toString c))


fun internal_toEscapedAscii us =
    let
	fun esc (c, ls) = 
	    if internal_wcharIsChar c then
		(internal_wcharToChar c) :: ls
	    else
		(List.rev (explode (internal_wcharToString c))) @ ls
    in
	implode (List.rev (Vector.foldl esc [] us))
    end

fun fixNegative s = 
    if String.sub (s, 0) = #"~"
    then "-" ^ String.extract (s, 1, NONE)
    else s

fun internal_fromInt n = internal_fromString (fixNegative (Int.toString n))

fun internal_fromInt32 n = internal_fromString (fixNegative (Int32.toString n))

fun internal_fromCharCode n = #[Word.fromInt n]

fun internal_stringLength us = Vector.length us

fun internal_stringAppend a b = Vector.concat [a, b]

fun internal_charCodeAt us n = Word.toInt (Vector.sub (us, n))

fun internal_compare a b = Vector.collate Word.compare (a,b)

fun internal_substring us m n = VectorSlice.vector (VectorSlice.slice(us, m, SOME n))

fun internal_append uss = Vector.concat uss

fun internal_sourceFromUstring s = Vector.foldr (op ::) [] s

(*
 * Public interface
 *)

fun isWs (0wx0009) (* <TAB> *) = true
  | isWs (0wx0020) (* <SPACE> *) = true
  | isWs (0wx00A0) (* <NBSP> *) = true
  | isWs (0wx000C) (* <FF> *) = true
  | isWs (0wx000B) (* <VT> *) = true
  | isWs (0wx000D) (* <CR> *) = true
  | isWs (0wx000A) (* <LF> *) = true
  | isWs (0wx2028) (* <LS> *) = true
  | isWs (0wx2029) (* <PS> *) = true
				
  (* <USP> = "all other unicode Zs whitespace chars" *)
  | isWs (0wx1680) (* <OGHAM SPACE MARK> *) = true
  | isWs (0wx180E) (* <MONGOLIAN VOWEL SEPARATOR> *) = true
  | isWs (0wx2000) (* <EN QUAD> *) = true
  | isWs (0wx2001) (* <EM QUAD> *) = true
  | isWs (0wx2002) (* <EN SPACE> *) = true
  | isWs (0wx2003) (* <EM SPACE> *) = true
  | isWs (0wx2004) (* <THREE-PER-EM SPACE> *) = true
  | isWs (0wx2005) (* <FOUR-PER-EM SPACE> *) = true
  | isWs (0wx2006) (* <SIX-PER-EM SPACE> *) = true
  | isWs (0wx2007) (* <FIGURE SPACE> *) = true
  | isWs (0wx2008) (* <PUNCTUATION SPACE> *) = true
  | isWs (0wx2009) (* <THIN SPACE> *) = true
  | isWs (0wx200A) (* <HAIR SPACE> *) = true
  | isWs (0wx202F) (* <NARROW NO-BREAK SPACE> *) = true
  | isWs (0wx205F) (* <MEDIUM MATHEMATICAL SPACE> *) = true
  | isWs (0wx3000) (* <IDEOGRAPHIC SPACE> *) = true

  | isWs _ = false

fun explode (s:STRING) : WCHAR list = Vector.foldr (List.::) [] s

fun wcharFromChar (c:char ) : WCHAR = internal_wcharFromChar c
fun wcharIsChar   (c:WCHAR) : bool  = internal_wcharIsChar   c
fun wcharToChar   (c:WCHAR) : char  = internal_wcharToChar   c

fun toAscii      (s:STRING   ) : string = internal_toEscapedAscii s
fun toFilename   (s:STRING   ) : string = internal_toEscapedAscii s  (* FIXME: what should I do here? *)

fun fromString   (s:string   ) : STRING = internal_fromString   s  (* Should be used sparingly. *)
fun fromSource   (s:string   ) : SOURCE = internal_fromSource   s
fun fromInt      (i:int      ) : STRING = internal_fromInt      i
fun fromInt32    (i:Int32.int) : STRING = internal_fromInt32    i
fun fromCharCode (i:int      ) : STRING = internal_fromCharCode i

fun stringLength (s:STRING   ) : int    = internal_stringLength s

fun stringAppend (a:STRING) (b:STRING) : STRING = internal_stringAppend a b
fun charCodeAt   (s:STRING) (i:int   ) : int    = internal_charCodeAt   s i
fun charCodeOf   (c:WCHAR) : int = Word.toInt c
fun compare      (a:STRING) (b:STRING) : order  = internal_compare      a b

fun substring    (s:STRING) (m:int) (n:int) : STRING = internal_substring s m n

fun append       (l:STRING list) : STRING = internal_append l

fun sourceFromUstring (s:STRING) : SOURCE = internal_sourceFromUstring s


(*
 * pre-defined strings
 *)

val empty                    = fromString ""
val zero                     = fromString "0"
val asterisk                 = fromString "*"
val dollar                   = fromString "$"
val dot                      = fromString "."
val comma                    = fromString ","
val dash                     = fromString "-"
val lparen                   = fromString "("
val rparen                   = fromString ")"
val lbrace                   = fromString "{"
val rbrace                   = fromString "}"

val undefined_               = fromString "undefined"
val temp_                    = fromString "temp"
val Object_                  = fromString "Object"
val object_                  = fromString "object"
val has_                     = fromString "has"
val call_                    = fromString "call"
val convert_                 = fromString "convert"
val get_                     = fromString "get"
val set_                     = fromString "set"
val type_                    = fromString "type"
val number_                  = fromString "number"
val boolean_                 = fromString "boolean"
val function_                = fromString "function"
val string_                  = fromString "string"
val null_                    = fromString "null"
val constructor_             = fromString "constructor"
val length_                  = fromString "length"
val source_                  = fromString "source"
val prototype_               = fromString "prototype"
val toString_                = fromString "toString"
val valueOf_                 = fromString "valueOf"
val global_                  = fromString "global"
val invoke_                  = fromString "invoke"
val this_                    = fromString "this"
val arguments_               = fromString "arguments"
val Type_                    = fromString "Type"
val Interface_               = fromString "Interface"
val Class_                   = fromString "Class"
val Namespace_               = fromString "Namespace"
val Name_                    = fromString "Name"
val qualifier_               = fromString "qualifier"
val identifier_              = fromString "identifier"
val Error_                   = fromString "Error"
val EvalError_               = fromString "EvalError"
val RangeError_              = fromString "RangeError"
val ReferenceError_          = fromString "ReferenceError"
val SyntaxError_             = fromString "SyntaxError"
val TypeError_               = fromString "TypeError"
val ToPrimitive_             = fromString "ToPrimitive"
val ToBoolean_               = fromString "ToBoolean"
val ToInteger_               = fromString "ToInteger"
val ToInt_                   = fromString "ToInt"
val ToUint_                  = fromString "ToUint"
val ToDouble_                = fromString "ToDouble"
val ToDecimal_               = fromString "ToDecimal"
val ToString_                = fromString "ToString"
val ToObject_                = fromString "ToObject"
val ToNumeric_               = fromString "ToNumeric"
val NaN_                     = fromString "NaN"
val Infinity_                = fromString "Infinity"
val eval_                    = fromString "eval"
val parseInt_                = fromString "parseInt"
val parseFloat_              = fromString "parseFloat"
val isNaN_                   = fromString "isNaN"
val isFinite_                = fromString "isFinite"
val decodeURI_               = fromString "decodeURI"
val decodeURIComponent_      = fromString "decodeURIComponent"
val encodeURI_               = fromString "encodeURI"
val encodeURIComponent_      = fromString "encodeURIComponent"
val JSON_                    = fromString "JSON"
val print_                   = fromString "print"
val load_                    = fromString "load"
val assert_                  = fromString "assert"
val typename_                = fromString "typename"
val Function_                = fromString "Function"
val Boolean_                 = fromString "Boolean"
val Number_                  = fromString "Number"
val double_                  = fromString "double"
val int_                     = fromString "int"
val uint_                    = fromString "uint"
val decimal_                 = fromString "decimal"
val Numeric_                 = fromString "Numeric"
val String_                  = fromString "String"
val Array_                   = fromString "Array"
val ByteArray_               = fromString "ByteArray"
val Date_                    = fromString "Date"
val RegExp_                  = fromString "RegExp"
val regexp_                  = fromString "regexp"
val exception_               = fromString "exception"
val emit_                    = fromString "emit"
val parse_                   = fromString "parse"
val magic_                   = fromString "magic"
val meta_                    = fromString "meta"
val cursor_                  = fromString "cursor"
val native_function_         = fromString "native function"
val interface_               = fromString "interface"
val class_                   = fromString "class"
val namespace_               = fromString "namespace"
val bool_                    = fromString "bool"
val bytearray_               = fromString "bytearray"
val proto_                   = fromString "proto"
val dumpFunc_                = fromString "dumpFunc"
val inspect_                 = fromString "inspect"
val tan_                     = fromString "tan"
val sqrt_                    = fromString "sqrt"
val sin_                     = fromString "sin"
val round_                   = fromString "round"
val random_                  = fromString "random"
val pow_                     = fromString "pow"
val log_                     = fromString "log"
val floor_                   = fromString "floor"
val exp_                     = fromString "exp"
val cos_                     = fromString "cos"
val ceil_                    = fromString "ceil"
val atan2_                   = fromString "atan2"
val atan_                    = fromString "atan"
val asin_                    = fromString "asin"
val acos_                    = fromString "acos"
val abs_                     = fromString "abs"
val now_                     = fromString "now"
val toPrecision_             = fromString "toPrecision"
val toExponential_           = fromString "toExponential"
val toFixedStep10_           = fromString "toFixedStep10"
val DaylightSavingsTA_       = fromString "DaylightSavingsTA"
val LocalTZA_                = fromString "LocalTZA"
val setByteArrayByte_        = fromString "setByteArrayByte"
val getByteArrayByte_        = fromString "getByteArrayByte"
val stringAppend_            = fromString "stringAppend"
val stringLength_            = fromString "stringLength"
val fromCharCode_            = fromString "fromCharCode"
val charCodeAt_              = fromString "charCodeAt"
val fnLength_                = fromString "fnLength"
val apply_                   = fromString "apply"
val bindString_              = fromString "bindString"
val bindBoolean_             = fromString "bindBoolean"
val bindDecimal_             = fromString "bindDecimal"
val bindDouble_              = fromString "bindDouble"
val bindUInt_                = fromString "bindUInt"
val bindInt_                 = fromString "bindInt"
val newDouble_               = fromString "newDouble"
val newUInt_                 = fromString "newUInt"
val newInt_                  = fromString "newInt"
val toPrimitive_             = fromString "toPrimitive"
val isPrimitive_             = fromString "isPrimitive"
val defaultValue_            = fromString "defaultValue"
val setPropertyIsDontEnum_   = fromString "setPropertyIsDontEnum"
val getPropertyIsDontDelete_ = fromString "getPropertyIsDontDelete"
val getPropertyIsDontEnum_   = fromString "getPropertyIsDontEnum"
val hasOwnProperty_          = fromString "hasOwnProperty"
val getPrototype_            = fromString "getPrototype"
val getClassName_            = fromString "getClassName"
val construct_               = fromString "construct"
val true_                    = fromString "true"
val false_                   = fromString "false"
val x_                       = fromString "x"

val emptySource              = []


end
