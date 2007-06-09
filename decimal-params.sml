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

structure DecimalParams = struct

datatype DEC = Dec of string

exception DecimalException of string

datatype ROUNDING_MODE =
         Ceiling
       | Floor
       | Up
       | Down
       | HalfUp
       | HalfDown
       | HalfEven

fun rmToString (rm:ROUNDING_MODE) 
    : string = 
    case rm of 
         Ceiling => "Ceiling"
       | Floor => "Floor"
       | Up => "Up"
       | Down => "Down"
       | HalfUp => "HalfUp"
       | HalfDown => "HalfDown"
       | HalfEven => "HalfEven"	

datatype DECIMAL_OPERATOR =
         Abs
       | Add
       | Compare
       | CompareTotal
       | Divide
       | DivideInteger
       | Exp
       | Ln
       | Log10
       | Max
       | Min
       | Minus
       | Multiply
       | Normalize
       | Plus
       | Power
       | Quantize
       | Remainder
       | RemainderNear
       | Rescale
       | SameQuantum
       | SquareRoot
       | Subtract
       | ToIntegralValue

fun opToString (dop:DECIMAL_OPERATOR)
    : string =
    case dop of
         Abs => "abs"
       | Add => "add"
       | Compare => "compare"
       | CompareTotal => "compareTotal"
       | Divide => "divide"
       | DivideInteger => "divideInteger"
       | Exp => "exp"
       | Ln => "ln"
       | Log10 => "log10"
       | Max => "max"
       | Min => "min"
       | Minus => "minus"
       | Multiply => "multiply"
       | Normalize => "normalize"
       | Plus => "plus"
       | Power => "power"
       | Quantize => "quantize"
       | Remainder => "remainder"
       | RemainderNear => "remainderNear"
       | Rescale => "rescale"
       | SameQuantum => "sameQuantum"
       | SquareRoot => "squareRoot"
       | Subtract => "subtract"
       | ToIntegralValue => "toIntegralValue"

end
