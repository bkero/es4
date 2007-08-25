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

(* 
 * This module contains only functions that depend directly on Ast. It
 * provides common infrastructure for inquiring about AST nodes in concise
 * terms, without resorting to code duplication or awkward cyclical module
 * dependencies. 
 *)

structure AstQuery = struct

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[ast-query] " :: ss) else ()
fun error ss = LogErr.parseError ss

fun typeExprOf (ty:Ast.TY) : Ast.TYPE_EXPR = 
    let
        val Ast.Ty { expr, ... } = ty
    in
        expr
    end

fun funcTy (func:Ast.FUNC)
    : Ast.TY = 
    let
	    val Ast.Func { ty, ... } = func
    in
	    ty
    end

fun funcTypeParams (func:Ast.FUNC)
    : Ast.IDENT list = 
    let
	    val Ast.Func { fsig = Ast.FunctionSignature { typeParams, ... }, ... } = func
    in
	    typeParams
    end


fun extractFuncType (ty:Ast.TY) 
    : (Ast.FUNC_TYPE * Ast.RIBS * Ast.UNIT_NAME option) = 
    case ty of 
        Ast.Ty {expr=Ast.FunctionType fty, nonTopRibs, topUnit} => 
        (fty, nonTopRibs, topUnit)
      | _ => error ["extracting FunctionType from non-FunctionType"]


fun extractInstanceType (ty:Ast.TY) 
    : (Ast.INSTANCE_TYPE * Ast.RIBS * Ast.UNIT_NAME option) = 
    case ty of 
        Ast.Ty {expr=Ast.InstanceType ity, nonTopRibs, topUnit} => 
        (ity, nonTopRibs, topUnit)
      | _ => error ["extracting InstanceType from non-InstanceType"]


fun lift (q:('a -> Ast.TYPE_EXPR))
         (x:(Ast.TY -> ('a * Ast.RIBS * (Ast.UNIT_NAME option))))
         (ty:Ast.TY) 
    : (Ast.TY) =
    let 
        val (a, nonTopRibs, topUnit) = x ty
    in
        Ast.Ty { expr = (q a),
                 nonTopRibs = nonTopRibs,
                 topUnit = topUnit }
    end

and liftOpt (q:('a -> Ast.TYPE_EXPR option))
            (x:(Ast.TY -> ('a * Ast.RIBS * (Ast.UNIT_NAME option))))
            (ty:Ast.TY) 
    : (Ast.TY option) =
    let
        val (a, nonTopRibs, topUnit) = x ty
    in
        case q a of 
            NONE => NONE
          | SOME expr => 
            SOME (Ast.Ty { expr = expr,
                           nonTopRibs = nonTopRibs,
                           topUnit = topUnit })
    end
    
val conversionTyOfInstanceTy = liftOpt (#conversionTy) extractInstanceType
val resultTyOfFuncTy = lift (#result) extractFuncType
val resultTypeOfFuncTy = typeExprOf o resultTyOfFuncTy

fun paramTypesOfFuncTy (funcTy:Ast.TY)
    : Ast.TYPE_EXPR list = 
        case funcTy of 
            Ast.Ty { expr = Ast.FunctionType { params, ... }, ... } => params
          | _ => error ["paramTypesOfFuncTy: expected Ty with FunctionType"]


fun singleParamTyOfFuncTy (ty:Ast.TY) 
    : Ast.TY = 
    let
        val (funcTy, nonTopRibs, topUnit) = extractFuncType ty                                            
    in 
        case (#params funcTy) of
            [t] => Ast.Ty { expr=t,
                            nonTopRibs = nonTopRibs,
                            topUnit=topUnit }
          | _ => error ["singleParamTyOfFuncTy: non-unique parameter"]
    end    
                 
fun funcIsAbstract (func:Ast.FUNC) 
    : bool = 
    case func of 
	Ast.Func { block = NONE, ... } => true
      | _ => false
                 
end
