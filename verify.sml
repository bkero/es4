(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Verify = struct

open LogErr

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[verify] " :: ss) else ()
fun error ss = LogErr.verifyError ss

(* 
    Verify a program
 *)

type CONTEXT = Ast.FIXTURES

type ENV = { returnType: Ast.TYPE_EXPR option,
             context: CONTEXT }

fun withReturnType { returnType=_, context } returnType =
    { returnType=returnType, context=context }

fun withContext { returnType, context=_ } context =
    { returnType=returnType, context=context }

(*
    HEAD
*)

and verifyHead (env:ENV) (h:Ast.HEAD)
    : Ast.HEAD = h

(*
    EXPR
*)

and verifyExpr (env:ENV) 
               (expr:Ast.EXPR) 
    : (Ast.EXPR * Ast.TYPE_EXPR) = 
    let
        fun verifySub e = 
            let
                val (e,t) = verifyExpr env e
            in
                e
            end
        val dummyType = Ast.SpecialType Ast.Any
    in
        case expr of 
            Ast.TernaryExpr (t, e1, e2, e3) => 
            (Ast.TernaryExpr (t, e1, e2, e3), dummyType)
            
          | Ast.BinaryExpr (b, e1, e2) => 
            (Ast.BinaryExpr (b, e1, e2), dummyType)
            
          | Ast.BinaryTypeExpr (b, e, te) =>
            (Ast.BinaryTypeExpr (b, e, te), dummyType)

          | Ast.UnaryExpr (u, e) =>
            (Ast.UnaryExpr (u, e), dummyType)
                         
          | Ast.TypeExpr t => 
            (Ast.TypeExpr t, dummyType)

          | Ast.ThisExpr => 
            (Ast.ThisExpr, dummyType)

          | Ast.YieldExpr eo => 
            (Ast.YieldExpr eo, dummyType)

          | Ast.SuperExpr eo => 
            (Ast.SuperExpr eo, dummyType)
            
          | Ast.LiteralExpr le => 
            (Ast.LiteralExpr le, dummyType)
            
          | Ast.CallExpr {func, actuals} => 
            (Ast.CallExpr {func = verifySub func,
                          actuals = map verifySub actuals}, dummyType)

          | Ast.ApplyTypeExpr { expr, actuals } =>
            (Ast.ApplyTypeExpr { expr = expr,
                                actuals = actuals }, dummyType)

          | Ast.LetExpr le => 
            (Ast.LetExpr le, dummyType)

          | Ast.NewExpr { obj, actuals } => 
            (Ast.NewExpr { obj = verifySub obj,
                          actuals = map verifySub actuals }, dummyType)

          | Ast.ObjectRef { base, ident, pos } =>
            (Ast.ObjectRef { base=base, ident=ident, pos=pos }, dummyType)

          | Ast.LexicalRef { ident, pos } => 
            (Ast.LexicalRef { ident=ident, pos=pos }, dummyType)

          | Ast.SetExpr (a, le, re) => 
            (Ast.SetExpr (a, le, re), dummyType)

          | Ast.GetTemp n => 
            (Ast.GetTemp n, dummyType)

          | Ast.GetParam n => 
            (Ast.GetParam n, dummyType)

          | Ast.ListExpr es => 
            (Ast.ListExpr (map verifySub es), dummyType)

          | Ast.SliceExpr (a, b, c) => 
            (Ast.SliceExpr (verifySub a, verifySub b, verifySub c), dummyType)

          | Ast.InitExpr ie =>
            (Ast.InitExpr ie, dummyType)
    end
    
    
and verifyExprs (env:ENV) 
             (exprs:Ast.EXPR list) 
    : Ast.EXPR list * Ast.TYPE_EXPR list = 
    let
        val es = ListPair.unzip (map (verifyExpr env) exprs)
    in
        es
    end





(*
    STMT
*)

and verifyStmt (env:ENV) 
            (stmt:Ast.STMT) 
    : Ast.STMT = 
    let
    in
        case stmt of
            Ast.EmptyStmt => 
            Ast.EmptyStmt
         
          | Ast.ExprStmt e => 
            let
                val (expr,ty) = verifyExpr env e
            in
                Ast.ExprStmt expr
            end

          | Ast.ForInStmt fe => 
            Ast.ForInStmt fe
            
          | Ast.ThrowStmt es =>
            Ast.ThrowStmt es
            
          | Ast.ReturnStmt es =>
            Ast.ReturnStmt es
            
          | Ast.BreakStmt i => 
            Ast.BreakStmt i

          | Ast.ContinueStmt i =>
            Ast.ContinueStmt i

          | Ast.BlockStmt b =>
            Ast.BlockStmt b 
            
          | Ast.ClassBlock cb =>
            Ast.ClassBlock cb
            
          | Ast.LabeledStmt (id, s) =>
            Ast.LabeledStmt (id, s)
            
          | Ast.LetStmt b =>
            Ast.LetStmt b
            
          | Ast.WhileStmt w => 
            Ast.WhileStmt w
            
          | Ast.DoWhileStmt w => 
            Ast.DoWhileStmt w
            
          | Ast.ForStmt f => 
            Ast.ForStmt f

          | Ast.IfStmt {cnd, els, thn} => 
            Ast.IfStmt {cnd=cnd, els=els, thn=thn}
            
          | Ast.WithStmt {obj, ty, body} =>
            Ast.WithStmt {obj=obj, ty=ty, body=body}
        
          | Ast.TryStmt {block, catches, finally} =>
            Ast.TryStmt {block=block, catches=catches, finally=finally}
        
          | Ast.SwitchStmt {cond, cases, mode, labels} => 
            Ast.SwitchStmt {cond=cond, cases=cases, mode=mode, labels=labels}
        
          | Ast.SwitchTypeStmt {cond, ty, cases} =>
            Ast.SwitchTypeStmt {cond=cond, ty=ty, cases=cases}
            
          | Ast.Dxns x => 
            Ast.Dxns x

    end

and verifyStmts (env) (stmts:Ast.STMT list)
    : Ast.STMT list =
    case stmts of
        (stmt::stmts) =>
            let 
                val s = verifyStmt env stmt
                val ss = verifyStmts env stmts
            in
                s::ss
            end
      | [] => []

(*
    BLOCK
*)

and verifyBlock (env:ENV) 
             (b:Ast.BLOCK) 
    : Ast.BLOCK =
    let
    in case b of
        Ast.Block { head, body, pos, pragmas=[], defns=[] } =>
            let
                val _ = LogErr.setPos pos
                val head = case head of SOME h => verifyHead env h 
                                      | _ => ([],[])
                val body = verifyStmts env body
            in
                Ast.Block { pragmas = [],
                            defns = [],
                            body = body,
                            head = SOME head,
                            pos = pos }
            end
      | _ => internalError ["defn did not remove pragmas and definitions"]
    end


(*
    PROGRAM
*)

and topEnv () = { context = !Defn.topFixtures,
                  returnType = NONE }

and verifyPackage (p:Ast.PACKAGE)
    : Ast.PACKAGE =
    raise UnimplError

and verifyProgram (p:Ast.PROGRAM) 
    : Ast.PROGRAM =
    let 
    in case p of
        { packages, fixtures, block } =>
            let
                val _ = LogErr.setPos NONE
                val e = topEnv ()
                val block = verifyBlock e block
                val result = { packages = packages,
                               block = block,
                               fixtures = fixtures }
            in
                trace ["verification complete"];
                (if !doTrace 
                 then Pretty.ppProgram result
                 else ());
                result
            end
    end
end
