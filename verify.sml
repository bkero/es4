(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Verify = struct

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[verify] " :: ss) else ()
fun error ss = LogErr.verifyError ss

(* 
    Verify a program
 *)

datatype LABEL_KIND =
          IterationLabel
        | SwitchLabel
        | StatementLabel

type LABEL = (Ast.IDENT * LABEL_KIND)

type CONTEXT = { fixtures: Ast.FIXTURES }

type ENV = CONTEXT list

fun dumpEnv (e:ENV) : unit =
    case e of
        {fixtures,...}::p => if (!doTrace) then (Pretty.ppFixtures fixtures; dumpEnv p) else ()
      | _ => ()

val (topFixtures:Ast.FIXTURES ref) = ref []

fun resetTopFixtures _ = 
    topFixtures := [ (Ast.PropName (Name.public "meta"), Ast.NamespaceFixture Name.metaNS),
                     (Ast.PropName (Name.public "magic"), Ast.NamespaceFixture Name.magicNS) ]

fun hasFixture (b:Ast.FIXTURES) 
               (n:Ast.FIXTURE_NAME) 
    : bool = 
    let 
        fun search [] = false
          | search ((k,v)::bs) = 
            if k = n 
            then true
            else search bs
    in
        search b    
    end

fun hasNamespace (nl:Ast.NAMESPACE list) 
                 (n:Ast.NAMESPACE) 
    : bool = 
    let 
        fun search [] = false
          | search (first::rest) = 
            if n = first
            then true
            else search rest
    in
        search nl    
    end


fun getFixture (b:Ast.FIXTURES) 
               (n:Ast.FIXTURE_NAME) 
    : Ast.FIXTURE = 
    let 
        fun search [] = LogErr.hostError ["fixture binding not found: ", 
                                          (LogErr.fname n)]
          | search ((k,v)::bs) = 
            if k = n 
            then v
            else search bs
    in
        search b
    end


fun replaceFixture (b:Ast.FIXTURES) 
                   (n:Ast.FIXTURE_NAME) 
                   (v:Ast.FIXTURE)
    : Ast.FIXTURES = 
    let 
        fun search [] = LogErr.hostError ["fixture binding not found: ", 
                                          (LogErr.fname n)]
          | search ((k,v0)::bs) = 
            if k = n 
            then (k,v) :: bs
            else (k,v0) :: (search bs)
    in
        search b
    end


(*
    resolve a multiname to a name and then get the corresponding fixture

    for each nested context in the environment, look for a fixture that matches
    a multiname. see if a particular scope has a fixture with one of a list of 
    multinames

    multiname = { nss : NAMESPACE list list, id: IDENT }
    name = { ns: NAMESPACE, id: IDENT }
*)

fun resolveMultinameToFixture (env:ENV) 
                              (mname:Ast.MULTINAME) 
    : Ast.NAME * Ast.FIXTURE =
    let
        fun envHeadHasFixture ([],n) = false
          | envHeadHasFixture ((env:ENV),n) = hasFixture (#fixtures (List.hd env)) (Ast.PropName n) 
        fun getEnvParent [] = NONE
          | getEnvParent (x::[]) = NONE
          | getEnvParent (x::xs) = SOME xs
    in
        case Multiname.resolve mname env envHeadHasFixture getEnvParent of
            NONE => LogErr.defnError ["unresolved fixture ", LogErr.multiname mname]
          | SOME (({fixtures, ...}::_), n) => (n, getFixture fixtures (Ast.PropName n))
          | SOME _ => LogErr.defnError ["fixture lookup error ", LogErr.multiname mname]
    end

fun multinameHasFixture (env:ENV) 
                        (mname:Ast.MULTINAME) 
    : bool =
    let
        fun envHeadHasFixture ([],n) = false
          | envHeadHasFixture ((env:ENV),n) = hasFixture (#fixtures (List.hd env)) (Ast.PropName n) 
        fun getEnvParent [] = NONE
          | getEnvParent (x::[]) = NONE
          | getEnvParent (x::xs) = SOME xs
    in
        case Multiname.resolve mname env envHeadHasFixture getEnvParent of
            NONE => false
          | SOME (({fixtures, ...}::_), n) => true
          | _ => LogErr.defnError ["fixture lookup error ", LogErr.multiname mname]
    end

(*
    Create a new context initialised with the provided fixtures and
    inherited environment
*)
             
fun extendEnvironment (env:ENV) 
                      (fixtures:Ast.FIXTURES) 
    : ENV = 
    case env of 
        [] => (trace ["extending empty environment"];
               {fixtures = fixtures} :: [])
      | _ => { fixtures = fixtures } :: env

fun updateFixtures (env:ENV) (fxtrs:Ast.FIXTURES)
    : ENV =
    let
    in case env of
        { fixtures } :: outer =>
            { fixtures = fxtrs @ fixtures } :: outer
      | [] =>
            LogErr.defnError ["cannot update an empty environment"]
    end

fun multinameFromName (n:Ast.NAME) = 
    { nss = [[(#ns n)]], id = (#id n) }

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
            Ast.TrinaryExpr (t, e1, e2, e3) => 
            (Ast.TrinaryExpr (t, e1, e2, e3), dummyType)
            
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
        Ast.Block { head, body, pos, ... } =>
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
    end


(*
    PROGRAM
*)

and topEnv _ = [ { fixtures = !topFixtures } ]

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
