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

functor Visitor (type RESULT) : VISITOR = struct

type RESULT = RESULT

datatype NEXT = Stop of RESULT (* abort the traversal *)
              | Skip of RESULT (* continue traversal but skip children *)
              | Cont of RESULT (* continue traversal, including children *)

type 'a METHOD = 'a * RESULT -> NEXT

type VISITOR = { visitExpr : Ast.EXPRESSION METHOD,
                 visitStmt : Ast.STATEMENT METHOD,
                 visitDefn : Ast.DEFN METHOD,
                 visitFunc : Ast.FUNC METHOD }

fun stop (_, x) = Stop x
fun skip (_, x) = Skip x
fun cont (_, x) = Cont x

val default = { visitExpr = cont,
                visitStmt = cont,
                visitDefn = cont,
                visitFunc = cont }

fun withVisitExpr ({ visitStmt, visitDefn, visitFunc, ... } : VISITOR, visitExpr) =
    { visitExpr = visitExpr, visitStmt = visitStmt, visitDefn = visitDefn, visitFunc = visitFunc }
fun withVisitStmt ({ visitExpr, visitDefn, visitFunc, ... } : VISITOR, visitStmt) =
    { visitExpr = visitExpr, visitStmt = visitStmt, visitDefn = visitDefn, visitFunc = visitFunc }
fun withVisitDefn ({ visitExpr, visitStmt, visitFunc, ... } : VISITOR, visitDefn) =
    { visitExpr = visitExpr, visitStmt = visitStmt, visitDefn = visitDefn, visitFunc = visitFunc }
fun withVisitFunc ({ visitExpr, visitStmt, visitDefn, ... } : VISITOR, visitFunc) =
    { visitExpr = visitExpr, visitStmt = visitStmt, visitDefn = visitDefn, visitFunc = visitFunc }

(* computing AST children *)

type CHILDREN = Ast.EXPRESSION list * Ast.STATEMENT list * Ast.DEFN list * Ast.FUNC list

fun initStepExprs is =
    (case is of
         Ast.InitStep (_, e) => [e]
       | Ast.AssignStep (e1, e2) => [e1, e2])

fun bindingsExprs (_, initSteps) = List.concat (map initStepExprs initSteps)

fun initsExprs (inits : Ast.INITS) = map (#2) inits

fun headExprs (Ast.Head (_, inits)) = initsExprs inits

fun nameExpr ne = []

fun fieldExprs ({ name, init, ... } : Ast.FIELD) = init::(nameExpr name)

fun blockChildren (Ast.Block directives) =
    (case directives of
         { defns, head=NONE, body, ... } => ([], body, defns, [])
       | { defns, head=SOME head, body, ... } => (headExprs head, body, defns, []))

fun ctorChildren (Ast.Ctor { settings, superArgs, func }) =
    (superArgs@(headExprs settings), [func])

fun classChildren (cls : Ast.CLS) =
    (case cls of
         Ast.Cls { constructor=SOME ctor, classRib, instanceRib, instanceInits, ... } =>
         let
             val (es1, ss1, ds1, fs1) = ribChildren classRib
             val (es2, ss2, ds2, fs2) = ribChildren instanceRib
             val (es, fs) = ctorChildren ctor
             val es' = headExprs instanceInits
         in
             (es1@es2@es@es', ss1@ss2, ds1@ds2, fs1@fs2@fs)
         end
       | Ast.Cls { constructor=NONE, classRib, instanceRib, instanceInits, ... } =>
         let
             val (es1, ss1, ds1, fs1) = ribChildren classRib
             val (es2, ss2, ds2, fs2) = ribChildren instanceRib
             val es = headExprs instanceInits
         in
             (es1@es2@es, ss1@ss2, ds1@ds2, fs1@fs2)
         end)

and ifaceChildren (Ast.Iface { instanceRib, ... }) =
    ribChildren instanceRib

and fixtureChildren fixture =
    (case fixture of
         Ast.ClassFixture cls => classChildren cls
       | Ast.InterfaceFixture iface => ifaceChildren iface
       | Ast.MethodFixture { func, ... } => ([], [], [], [func])
       | Ast.VirtualValFixture { getter=SOME f1, setter=SOME f2, ... } =>
         ([], [], [], [f1, f2])
       | Ast.VirtualValFixture { getter=SOME f1, setter=NONE, ... } =>
         ([], [], [], [f1])
       | Ast.VirtualValFixture { getter=NONE, setter=SOME f2, ... } =>
         ([], [],[], [f2])
       | _ => ([], [], [], []))

and fixturesChildren (fixtures : Ast.FIXTURE list) =
    (case fixtures of
         [] => ([], [], [], [])
       | (fixture::fixtures) =>
         let
             val (es, ss, ds, fs) = fixtureChildren fixture
             val (es', ss', ds', fs') = fixturesChildren fixtures
         in
             (es@es', ss@ss', ds@ds', fs@fs')
         end)

and ribChildren (pairs : Ast.RIB) = fixturesChildren (map (#2) pairs)

and catchChildren (catch : Ast.CATCH_CLAUSE) =
    (case catch of
         { bindings, rib=SOME rib, inits=SOME inits, block, ... } =>
         let
             val (es, ss, ds, fs) = ribChildren rib
             val (es', ss', ds', fs') = blockChildren block
             val es'' = bindingsExprs bindings
             val es''' = initsExprs inits
         in
             (es@es'@es''@es''', ss@ss', ds@ds', fs@fs')
         end
       | { bindings, rib=SOME rib, inits=NONE, block, ... } =>
         let
             val (es, ss, ds, fs) = ribChildren rib
             val (es', ss', ds', fs') = blockChildren block
             val es'' = bindingsExprs bindings
         in
             (es@es'@es'', ss@ss', ds@ds', fs@fs')
         end
       | { bindings, rib=NONE, inits=SOME inits, block, ... } =>
         let
             val (es, ss, ds, fs) = blockChildren block
             val es' = bindingsExprs bindings
             val es'' = initsExprs inits
         in
             (es@es'@es'', ss, ds, fs)
         end
       | { bindings, rib=NONE, inits=NONE, block, ... } =>
         let
             val (es, ss, ds, fs) = blockChildren block
             val es' = bindingsExprs bindings
         in
             (es@es', ss, ds, fs)
         end)

and catchesChildren (catches : Ast.CATCH_CLAUSE list) =
    (case catches of
         [] => ([], [], [], [])
       | (catch::catches) =>
         let
             val (es, ss, ds, fs) = catchChildren catch
             val (es', ss', ds', fs') = catchesChildren catches
         in
             (es@es', ss@ss', ds@ds', fs@fs')
         end)

and caseChildren (c1 : Ast.CASE) =
    (case c1 of
         { label=SOME label, inits=SOME inits, body } =>
         let
             val (es, ss, ds, fs) = blockChildren body
         in
             (label::(initsExprs inits)@es, ss, ds, fs)
         end
       | { label=SOME label, inits=NONE, body } =>
         let
             val (es, ss, ds, fs) = blockChildren body
         in
             (label::es, ss, ds, fs)
         end
       | { label=NONE, inits=SOME inits, body } =>
         let
             val (es, ss, ds, fs) = blockChildren body
         in
             ((initsExprs inits)@es, ss, ds, fs)
         end
       | { label=NONE, inits=NONE, body } =>
         blockChildren body)

and casesChildren (cases : Ast.CASE list) =
    (case cases of
         [] => ([], [], [], [])
       | (c1::cs) =>
         let
             val (es, ss, ds, fs) = caseChildren c1
             val (es', ss', ds', fs') = casesChildren cs
         in
             (es@es', ss@ss', ds@ds', fs@fs')
         end)

fun varDefnExprs (defn : Ast.VAR_DEFN) =
    (case defn of
         { ns=SOME ns, bindings, ... } => (bindingsExprs bindings)
       | { ns=NONE, bindings, ... } => bindingsExprs bindings)

fun forEnumChildren (enum : Ast.FOR_ENUM_STATEMENT) =
    (case enum of
         { defn=SOME defn, obj, rib=SOME fixtures, next, body, ... } =>
         let
             val (es, ss, ds, fs) = ribChildren fixtures
             val es' = varDefnExprs defn
         in
             (obj::es@es', next::body::ss, ds, fs)
         end
       | { defn=SOME defn, obj, rib=NONE, next, body, ... } =>
         let
             val es = varDefnExprs defn
         in
             (obj::es, [next, body], [], [])
         end
       | { defn=NONE, obj, rib=SOME fixtures, next, body, ... } =>
         let
             val (es, ss, ds, fs) = ribChildren fixtures
         in
             (obj::es, next::body::ss, ds, fs)
         end
       | { defn=NONE, obj, rib=NONE, next, body, ... } =>
         ([obj], [next, body], [], []))

fun whileChildren (ws : Ast.WHILE_STATEMENT) =
    (case ws of
         { cond, rib=SOME fixtures, body, ... } =>
         let
             val (es, ss, ds, fs) = ribChildren fixtures
         in
             (cond::es, body::ss, ds, fs)
         end
       | { cond, rib=NONE, body, ... } => ([cond], [body], [], []))

fun forChildren (fs : Ast.FOR_STATEMENT) =
    (case fs of
         { rib=SOME fixtures, defn=SOME defn, init, cond, update, body, ... } =>
         let
             val (es, ss, ds, fs) = ribChildren fixtures
             val es' = varDefnExprs defn
         in
             ([cond,update]@es@es', body::init@ss, ds, fs)
         end
       | { rib=NONE, defn=SOME defn, init, cond, update, body, ... } =>
         let
             val es = varDefnExprs defn
         in
             ([cond,update]@es, body::init, [], [])
         end
       | { rib=SOME fixtures, defn=NONE, init, cond, update, body, ... } =>
         let
             val (es, ss, ds, fs) = ribChildren fixtures
         in
             ([cond,update]@es, body::init@ss, ds, fs)
         end
       | { rib=NONE, defn=NONE, init, cond, update, body, ... } =>
         ([cond,update], body::init, [], []))

fun exprChildren (expr : Ast.EXPRESSION)
    : CHILDREN =
    case expr of
        Ast.TernaryExpr (e1, e2, e3) => ([e1, e2, e3], [], [], [])
      | Ast.BinaryExpr (_, e1, e2) => ([e1, e2], [], [], [])
      | Ast.BinaryTypeExpr (_, e, _) => ([e], [], [], [])
      | Ast.UnaryExpr (_, e) => ([e], [], [], [])
      | Ast.YieldExpr (SOME e) => ([e], [], [], [])
      | Ast.SuperExpr (SOME e) => ([], [], [], [])
      | Ast.LiteralExpr (Ast.LiteralArray { exprs, ... }) => ([exprs], [], [], [])
      | Ast.LiteralExpr (Ast.LiteralXML es) => (es, [], [], [])
      | Ast.LiteralExpr (Ast.LiteralObject { expr, ... }) => (List.concat (map fieldExprs expr), [], [], [])
      | Ast.LiteralExpr (Ast.LiteralFunction f) => ([], [], [], [f])
      | Ast.CallExpr { func, actuals } => (func::actuals, [], [], [])
      | Ast.ApplyTypeExpr { expr, ... } => ([expr], [], [], [])
      | Ast.LetExpr { defs, body, head=SOME head } => (body::(bindingsExprs defs) @ (headExprs head), [], [], [])
      | Ast.LetExpr { defs, body, head=NONE } => (body::(bindingsExprs defs), [], [], [])
      | Ast.NewExpr { obj, actuals } => (obj::actuals, [], [], [])
      | Ast.ObjectNameReference { object, name, ... } => (object::(nameExpr name), [], [], [])
      | Ast.ObjectIndexReference { object, index, ... } => ([object, index], [], [], [])
      | Ast.LexicalReference { name, ... } => (nameExpr name, [], [], [])
      | Ast.SetExpr (_, e1, e2) => ([e1, e2], [], [], [])
      | Ast.ListExpr es => (es, [], [], [])
      | Ast.InitExpr (_, head, inits) => ((headExprs head) @ (initsExprs inits), [], [], [])
      | Ast.Comprehension (e, head, SOME e') => ([e, e'], [], [], [])
      | Ast.Comprehension (e, head, NONE) => ([e], [], [], [])
      | _ => ([], [], [], [])

fun stmtChildren (stmt : Ast.STATEMENT)
    : CHILDREN =
    case stmt of
        Ast.ExprStmt e => ([e], [], [], [])
      | Ast.InitStmt { temps, inits, ... } =>
        ((bindingsExprs temps) @ (List.concat (map initStepExprs inits)), [], [], [])
      | Ast.ClassBlock { block, ... } => blockChildren block
      | Ast.ForInStmt enum => forEnumChildren enum
      | Ast.ThrowStmt e => ([e], [], [], [])
      | Ast.ReturnStmt e => ([e], [], [], [])
      | Ast.BlockStmt block => blockChildren block
      | Ast.LabeledStmt (_, s) => ([], [s], [], [])
      | Ast.LetStmt block => blockChildren block
      | Ast.WhileStmt ws => whileChildren ws
      | Ast.DoWhileStmt ws => whileChildren ws
      | Ast.ForStmt fs => forChildren fs
      | Ast.IfStmt { cnd, thn, els } => ([cnd], [thn, els], [], [])
      | Ast.WithStmt { obj, body, ... } => ([obj], [body], [], [])
      | Ast.TryStmt { block, catches, finally=SOME block' } =>
        let
            val (es, ss, ds, fs) = blockChildren block
            val (es', ss', ds', fs') = catchesChildren catches
            val (es'', ss'', ds'', fs'') = blockChildren block'
        in
            (es@es'@es'', ss@ss'@ss'', ds@ds'@ds'', fs@fs'@fs'')
        end
      | Ast.TryStmt { block, catches, finally=NONE } =>
        let
            val (es, ss, ds, fs) = blockChildren block
            val (es', ss', ds', fs') = catchesChildren catches
        in
            (es@es', ss@ss', ds@ds', fs@fs')
        end
      | Ast.SwitchStmt { cond, cases, ... } =>
        let
            val (es, ss, ds, fs) = casesChildren cases
        in
            (cond::es, ss, ds, fs)
        end
      | Ast.SwitchTypeStmt { cond, cases, ... } =>
        let
            val (es, ss, ds, fs) = catchesChildren cases
        in
            (cond::es, ss, ds, fs)
        end
      (* XXX: what is DXNStmt? *)
      | _ => ([], [], [], [])

fun defnChildren (defn : Ast.DEFN)
    : CHILDREN =
    (case defn of
         Ast.ClassDefn { ctorDefn=SOME ctor, classDefns, instanceDefns, instanceStmts, ... } =>
         let
             val (es, fs) = ctorChildren ctor
         in
             (es, instanceStmts, classDefns@instanceDefns, [])
         end
       | Ast.ClassDefn { ctorDefn=NONE, classDefns, instanceDefns, instanceStmts, ... } =>
         ([], instanceStmts, classDefns@instanceDefns, [])
       | Ast.VariableDefn defn => (varDefnExprs defn, [], [], [])
       | Ast.FunctionDefn { func, ... } =>
         ([], [], [], [func])
       | Ast.ConstructorDefn ctor =>
         let
             val (es, fs) = ctorChildren ctor
         in
             (es, [], [], fs)
         end
       | Ast.InterfaceDefn { instanceDefns, ... } =>
         ([], [], instanceDefns, [])
       | _ =>
         ([], [], [], []))

fun funcChildren (func : Ast.FUNC)
    : CHILDREN =
    (case func of
         Ast.Func { block=SOME block, param, defaults, ... } =>
         let
             val (es, ss, ds, fs) = blockChildren block
             val es' = headExprs param
         in
             (defaults@es@es', ss, ds, fs)
         end
       | Ast.Func { block=NONE, param, defaults, ... } =>
         (defaults@(headExprs param), [], [], []))

(* traversals in CPS *)

fun continue (next : NEXT, skipk : RESULT -> RESULT, contk : RESULT -> RESULT)
    : RESULT =
    case next of
        Stop x => x
      | Skip x => skipk x
      | Cont x => contk x

fun foldExpr' (v as { visitExpr, ... }, expr, init, k) =
    continue (visitExpr (expr, init), k, fn x => foldAll (v, exprChildren expr, x, k))

and foldStmt' (v as { visitStmt, ... }, stmt, init, k) =
    continue (visitStmt (stmt, init), k, fn x => foldAll (v, stmtChildren stmt, x, k))

and foldDefn' (v as { visitDefn, ... }, defn, init, k) =
    continue (visitDefn (defn, init), k, fn x => foldAll (v, defnChildren defn, x, k))

and foldFunc' (v as { visitFunc, ... }, func, init, k) =
    continue (visitFunc (func, init), k, fn x => foldAll (v, funcChildren func, x, k))

and foldAll (v : VISITOR,
             (exprs, stmts, defns, funcs) : CHILDREN,
             init : RESULT,
             k : RESULT -> RESULT)
    : RESULT =
let
    fun foldList f (v, asts, init, k) =
        case asts of
            [] => k init
          | [ast] => f (v, ast, init, k)
          | ast::asts => f (v, ast, init, fn result => foldList f (v, asts, result, k))
in
    foldList foldExpr' (v, exprs, init, fn result =>
        foldList foldStmt' (v, stmts, result, fn result' =>
            foldList foldDefn' (v, defns, result', fn result'' =>
                foldList foldFunc' (v, funcs, result'', k))))
end

(* publicly exported traversals *)

fun id x = x

fun foldExpr (v, expr, init) =
    foldExpr' (v, expr, init, id)

fun foldStmt (v, stmt, init) =
    foldStmt' (v, stmt, init, id)

fun foldDefn (v, defn, init) =
    foldDefn' (v, defn, init, id)

fun foldFunc (v, func, init) =
    foldFunc' (v, func, init, id)

end

(* visitors for checking boolean properties of an AST *)
structure CheckAst = Visitor (type RESULT = bool)
