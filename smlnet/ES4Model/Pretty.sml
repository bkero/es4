structure Pretty =
struct

open Ast

fun ppProgram (p : PROGRAM) = "<program>"

fun ppExpr (e : EXPR) = "<expr>"

fun ppStmt (s : STMT) = "<stmt>"

fun ppDefinition (s : DEFN) = "<defn>"

fun ppVarDefn (s : VAR_BINDING) = "<var defn>"

end
