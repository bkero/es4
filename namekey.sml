structure NameKey = struct

type ord_key = Ast.NAME

val intrinsic = Ustring.fromCharCode 1
val operator = Ustring.fromCharCode 2
val private = Ustring.fromCharCode 3
val protected = Ustring.fromCharCode 4
val public = Ustring.fromCharCode 5
val internal = Ustring.fromCharCode 6
val user = Ustring.fromCharCode 7
val anon = Ustring.fromCharCode 8
val lim = Ustring.fromCharCode 9

fun decomposeNS ns =
    case ns of 
	Ast.Intrinsic => [intrinsic]
      | Ast.OperatorNamespace => [operator]
      | Ast.Private ident => [private, ident]
      | Ast.Protected ident => [protected, ident]
      | Ast.Public ident => [public, ident]
      | Ast.Internal ident => [internal, ident]
      | Ast.UserNamespace str => [user, str]
      | Ast.AnonUserNamespace i => [anon, Ustring.fromCharCode i]
      | Ast.LimitedNamespace (id, lns) => lim :: id :: (decomposeNS lns)

fun cmp (a,b) = Ustring.compare a b

fun compare (a:ord_key, b:ord_key) 
    : order =
    case Ustring.compare (#id a) (#id b) of
	LESS => LESS
      | GREATER => GREATER
      | EQUAL => 
	List.collate cmp ((decomposeNS (#ns a)), (decomposeNS (#ns b)))
end
