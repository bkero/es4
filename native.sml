(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* Implementation of "native" methods visible to ES4 base objects. *)

structure Native = struct 

fun internal (n:Ast.IDENT) = { ns = Ast.Internal "", id = n }

type nativeMethod = (Mach.SCOPE -> Mach.OBJ -> Mach.VAL list -> Mach.VAL)

fun functionCtor (scope:Mach.SCOPE) (obj:Mach.OBJ) (args:Mach.VAL list) 
    : Mach.VAL = 
    case args of 
	[] => LogErr.evalError ["Function constructor called with no args"]
      | x::xs => (Mach.defValue obj (internal "source") (Mach.newString (Mach.toString x)); 
		  Mach.Object obj)
    
    
val nativeMethods:(Ast.NAME * Ast.NAME * nativeMethod) list = 
    [
     ((internal "Function"), (internal "Function"), functionCtor)
    ]

fun getNativeMethod (class:Ast.NAME) (method:Ast.NAME) 
    : nativeMethod = 
    let 
        fun search [] = LogErr.hostError ["no native method found for method ",
					  LogErr.name method,
					  " on class ",
					  LogErr.name class]
          | search ((c,m,v)::bs) = 
            if c = class andalso m = method
            then v
            else search bs
    in
        search nativeMethods
    end
end
