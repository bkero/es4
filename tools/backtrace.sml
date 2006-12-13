structure BackTrace =
struct

fun printStackTrace e =
    let val ss = SMLofNJ.exnHistory e
        val s = General.exnMessage e
        val name = General.exnName e
        val details = if s = name then "" else (" [" ^ s ^ "]")
    in
        TextIO.print ("uncaught exception " ^ name ^ details ^ "\n");
        case ss of
             [] => ()
           | (s::ss') => (
                             TextIO.print ("  raised at: " ^ s ^ "\n");
                             List.app (fn s' => TextIO.print ("             " ^ s' ^ "\n")) ss'
                         )
    end

fun monitor f = f() handle e => (printStackTrace e; 1)

end
