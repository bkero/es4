structure ES4Model
  : sig val main: string option array option -> unit
    end = 
struct
    fun confirmExit args = List.exists (fn s => (s = "x") orelse (s = "X"))
                                       args

    fun when b thunk = if b then SOME (thunk()) else NONE

    fun isParam (p : string) : bool = String.isPrefix "-" p

    fun parseParam (p : string) : string option =
    (
        when (isParam p)
            (fn _ => String.extract (p, 1, NONE))
    )

    fun main' (a : string list) =
    (
        let val asts = List.map Parser.parseFile a
        in
            TextIO.print "evaluating...\n";
            List.map Eval.evalProgram asts;
            TextIO.print "evaluated!\n"
        end
    )

    fun main (a : string option array option) = 
    (
        let val args = case a of
                            NONE => []
                          | SOME a => List.mapPartial (fn x => x)
													  (Array.foldr (fn (a,b) => a::b) [] a)
            val params = List.mapPartial parseParam args
        in
            main' (List.filter (not o isParam) args);
            when (confirmExit params)
              (fn _ => (
                           TextIO.print("press <enter> to exit\n");
                           System.Console.ReadLine()
                       ));
            ()
        end
    )
end
