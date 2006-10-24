structure Main =
    struct
        open CML       (* Concurrent ML primitives *)
        open Value     (* data model for ES values *)
        open Generator (* generator operations     *)

        fun fib () =
            makeGen (fn (g) =>
                         let val rec fib = fn (i,j) =>
                                           (
                                               yieldFromGen (g, Number i);
                                               fib (j, i+j)
                                           )
                         in
                             fib (0,1)
                         end)

        fun main () =
            RunCML.doit(fn () =>
                            let val g = fib()
                                val rec loop = fn i =>
                                                   if i < 10
                                                   then (print (sendToGen (g, Undefined));
                                                         loop (i+1))
                                                   else ()
                            in
                                loop 0
                            end,
                        NONE)

    end
