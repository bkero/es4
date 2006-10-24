functor MainFun(structure G : GENERATOR) =
    struct
        open CML       (* Concurrent ML primitives *)
        open Value     (* data model for ES values *)

        structure Gen : GENERATOR = G

        fun fib () =
            Gen.make (fn (g) =>
                          let val rec fib = fn (i,j) =>
                                            (
                                                Gen.yield (g, Number i);
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
                                                   then (print (Gen.send (g, Undefined));
                                                         loop (i+1))
                                                   else ()
                            in
                                loop 0
                            end,
                        NONE)

    end

structure Main = MainFun(structure G = ThreadGen)
