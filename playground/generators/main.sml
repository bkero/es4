functor MainFun(structure Co : COROUTINE) =
    struct
        open Value

        structure Gen : GENERATOR = GeneratorFun(structure Coroutine = Co)

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
            Co.run(fn () =>
                       let val g = fib()
                           val rec loop = fn i =>
                                              if i < 10
                                              then (print (Gen.send (g, Undefined));
                                                    loop (i+1))
                                              else ()
                       in
                           loop 0
                       end)
    end

structure Main = MainFun(structure Co = ThreadCoroutine)
