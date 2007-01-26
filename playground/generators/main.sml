(* An example ES4 program generating 10 Fibonacci numbers. *)

functor Example (structure G : GENERATOR) =
struct
    open Value

    fun fib () =
        G.make (fn (g) =>
                    let val rec fib = fn (i,j) =>
                                      (
                                          G.yield (g, Number i);
                                          fib (j, i+j)
                                      )
                    in
                        fib (0,1)
                    end)

    fun main (arg0:string, restArgs:string list) =
    (
        G.run(fn () =>
                 let val g = fib()
                     val rec loop = fn i =>
                                       if i < 10 then
                                       (
                                           print (G.send (g, Undefined));
                                           loop (i+1)
                                       )
                                       else ()
                 in
                     loop 0
                 end);
        0
    )
    handle Value.InternalError s => (TextIO.print ("internal error: " ^ s ^ "\n"); 1)
end

structure Main = Example(structure G = Generator(functor MkCoroutine = ShiftCoroutine))
