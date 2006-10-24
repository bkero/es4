signature GENERATOR =
sig
    type G

    val make  : (G -> unit) -> G
    val yield : G * Value.value -> Value.value
    val send  : G * Value.value -> Value.value
    val throw : G * Value.value -> Value.value
    val close : G -> unit
end
