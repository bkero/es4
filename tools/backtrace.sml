structure BackTrace =
struct

fun monitor f = f() handle e => (PrintTrace.printStackTrace e; 1)

end
