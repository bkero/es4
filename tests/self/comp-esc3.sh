### STEP1: compile esc3 .es to .ast

#./esc1-r.sh debug.es
#./esc1-r.sh ast.es
#./esc1-r.sh util.es
#./esc1-r.sh decoder.es
#./esc1-r.sh util-tamarin.es
./esc1-r.sh bytestream.es
#./esc1-r.sh assembler.es
#./esc1-r.sh abcfile.es
#./esc1-r.sh emitter.es
#./esc1-r.sh cogen.es
#./esc1-r.sh cogen-stmt.es
#./esc1-r.sh cogen-expr.es
#./esc1-r.sh esc3-t.es

### STEP2: compile esc3 .ast to .ast.abc

#./esc2-r.sh debug.es
#./esc2-r.sh ast.es
#./esc2-r.sh util.es
#./esc2-r.sh decoder.es
#./esc2-r.sh util-tamarin.es
#./esc2-r.sh bytestream.es
#./esc2-r.sh assembler.es
#./esc2-r.sh abcfile.es
#./esc2-r.sh emitter.es
#./esc2-r.sh cogen.es
#./esc2-r.sh cogen-stmt.es
#./esc2-r.sh cogen-expr.es
#./esc2-r.sh esc3-t.es

### STEP 3: compile .ast to .abc using esc3-r

#./esc3-r.sh debug.es
#./esc3-r.sh ast.es
#./esc3-r.sh util.es
#./esc3-r.sh decoder.es
#./esc3-r.sh util-tamarin.es
./esc3-r.sh bytestream.es
#./esc3-r.sh assembler.es
#./esc3-r.sh abcfile.es
#./esc3-r.sh emitter.es
#./esc3-r.sh cogen.es
#./esc3-r.sh cogen-stmt.es
#./esc3-r.sh cogen-expr.es
#./esc3-r.sh esc3-t.es

### STEP 4: compile .ast.abc to .abc using esc3-t

#./esc3-t.sh debug.es
#./esc3-t.sh ast.es
#./esc3-t.sh util.es
#./esc3-t.sh decoder.es
#./esc3-t.sh util-tamarin.es
#./esc3-t.sh bytestream.es
#./esc3-t.sh assembler.es
#./esc3-t.sh abcfile.es
#./esc3-t.sh emitter.es
#./esc3-t.sh cogen.es
#./esc3-t.sh cogen-stmt.es
#./esc3-t.sh cogen-expr.es
#./esc3-t.sh esc3-t.es
