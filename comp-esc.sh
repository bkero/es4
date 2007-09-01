# This script is used to make ESC compile itself

### STAGE 1 - generate asts

#./esc1.sh tests/self/debug.es
#./esc1.sh tests/self/util.es
#./esc1.sh tests/self/lex-char.es
#./esc1.sh tests/self/lex-token.es
#./esc1.sh tests/self/lex-scan.es
#./esc1.sh tests/self/ast.es
#./esc1.sh tests/self/parse-util.es
#./esc1.sh tests/self/parse-ident.es
#./esc1.sh tests/self/parse-expr.es
#./esc1.sh tests/self/parse-ptrn.es
#./esc1.sh tests/self/parse-type.es
#./esc1.sh tests/self/parse-stmt.es
#./esc1.sh tests/self/parse-defn.es
#./esc1.sh tests/self/parse-prgm.es
#./esc1.sh tests/self/decoder.es
#./esc1.sh tests/self/encoder.es
#./esc1.sh tests/self/util-tamarin.es
#./esc1.sh tests/self/bytestream.es
#./esc1.sh tests/self/assembler.es
#./esc1.sh tests/self/abcfile.es
#./esc1.sh tests/self/emitter.es
#./esc1.sh tests/self/cogen.es
#./esc1.sh tests/self/cogen-stmt.es
#./esc1.sh tests/self/cogen-expr.es
#./esc1.sh tests/self/tamarin-esc.es

### STAGE 2 - compile asts to abcs for the ast object literal

#./esc2.sh tests/self/debug.es
#./esc2.sh tests/self/util.es
#./esc2.sh tests/self/lex-char.es
#./esc2.sh tests/self/lex-token.es
#./esc2.sh tests/self/lex-scan.es
#./esc2.sh tests/self/ast.es
#./esc2.sh tests/self/parse-util.es
#./esc2.sh tests/self/parse-ident.es
#./esc2.sh tests/self/parse-expr.es
#./esc2.sh tests/self/parse-ptrn.es
#./esc2.sh tests/self/parse-type.es
#./esc2.sh tests/self/parse-stmt.es
#./esc2.sh tests/self/parse-defn.es
#./esc2.sh tests/self/parse-prgm.es
#./esc2.sh tests/self/decoder.es
#./esc2.sh tests/self/encoder.es
#./esc2.sh tests/self/util-tamarin.es
#./esc2.sh tests/self/bytestream.es
#./esc2.sh tests/self/assembler.es
#./esc2.sh tests/self/abcfile.es
#./esc2.sh tests/self/emitter.es
#./esc2.sh tests/self/cogen.es
#./esc2.sh tests/self/cogen-stmt.es
#./esc2.sh tests/self/cogen-expr.es
#./esc2.sh tests/self/tamarin-esc.es

### STAGE 3 - translate ast to abc

#./esc3.sh tests/self/debug.es
#./esc3.sh tests/self/util.es
#./esc3.sh tests/self/lex-char.es
#./esc3.sh tests/self/lex-token.es
#./esc3.sh tests/self/lex-scan.es
#./esc3.sh tests/self/ast.es
#./esc3.sh tests/self/parse-util.es
#./esc3.sh tests/self/parse-ident.es
#./esc3.sh tests/self/parse-expr.es
#./esc3.sh tests/self/parse-ptrn.es
#./esc3.sh tests/self/parse-type.es
#./esc3.sh tests/self/parse-stmt.es
#./esc3.sh tests/self/parse-defn.es
#./esc3.sh tests/self/parse-prgm.es
#./esc3.sh tests/self/decoder.es
#./esc3.sh tests/self/encoder.es
#./esc3.sh tests/self/util-tamarin.es
#./esc3.sh tests/self/bytestream.es
#./esc3.sh tests/self/assembler.es
#./esc3.sh tests/self/abcfile.es
#./esc3.sh tests/self/emitter.es
#./esc3.sh tests/self/cogen.es
#./esc3.sh tests/self/cogen-stmt.es
#./esc3.sh tests/self/cogen-expr.es
#./esc3.sh tests/self/tamarin-esc.es

/work/tamarin/bin/shell tests/self/debug.es.abc tests/self/ast.es.abc tests/self/util.es.abc tests/self/lex-char.es.abc tests/self/lex-token.es.abc tests/self/lex-scan.es.abc tests/self/parse-util.es.abc tests/self/parse-ident.es.abc tests/self/parse-expr.es.abc tests/self/parse-ptrn.es.abc tests/self/parse-type.es.abc tests/self/parse-stmt.es.abc tests/self/parse-defn.es.abc tests/self/parse-prgm.es.abc tests/self/util-tamarin.es.abc tests/self/decoder.es.abc tests/self/encoder.es.abc tests/self/util-tamarin.es.abc tests/self/bytestream.es.abc tests/self/assembler.es.abc tests/self/abcfile.es.abc tests/self/emitter.es.abc tests/self/cogen.es.abc tests/self/cogen-stmt.es.abc tests/self/cogen-expr.es.abc tests/self/tamarin-esc.es.abc -- $1
