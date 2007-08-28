# This script is used to make ESC compile itself

### STAGE 1

### DECODE

./esc1.sh tests/self/debug.es
./esc1.sh tests/self/ast.es
./esc1.sh tests/self/decoder.es
./esc1.sh tests/self/encoder.es
./esc1.sh tests/self/esc2.es

### PARSE

./esc1.sh tests/self/lex-char.es
./esc1.sh tests/self/lex-token.es
./esc1.sh tests/self/lex-scan.es
#./esc1.sh tests/self/parse-util.es
#./esc1.sh tests/self/parse-ident.es
#./esc1.sh tests/self/parse-expr.es
#./esc1.sh tests/self/parse-ptrn.es
#./esc1.sh tests/self/parse-type.es
#./esc1.sh tests/self/parse-stmt.es
#./esc1.sh tests/self/parse-prgm.es
#./esc1.sh tests/self/esc1.es

### GENERATE

#./esc1.sh tests/self/util.es
#./esc1.sh tests/self/bytestream.es
#./esc1.sh tests/self/abcfile.es
#./esc1.sh tests/self/assembler.es
#./esc1.sh tests/self/emitter.es
#./esc1.sh tests/self/cogen-stmt.es
#./esc1.sh tests/self/cogen-expr.es
#./esc1.sh tests/self/cogen.es
#./esc1.sh tests/self/esc3.es

### STAGE 2

### DECODE

#./esc2.sh tests/self/debug.es
#./esc2.sh tests/self/ast.es
#./esc2.sh tests/self/decoder.es
#./esc2.sh tests/self/encoder.es
#./esc2.sh tests/self/esc2.es

### PARSE

#./esc2.sh tests/self/lexer.es
#./esc2.sh tests/self/parser-util.es
#./esc2.sh tests/self/parser-identexpr.es
#./esc2.sh tests/self/parser-expr.es
#./esc2.sh tests/self/parser-pattern.es
#./esc2.sh tests/self/parser-typeexpr.es
#./esc2.sh tests/self/parser-stmt.es
#./esc2.sh tests/self/parser-program.es
#./esc2.sh tests/self/esc1.es

### GENERATE

#./esc2.sh tests/self/util.es
#./esc2.sh tests/self/bytestream.es
#./esc2.sh tests/self/abcfile.es
#./esc2.sh tests/self/assembler.es
#./esc2.sh tests/self/emitter.es
#./esc2.sh tests/self/cogen-stmt.es
#./esc2.sh tests/self/cogen-expr.es
#./esc2.sh tests/self/cogen.es
#./esc2.sh tests/self/esc3.es

