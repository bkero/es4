# NOTE: this doesn't work yet

# PARSER
./esc1.sh tests/self/debug.es
./esc1.sh tests/self/ast.es
./esc1.sh tests/self/lexer.es
./esc1.sh tests/self/parser-util.es
./esc1.sh tests/self/parser-identexpr.es
./esc1.sh tests/self/parser-expr.es
./esc1.sh tests/self/parser-pattern.es
./esc1.sh tests/self/parser-typeexpr.es
./esc1.sh tests/self/parser-stmt.es
./esc1.sh tests/self/parser-program.es
./esc1.sh tests/self/encoder.es
./esc1.sh tests/self/esc1.es

# DECODER
./esc1.sh tests/self/decoder.es
./esc1.sh tests/self/esc2.es

# GENERATOR
./esc1.sh tests/self/util.es
./esc1.sh tests/self/bytestream.es
./esc1.sh tests/self/abcfile.es
./esc1.sh tests/self/assembler.es
./esc1.sh tests/self/emitter.es
./esc1.sh tests/self/cogen-stmt.es
./esc1.sh tests/self/cogen-expr.es
./esc1.sh tests/self/cogen.es
./esc1.sh tests/self/esc3.es
