cp $1 esc-tmp.es
make run-dumped FILE="tests/self/debug.es tests/self/ast.es tests/self/encoder.es tests/self/decoder.es  tests/self/lex-char.es tests/self/lex-token.es tests/self/lex-scan.es tests/self/util.es tests/self/parser.es tests/self/util-es4ri.es tests/self/bytestream.es tests/self/abcfile.es tests/self/assembler.es tests/self/emitter.es tests/self/cogen-stmt.es tests/self/cogen-expr.es tests/self/cogen.es tests/self/esc.es"

cp esc-tmp.il $1.il
cp esc-tmp.abc $1.abc

/work/tamarin/bin/shell esc-tmp.abc