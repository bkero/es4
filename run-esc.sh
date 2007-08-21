cp $1 esc-tmp.es
make run-dumped FILE="tests/self/debug.es tests/self/ast1.es tests/self/ast2.es tests/self/ast_encoder.es tests/self/ast_decoder.es tests/self/lexer.es tests/self/util.es tests/self/parser.es tests/self/util-es4ri.es tests/self/bytestream.es tests/self/abcfile.es tests/self/assembler.es tests/self/emitter.es tests/self/cogen-stmt.es tests/self/cogen-expr.es tests/self/cogen.es tests/self/esc1.es tests/self/esc3.es"

java -cp /work/asc/lib/asc.jar macromedia.asc.embedding.Main -d -m esc-tmp.as

cp esc-tmp.il $1.il
cp esc-tmp.abc $1.abc

/work/tamarin/bin/shell esc-tmp.abc