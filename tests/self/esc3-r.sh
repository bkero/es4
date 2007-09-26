### compile .ast file to a .abc file using the RI

echo esc3-r $1.ast
cd ../../
cp tests/self/$1.ast esc-tmp.ast
make run-dumped FILE="tests/self/debug.es tests/self/ast.es tests/self/decoder.es tests/self/util.es tests/self/util-es4ri.es tests/self/bytestream.es tests/self/assembler.es tests/self/abcfile.es tests/self/emitter.es tests/self/cogen.es tests/self/cogen-stmt.es tests/self/cogen-expr.es tests/self/esc3-r.es"
cp esc-tmp.abc tests/self/$1.abc
rm esc-tmp.ast
cd tests/self/