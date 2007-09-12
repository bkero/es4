echo esc-r $1
cd ../../
cp tests/self/$1 esc-tmp.es
make run-dumped FILE="tests/self/debug.es tests/self/ast.es tests/self/encoder.es tests/self/decoder.es tests/self/lex-char.es tests/self/lex-token.es tests/self/lex-scan.es tests/self/util.es tests/self/parse-util.es tests/self/parse-ident.es tests/self/parse-lhsexpr.es tests/self/parse-assignexpr.es tests/self/parse-ptrn.es tests/self/parse-type.es tests/self/parse-stmt.es tests/self/parse-defn.es tests/self/parse-prgm.es tests/self/util-es4ri.es tests/self/bytestream.es tests/self/assembler.es tests/self/abcfile.es tests/self/emitter.es tests/self/cogen.es tests/self/cogen-stmt.es tests/self/cogen-expr.es tests/self/esc-r.es"

cp esc-tmp.abc tests/self/$1.abc
rm esc-tmp.es
cd tests/self