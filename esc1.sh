echo esc1 $1
cp $1 esc-tmp.es

make run-dumped FILE="tests/self/debug.es tests/self/util.es tests/self/util-es4ri.es tests/self/ast.es tests/self/decoder.es tests/self/encoder.es tests/self/lex-char.es tests/self/lex-token.es tests/self/lex-scan.es tests/self/parse-util.es tests/self/parse-ident.es tests/self/parse-expr.es tests/self/parse-ptrn.es tests/self/parse-type.es tests/self/parse-stmt.es tests/self/parse-defn.es tests/self/parse-prgm.es tests/self/esc1.es"

#/work/tamarin/bin/shell tests/self/tamarin-util.es.abc tests/self/debug.es.abc tests/self/lex-char.es.abc tests/self/lex-token.es.abc tests/self/lex-scan.es.abc tests/self/parse-util.es.abc tests/self/parse-ident.es.abc tests/self/parse-expr.es.abc tests/self/parse-ptrn.es.abc tests/self/parse-type.es.abc tests/self/parse-smtn.es.abc tests/self/parse-defn.es.abc tests/self/parse-prgm.es.abc tests/self/esc1.es.abc

cp esc-tmp.ast $1.ast
cp esc-tmp.ast $1.ast1
rm esc-tmp.es
rm esc-tmp.ast


