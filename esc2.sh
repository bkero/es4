cp $1.ast esc-tmp.ast
cp $1.ast $1.ast1

#make run-dumped FILE="tests/self/debug.es tests/self/ast.es tests/self/encoder.es tests/self/decoder.es tests/self/util.es tests/self/util-es4ri.es esc-tmp.ast tests/self/esc2.es"

./esc.sh esc-tmp.es.ast

/work/tamarin/bin/shell tests/self/debug.es.abc tests/self/ast.es.abc tests/self/esc-tmp.es.ast.abc tests/self/decoder.es.abc tests/self/encoder.es.abc tests/self/util-tamarin.es.abc tests/self/esc2.es.abc

cp esc-tmp.ast $1.ast
cp esc-tmp.ast $1.ast2
diff -s $1.ast1 $1.ast2
rm esc-tmp.ast
