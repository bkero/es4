cp $1.ast esc-tmp.ast
cp $1.ast $1.1.ast
make run-dumped FILE="tests/self/debug.es tests/self/ast.es tests/self/encoder.es tests/self/decoder.es tests/self/util.es tests/self/util-es4ri.es esc-tmp.ast tests/self/esc2.es"
cp esc-tmp.ast $1.ast
cp esc-tmp.ast $1.2.ast
diff -s $1.1.ast $1.2.ast
