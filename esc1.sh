cp $1 esc-tmp.es
make run-dumped FILE="tests/self/debug.es tests/self/ast1.es tests/self/ast2.es tests/self/ast_encoder.es tests/self/ast_decoder.es tests/self/lexer.es tests/self/util.es tests/self/parser.es tests/self/util-es4ri.es tests/self/esc1.es"
cp esc-tmp.ast $1.1.ast
cp esc-tmp.ast $1.ast
rm esc-tmp.es
rm esc-tmp.ast