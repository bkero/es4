echo "compiling " $1
cp $1 parse-tmp.es
make run-dumped FILE="tests/self/debug.es tests/self/ast.es tests/self/ast_encoder.es tests/self/ast_decoder.es tests/self/lexer.es tests/self/util.es tests/self/parser.es tests/self/util-es4ri.es tests/self/parse-driver.es"
cp parse-tmp.ast $1.ast