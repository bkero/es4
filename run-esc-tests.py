make run-dumped FILE="tests/self/debug.es tests/self/ast.es tests/self/ast_encoder.es tests/self/ast_decoder.es tests/self/lexer.es tests/self/util.es tests/self/parser.es"

java -cp /work/asc/lib/asc.jar macromedia.asc.embedding.Main -d -m esc-tmp.es

/work/tamarin/bin/shell esc-tmp.abc