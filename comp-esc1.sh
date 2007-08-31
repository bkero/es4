#./esc1.sh tests/self/debug.es
#./esc1.sh tests/self/lex-char.es
#./esc1.sh tests/self/lex-token.es
#./esc1.sh tests/self/lex-scan.es
#./esc1.sh tests/self/ast.es
#./esc1.sh tests/self/parse-util.es
#./esc1.sh tests/self/parse-ident.es
#./esc1.sh tests/self/parse-expr.es
#./esc1.sh tests/self/parse-ptrn.es
#./esc1.sh tests/self/parse-type.es
#./esc1.sh tests/self/parse-stmt.es
#./esc1.sh tests/self/parse-defn.es
#./esc1.sh tests/self/parse-prgm.es
#./esc1.sh tests/self/util-tamarin.es
#./esc1.sh tests/self/esc1.es

#./esc3.sh tests/self/debug.es
#./esc3.sh tests/self/ast.es
#./esc3.sh tests/self/lex-char.es
#./esc3.sh tests/self/lex-token.es
#./esc3.sh tests/self/lex-scan.es
#./esc3.sh tests/self/parse-util.es
#./esc3.sh tests/self/parse-ident.es
#./esc3.sh tests/self/parse-expr.es
#./esc3.sh tests/self/parse-ptrn.es
#./esc3.sh tests/self/parse-type.es
#./esc3.sh tests/self/parse-stmt.es
./esc3.sh tests/self/parse-defn.es
./esc3.sh tests/self/parse-prgm.es
#./esc3.sh tests/self/util-tamarin.es
#./esc3.sh tests/self/esc1.es

/work/tamarin/bin/shell -Dinterp tests/self/debug.es.abc tests/self/lex-char.es.abc tests/self/lex-token.es.abc tests/self/lex-scan.es.abc tests/self/parse-util.es.abc tests/self/parse-ident.es.abc tests/self/parse-expr.es.abc tests/self/parse-ptrn.es.abc tests/self/parse-type.es.abc tests/self/parse-stmt.es.abc tests/self/parse-defn.es.abc tests/self/parse-prgm.es.abc tests/self/util-tamarin.es.abc tests/self/esc1.es.abc

zip abc tests/self/debug.es.abc tests/self/lex-char.es.abc tests/self/lex-token.es.abc tests/self/lex-scan.es.abc tests/self/parse-util.es.abc tests/self/parse-ident.es.abc tests/self/parse-expr.es.abc tests/self/parse-ptrn.es.abc tests/self/parse-type.es.abc tests/self/parse-stmt.es.abc tests/self/parse-defn.es.abc tests/self/parse-prgm.es.abc tests/self/util-tamarin.es.abc tests/self/esc1.es.abc tests/self/debug.es.ast tests/self/lex-char.es.ast tests/self/lex-token.es.ast tests/self/lex-scan.es.ast tests/self/parse-util.es.ast tests/self/parse-ident.es.ast tests/self/parse-expr.es.ast tests/self/parse-ptrn.es.ast tests/self/parse-type.es.ast tests/self/parse-stmt.es.ast tests/self/parse-defn.es.ast tests/self/parse-prgm.es.ast tests/self/util-tamarin.es.ast tests/self/esc1.es.ast tests/self/debug.es tests/self/lex-char.es tests/self/lex-token.es tests/self/lex-scan.es tests/self/parse-util.es tests/self/parse-ident.es tests/self/parse-expr.es tests/self/parse-ptrn.es tests/self/parse-type.es tests/self/parse-stmt.es tests/self/parse-defn.es tests/self/parse-prgm.es tests/self/util-tamarin.es tests/self/esc1.es
