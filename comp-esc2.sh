#./esc1.sh tests/self/debug.es
#./esc1.sh tests/self/ast.es
#./esc1.sh tests/self/decoder.es
#./esc1.sh tests/self/encoder.es
#./esc1.sh tests/self/esc2.es

./esc2.sh tests/self/debug.es
./esc2.sh tests/self/ast.es
./esc2.sh tests/self/decoder.es
./esc2.sh tests/self/encoder.es
./esc2.sh tests/self/esc2.es

#./esc3.sh tests/self/debug.es
#./esc3.sh tests/self/ast.es
#./esc3.sh tests/self/decoder.es
#./esc3.sh tests/self/encoder.es
#./esc.sh tests/self/esc2.es

### compile test case: print('hello world') will do
#./esc1.sh tests/self/t.es
#./esc.sh tests/self/t.es.ast

/work/tamarin/bin/shell -Dinterp -Dverbose tests/self/debug.es.abc tests/self/ast.es.abc tests/self/t.es.ast.abc tests/self/decoder.es.abc tests/self/encoder.es.abc tests/self/esc2.es.abc
