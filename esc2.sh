cp $1.ast $1.ast1

#make run-dumped FILE="tests/self/debug.es tests/self/ast.es tests/self/encoder.es tests/self/decoder.es tests/self/util.es tests/self/util-es4ri.es esc-tmp.ast tests/self/esc2.es"

#./esc1.sh $1

#./esc.sh $1.ast   # compile ast to abc so it can be loaded as an object. this will be handled by eval when it works
java -cp /work/asc/lib/asc.jar macromedia.asc.embedding.Main $1.ast

/work/tamarin/bin/shell tests/self/debug.es.abc tests/self/ast.es.abc tests/self/decoder.es.abc tests/self/encoder.es.abc tests/self/util-tamarin.es.abc tests/self/esc2.es.abc -- $1.ast

cp $1.ast $1.ast2
diff -s $1.ast1 $1.ast2
