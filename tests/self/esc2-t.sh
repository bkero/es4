cp $1.ast $1.ast1

# encode .ast as .ast.abc
java -cp /work/asc/lib/asc.jar macromedia.asc.embedding.Main $1.ast

#/work/tamarin/bin/shell debug.es.abc ast.es.abc decoder.es.abc encoder.es.abc util-tamarin.es.abc esc2.es.abc -- $1.ast

cp $1.ast $1.ast2
diff -s $1.ast1 $1.ast2
