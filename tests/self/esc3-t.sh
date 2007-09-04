### translate .ast.abc (ast encoded in abc) to .abc

/work/tamarin/bin/shell -Dinterp -Dverbose util.es.abc debug.es.abc ast.es.abc decoder.es.abc bytestream.es.abc util-tamarin.es.abc assembler.es.abc abcfile.es.abc emitter.es.abc cogen.es.abc cogen-stmt.es.abc cogen-expr.es.abc esc3-t.es.abc -- $1
