es4.heap:
	ml-build es4.cm Main.main es4.heap

gen-pretty.heap: gen-pretty.cm gen-pretty.sml
	ml-build gen-pretty.cm GenPretty.main gen-pretty.heap

check: es4.heap
	sml @SMLload=es4.heap tests/ident.js
	sml @SMLload=es4.heap tests/numberliteral.es
	sml @SMLload=es4.heap tests/listexpr.es
	sml @SMLload=es4.heap tests/mult.es
	sml @SMLload=es4.heap tests/div.es
	sml @SMLload=es4.heap tests/cond.es

gen: gen-pretty.heap
	sml @SMLload=gen-pretty.heap ast.sml pretty-cvt-UNTESTED.sml
