es4.heap:
	ml-build es4.cm Main.main es4.heap


check: es4.heap
	sml @SMLload=es4.heap tests/fexpr.es
	sml @SMLload=es4.heap tests/ident.js
	sml @SMLload=es4.heap tests/numberliteral.es
	sml @SMLload=es4.heap tests/stringliteral.es
	sml @SMLload=es4.heap tests/listexpr.es
	sml @SMLload=es4.heap tests/mult.es
	sml @SMLload=es4.heap tests/div.es
	sml @SMLload=es4.heap tests/cond.es
	sml @SMLload=es4.heap tests/fexpr.es
