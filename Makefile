es4.heap: $(wildcard *.sml)
	ml-build es4.cm Main.main es4.heap

pretty-cvt.sml: tools/gen-pretty.heap ast.sml
	cd tools && sml @SMLload=gen-pretty.heap ../ast.sml ../pretty-cvt.sml

tools/gen-pretty.heap: tools/gen-pretty.cm tools/gen-pretty.sml
	cd tools && ml-build gen-pretty.cm GenPretty.main gen-pretty.heap

check: es4.heap
	sml @SMLload=es4.heap tests/ident.js
	sml @SMLload=es4.heap tests/numberliteral.es
	sml @SMLload=es4.heap tests/stringliteral.es
	sml @SMLload=es4.heap tests/listexpr.es
	sml @SMLload=es4.heap tests/mult.es
	sml @SMLload=es4.heap tests/div.es
	sml @SMLload=es4.heap tests/cond.es
	sml @SMLload=es4.heap tests/fexpr.es
	sml @SMLload=es4.heap tests/atident.es
	sml @SMLload=es4.heap tests/assign.es
#	sml @SMLload=es4.heap tests/assign_err.es
	sml @SMLload=es4.heap tests/call.es
	sml @SMLload=es4.heap tests/objref.es
	sml @SMLload=es4.heap tests/objectliteral.es
	sml @SMLload=es4.heap tests/arrayliteral.es
	sml @SMLload=es4.heap tests/cast.es
	sml @SMLload=es4.heap tests/objectpattern.es
	sml @SMLload=es4.heap tests/typedident.es
	sml @SMLload=es4.heap tests/typeexpr.es
	sml @SMLload=es4.heap tests/vardefn.es
