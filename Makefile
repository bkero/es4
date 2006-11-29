SML_BIN = $(shell dirname `which sml`)
SOURCES = ast.sml main.sml pretty.sml typechk.sml eval.sml mach.sml \
	parser.sml  pretty-rep.sml token.sml
HEAP_SUFFIX = $(shell if [ -e $(SML_BIN)/.arch-n-opsys ]; then $(SML_BIN)/.arch-n-opsys | sed 's/^.*HEAP_SUFFIX=//'; else echo x86-linux; fi )
PARSE_TESTS = tests/ident.js tests/numberliteral.es tests/stringliteral.es tests/listexpr.es tests/mult.es tests/div.es tests/cond.es tests/fexpr.es tests/atident.es tests/assign.es tests/call.es tests/objectref.es tests/objectliteral.es tests/arrayliteral.es tests/cast.es tests/objectpattern.es tests/typedident.es tests/typeexpr.es tests/typedarray.es tests/vardefn.es tests/uniontype.es tests/nullability.es tests/recordtype.es tests/letexpr.es tests/nolist.es tests/arraypattern.es tests/equality.es tests/relational.es tests/primary.es tests/asi.es tests/return.es
#tests/assign_err.es
#tests/nolist_err.es

TC_TESTS = tests/numberliteral.es
MAKE_HEAP=ml-build -Ctdp.instrument=true \$$smlnj-tdp/back-trace.cm

es4.heap.$(HEAP_SUFFIX): $(wildcard *.sml) pretty-cvt.sml
	$(MAKE_HEAP) es4.cm Main.main es4.heap

pretty-cvt.sml: tools/gen-pretty.heap.$(HEAP_SUFFIX) ast.sml
	cd tools && sml @SMLload=gen-pretty.heap ../ast.sml ../pretty-cvt.sml

tools/gen-pretty.heap.$(HEAP_SUFFIX): tools/gen-pretty.cm $(wildcard tools/*.sml)
	cd tools && $(MAKE_HEAP) gen-pretty.cm Main.main gen-pretty.heap

check: es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=es4.heap $(PARSE_TESTS)

checktc: es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=es4.heap -tc $(TC_TESTS)
#	sml @SMLload=es4.heap tests/vardefn.es

wc:
	wc ${SOURCES}

