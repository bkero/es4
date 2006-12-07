SML_BIN = $(shell dirname `which sml`)
SOURCES = ast.sml main.sml pretty.sml typechk.sml eval.sml mach.sml \
	parser.sml  pretty-rep.sml token.sml
HEAP_SUFFIX = $(shell if [ -e $(SML_BIN)/.arch-n-opsys ]; then $(SML_BIN)/.arch-n-opsys | sed 's/^.*HEAP_SUFFIX=//'; else echo x86-linux; fi )
PARSE_TESTS = tests/ident.js tests/numberliteral.es tests/stringliteral.es tests/listexpr.es tests/mult.es tests/div.es tests/cond.es tests/fexpr.es tests/atident.es tests/assign.es tests/call.es tests/objectref.es tests/objectliteral.es tests/arrayliteral.es tests/cast.es tests/objectpattern.es tests/typedident.es tests/typeexpr.es tests/typedarray.es tests/uniontype.es tests/nullability.es tests/recordtype.es tests/letexpr.es tests/nolist.es tests/arraypattern.es tests/equality.es tests/relational.es tests/primary.es tests/asi.es tests/return.es tests/vardefn.es tests/pragma.es tests/block.es tests/ifstmt.es tests/switch.es tests/destruct.es tests/switchtype.es tests/superstmt.es tests/dowhile.es tests/while.es tests/for.es tests/forin.es tests/foreach.es tests/t.es
#tests/assign_err.es
#tests/nolist_err.es

TC_TESTS = tests/numberliteral.es
EV_TESTS = tests/exec.es

# A total hack to check whether a given CM anchor is installed.
anchorhome=$(shell cat .$(strip $(1)) 2>/dev/null || \
	(echo -e '(\#get (CM.Anchor.anchor "$(strip $(1))"))();\n' \
		| sml \
		| fgrep 'val it =' \
		| sed -e 's/.*\(NONE\|SOME "\(.*\)"\).*/\2/' \
		>.$(strip $(1)) \
	&& cat .$(strip $(1))))

MLBUILD=ml-build
MLBUILD_ARGS=$(if $(call anchorhome,smlnj-tdp),-Ctdp.instrument=true -DBACKTRACE \$$smlnj-tdp/back-trace.cm)

.PHONY: check checktc checkev wc clean cleanml

es4.heap.$(HEAP_SUFFIX): $(wildcard *.sml) pretty-cvt.sml
	$(MLBUILD) $(MLBUILD_ARGS) es4.cm Main.main es4.heap

pretty-cvt.sml: tools/gen-pretty.heap.$(HEAP_SUFFIX) ast.sml
	cd tools && sml @SMLload=gen-pretty.heap ../ast.sml ../pretty-cvt.sml

tools/gen-pretty.heap.$(HEAP_SUFFIX): tools/gen-pretty.cm $(wildcard tools/*.sml)
	cd tools && $(MLBUILD) $(MLBUILD_ARGS) gen-pretty.cm Main.main gen-pretty.heap

check: es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=es4.heap $(PARSE_TESTS)

checktc: es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=es4.heap -tc $(TC_TESTS)
#	sml @SMLload=es4.heap tests/vardefn.es

checkev: es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=es4.heap -ev $(EV_TESTS)

wc:
	wc ${SOURCES}

clean:
	rm -rf .cm tools/.cm es4.heap.$(HEAP_SUFFIX) tools/gen-pretty.heap.$(HEAP_SUFFIX)

cleanml:
	rm -f .smlnj-tdp
