SML_BIN = $(shell dirname "`which sml`" )
SOURCES = ast.sml main.sml pretty.sml verify.sml eval.sml mach.sml \
	parser.sml  pretty-rep.sml token.sml defn.sml
# HEAP_SUFFIX = $(shell if [ -e "$(SML_BIN)/.arch-n-opsys" ]; then "$(SML_BIN)/.arch-n-opsys" | sed 's/^.*HEAP_SUFFIX=//'; else echo x86-linux; fi )
# dave, fixme: previous line is commented out because with it the heap does not get rebuilt on Jeff's machine

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
# MLBUILD_ARGS=$(if $(call anchorhome,smlnj-tdp),-Ctdp.instrument=true -DBACKTRACE \$$smlnj-tdp/back-trace.cm)

.PHONY: check checktc checkev wc clean cleanml

es4.heap.$(HEAP_SUFFIX): $(wildcard *.sml) pretty-cvt.sml
	$(MLBUILD) $(MLBUILD_ARGS) es4.cm Main.main es4.heap

pretty-cvt.sml: tools/gen-pretty.heap.$(HEAP_SUFFIX) ast.sml
	cd tools && sml @SMLload=gen-pretty.heap ../ast.sml ../pretty-cvt.sml

tools/gen-pretty.heap.$(HEAP_SUFFIX): tools/gen-pretty.cm tools/gen-convert.sml tools/gen-pretty.sml tools/quasiquote.sml tools/smlast.sml
	cd tools && $(MLBUILD) $(MLBUILD_ARGS) gen-pretty.cm GenPretty.main gen-pretty.heap

tools/unit.heap.$(HEAP_SUFFIX): tools/unit.cm tools/unit.sml
	cd tools && $(MLBUILD) $(MLBUILD_ARGS) unit.cm UnitTests.main unit.heap

# TODO: "check" should do all the *.test files, not just parse tests
check: tools/unit.heap.$(HEAP_SUFFIX) es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=tools/unit.heap tests/parse.test

checktc: tools/unit.heap.$(HEAP_SUFFIX) es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=tools/unit.heap tests/tc.test

checkev: es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=es4.heap -ev $(EV_TESTS)

wc:
	wc ${SOURCES}

clean:
	rm -rf .cm tools/.cm es4.heap.$(HEAP_SUFFIX) tools/gen-pretty.heap.$(HEAP_SUFFIX)

cleanml:
	rm -f .smlnj-tdp
