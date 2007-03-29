# ------------------------------------------------------------
# file listings
# ------------------------------------------------------------

SOURCES := ast.sml main.sml pretty.sml verify.sml eval.sml mach.sml \
	parser.sml  pretty-rep.sml token.sml defn.sml logerr.sml native.sml 

EV_TESTS := tests/exec.es

# ------------------------------------------------------------
# make functions
# ------------------------------------------------------------

sml=$(shell echo -e 'TextIO.output (TextIO.stdErr, $(strip $(1)));' | (sml >/dev/null) 2>&1)

anchorhome=$(call sml,valOf((\#get (CM.Anchor.anchor "$(strip $(1))")())))

# ------------------------------------------------------------
# build parameters
# ------------------------------------------------------------

HEAP_SUFFIX := $(call sml,SMLofNJ.SysInfo.getHeapSuffix())

MLBUILD := ml-build

#ifneq ($(call anchorhome,smlnj-tdp),)
# TODO: uncomment this once everyone is using the latest SML/NJ svn sources
#MLBUILD_ARGS=-Ctdp.instrument=true -DBACKTRACE \$$smlnj-tdp/back-trace.cm
#endif

# ------------------------------------------------------------
# targets
# ------------------------------------------------------------

.PHONY: check checktc checkev wc clean cleanml

es4.heap.$(HEAP_SUFFIX): $(wildcard *.sml) lexer.lex pretty-cvt.sml
	$(MLBUILD) $(MLBUILD_ARGS) es4.cm Main.main es4.heap

pretty-cvt.sml: tools/gen-pretty.heap.$(HEAP_SUFFIX) ast.sml
	cd tools && sml @SMLload=gen-pretty.heap ../ast.sml ../pretty-cvt.sml

tools/gen-pretty.heap.$(HEAP_SUFFIX): tools/gen-pretty.cm tools/gen-convert.sml tools/gen-pretty.sml tools/quasiquote.sml tools/smlast.sml
	cd tools && $(MLBUILD) $(MLBUILD_ARGS) gen-pretty.cm GenPretty.main gen-pretty.heap

tools/unit.heap.$(HEAP_SUFFIX): tools/unit.cm tools/unit.sml $(wildcard *.sml) pretty-cvt.sml
	cd tools && $(MLBUILD) $(MLBUILD_ARGS) unit.cm UnitTests.main unit.heap

# TODO: "check" should do all the *.test files, not just parse tests
check: tools/unit.heap.$(HEAP_SUFFIX) es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=tools/unit.heap $(TRACE) tests/parse.test

checktc: tools/unit.heap.$(HEAP_SUFFIX) es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=tools/unit.heap $(TRACE) tests/tc.test

checkev: tools/unit.heap.$(HEAP_SUFFIX) es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=tools/unit.heap $(TRACE) tests/exec/exec.test

checklth: tools/unit.heap.$(HEAP_SUFFIX) es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=tools/unit.heap $(TRACE) tests/lth_tests/lth_tests.test

run: es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=es4.heap $(TRACE) -ev $(FILE)

repl: es4.heap.$(HEAP_SUFFIX)
	perl repl-with-readline.pl

replNoReadline: es4.heap.$(HEAP_SUFFIX)
	sml @SMLload=es4.heap -r

wc:
	wc ${SOURCES}

clean:
	rm -rf .cm tools/.cm es4.heap.$(HEAP_SUFFIX) tools/gen-pretty.heap.$(HEAP_SUFFIX)
