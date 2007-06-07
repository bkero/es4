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

.PHONY: check checktc checkev wc clean cleanml profile decimal

es4-init.heap.$(HEAP_SUFFIX): $(wildcard *.sml) pretty-cvt.sml
	$(MLBUILD) $(MLBUILD_ARGS) es4.cm Main.main es4-init.heap

pretty-cvt.sml: tools/gen-pretty.heap.$(HEAP_SUFFIX) ast.sml
	cd tools && sml @SMLload=gen-pretty.heap ../ast.sml ../pretty-cvt.sml

tools/gen-pretty.heap.$(HEAP_SUFFIX): tools/gen-pretty.cm tools/gen-convert.sml tools/gen-pretty.sml tools/quasiquote.sml tools/smlast.sml
	cd tools && $(MLBUILD) $(MLBUILD_ARGS) gen-pretty.cm GenPretty.main gen-pretty.heap

tools/unit.heap.$(HEAP_SUFFIX): tools/unit.cm tools/unit.sml $(wildcard *.sml) pretty-cvt.sml
	cd tools && $(MLBUILD) $(MLBUILD_ARGS) unit.cm UnitTests.main unit.heap

# TODO: "check" should do all the *.test files, not just parse tests
check: tools/unit.heap.$(HEAP_SUFFIX) es4-init.heap.$(HEAP_SUFFIX)
	sml @SMLload=tools/unit.heap $(TRACE) tests/parse.test

checktc: tools/unit.heap.$(HEAP_SUFFIX) es4-init.heap.$(HEAP_SUFFIX)
	sml @SMLload=tools/unit.heap $(TRACE) tests/tc.test

checkev: tools/unit.heap.$(HEAP_SUFFIX) es4-init.heap.$(HEAP_SUFFIX)
	sml @SMLload=tools/unit.heap $(TRACE) tests/exec/exec.test

checklth: tools/unit.heap.$(HEAP_SUFFIX) es4-init.heap.$(HEAP_SUFFIX)
	sml @SMLload=tools/unit.heap $(TRACE) tests/lth_tests/lth_tests.test

es4-dump.heap.$(HEAP_SUFFIX): es4-init.heap.$(HEAP_SUFFIX) $(wilcard builtins/*.es)
	sml @SMLload=es4-init.heap -dump es4-dump.heap

smoketest: es4-dump.heap.$(HEAP_SUFFIX)
	sml @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/Array/15.4.2.2-2.js
	sml @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/Boolean/15.6.1.js
	sml @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/Date/15.9.2.2-6.js
	sml @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/LexicalConventions/7.6.js
	sml @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/Statements/12.6.3-4.js
	sml @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/TypeConversion/9.3.js

dump-heap: es4-dump.heap.$(HEAP_SUFFIX)

# Do *not* give this dependencies to see if the heap is up-to-date.
run-dumped:
	sml @SMLload=es4-dump.heap -e $(TRACE) $(FILE)

# Obsolete now?
run: 
	sml @SMLload=es4-init.heap $(TRACE) -e $(FILE)

repl: es4-dump.heap.$(HEAP_SUFFIX)
	perl bin/repl-with-readline.pl

replNoReadline: es4-init.heap.$(HEAP_SUFFIX)
	sml @SMLload=es4-init.heap -r

wc:
	wc ${SOURCES}

clean:
	rm -rf .cm tools/.cm es4-init.heap.$(HEAP_SUFFIX) tools/gen-pretty.heap.$(HEAP_SUFFIX)

profile: 
	touch multiname.sml mach.sml eval.sml 
	sml -Ctdp.instrument=true profile.sml 2>&1 | tee profile.txt

exec: dump-heap
	rm -rf exec
	mkdir -p exec 
	heap2exec es4-dump.heap.$(HEAP_SUFFIX) ./exec/es4
	gzip ./exec/es4

decimal:
	cd decimal && make decimal && cp decimal ../bin/
