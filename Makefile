# The following licensing terms and conditions apply and must be
# accepted in order to use the Reference Implementation:
# 
#    1. This Reference Implementation is made available to all
# interested persons on the same terms as Ecma makes available its
# standards and technical reports, as set forth at
# http://www.ecma-international.org/publications/.
# 
#    2. All liability and responsibility for any use of this Reference
# Implementation rests with the user, and not with any of the parties
# who contribute to, or who own or hold any copyright in, this Reference
# Implementation.
# 
#    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
# HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
# BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
# OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
# IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# End of Terms and Conditions
# 
# Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
# Software ASA, and others.

# ------------------------------------------------------------
# configurable parameters
# ------------------------------------------------------------

SML := sml
MLBUILD := ml-build

# ------------------------------------------------------------
# file listings
# ------------------------------------------------------------

SOURCES := ast.sml boot.sml decimal-params.sml decimal.sml defn.sml eval.sml logerr.sml mach.sml main.sml multiname.sml name.sml native.sml lexer.sml parser.sml pretty-rep.sml pretty.sml profile.sml token.sml ustring.sml verify.sml type.sml fixture.sml fixture.sig smlnj.sml decimal-external.sml

DECIMAL_SOURCES=decimal/decimal-ffi.c decimal/decNumber/decNumber.c decimal/decNumber/decContext.c decimal/decNumber/decimal128.c decimal/decNumber/decimal64.c decimal/decNumber/decimal32.c

EV_TESTS := tests/exec.es

# ------------------------------------------------------------
# make functions
# ------------------------------------------------------------

sml=$(shell echo -e 'TextIO.output (TextIO.stdErr, $(strip $(1)));' | ($(SML) >/dev/null) 2>&1)

anchorhome=$(call sml,valOf((\#get (CM.Anchor.anchor "$(strip $(1))")())))

# ------------------------------------------------------------
# build parameters
# ------------------------------------------------------------

HEAP_SUFFIX := $(call sml,SMLofNJ.SysInfo.getHeapSuffix())

#ifneq ($(call anchorhome,smlnj-tdp),)
# TODO: uncomment this once everyone is using the latest SML/NJ svn sources
#MLBUILD_ARGS=-Ctdp.instrument=true -DBACKTRACE \$$smlnj-tdp/back-trace.cm
#endif

# ------------------------------------------------------------
# targets
# ------------------------------------------------------------

.PHONY: compile check checktc checkev wc clean cleanml profile decimal exec-release heap-release

compile: es4-init.heap.$(HEAP_SUFFIX)

es4-init.heap.$(HEAP_SUFFIX): $(wildcard *.sml) pretty-cvt.sml
	$(MLBUILD) $(MLBUILD_ARGS) es4.cm SMLofNJEntry.main es4-init.heap

pretty-cvt.sml: tools/gen-pretty.heap.$(HEAP_SUFFIX) ast.sml
	cd tools && $(SML) @SMLload=gen-pretty.heap ../ast.sml ../pretty-cvt.sml

tools/gen-pretty.heap.$(HEAP_SUFFIX): tools/gen-pretty.cm tools/gen-convert.sml tools/gen-pretty.sml tools/quasiquote.sml tools/smlast.sml
	cd tools && $(MLBUILD) $(MLBUILD_ARGS) gen-pretty.cm GenPretty.main gen-pretty.heap

tools/unit.heap.$(HEAP_SUFFIX): tools/unit.cm tools/unit.sml $(wildcard *.sml) pretty-cvt.sml
	cd tools && $(MLBUILD) $(MLBUILD_ARGS) unit.cm UnitTests.main unit.heap

# TODO: "check" should do all the *.test files, not just parse tests
check: tools/unit.heap.$(HEAP_SUFFIX) es4-init.heap.$(HEAP_SUFFIX)
	$(SML) @SMLload=tools/unit.heap $(TRACE) tests/parse.test

checktc: tools/unit.heap.$(HEAP_SUFFIX) es4-init.heap.$(HEAP_SUFFIX)
	$(SML) @SMLload=tools/unit.heap $(TRACE) tests/tc.test

checkev: tools/unit.heap.$(HEAP_SUFFIX) es4-init.heap.$(HEAP_SUFFIX)
	$(SML) @SMLload=tools/unit.heap $(TRACE) tests/exec/exec.test

checklth: tools/unit.heap.$(HEAP_SUFFIX) es4-init.heap.$(HEAP_SUFFIX)
	$(SML) @SMLload=tools/unit.heap $(TRACE) tests/lth_tests/lth_tests.test

es4-dump.heap.$(HEAP_SUFFIX): es4-init.heap.$(HEAP_SUFFIX) $(wildcard builtins/*.es)
	$(SML) @SMLload=es4-init.heap -dump es4-dump.heap

smoketest: es4-dump.heap.$(HEAP_SUFFIX)
	$(SML) @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/Array/15.4.2.2-2.js
	$(SML) @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/Boolean/15.6.1.js
	$(SML) @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/Date/15.9.2.2-6.js
	$(SML) @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/LexicalConventions/7.6.js
	$(SML) @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/Statements/12.6.3-4.js
	$(SML) @SMLload=es4-dump.heap -e $(TRACE) tests/spidermonkey/ecma/shell.js tests/spidermonkey/ecma/TypeConversion/9.3.js

dump-heap: es4-dump.heap.$(HEAP_SUFFIX)

# Do *not* give this dependencies to see if the heap is up-to-date.
run-dumped:
	$(SML) @SMLload=es4-dump.heap -e $(TRACE) $(FILE)

# Obsolete now?
run: 
	$(SML) @SMLload=es4-init.heap -e $(TRACE) $(FILE)

repl: es4-dump.heap.$(HEAP_SUFFIX)
	perl bin/repl-with-readline.pl

replNoReadline: es4-init.heap.$(HEAP_SUFFIX)
	$(SML) @SMLload=es4-init.heap -r

wc:
	wc ${SOURCES}

clean:
	rm -rf .cm tools/.cm es4-init.heap.$(HEAP_SUFFIX) tools/gen-pretty.heap.$(HEAP_SUFFIX)

profile: 
	touch multiname.sml mach.sml eval.sml 
	$(SML) -Ctdp.instrument=true profile.sml 2>&1 | tee profile.txt

decimal:
	cd decimal && make decimal && cp decimal ../bin/

exec-release: exec/es4.tar.gz

exec/es4.tar.gz: dump-heap decimal
	rm -rf exec/es4
	mkdir -p exec/es4
	heap2exec es4-dump.heap.$(HEAP_SUFFIX) exec/es4/es4
	cp bin/decimal exec/es4/
	chmod -R u=rwx,go=rx exec/es4
	cd exec && tar cf es4.tar es4
	gzip -v9 exec/es4.tar

heap-release: heap/es4.tar.gz

heap/es4.tar.gz: dump-heap decimal
	rm -rf heap
	mkdir -p heap/es4
	cp bin/run-cygwin.sh heap/es4/es4
	cp es4-dump.heap.$(HEAP_SUFFIX) heap/es4/es4.heap.$(HEAP_SUFFIX)
	cp bin/decimal heap/es4/
	chmod -R u=rwx,go=rx heap/es4
	chmod u=rw,go=r heap/es4/es4.heap.$(HEAP_SUFFIX)
	cd heap && tar cf es4.tar es4
	gzip -v9 heap/es4.tar
