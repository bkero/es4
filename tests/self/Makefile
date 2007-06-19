WINROOT=/src/adobe_flashfarm/main
ROOT=/cygdrive/c/$(WINROOT)
ASC=asc
ASCFLAGS=-d -import $(WINROOT)/player/FlashPlayer/avmglue/global.abc -import $(WINROOT)/player/FlashPlayer/avmglue/playerglobal.abc
AVM=$(ROOT)/player/FlashPlayer/avmplus/platform/win32/obj_8/shell/Debug_Debugger/avmplus_sd.exe

.SUFFIXES: .abc .es

.es.abc:
	$(ASC) $(ASCFLAGS) $<

test: test_hello test_fib

test_hello: esc.abc test.abc
	echo '{ import assembler.*; assembler.testHelloWorld(); }' > testdriver.es
	$(MAKE) testdriver.abc
	rm -f hello-test.es
	$(AVM) $^ testdriver.abc
	$(MAKE) hello-test.abc
	$(AVM) hello-test.abc

test_fib: esc.abc test.abc
	echo '{ import assembler.*; assembler.testFib(); }' > testdriver.es
	$(MAKE) testdriver.abc
	rm -f fib-test.es
	$(AVM) $^ testdriver.abc
	$(MAKE) fib-test.abc
	$(AVM) fib-test.abc

esc.abc: esc.es util-as3.es util.es bytestream.es abcfile.es assembler.es emitter.es
	$(ASC) $(ASCFLAGS) esc.es

test.abc: test.es bytestream_test.es abcfile_test.es assembler_test.es emitter_test.es
	$(ASC) $(ASCFLAGS) test.es

clean:
	rm -f *~ *.abc testdriver.* hello-test.es fib-test.es esc-ri.es

