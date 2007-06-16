WINROOT=/src/adobe_flashfarm/main
ROOT=/cygdrive/c/$(WINROOT)
ASC=asc
ASCFLAGS=-d -import $(WINROOT)/player/FlashPlayer/avmglue/global.abc -import $(WINROOT)/player/FlashPlayer/avmglue/playerglobal.abc
AVM=$(ROOT)/player/FlashPlayer/avmplus/platform/win32/obj_8/shell/Debug_Debugger/avmplus_sd.exe

.SUFFIXES: .abc .es

.es.abc:
	$(ASC) $(ASCFLAGS) $<

test: esc.abc assembler_test.abc emitter_test.abc
	echo '{ import es4.*; /* testABCByteStream(); testABCFile(); */ testABCAssembler(); /* testABCEmitter(); */ }' > testdriver.as
	$(ASC) $(ASCFLAGS) testdriver.as
	$(AVM) $^ testdriver.abc

esc.abc: esc.es util.es bytestream.es abcfile.es assembler.es emitter.es
	$(ASC) $(ASCFLAGS) esc.es

clean:
	rm -f *~ *.abc testdriver.as
