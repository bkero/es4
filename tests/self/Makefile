WINROOT=/src/adobe_flashfarm/main
ROOT=/cygdrive/c/$(WINROOT)
ASC=asc
ASCFLAGS=-d -import $(WINROOT)/player/FlashPlayer/avmglue/global.abc -import $(WINROOT)/player/FlashPlayer/avmglue/playerglobal.abc
AVM=$(ROOT)/player/FlashPlayer/avmplus/platform/win32/obj_8/shell/Debug_Debugger/avmplus_sd.exe

.SUFFIXES: .abc .es

.es.abc:
	$(ASC) $(ASCFLAGS) $<

test: util.abc bytestream.abc abcfile.abc assembler.abc assembler_test.abc
	echo '{ import es4.*; testABCByteStream(); testABCFile(); testABCAssembler() }' > testdriver.as
	$(ASC) $(ASCFLAGS) testdriver.as
	$(AVM) $^ testdriver.abc

clean:
	rm -f *~ *.abc testdriver.as
