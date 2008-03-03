# 
# Copyright (c) 2007 David Herman
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# A sample makefile for building a project with ml-editline.

EDITLINE_HOME=..
MLTON=mlton

ifeq ($(shell uname | sed -e 's/MINGW.*/MINGW/'),MINGW)
TERMINAL_ARCH=win32
LIB_TERMINAL=terminal.dll terminal.def
MLTON_LINK_OPT=terminal.def
else
TERMINAL_ARCH=unix
LIB_TERMINAL=libterminal.a
MLTON_LINK_OPT=-lcurses
endif

TERMINAL_HOME=$(EDITLINE_HOME)/terminal
TERMINAL_SOURCES=$(TERMINAL_HOME)/*.mlb $(TERMINAL_HOME)/*.sig $(TERMINAL_HOME)/*.sml
EDITLINE_SOURCES=$(EDITLINE_HOME)/*.mlb $(EDITLINE_HOME)/*.sig $(EDITLINE_HOME)/*.sml

.PHONY: all clean dist

all: example

clean:
	rm -f *~ *.a *.dll *.exe example terminal.def mlb-path-map
	rm -rf dist

dist: all
	rm -rf dist
	mkdir -p dist
	cp example dist/ || cp example.exe dist/
	cp *.dll dist/

libterminal.a:
	cd $(TERMINAL_HOME)/unix && $(MAKE) libterminal.a
	cp $(TERMINAL_HOME)/unix/libterminal.a ./libterminal.a

terminal.dll: $(TERMINAL_HOME)/win32/Release/terminal.dll
	cp $(TERMINAL_HOME)/win32/Release/terminal.dll ./terminal.dll

terminal.def: terminal.dll
	$(LD) --dll --output-def terminal.def terminal.dll
	rm -f a.exe

example: example.mlb $(EDITLINE_SOURCES) $(TERMINAL_SOURCES) $(LIB_TERMINAL) example.sml
	echo TERMINAL_ARCH $(TERMINAL_ARCH) > mlb-path-map
	$(MLTON) -mlb-path-map mlb-path-map -link-opt -L./ -link-opt -lterminal -link-opt $(MLTON_LINK_OPT) example.mlb
	cp $(EDITLINE_HOME)/*.dll ./
