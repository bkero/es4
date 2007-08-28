export TESTNAME="hello-test"
make run FILE="tests/self/debug.es tests/self/ast.es tests/self/ast_encoder.es tests/self/util.es tests/self/util-es4ri.es tests/self/bytestream.es tests/self/abcfile.es tests/self/assembler.es tests/self/emitter.es tests/self/cogen-stmt.es tests/self/cogen-expr.es tests/self/cogen.es tests/self/cogen_test.es tests/self/cogen-driver.es"

java -cp c:/hg/my-asc-working/lib/asc.jar macromedia.asc.embedding.Main -d -m $TESTNAME.es

/cygdrive/c/hg/my-tamarin-central/platform/win32/obj_8/shell/Release_Debugger/avmplus_s.exe $TESTNAME.abc