Notes for running testcases on es4 ri.

REQUIREMENTS:

- some basic python in your path
- the es4 ri monotone repository synced

SETUP:

- build ri
$ cd com.mozilla.es4.smlnj
$ make clean
$ make
$ sml @SMLload=es4-init.heap -dump es4-dump.heap


BUILTINS:
To run builtins tests
$ cd test
$ ./runtests.py builtins/
Writing results to 2007-11-08.1.html
tests started: 2007-11-08 14:20:09.878803
current configuration: es4ri
Executing 24 tests against : es4-dump.heap.x86-cygwin
...
test run PASSED!
Tests complete at 2007-11-08 14:20:56.793803
Start Date: 2007-11-08 14:20:09.878803
End Date  : 2007-11-08 14:20:56.793803
Test Time : 0:00:46.915000
passes               : 282
failures             : 0
expected failures    : 5
Results were written to 2007-11-08.1.html

SMOKES:
To run smoke tests:
$ ./runtests.py smokes/
...

TEST CONFIGURATION:
The file testconfig.txt contains a description of known failures.  All known failures should
have trac issue numbers http://bugs.ecmascript.org.  Marking a failure as known is a way
to account for active issues.

The format of testconfig.txt:

builtins/array/concat                                                       ,es4ri   ,expectedfail   ,http://bugs.ecmascript.org/ticket/290 Array concat gives exception
builtins/array/sort:sort\(\) var A=...undefined.*                           ,es4ri   ,expectedfail   ,http://bugs.ecmascript.org/ticket/292 sort throws exception when contains undefined element

dschaffe@adobe.com 8 Nov 2007


