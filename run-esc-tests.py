#!/usr/bin/env python

import array,commands,re,sys,os.path
from time import clock
from os import environ,path,system,remove,listdir
from sys import exit

def find_tests():
  if len(sys.argv)>1:
    tests=sys.argv[1:]
  else:
    ignore_list=['abcfile.es','abcfile_test.es','assembler.es','assembler_test.es','ast.es','ast_encoder.es','bytestream.es','bytestream_test.es','cogen-driver.es','cogen-expr.es','cogen-stmt.es','cogen.es','cogen_test.es','datatypes.es','debug.es','emitter.es','emitter_test.es','esc-driver.es','esc-testdriver.es','esc.es','fib.es','lexer.es','parser.es','test.es','t.es','token.es','util-as3.es','util-es4ri.es','util.es','var.es']
    tests=listdir('tests/self')
    re1=re.compile('.*\.es$')
    i=0
    while(i<len(tests)):
      if not re1.match(tests[i]):
        del tests[i]
        i-=1
      else:
        if tests[i] in ignore_list:
          del tests[i]
          i-=1
        else:
          tests[i]='tests/self/'+tests[i]
      i+=1
  return tests

def generate_driver(test):
  testdriver='{\n  import util.*;\n  import cogen.*;\n  use namespace Ast;\n  use namespace Parser;\n\n  var str=readFile("tests/self/testfile.es");\n  var parser=new Parser(str);\n  var [ts1,nd1]=parser.program();\n  print(Ast::encodeProgram(nd1));\n  dumpABCFile(cogen.cg(nd1),"esc-tmp.es");\n}\n'
  re1=re.compile('tests/self/testfile.es')
  fd=open('tests/self/esc-testdriver.es','w')
  driver=re1.sub(test,testdriver)
  fd.write(driver)
  fd.close()
  return

def build_bytecode():
  cmd='make run-dumped FILE="tests/self/debug.es tests/self/ast.es tests/self/ast_encoder.es tests/self/lexer.es tests/self/parser.es tests/self/util.es tests/self/util-es4ri.es tests/self/bytestream.es tests/self/abcfile.es tests/self/assembler.es tests/self/emitter.es tests/self/cogen-stmt.es tests/self/cogen-expr.es tests/self/cogen.es tests/self/esc-testdriver.es"'
  print('$ %s' % cmd)
  system(cmd)
  return

def write_abc(infile,outfile):
  fd1=open('esc-tmp.es','r')
  bytesstring=None
  for l in fd1:
    if l.startswith("})(["):
      bytestring=l[3:len(l)-1]
      break
  fd1.close()
  if not bytestring:
    print("ERROR: could not find byte string in file %s." % sys.argv[1])
    sys.exit(1)
  bytesarray=eval(bytestring)
  fileobj=open(outfile,mode='wb')
  outvalues=array.array('B')
  outvalues.fromlist(bytesarray)
  outvalues.tofile(fileobj)
  fileobj.close()
  return

def cleanup(testabc):
  if path.exists('esc-tmp.es'):
    remove('esc-tmp.es')
  if path.exists('tests/self/esc-testdriver.es'):
    remove('tests/self/esc-testdriver.es')
  if path.exists(testabc):
    remove(testabc)
  return

def run_test(testabc):
  cmd='%s %s' % (globs['avm'],testabc)
  print('$ %s' % cmd)
  return commands.getoutput(cmd)

def test_output(test,actual):
  testdir='tests/self/baselines'
  (dir,testbase)=os.path.split(test)
  testbase=testdir+'/'+testbase
  if path.exists(testbase+'.expected'):
    fd=open(testbase+'.expected')
    expected=fd.read()
    fd.close()
    if actual!=expected:
      fd2=open(testbase+'.actual','w')
      fd2.write(actual)
      fd2.close()
      failed(test,'\nexpected:\n%s\nactual:\n%s' % (expected,actual))
    else:
      if path.exists(testbase+'.actual'):
        remove(testbase+'.actual')
      passed(test)
  else:
    fd=open(testbase+'.actual','w')
    fd.write(actual)
    fd.close()
    failed(test,"expected file '%s.expected' does not exists" % testbase)
  return

def failed(test,msg):
  print('%s FAILED %s' % (test,msg))
  globs['output']+='%s FAILED %s\n' % (test,msg)
  globs['fails']+=1
  return

def passed(test,msg=''):
  print('%s PASSED' % test)
  tm=clock()-globs['testtime']
  globs['output']+='%s PASSED (time %.2fs)\n' % (test,tm)
  globs['passes']+=1
  return

def print_report():
  tm=clock()-globs['time']
  print('\nTESTS FINISHED:')
  print('RESULTS:')
  print(globs['output'])
  print('\n')
  print('total results (total time %.2fs):' % tm)
  print('passes   : %d' % globs['passes'])
  print('failures : %d' % globs['fails'])
  return


def main():
  if 'AVM' in environ:
    globs['avm']=environ['AVM'].strip()
  if not path.exists(globs['avm']):
    print "ERROR AVM environment variable set to '%s' does not exist" % globs['avm']
    exit(1)

  tests=find_tests()
  print("starting esc test suite, running %d tests: %s" % (len(tests),tests))
  re1=re.compile('\.[a-z]+')
  globs['time']=clock()
  for test in tests:
    testabc=re1.sub(".abc",test)
    print("testing...%s" % test)
    cleanup(testabc)
    globs['testtime']=clock()
    generate_driver(test)
    build_bytecode()
    if not path.exists('esc-tmp.es'):
      failed(test,'did not parse %s and generate esc-tmp.es\n' % test)
      continue
    write_abc(test,testabc)
    if not path.exists(testabc):
      failed('did not generate %s' % testabc)
      continue
    out=run_test(testabc)
    test_output(test,out)
  print_report()

globs={'avm':'','output':'','fails':0,'passes':0}
if __name__ == "__main__":
  main()

