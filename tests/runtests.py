#!/usr/bin/env python
#
# simple script that executes es4ri tests. 
#


import os, os.path, sys, getopt, datetime, pipes, glob, itertools, tempfile, string, re, platform
from os.path import *
from os import getcwd,environ,walk
from datetime import datetime
from glob import glob
from sys import argv, exit
from getopt import getopt
from itertools import count

verbose = False
timestamps = True
forcerebuild = False
sml='/usr/local/smlnj/bin/sml'
es4ri='es4-dump.heap.x86-cygwin'

# needed for pipe
fd,tmpfile = tempfile.mkstemp()
os.close(fd)

ostype={'CYGWIN_NT-5.2':'win','CYGWIN_NT-5.1':'win','Windows':'win','Darwin':'mac','Linux':'lnx','Solaris':'sol',}[platform.system()]

globs = { 'exclude':[], 'tmpfile':tmpfile, 'config':'es4ri', 'vmargs':''}
if 'CONFIG' in environ:
	globs['config'] = environ['CONFIG'].strip()
if 'VMARGS' in environ:
	globs['vmargs'] = environ['VMARGS'].strip()

def verbose_print(m, start="", end=""):
	if verbose:
		js_print(m, start, end)

def err_print(m):
	js_print(m, "<font color=#990000>", "</font><br/>")

def js_print(m, start_tag="<p><tt>", end_tag="</tt></p>"):
	print m
	if js_output:
		js_output_f.write("%s %s %s\n" % (start_tag, m, end_tag))
		js_output_f.flush()

pf = 'runtests.properties'
if os.path.exists(pf):
	verbose_print( "reading properties from file '%s'" % (pf) )
	fd = open(pf,'r')
	for l in fd:
		setting = l.strip().split('=')
		if l.startswith('#') or len(setting) < 2 or len(setting[1]) <= 0:
			continue
		val = setting[1].strip()
		option = setting[0].split('.')  # see if we have x.y = z
		nm = option[0].strip()
		if len(option) > 1:
			val = globs[nm] + ' ' + val  # concat
		globs[nm] = val
	fd.close()

def usage(c):
	print "usage: %s [options] [tests]" % basename(argv[0])
	print " -v --verbose       enable additional output"
	print " -x --exclude       comma separated list of directories to skip"
	print " -h --help          display help and exit"
	print " -t --notime        do not generate timestamps (cleaner diffs)"
	print " -c --config        sets the config string [default qvm]"
	print "    --vmargs	       args to pass to vm"
	exit(c)

try:
	opts, args = getopt(argv[1:], "vx:htc:", ["verbose","exclude=","help","notime","config=","vmargs="])
except:
	usage(2)

if not args:
	args = ["."]
for o, v in opts:
	if o in ("-v", "--verbose"):
		verbose = True
	elif o in ("-h", "--help"):
		usage(0)
	elif o in ("-x", "--exclude"):
		globs['exclude'] += v.split(",")
	elif o in ("-t", "--notime"):
		timestamps = False
	elif o in ("-c", "--config"):
		globs['config'] = v
	elif o in ("--vmargs"):
		globs['vmargs'] = v
	

exclude = globs['exclude']

def istest(f):
	return (f.endswith(".as") or f.endswith(".js") or f.endswith(".es") and \
	basename(f) != "shell.as" and not f.endswith("Util.as") and \
	basename(f) != "shell.js" and not f.endswith("Util.js") and \
	basename(f) != "shell.es" and not f.endswith("Util.es"))

tests = [a for a in args if isfile(a) and istest(a)]
for a in [d for d in args if isdir(d) and not (basename(d) in exclude)]:
	for d, dirs, files in walk(a):
		tests += [join(d,f) for f in files if istest(f)]
		utils = [d for d in dirs if d+".as" in files]
		for x in [x for x in exclude+utils if x in dirs]:
			dirs.remove(x)

# set the output file name.  let's base its name on the date and platform,
# and give it a sequence number.

now = datetime.today()
for i in count(1):
	js_output = "%d-%s-%s.%d.html" % (now.year, str(now.month).zfill(2), str(now.day).zfill(2), i)
	if not isfile(js_output):
		break

print "Writing results to %s" % js_output
js_output_f = open(js_output, "w")

def verbose_print(m, start="", end=""):
	if verbose:
		js_print(m, start, end)

def err_print(m):
	js_print(m, "<font color=#990000>", "</font><br/>")

def js_print(m, start_tag="<p><tt>", end_tag="</tt></p>"):
	print m
	if js_output:
		js_output_f.write("%s %s %s\n" % (start_tag, m, end_tag))
		js_output_f.flush()

if timestamps:
	# get the start time
	start_time = datetime.today()
	js_print("tests started: %s" % start_time)
js_print("current configuration: %s" % globs['config'])
#
# run the tests
#

allpasses=0
allfails=0
allunpass=0
allexpfails=0
allskips=0
failmsgs=[]
expfailmsgs=[]
unpassmsgs=[]

def parents(d):
	while d != "":
		yield d
		d = dirname(d)
	yield d

# run a command and return its output
def run_pipe(cmd):
	t = pipes.Template()
	t.append("%s 2>&1" % cmd, "--")
	verbose_print(cmd)
	return t.open(globs['tmpfile'], "r")

def list_match(list,test):
	for k in list:
		if re.search(k,test):
			return True
	return False
	
def dict_match(dict,test,value):
	for k in dict.keys():
		if re.search(k,test):
			if dict[k].has_key(value):
				return dict[k][value]

def build_incfiles(as):
	files=[]
	(dir, file) = split(as)
	for p in parents(dir):
		shell = join(p,"shell.as")
		if isfile(shell):
			files.append(shell)
		shell = join(p,"shell.js")
		if isfile(shell):
			files.append(shell)
		shell = join(p,"shell.es")
		if isfile(shell):
			files.append(shell)
	(testdir, ext) = splitext(as)
	for util in glob(join(testdir,"*.as")) + glob(join(dir,"*Util.as")) + glob(join(dir,"*Util.es")):
		files.append(string.replace(util, "$", "\$"))
	for util in glob(join(testdir,"*.js")) + glob(join(dir,"*Util.js")) + glob(join(dir,"*Util.es")):
		files.append(string.replace(util, "$", "\$"))
	return files

def fail(abc, msg, failmsgs):
	msg = msg.strip()
	err_print("   %s" % msg)
	failmsgs += ["%s : %s" % (abc, msg)]

vmargs = globs['vmargs']

js_print("Executing %d tests against : %s" % (len(tests), es4ri));
testnum = len(tests)
for ast in tests:
	if ast.startswith("./"):
		ast=ast[2:]
	testnum -= 1
	lpass = 0
	lfail = 0
	lexpfail = 0
	lunpass = 0
	dir = ast[0:ast.rfind('/')]
	root,ext = splitext(ast)
	tname = root[root.rfind('/')+1:]
	abc = "%s.abc" % root
	settings=dict()
	names=None
	lines=[]
	includes=None
	if isfile(dir+'/testconfig.txt'):
		lines=open(dir+'/testconfig.txt').read().splitlines()
		for i in range(len(lines)):
			if not lines[i].startswith("#"):
				lines[i] = '%s/%s' %(dir,lines[i])
	if isfile('./testconfig.txt'):
		for line in open('./testconfig.txt').read().splitlines():
			lines.append(line)
	for line in lines:
		if line.startswith('#') or len(line)==0:
			continue
		fields = line.split(',')
		for f in range(len(fields)):
			fields[f]=fields[f].strip()
		while len(fields)<4:
			fields.append("");
		names=fields[0].split(':')
		if len(names)==1:
			names.append('.*')
		rs="^%s$" % names[0]
		if re.search(fields[1],globs['config']) and fields[2]=='include':
			if includes==None:
				includes=[]
			includes.append(fields[0])
		if re.search(rs,root) and re.search("^%s$" % fields[1],globs['config']):
			if not settings.has_key(names[1]):
				settings[names[1]]={}
			settings[names[1]][fields[2]]=fields[3]
	if includes and not list_match(includes,root):
		continue
	js_print("%d running %s" % (testnum, ast), "<b>", "</b><br/>");
	if names and dict_match(settings,names[1],"skip"):
		js_print("  skipping")
		allskips += 1
		continue
	line=''
	incfiles=build_incfiles(ast)
	for incfile in incfiles:
		line+="tests/"+incfile+" "
	line+="tests/"+ast
	f = run_pipe("cd ..;%s @SMLload=%s -e %s" % (sml,es4ri,line))
	try:
		for line in f:
			verbose_print(line.strip())
			testcase=""
			if len(line)>9:
				testcase=line.strip()
			if dict_match(settings,testcase,'skip'):
				js_print("  skipping %s" % line.strip())
				allskips+=1
				continue
			if "PASSED!" in line:
				res=dict_match(settings,testcase,'expectedfail')
				if res:
					fail(root, "unexpected pass: " + line.strip() + " reason: "+res, unpassmsgs)
					lunpass += 1
				else:
					lpass += 1
			if "FAILED!" in line:
				res=dict_match(settings,testcase,"expectedfail")
				if res:
					fail(root, "expected failure: " + line.strip() + " reason: "+res, expfailmsgs)
					lexpfail += 1
				else:
					lfail += 1
					fail(root, line, failmsgs)
	except:
		print "exception"
		exit(-1)
	if lpass == 0 and lfail == 0 and lunpass==0 and lexpfail==0:
		res=dict_match(settings,"*","expectedfail")
		if res:
			fail(root, "expected failure: FAILED contained no testcase messages reason: %s" % res,expfailmsgs)
			lexpfail += 1
		else:
			lfail = 1
			fail(root, "   FAILED contained no testcase messages", failmsgs)
	allfails += lfail
	allpasses += lpass
	allexpfails += lexpfail
	allunpass += lunpass
	if lfail or lunpass:
		js_print("   FAILED passes:%d fails:%d unexpected passes: %d expected failures: %d" % (lpass,lfail,lunpass,lexpfail), "", "<br/>")
	else:
		js_print("   PASSED passes:%d fails:%d unexpected passes: %d expected failures: %d" % (lpass,lfail,lunpass,lexpfail), "", "<br/>")

#
# cleanup
#
if failmsgs:
	js_print("\nFAILURES:", "", "<br/>")
	for m in failmsgs:
		js_print("  %s" % m, "", "<br/>")

if expfailmsgs:
	js_print("\nEXPECTED FAILURES:", "", "<br/>")
	for m in expfailmsgs:
		js_print("  %s" % m, "", "<br/>")

if unpassmsgs:
	js_print("\nUNEXPECTED PASSES:", "", "<br/>")
	for m in unpassmsgs:
		js_print("  %s" % m, "", "<br/>")

if not allfails and not allunpass:
	js_print("\ntest run PASSED!")
else:
	js_print("\ntest run FAILED!")

if timestamps:
	end_time = datetime.today()
	js_print("Tests complete at %s" % end_time, "<hr><tt>", "</tt>")
	js_print("Start Date: %s" % start_time, "<tt><br>", "")
	js_print("End Date  : %s" % end_time, "<br>", "")
	js_print("Test Time : %s" % (end_time-start_time), "<br>", "")
js_print("passes               : %d" % allpasses, "<br>", "")
js_print("failures             : %d" % allfails, "<br>", "")
if allunpass>0:
	js_print("unexpected passes    : %d" % allunpass, "<br>", "")
if allexpfails>0:
	js_print("expected failures    : %d" % allexpfails, "<br>", "")
if allskips>0:
	js_print("tests skipped        : %d" % allskips, "<br>", "")

print "Results were written to %s" % js_output

