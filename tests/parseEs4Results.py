#!/usr/local/bin/python

import os, os.path, sys, getopt, datetime, pipes, glob, itertools, tempfile, string, re, platform
from os.path import *
from os import getcwd,environ,walk
from datetime import datetime
from glob import glob
from sys import argv, exit
from getopt import getopt
from itertools import count

def loadTestConfig(file):
	settings=dict()
	lines=[]
	if isfile(file):
		for line in open(file).read().splitlines():
			lines.append(line)
	for line in lines:
		#print line
		if line.startswith('#') or len(line)==0:
			continue
		line = line.replace('\,','^!^')				#look for \, which indicates not splitting on the ,
		fields = line.split(',')
		fields[0] = fields[0].replace('^!^',',')	#put the comma back (w/o the backslash)
		for f in range(len(fields)):
			fields[f]=fields[f].strip()
		while len(fields)<4:
			fields.append("");
		names=fields[0].split(':')
		if len(names)==1:
			names.append('.*')
		elif len(names) > 2:
			#concat extra : splits back together (if : is part of testcase desc)
			for i in range(2,len(names)):
				names[1] = names[1]+':'+names[i] 
		#rs="^%s$" % names[0]
		for f in range(len(names)):
			names[f]=names[f].strip()
		if re.search(fields[1],'es4ri') and fields[2]=='include':
			if includes==None:
				includes=[]
			includes.append(fields[0])
		#if re.search(rs,root) and re.search("^%s$" % fields[1],'es4ri'):
		if re.search("^%s$" % fields[1],'es4ri'):
			if not settings.has_key(names[0]):
				settings[names[0]]={}
			if not settings[names[0]].has_key(names[1]):
				settings[names[0]][names[1]]={}
			settings[names[0]][names[1]][fields[2]]=fields[3]
	return settings
			
def dict_match(dict,test,value):
	for k in dict.keys():
		if re.search(k,test):
			if dict[k].has_key(value):
				return dict[k][value]
				
def settings_match(test,testcase,value):
	if settings.has_key(test):
		for k in settings[test].keys():
			if re.search(k,testcase):
				if settings[test][k].has_key(value):
					return settings[test][k][value]
	return None

def err_print(m):
	verbose_print(m, "<font color=#990000>", "</font><br/>")

def js_print(m, start_tag="<p><tt>", end_tag="</tt></p>"):
	print m
	if js_output:
		js_output_f.write("%s %s %s\n" % (start_tag, m, end_tag))
		js_output_f.flush()
		
def verbose_print(m, start="", end=""):
	if verbose:
		js_print(m, start, end)

def fail(abc, msg, failmsgs):
	msg = msg.strip()
	err_print("   %s" % msg)
	failmsgs += ["%s : %s" % (abc, msg)]

def parseTestResults(file):
	lpass = 0
	lfail = 0
	lexpfail = 0
	lunpass = 0
	allpasses=0
	allfails=0
	allunpass=0
	allexpfails=0
	allskips=0
	failmsgs=[]
	expfailmsgs=[]
	unpassmsgs=[]
	lines=[]
	
	f = open(file,'r')
	test = ""
	testcase = ""
	for line in f:
		#verbose_print(line.strip())
		#find testcase name by looking for @SMLload line
		if "SMLload" in line:
			#process last test results first
			if test != "":
				if lpass == 0 and lfail == 0 and lunpass==0 and lexpfail==0:
					res=settings_match(test,"*","expectedfail")
					if res:
						fail(test, "expected failure: FAILED contained no testcase messages reason: %s" % res,expfailmsgs)
						lexpfail += 1
					else:
						lfail = 1
						fail(test, "   FAILED contained no testcase messages", failmsgs)
				allfails += lfail
				allpasses += lpass
				allexpfails += lexpfail
				allunpass += lunpass
				if lfail or lunpass:
					verbose_print("   FAILED passes:%d fails:%d unexpected passes: %d expected failures: %d" % (lpass,lfail,lunpass,lexpfail), "", "<br/>")
				else:
					verbose_print("   PASSED passes:%d fails:%d unexpected passes: %d expected failures: %d" % (lpass,lfail,lunpass,lexpfail), "", "<br/>")
			
			#get new testcase name
			test=line.strip().split()[-1]
			test, ext = splitext(test)
			#reset per testcase vars
			lpass = 0
			lfail = 0
			lexpfail = 0
			lunpass = 0
			verbose_print ('Test: %s' % test,"<b>","</b><br>")
		elif "PASSED!" in line:
			testcase = line.strip()
			res=settings_match(test,testcase,'expectedfail')
			if res:
				fail(test, "unexpected pass: " + line.strip() + " reason: "+res, unpassmsgs)
				lunpass += 1
			else:
				lpass += 1
		elif "FAILED!" in line:
			testcase = line.strip()
			res=settings_match(test,testcase,"expectedfail")
			if res:
				fail(test, "expected failure: " + line.strip() + " reason: "+res, expfailmsgs)
				lexpfail += 1
			else:
				lfail += 1
				fail(test, line, failmsgs)
		#TODO: fix how skips go over testcases and / or tests
		if settings_match(test,testcase,'skip'):
			js_print("  skipping %s" % testcase)
			allskips+=1
			testcase=""
			
			
	#print summary
	if failmsgs:
		js_print("\nFAILURES:", "", "<br/>")
		for m in failmsgs:
			js_print("%s" % m, "", "<br/>")
			if createTCFile:
				resultStr = m[0:m.index('FAILED')].strip()
				fc = resultStr.find(':')
				testStr = resultStr[0:fc]
				testCaseStr = createReString(resultStr[fc:])
				TCFile.write("%s%s, %s, %s, %s\n" % (testStr,testCaseStr, 'es4ri', 'expectedfail', 'auto-generated error')) 
	
	if expfailmsgs:
		js_print("\nEXPECTED FAILURES:", "", "<br/>")
		for m in expfailmsgs:
			js_print("%s" % m, "", "<br/>")
	
	if unpassmsgs:
		js_print("\nUNEXPECTED PASSES:", "", "<br/>")
		for m in unpassmsgs:
			js_print("%s" % m, "", "<br/>")
	
	if not allfails and not allunpass:
		js_print("\ntest run PASSED!")
	else:
		js_print("\ntest run FAILED!")

	js_print("passes               : %d" % allpasses, "<br>", "")
	js_print("failures             : %d" % allfails, "<br>", "")
	if allunpass>0:
		js_print("unexpected passes    : %d" % allunpass, "<br>", "")
	if allexpfails>0:
		js_print("expected failures    : %d" % allexpfails, "<br>", "")
	if allskips>0:
		js_print("tests skipped        : %d" % allskips, "<br>", "")

def createReString(str):
	str = str.replace('\\','\\\\')
	str = str.replace('(',r'\(')
	str = str.replace(')',r'\)')
	str = str.replace(',',r'\,')
	str = str.replace('.',r'\.')
	str = str.replace('[',r'\[')
	str = str.replace(']',r'\]')
	str = str.replace('^',r'\^')
	str = str.replace('$',r'\$')
	str = str.replace('*',r'\*')
	str = str.replace('+',r'\+')
	str = str.replace('?',r'\?')
	str = str.replace('{',r'\{')
	str = str.replace('}',r'\}')
	str = str.replace('|',r'\|')
	return str

def outputHtmlFile(file):
	# set the output file name.  let's base its name on the date and platform,
	# and give it a sequence number.
	if file:
		js_output = file
	else:
		now = datetime.today()
		for i in count(1):
			js_output = "%d-%s-%s.%d.html" % (now.year, str(now.month).zfill(2), str(now.day).zfill(2), i)
			if not isfile(js_output):
				break
	print "Writing results to %s" % js_output
	return open(js_output, "w")

verbose = False
js_output = True
configFile = "testconfig.txt"
outputFile = None
createTCFile = False

def usage(c):
	print "usage: %s [options] [file to parse]" % basename(argv[0])
	print " -v --verbose       enable additional output"
	print " -c --config        testconfig.txt file to load"
	print " -h --help          display help and exit"
	print " -o --output		   html output file"
	print " -x --nohtml		   don't output html"
	print " --tc [filename]    output a testconfig.txt file containing all failures"
	exit(c)



if __name__ == "__main__":
	try:
		opts, args = getopt(argv[1:], "vhc:o:x", ["verbose","help","config=","output","nohtml","tc="])
	except:
		usage(2)
		
	if not args:
		args = ["../spidermonkey-test.log"]
	for o, v in opts:
		if o in ("-v", "--verbose"):
			verbose = True
		elif o in ("-h", "--help"):
			usage(0)
		elif o in ("-x", "--nohtml"):
			js_output = False
		elif o in ("-c", "--config"):
			if not isfile(v):
				print "Config file does not exist.  Please specify a valid config file."
				exit(2)
			else:
				configFile = v
		elif o in ("-o","--output"):
			outputFile = v
		elif o in ("--tc"):
			createTCFile = True
			TCFile = open(v,'w')
			
	inputFile = args[0]
	if not isfile(inputFile):
		print "Parse file does not exist. Please specify a valid es4 ri result file to parse."
		exit(2)
	
	if js_output:
		js_output_f = outputHtmlFile(outputFile)
	settings = loadTestConfig(configFile)
	parseTestResults(inputFile)
