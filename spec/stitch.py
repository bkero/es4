# lhansen@adobe.com / 2007-07-26
#
# Usage:
#  stitch <inputfile> <outputfile>
#
# This program transforms input to output by peforming the following actions.
#
#  - check that there are no illegal characters in the input: ASCII only, no tabs
#  - remove HTML comments
#  - replace wiki formatting with HTML formatting
#  - renumber headers to account for inclusion levels (all files start with H1)
#  - stitch together files by processing INCLUDEs that reference HTML and CSS files
#  - include source code by processing INCLUDEs that reference ES4 or SML fragments
#  - resolve cross references
#
# Wiki formatting summary:
#
#  - = ... = for H1, == ... == for H2, etc
#  - ''...'' for <code>...</code>
#  - [[...]] for <code>[[...]]</code>


# Rules for processing in general:
#  - Read the entire file
#  - Pass 1:
#    - scan for illegal characters
#  - Pass 2:
#    - remove /<!-- ... -->/
#    - replace /(\=)+\s+([...]+)\s+\1/ with the appropriate header tag 
#    - replace /<h([1-5]) (...)>(...)</h\1>/ with appropriate tags and the
#      content from the tag and text
#    - replace <XREF ...> with nothing, for now
#  - Pass 3:
#    - process INCLUDE files:
#      - for HTML files, process them separately using the same process, 
#        incrementing the header level counter on entry and CDing to the
#        correct directory.  Then insert the finished text.
#      - for source files, extract the contents as requested and insert
#        it, undented, in a <pre> tag
#  - Finally, if we're at the top level file:
#    - Spit out the file
#  - Otherwise, our caller consumes the text, so just return it
#
# Directories for processing INCLUDE:
#  - the startup directory is resolve(".")
#  - the directory for .sml files is resolve("..")
#  - the directory for .es files is resolve("../builtins")
#  - the directory for .html and .css files is always the directory of 
#    the including file


# FIXME: get the header computation logic right and document it.
# FIXME: include SML source code
# FIXME: undent source code

import re, sys, os, os.path

htmlcomment = re.compile(r"<!--(?:.|\s)*?-->")
wikiheader = re.compile(r"^(\=+)\s+(.*?)\s+\1", re.M)
htmlheader = re.compile(r"<h([1-6])((?:\".*?\"|[^\"])*?)>(.*?)</h\1>")
xreftag = re.compile(r"<XREF(?:\".*?\"|[^\"])*?>")
includetag = re.compile(r"<h[1-6]|<INCLUDE(?:\".*?\"|[^\"])*?>")
htmlInclude = re.compile(r"<INCLUDE\s*file=\"(.*?\.(?:html|css))\">")
smlInclude = re.compile(r"<INCLUDE\s+file=\"(.*?\.sml)\"\s+name=\"(.*?)\">")
esInclude = re.compile(r"<INCLUDE\s+file=\"(.*?\.es)\"\s+name=\"(.*?)\">")
hdrprefix = re.compile(r"<h([1-6])")
wikiformatCode = re.compile(r"''(.*?)''");
wikiformatSpecial = re.compile(r"(\[\[(.*?)\]\])");

currentlevel = 0

es_dir = os.path.abspath("../builtins")
sml_dir = os.path.abspath("..")

# fn is the filename from the INCLUDE tag: it has no directory component.
# Returns a sequence of strings.
#
# Works by looking for the string 'name' at the beginning of a line
# preceded only by n whitespace chars, grabbing that line and all
# subsequent lines up to a line with <= n whitespace chars preceding
# non-whitespace chars or up to and including the line with a } in
# position n.  Trailing empty lines, if any, are removed.
#
# If it's too slow to process a file every time we want to extract a
# function then this interface can be backed by a simple program that
# processes each file once, extracting all functions and storing them
# in some easier format (maybe).

def extractES(fn, name):
    f = open(os.path.normpath(es_dir + "/" + fn), 'r')
    outside = True
    # FIXME: escape punctuation in name before using it
    starting = re.compile("^( *)" + name)
    for line in f:
	if outside:
	    m = starting.search(line)
	    if m:
		ending = re.compile("^" + m.group(1) + r"[^\s]")
		openbrace = re.compile(m.group(1) + r"\{")
		closebrace = re.compile(m.group(1) + r"\}")
		res = [line.rstrip()]
		outside = False
		continue
	else:
	    line = line.rstrip()
	    blanks = 0
	    if ending.search(line):
		# Special case for common pattern: open brace indented like the name
		if openbrace.search(line):
		    res = res + [line]
		    continue
		if closebrace.search(line):
		    res = res + [line]
		    break
		break
	    if line == "":
		blanks = blanks+1
	    else:
		for i in range(blanks):
		    res = res + ""
		blanks = 0
		res = res + [line]
    f.close()
    if outside:
	print fn + ": Could not find definition for " + name
	sys.exit(1)
    ss = "\n"
    for s in res:
	ss = ss + s + "\n"
    return ss

def replaceWiki(m):
    return "<h" + str(len(m.group(1))) + ">" + m.group(2) + "</h" + str(len(m.group(1))) + ">"

def replaceHTML(m, hdrlvl):
    k = str(int(m.group(1)) + hdrlvl - 1)
    return "<h" + k + m.group(2) + ">" + m.group(3) + "</h" + k + ">"

def replaceInclude(m, hdrlvl, fn):
    global currentlevel
    ms = hdrprefix.match(m.group(0))
    if ms:
	currentlevel = int(ms.group(1))
	return m.group(0)
    ms = htmlInclude.match(m.group(0))
    if ms:
	olddir = os.getcwd()
	oldlevel = currentlevel
	if os.path.dirname(ms.group(1)) != "":
	    os.chdir(os.path.dirname(ms.group(1)))
	r = process(os.path.basename(ms.group(1)), currentlevel+1)
	currentlevel = oldlevel
	os.chdir(olddir)
	return r
    ms = smlInclude.match(m.group(0))
    if ms:
	return ""
    ms = esInclude.match(m.group(0))
    if ms:
	return "<pre><code>" + extractES(ms.group(1), ms.group(2)) + "</code></pre>"
    print fn + ": Invalid INCLUDE directive: " + m.group(0)
    sys.exit(1)

def process(fn, hdrlvl):
    input = open(fn, 'r')
    text = input.read()
    text = re.sub(htmlcomment, "", text)
    for i in range(len(text)):
	cc = ord(text[i])
	if cc > 127 or cc < 32 and cc != 10 and cc != 13:
	    print fn + ": Non-ASCII character in at location " + str(i) + ": " + str(cc)
	    sys.exit(1)
    text = re.sub(wikiheader, replaceWiki, text)
    text = re.sub(htmlheader, lambda m: replaceHTML(m, hdrlvl), text)
    text = re.sub(xreftag, "", text)
    text = re.sub(wikiformatCode, r"<code>\1</code>", text)
    text = re.sub(wikiformatSpecial, r"<code>\1</code>", text)
    text = re.sub(includetag, lambda m: replaceInclude(m, hdrlvl, fn), text)
    return text

if len(sys.argv) != 3:
    print "Usage: stitch inputfile outputfile"
    sys.exit(2)

text = process(sys.argv[1], 1)
output = open(sys.argv[2], 'w')
output.write(text)
output.close()

