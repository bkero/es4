#!/usr/bin/python

#Code Generator

import types
import random
import sys
from pprint import pprint
import getopt
import os

# Config
debug = False
sys.setrecursionlimit(11000)
maxRecursionDepth = 100

# globals
nodes = {}
output = []
lastNodeAdded = None
lastNodeListIndex = 0
recursionDepth = 0
SCOPES = ['global','class','interface','local']
version = 3

def main(argv=None):
    global debug
    global maxRecursionDepth
    global version
    startNode = 'Program'
    scope = []
    outFile = None
    reps = 1
    fbn = False
    seed = None
    version = 4
    #DEBUG
    seed = random.randint(0,90000000)
    
    
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hf:dr:v:',['help','file=','debug','findBrokenNodes','seed=','reps='])
    except getopt.GetoptError:
        usage(2)
    for o,a in opts:
        if o in ('-h','--help'):
            usage()
        if o in ('-f','--file'):
            outFile = a
        if o in ('-d','--debug'):
            debug = True
        if o == '-r':
            try:
                maxRecursionDepth = int(a)
            except ValueError:
                pass
        if o == '--findBrokenNodes':
            fbn = True
        if o == '--seed':
            try:
                print a
                seed = int(a)
            except:
                print 'cant parse seed'
        if o == '--reps':
            if outFile:
                reps = int(a)
        if o == '-v':
            version = int(a)
    try:
        startNode = args[0]
    except IndexError:
        pass
        
    try:
        scope = args[1].split(',')
    except:
        scope = []
    
    if fbn:
       findBrokenNodes()
       sys.exit()
    
    if seed:
        random.seed(seed)
    
    if debug:
        print 'Random Seed: %s' % seed
    
    if outFile:
        (name, ext) = os.path.splitext(outFile)
        try:
            for i in range(reps):
              f = open('%s-%s%s' % (name,i,ext),'w')
              f.writelines(getNodeStr(startNode,scope))
              f.close()
        except IOError:
            print 'IOError - please check filename'
            sys.exit(2)
    else:
        
        print (getNodeStr(startNode,scope))
    

def usage(exitCode=None):
    print 'ES4 Grammar Based Language Generator'
    print 'usage: codegen.py [options] [startNode] [scope]'
    print '\tstartNode defaults to Program'
    print '\tmultiple scopes can be comma (no space) delimited'
    print 'options:'
    print '\t-h, --help\t\t show this help and exit'
    print '\t-f FILE, --file=FILE\t write output to FILE'
    print '\t--reps REPS\t number of files to output (file must be specified)'
    print '\t-d, --debug\t\t output debug info'
    print '\t-r LEVEL\t\t set maximum recursion depth (default = %s)' % maxRecursionDepth
    print '\t\t\t\t (this controls the output length)'
    print '\t--findBrokenNodes\t continuously loop and search for broken nodes'
    print
    print 'e.g.: ./codegen.py IfStatement abbrev'
    sys.exit(exitCode)

#util functions
def getNodeStr(node,scope=[]):
    out = ''
    while out.strip()=='' or out.strip()==';':
        out = getRandomNodeChild(node,scope)
    return out
    
def replaceChars(str,charsToReplace,replacementChar=''):
    for c in charsToReplace:
        str = str.replace(c,replacementChar)
    return str

def findBrokenNodes():
    global lastNodeListIndex
    global recursionDepth
    loop = True
    x = 1
    while loop:
        lastNodeListIndex = 0
        recursionDepth = 0
        random.seed(x)
        x+=1
        try:
            print getNodeStr('Program')
        except Exception, e:
            print 'Node '+e[1]+' not found.'
            loop = False
            

# scope definitions - from grammar.py
# a = ['allowcolon','nocolon']
# b = ['allowin','noin']
# g = ['allowexpr','noexpr']
# t = ['global','class','interface','local']
# w = ['abbrev','noshortif','full']

#{ Node : [scope : [''], dir(ective):[''], children : [ { version:int,subnodes : [{name:'',scope:[],dir:[]}], syntax : ''|lambda, literal : boolean} ] } ] } 
def getRandomNodeChild(node, scopeAndDirective = []):
    child = None
    if debug:
        print('called for: %s %s' % (node,scopeAndDirective))
    if not nodes.has_key(node):
        raise Exception('NodeNotFound',node)
    scope, directive = expandScope(scopeAndDirective)
    
    #scope should never be more than one - convert to str from list
    if len(scope) > 1:
        raise Exception('Too many scope def in input: %s' % scope)
    elif len(scope) == 0:
        scope = None
    else:
        scope = scope[0]
    
    
    #find the node(s) with the correct scope
    childNodeSelection = []
    if scope:
        #only get children w/in scope
        for n in nodes[node]:
            if scope in n['scope']:
                if directive:
                    for d in directive:
                        if d in n['dir']:
                            if n['children'] not in childNodeSelection:
                                childNodeSelection.append(n['children']) 
                else:
                    childNodeSelection.append(n['children'])
    else:
        for n in nodes[node]:
            if directive:
                for d in directive:
                    if d in n['dir']:
                        if n['children'] not in childNodeSelection:
                            childNodeSelection.append(n['children']) 
            else:
                if n['children'] not in childNodeSelection:
                    childNodeSelection.append(n['children'])
    
    if debug:
        print('childNodeSelection: %s' % childNodeSelection)
    
    #filter for version
    '''
    for i,cl in enumerate(childNodeSelection):
      for c in cl:
        print (c)
        if c['version'] > version:
          childNodeSelection[i].remove(c)
    '''
    
    if debug:
        print('childNodeSelection: %s' % len(childNodeSelection))
    
    if len(childNodeSelection) == 0:
        #TODO: This is a hack to keep it working until grammar issues are addressed
        # can't find any child - just grab one w/o regard to scope
        print 'WARNING: child is None for node: %s scope: %s' % (node,scope)
        child = getRandomChild(n['children'])
    else:
        child = getRandomChild(random.choice(childNodeSelection))

    
    if debug:
        pprint(child)
    if child['literal']:
        if isinstance(child['syntax'],types.StringType):
            return child['syntax']
        else:
            return child['syntax']()
    else:
        #Get subnode values
        values = []
        for sn in child['subnodes']:
            #check subnode for scopes and directives
            snScopeAndDir = []  #make a copy of the sn scope
            if scope:
                if scope in sn['scope']:
                    snScopeAndDir.append(scope)
            else:
                if len(sn['scope']) > 1:
                    raise Exception('Too Many scopes!  May not have properly been defined in command line args!\nsn[scope]: %s' % sn['scope'])
                if sn['scope']:
                    snScopeAndDir.append(sn['scope'][0])
            for d in sn['dir']:
                snScopeAndDir.append(d)
            rnc = getRandomNodeChild(sn['name'],snScopeAndDir)
            values.append(rnc)
        return child['syntax'] % tuple(values)


# children is a list of possible children
def getRandomChild(children):
    global recursionDepth
    recursionDepth += 1
    #print recursionDepth
    if recursionDepth >= maxRecursionDepth:
        #Try to only select literals
        literalChildren = []
        for c in children:
            if c['literal']:
                literalChildren.append(c)
        if len(literalChildren):
            return random.choice(literalChildren)
        else:
            #no literals, try to get smaller node
            #TODO: clunky code - clean it up
            nodeCount = []
            for c in children:
                nodeCount.append(len(c['subnodes']))
            leastNode = min(nodeCount)
            #get the positions of the leastNodes
            pos = [i for i,c in enumerate(nodeCount) if c == leastNode]
            return children[random.choice(pos)]
    else:
        return random.choice(children)


#### Grammar Construtor Functions ####
# newNode
def nn(name,scopeAndDirective=[]):
    global lastNodeAdded
    global lastNodeListIndex
    scope,directive = expandScope(scopeAndDirective)
    if nodes.has_key(name):
        nodes[name].append({'scope':scope,'dir':directive,'children':[]})
        lastNodeListIndex = len(nodes[name])-1
    else:
        nodes[name] = [{'scope':scope,'dir':directive,'children':[]}]
        lastNodeListIndex = 0
    lastNodeAdded = name
    

# addChild
def ac(ver, subnodes, syntax='%s', literal=False,node=None,nolb=[]):
    if isinstance(subnodes, types.StringType):
        subnodes = [sn(subnodes)]
    elif isinstance(subnodes, types.TupleType):
        subnodes = [sn(subnodes[0],subnodes[1])]
    else:
        #more than one term per subnode
        subnodelist = []
        if syntax=='%s': # auto-create the syntax string if none specified
            syntax = ('%s '*len(subnodes))[:-1]
        for snode in subnodes:
            if isinstance(snode,types.StringType):
                subnodelist.append(sn(snode))
            else:
                subnodelist.append(sn(snode[0],snode[1]))
        subnodes = subnodelist
    if not node:
        node = lastNodeAdded
    # add child to node
    
    #version hack
    if ver <= version:
      nodes[node][lastNodeListIndex]['children'].append({'version':version,'subnodes':subnodes,'syntax':syntax,'literal':literal,'nolb':nolb})

# subNode
def sn(name, sd=[]):
    if isinstance(sd, types.StringType):
        sd = [sd]
    scope, directive = expandScope(sd)
    return {'name':name,'scope':scope,'dir':directive}

def expandScope(scopeAndDirective):
    #Break apart scopeAndDirective
    scope = []
    directive = []
    for i in scopeAndDirective:
        if i in SCOPES:
            scope.append(i)
        else:
            directive.append(i)
    return scope,directive

if __name__ == '__main__':
    # Load Grammar
    from grammar import *;
    main()