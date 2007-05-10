/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is [Open Source Virtual Machine.].
 *
 * The Initial Developer of the Original Code is
 * Adobe System Incorporated.
 * Portions created by the Initial Developer are Copyright (C) 2004-2006
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Adobe AS3 Team
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

namespace lexer

{
    use default namespace lexer
    use namespace token

    class Scanner 
    {
 
        private const lexemePattern  = /<<=|>>=|>>>=|\^=|\^\^=|[|]=|[|][|]=|\/=|-=|\+=|\&\&=|\&=|\%=|\*=|===|\!==|\!=|==|>=|<=|&&|\^\^|\|\||[.][.][.]?|\+\+|--|>>>|<<|>>|::|\.<|[a-zA-Z_][a-zA-Z_0-9]*|-?[0-9]+|'[^']*'|"[^"]*"|\n|./g
        private var lexemeSequence
        private var index
        private const tokenInstances = [null]
        private var lastKind


        const utf8Pool = []  
        const qualidPool = []

        function equals(v1,v2)
        {
            if( v1 == v2 )
            {
                var result = true
            }
            else
            if( v1 is Array && v1.length === v2.length )
            {
                var result = true
                for( var n = v1.length-1; n>=0; --n )
                {
                    if( v1[n] != v2[n] )
                    {
                        result = false
                    }
                }
            }
            else
            {
                var result = false
            }
            return result
        }

        function poolIndexOf(pool,val)
        {
            Debug.enter("poolIndexOf",pool,val)

            for( var index:int = pool.length-1; index>=0; --index )
            {
                if( equals(pool[index],val) )
                {
//print("found",val)
                    break
                }
            }
            if( index < 0 )
            {
//print("adding",val)
                index = pool.length
                pool.push(val)
            }

            Debug.exit("poolIndexOf",index)
            return index
        }

        function utf8IndexOf(val)
        {
            return poolIndexOf(utf8Pool,val)
        }

        function qualidIndexOf(qual,id)
        {
            Debug.enter("qualidIndexOf",qual,id)

            var qualIndex = poolIndexOf(utf8Pool,qual)  // "" means runtime qualified
            var idIndex = poolIndexOf(utf8Pool,id)
            var index = poolIndexOf(qualidPool,[qualIndex,idIndex])

            Debug.exit("qualidIndexOf",index)
            return index
        }

        function printSuffix()
        {
              print("suffix",lexemeSequence.slice(index))
        }

            
        const slash_context = [regexpliteral_token]

        const package_names = {
                toString:void 0,
                valueOf:void 0,
                hasOwnProperty:void 0,
                constructor:void 0,
                toLocaleString:void 0,
                isPrototypeOf:void 0,
                propertyIsEnumerable:void 0,
                setPropertyIsEnumerable:void 0
        }  // zero out inherited prototype names

        function addPackageName(str)
        {
            package_names[str]=true
        }

        private function addToken(kind,text)
        {
            var result = tokenInstances.length
            switch(kind)
            {
                case stringliteral_token:
                    text = text.substring(1,text.length-1)
                    break
                default:
                    break
            }
            var index = utf8IndexOf(text)
            tokenInstances.push(new Token(kind,index))
            return result
        }
        
        function Scanner(src) 
        {
            this.lexemeSequence = src.match(lexemePattern)
            //print("lexemes="+lexemeSequence)
            this.index  = 0
            this.token(true) // prime the token stream
            //print("source="+lexemeSequence.join(""))
        }
        
        function isSlashContext(kind)
        {
            return slash_context[slash_context.length-1] == kind
        }
        
        function count()
        {
            return this.lexemeSequence.length
        }

        function next() 
        {
            do 
            {
                index++
                var kind = token(true)
            } 
            while( kind == whitespace_token || kind == eol_token )
            
            return kind
        }

        function isPackageNamePrefix(str)
        {
            if( isSlashContext(regexpliteral_token) )
            for( var name in package_names )
            {
                if( name.indexOf(str) === 0 && package_names[name] != void 0 ) // the second condition is to avoid false postives for zeroed names
                {
                    return true
                }
            }
            return false
        }

        function isIdentifier(str)
        {
            return /^[a-zA-Z_][a-zA-Z_0-9]*$/.test(str)
        }

        function isPackageName(str)
        {
            return isSlashContext(regexpliteral_token) && package_names[str] != void 0
        }

        function token(next=false) 
        {
            // if we already have a current token then just return it
            
            if( !next )
            {
                return lastKind
            }
            
            var kind = 0

            while(  lexemeSequence[index] == ' ' || lexemeSequence[index] == '\n' || lexemeSequence[index] == '\t' ||
                    (lexemeSequence[index] == '/' && lexemeSequence[index+1] == '*') ||
                    (lexemeSequence[index] == '/' && lexemeSequence[index+1] == '/') )
            {

                // strip whitespace
                while( lexemeSequence[index] == ' ' || lexemeSequence[index] == '\n' || lexemeSequence[index] == '\t' ) index++
                
                // strip block comment
                if( lexemeSequence[index] == '/' && lexemeSequence[index+1] == '*' )
                {
    
                    while( !(lexemeSequence[index] == '*' && lexemeSequence[index+1] == '/') ) 
                    {
                        index++
                        if( index+1 == lexemeSequence.length )
                        {
                            throw "unterminated block comment"
                        }
                    }
                    index += 2 // move past '*/'
                }
        
                // strip line comment
                if( lexemeSequence[index] == '/' && lexemeSequence[index+1] == '/' )
                {
        
                    while( !(lexemeSequence[index] == '\n' || lexemeSequence[index] == void 0) ) 
                    {
                        index++
                    }
                }
            }

            var lexeme = lexemeSequence[index]

            // first, merge lexemes that need to be merged

            if( isSlashContext(regexpliteral_token) )
            {
                switch(lexeme)
                {
                    case '/':
                        var start_index = index

                        while( lexemeSequence[index+1] != '/' )
                        {
                            index++
                            lexemeSequence[start_index]+=lexemeSequence[index]
                            lexemeSequence[index]=' '
                        }
                        index = index + 1  // ** replace with index++ to get verifier error
                        lexemeSequence[start_index]+=lexemeSequence[index]
                        lexemeSequence[index]=' '
                        
                        if( lexemeSequence[index+1] != undefined && /^[a-zA-Z][a-zA-Z]*$/.test(lexemeSequence[index+1]) ) // add flags
                        {
                            index = index + 1  // ** replace with index++ to get verifier error
                            lexemeSequence[start_index]+=lexemeSequence[index]
                            lexemeSequence[index]=' '
                        }
                        lexeme = lexemeSequence[start_index]
                        break
                    case '.':
                        var start_index = index

                        // see if we have an integer
                        if( lexemeSequence[index+1] != null && /-?[0-9]+/.test(lexemeSequence[index+1]) )
                        {
                            index++
                            lexemeSequence[start_index]+=lexemeSequence[index]
                            lexemeSequence[index]=' '
                            lexeme = lexemeSequence[start_index]

                            // todo: exponents
                        }
                        // check for package identifier or number literal
                        break
                    case '<':  // xml intialiser
                        break
                    default:
                        var start_index = index

                        // see if we have an integer possibly followed by a dot possibly followed by another integer
                        if( lexeme != null && /-?[0-9]+/.test(lexeme) )
                        {
                            if( lexemeSequence[index+1] == '.' )
                            {
                                index++
                                lexemeSequence[start_index]+=lexemeSequence[index]
                                lexemeSequence[index]=' '
                                if( lexemeSequence[index+1] != null && /-?[0-9]+/.test(lexemeSequence[index+1]) )
                                {
                                    index++
                                    lexemeSequence[start_index]+=lexemeSequence[index]
                                    lexemeSequence[index]=' '
                                }
                            }
                            lexeme = lexemeSequence[start_index]                                

                            // todo: exponents
                        }
                        else
                        if( isIdentifier(lexeme) ) // see if it is an identifier that is a prefix of a package name
                        {
                            // while lexeme is a prefix of a package name
                            //    and the next two lexemes are dot and identifier
                            //    concat these three lexems to make a temp lexeme
                            //    if temp lexeme is a complete package name, then merge tokens and continue
                            // 
                            // package p.q {}
                            // package p.q.r {}
                            // p.q.r.x

                            var start_index = index
                            var last_index = start_index
                            while( isPackageNamePrefix(lexeme) &&
                                lexemeSequence[last_index+1]    == "." &&
                                isIdentifier(lexemeSequence[last_index+2]) )
                            {
                                lexeme = lexeme+"."+lexemeSequence[last_index+2]
                                if( isPackageName(lexeme) )  // if its a match, merge
                                {
                                    lexemeSequence[last_index+2] = lexeme
                                    index = last_index+2  // last match
                                }
                                last_index += 2
                            }
                            for( var i = start_index; i<index; ++i )
                            {
                                lexemeSequence[i]=' '
                            }
                            
                            lexeme = lexemeSequence[index]
                        }
                        break
                }
            }
            
            // next, select the token kind
            
            switch(lexeme)
            {
            
            case void 0:     kind = eos_token; break;
            
            // punctuators

            case '.': kind = dot_token; break
            case '..': kind = doubledot_token; break
            case '...': kind = tripledot_token; break
            case '.<': kind = leftdotangle_token; break
            case '!': kind = not_token; break
            case '!=': kind = notequals_token; break
            case '!==': kind = strictnotequals_token; break
            case '%': kind = modulus_token; break
            case '%=': kind = modulusassign_token; break
            case '&': kind = bitwiseand_token; break
            case '&=': kind = bitwiseandassign_token; break
            case '&&': kind = logicaland_token; break
            case '&&=': kind = logicalandassign_token; break
            case '*': kind = mult_token; break
            case '*=': kind = multassign_token; break
            case '+': kind = plus_token; break
            case '+=': kind = plusassign_token; break
            case '++': kind = plusplus_token; break
            case '-': kind = minus_token; break
            case '--': kind = minusminus_token; break
            case '-=': kind = minusassign_token; break
            case '/': kind = div_token; break
            case '/=': kind = divassign_token; break
            case ',': kind = comma_token; break
            case ':': kind = colon_token; break
            case '::': kind = doublecolon_token; break
            case ';': kind = semicolon_token; break
            case '<': kind = lessthan_token; break
            case '<=': kind = lessthanorequals_token; break
            case '<<': kind = leftshift_token; break
            case '<<=': kind = leftshiftassign_token; break
            case '=': kind = assign_token; break
            case '==': kind = equals_token; break
            case '===': kind = strictequals_token; break
            case '>': kind = greaterthan_token; break
            case '>=': kind = greaterthanorequals_token; break
            case '>>': kind = rightshift_token; break
            case '>>=': kind = rightshiftassign_token; break
            case '>>>': kind = unsignedrightshift_token; break
            case '>>>=': kind = unsignedrightshiftassign_token; break
            case '^': kind = bitwisexor_token; break
            case '^=': kind = bitwisexorassign_token; break
            case '^^': kind = logicalxor_token; break
            case '^^=': kind = logicalxorassign_token; break
            case '|': kind = bitwiseor_token; break
            case '|=': kind = bitwiseorassign_token; break
            case '||': kind = logicalor_token; break
            case '||=': kind = logicalorassign_token; break
            case '?': kind = questionmark_token; break
            case '(': kind = leftparen_token; break
            case ')': kind = rightparen_token; break
            case '[': kind = leftbracket_token; break
            case ']': kind = rightbracket_token; break
            case '{': kind = leftbrace_token; break
            case '}': kind = rightbrace_token; break
            case '~': kind = bitwisenot_token; break
            case '@': kind = at_token; break
            case '</': kind = xmltagendend_token; break
            case '/>': kind = xmltagstartend_token; break

            // completely reserved words

            case 'as':   kind = as_token; break
            case 'break':   kind = break_token; break
            case 'call':   kind = call_token; break
            case 'case':   kind = case_token; break
            case 'cast':   kind = cast_token; break
            case 'catch':   kind = catch_token; break
            case 'class':   kind = class_token; break
            case 'const':   kind = const_token; break
            case 'continue':   kind = continue_token; break
            case 'default':   kind = default_token; break
            case 'delete':   kind = delete_token; break
            case 'do':   kind = do_token; break
            case 'else':   kind = else_token; break
            case 'enum': kind = enum_token; break
            case 'extends':   kind = extends_token; break
            case 'false':   kind = false_token; break
            case 'finally':   kind = finally_token; break
            case 'for':   kind = for_token; break
            case 'function':   kind = function_token; break
            case 'if':   kind = if_token; break
            case 'implements':   kind = implements_token; break
            case 'import':   kind = import_token; break
            case 'in':   kind = in_token; break
            case 'instanceof':   kind = instanceof_token; break
            case 'interface':   kind = interface_token; break
            case 'internal':   kind = internal_token; break
            case 'intrinsic':   kind = intrinsic_token; break
            case 'is':   kind = is_token; break
            case 'let':   kind = let_token; break
            case 'native':   kind = native_token; break
            case 'new':   kind = new_token; break
            case 'null':   kind = null_token; break
            case 'package': kind = package_token; break
            case 'private': kind = private_token; break
            case 'protected': kind = protected_token; break
            case 'public': kind = public_token; break
            case 'return': kind = return_token; break
            case 'super': kind = super_token; break
            case 'switch': kind = switch_token; break
            case 'this': kind = this_token; break
            case 'throw': kind = throw_token; break
            case 'to': kind = to_token; break
            case 'true': kind = true_token; break
            case 'try': kind = try_token; break
            case 'typeof': kind = typeof_token; break
            case 'use': kind = use_token; break
            case 'var': kind = var_token; break
            case 'while': kind = while_token; break
            case 'with': kind = with_token; break
            case 'yield': kind = yield_token; break;

            // contextually reserved words

            case 'debugger': kind = debugger_token; break
            case 'dynamic': kind = dynamic_token; break
            case 'each': kind = each_token; break
            case 'final': kind = final_token; break
            case 'get': kind = get_token; break
            case 'goto': kind = goto_token; break
            case 'include': kind = include_token; break
            case 'namespace': kind = namespace_token; break
            case 'native': kind = native_token; break
            case 'override': kind = override_token; break
            case 'prototype': kind = prototype_token; break
            case 'set': kind = set_token; break
            case 'static': kind = static_token; break
            case 'type': kind = type_token; break
            case 'xml': kind = xml_token; break

            // identifiers & literals

            default:
                var c0 = lexeme.charAt(0)
                if(c0=="'" || c0=='"') 
                { 
                    kind = addToken(stringliteral_token,lexeme)
                }
                else
                if( isPackageName(lexeme) )
                {
                    kind = addToken(packageidentifier_token,lexeme)
                }
                else
                if( /^[a-zA-Z_][a-zA-Z_0-9]*$/.test(lexeme) )
                {
                    kind = addToken(identifier_token,lexeme)  // interns and returns index
                }
                else
                if( /^\x2f[^\x2f.]*\x2f/.test(lexeme) )
                {
                    kind = addToken(regexpliteral_token,lexeme)  // interns and returns index
                }
                else
                if( /-?[0-9]*\.?[0-9]+/.test(lexeme) )
                {
                    kind = addToken(numberliteral_token,lexeme)  // interns and returns index
                }
                else
                {
                    throw "unrecognized lexeme: "+lexeme
                }
            }
            lastKind = kind
            //print("found "+tokenText(kind))
            return kind
        }

        function tokenKind(n)
        {
            if( +n != n )
            {
                throw "bogus token kind"
            }
            if( +n <= 0 )
            {
                return n
            }
            else
            {
                return tokenInstances[n].kind
            }
        }

        function tokenText(n)
        {
            if( n <= 0 )
            {
                return tokenNames[-n]
            }
            else
            {
                var utf8id = tokenInstances[n].utf8id
                  return utf8Pool[utf8id]
            }
        }

        function followsNewline()
        {
            var follows = false
            for( var i = index-1; i >= 0; --i )
            {
                //print("follows",lexemeSequence[i].charCodeAt(0))
                if( lexemeSequence[i] == "\n" )
                {
                    follows = true
                    break
                }
                else
                if( lexemeSequence[i] != " " &&
                    lexemeSequence[i] != "\t" )
                {
                    break
                }                
            }
            return follows
        }
    }
}

