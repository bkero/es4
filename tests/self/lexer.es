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

package macromedia.aac.parser {
	
	import macromedia.aac.util.*

	public class Scanner {
		
		private static var debug:Boolean = false;
		
		private static const slashdiv_context:int = 0x1;
		private static const slashregexp_context:int = 0x2;
		private static const KEEPSCANNING:int = 0;
		
		private var tokens:Array;	// vector of token instances
		private var slash_context:Array;	// slashdiv_content or slashregexp_content
		
		private var isFirstTokenOnLine:Boolean;
		
		private var ctx:Context;
		
		public var input:InputBuffer;
		
		private var startofxml:int;
		private var xmltagname:String = null; //never set!
		
		public var state:int;
    	public var level:int;

    	public var states:Array;
    	public var levels:Array;
    	public var slashcontexts:Array;
    	
    	private static const HAS_HASHPRAGMAS:Boolean = false;

		private function init(cx:Context):void {
			ctx = cx;
			tokens = new Array(200);
            tokens.push(null); // 0 entry not used.
			state = start_state;
			slash_context = new Array();
			level = 0;
			slash_context.push(slashregexp_context);
			states = new Array();
			levels = new Array();
			slashcontexts = new Array();
		}
		
		public function Scanner(cx:Context, input:String, origin:String) {
			init(cx);
			this.input = new InputBuffer(input, origin);
			cx.input = this.input;
		}
			
		public function nextchar():int {
			return input.nextchar(false);
		}
		
		public function nextchar_unnormalized():int {
			return input.nextchar(true);
		}
		
		public function lexeme():String {
			return input.copy(); // copies text since last mark
		}
    /*
     * retract() --
     * Causes one character of input to be 'put back' onto the
     * que. [Test whether this works for comments and white space.]
     */

    public function retract():void
    {
        input.retract();
    }

    /*
     * Various helper methods for managing and testing the
     * scanning context for slashes.
     */

    public function enterSlashDivContext():void
    {
        slash_context.push(slashdiv_context);
    }

    public function exitSlashDivContext():void
    {
        slash_context.pop();
    }

    public function enterSlashRegExpContext():void
    {
        slash_context.push(slashregexp_context);
    }

    public function exitSlashRegExpContext():void
    {
        slash_context.pop();
    }

    public function isSlashDivContext():Boolean
    {
        return slash_context[slash_context.length-1] == slashdiv_context;
    }

    public function isSlashRegexpContext():Boolean
    {
        return slash_context[slash_context.length-1] == slashregexp_context;
    }

    /*
     * makeTokenInstance() --
     * Make an instance of the specified token class using the lexeme string.
     * Return the index of the token which is its identifier.
     */

    public function makeTokenInstance(token_class:int, lexeme:String):int
    {
        tokens.push(new Token(token_class, lexeme));
        return tokens.length - 1; /* return the tokenid */
    }

   	/*
     * getTokenClass() --
     * Get the class of a token instance.
     */

    public function getTokenClass(token_id:int):int
    {

        // if the token id is negative, it is a token_class.
        if (token_id < 0)
        {
            return token_id;
        }

        // otherwise, get instance data from the instance vector.
        var tk:Token = tokens[token_id];
        return tk.getTokenClass();
    }

    /*
     * getTokenText() --
     * Get the text of a token instance.
     *
     */

    public function getTokenText(token_id:int):String
    {

        // if the token id is negative, it is a token_class.
        if (token_id < 0)
        {
            return Token.getTokenClassName(token_id);
        }

        // otherwise, get instance data from the instance vector.
		var tk:Token = tokens[token_id];
        return tk.getTokenText();
    }

    /*
     * getStringTokenText
     * Get text of literal string token as well as info about whether it was single quoted or not
     * Use first element of array to return second value.
     */
    public function getStringTokenText( token_id:int, is_single_quoted:Array ):String
    {
        // if the token id is negative, it is a token_class.
        if( token_id < 0 )
        {
            is_single_quoted[0] = new Boolean(false);
            return Token.getTokenClassName(token_id);
        }

        // otherwise, get tokenSourceText (which includes string delimiters)
        var tk:Token = tokens[ token_id ];
        var fulltext:String = tk.getTokenSource();
        is_single_quoted[0] = new Boolean((fulltext.charAt(0) == "\'" ? true : false));
        var enclosedText:String = fulltext.substring(1, fulltext.length - 1);
        
        return enclosedText;
    }

    private function getXMLText(begin:int, end:int):String
    {
        var len:int = end-begin; 

        var xmltext:String = null;
        if( len > 0 )
        {
            xmltext = input.source().substring(begin,end);
        }
        return xmltext;
    }

    /*
     * getLinePointer() --
     * Generate a string that contains a carat character at
     * a specified position.
     */

    public function getLinePointer():String
    {
        return InputBuffer.getLinePointer(0);
    }

    /*
     * Record an error.
     */
	public function error(code:int):void {
		ctx.error(input.positionOfNext(), code);
	}

	 public function skiperror(...args):void {
         var nc:int;
         var kind:int;
         if (args.length >= 1)
         	kind = args[0];
         else
			kind = ErrorConstants.kError_Lexical_General;
         switch (kind) {
         case ErrorConstants.kError_Lexical_General:
             return;
             // lots of other cases
         default:
             while (true) {
                 nc = nextchar();
                 if ((nc == CharCode.CHAR_semicolon) || (nc == CharCode.CHAR_newline) || (nc == 0))
                     return;
             }
         }
     }
    /*
     *
     *
     */

    public function followsLineTerminator():Boolean
    {
        dt("isFirstTokenOnLine = " + isFirstTokenOnLine);
        return isFirstTokenOnLine;
    }

    public function pushState():void
    {
        states.push(state);
        levels.push(level);
        var temp:Array = new Array(slash_context);
        slashcontexts.push(temp);
        state = start_state;
        level = 0;
        slash_context.length = 0;
        enterSlashRegExpContext();
    }

    public function popState():void
    {
        exitSlashRegExpContext();  // only necessary to do the following assert
        if (slash_context.size() != 0)
        {
            Debug.assert(false); // throw "internal error";
        }
        state = states.pop();
        level = levels.pop();
        slash_context = slashcontexts.pop();
    }

        private var doctextbuf:String;
        private var blockcommentbuf:String;
        private var stringliteralbuf:String;
        private var regexp_flags:int;

	public function nexttoken(resetState:Boolean = false):int {
        if (resetState)
        {
            isFirstTokenOnLine = false;
        }
        var action:Function;
        doctextbuf = null;
        blockcommentbuf = null;
        stringliteralbuf = null;
        startofxml = input.positionOfNext()+1;
        regexp_flags = 0;
        var result:int = KEEPSCANNING;
        while (result == KEEPSCANNING) {
            // var sname:String = stateName[state];
            dt("state = " + stateName[state] + ", next = " + input.positionOfNext());
            action = state_proc[state]; 
            result = action.call(this);
            // sname = stateName[state];
        }
        state = start_state;
        return result;
    }

    public function dt(s:String):void {
    	if (debug) {
    		trace(s);
    	}
	}

    /* Actions for scanner states.  
       The java code has many nested switch statements.  The current asc turns
       them into a sequence of if then else statements.  With over 400 states,
       it would take bloody forever to even get back to the actions for a given
       state.  I'm using an array of function objects (which in AS3 are actually
       closures whose "this" variable will point to the Scanner object.  For 
       transitions that don't change the state, I can loop within the state_proc.
    */

	private function start_state_proc():int {
		var c:int;
        while (true) {
            c = nextchar();
            input.mark();
            switch (c) {
            case 0xffffffef:
                state = utf8sig_state;
                return KEEPSCANNING;
            case  CharCode.CHAR_atsign:
                return Tokens.AT_TOKEN; 
            case CharCode.CHAR_singlequote:
                stringliteralbuf = String.fromCharCode(c);
                state = singlequote_state;
                return KEEPSCANNING;
            case CharCode.CHAR_doublequote:
                stringliteralbuf = String.fromCharCode(c);
                state = doublequote_state;
                return KEEPSCANNING;
            case CharCode.CHAR_minus:
                state = minus_state;
                return KEEPSCANNING;
            case CharCode.CHAR_bang:
                state = not_state;
                return KEEPSCANNING;
            case CharCode.CHAR_percent:
                state = remainder_state;
                return KEEPSCANNING;
            case CharCode.CHAR_ampersand:
                state = and_state;
                return KEEPSCANNING;
            case CharCode.CHAR_hash:
                if (HAS_HASHPRAGMAS) {
                    return Tokens.USE_TOKEN;
                } else {
                    state = error_state;
                    return KEEPSCANNING;
                }
            case CharCode.CHAR_leftparen:
                return  Tokens.LEFTPAREN_TOKEN;
            case CharCode.CHAR_rightparen:
                return  Tokens.RIGHTPAREN_TOKEN;
            case CharCode.CHAR_asterisk:
                state = star_state;
                return KEEPSCANNING;
            case CharCode.CHAR_comma:
                return  Tokens.COMMA_TOKEN;
            case CharCode.CHAR_period:
                state = dot_state;
                return KEEPSCANNING;
            case CharCode.CHAR_slash:
                state = slash_state;
                return KEEPSCANNING;
            case CharCode.CHAR_colon:
                state = colon_state;
                return KEEPSCANNING;
            case CharCode.CHAR_semicolon:
                return  Tokens.SEMICOLON_TOKEN;
            case CharCode.CHAR_questionmark:
                return  Tokens.QUESTIONMARK_TOKEN;
            case CharCode.CHAR_leftbracket:
                return  Tokens.LEFTBRACKET_TOKEN;
            case CharCode.CHAR_rightbracket:
                return  Tokens.RIGHTBRACKET_TOKEN;
            case CharCode.CHAR_caret:
                state = bitwisexor_state;
                return KEEPSCANNING;
            case CharCode.CHAR_leftbrace:
                return  Tokens.LEFTBRACE_TOKEN;
            case CharCode.CHAR_verticalbar:
                state = or_state;
                return KEEPSCANNING;
            case CharCode.CHAR_rightbrace:
                return  Tokens.RIGHTBRACE_TOKEN;
            case CharCode.CHAR_tilde:
                return  Tokens.BITWISENOT_TOKEN;
            case CharCode.CHAR_plus:
                state = plus_state;
                return KEEPSCANNING;
            case CharCode.CHAR_lessthan:
                state = lessthan_state;
                return KEEPSCANNING;
            case CharCode.CHAR_equal:
                state = equal_state;
                return KEEPSCANNING;
            case CharCode.CHAR_greaterthan:
                state = greaterthan_state;
                return KEEPSCANNING;
            case CharCode.CHAR_a:
                state = a_state;
                return KEEPSCANNING;
            case CharCode.CHAR_b:
                state = b_state;
                return KEEPSCANNING;
            case CharCode.CHAR_c:
                state = c_state;
                return KEEPSCANNING;
            case CharCode.CHAR_d:
                state = d_state;
                return KEEPSCANNING;
            case CharCode.CHAR_e:
                state = e_state;
                return KEEPSCANNING;
            case CharCode.CHAR_f:
                state = f_state;
                return KEEPSCANNING;
            case CharCode.CHAR_g:
                state = g_state;
                return KEEPSCANNING;
            case CharCode.CHAR_i:
                state = i_state;
                return KEEPSCANNING;
            case CharCode.CHAR_n:
                state = n_state;
                return KEEPSCANNING;
            case CharCode.CHAR_o:
                state = o_state;
                return KEEPSCANNING;
            case CharCode.CHAR_p:
                state = p_state;
                return KEEPSCANNING;
            case CharCode.CHAR_r:
                state = r_state;
                return KEEPSCANNING;
            case CharCode.CHAR_s:
                state = s_state;
                return KEEPSCANNING;
            case CharCode.CHAR_t:
                state = t_state;
                return KEEPSCANNING;
            case CharCode.CHAR_u:
                state = u_state;
                return KEEPSCANNING;
            case CharCode.CHAR_v:
                state = v_state;
                return KEEPSCANNING;
            case CharCode.CHAR_w:
                state = w_state;
                return KEEPSCANNING;
            case CharCode.CHAR_0:
                state = zero_state;
                return KEEPSCANNING;
            case CharCode.CHAR_space:
                continue;
            case CharCode.CHAR_newline:
                isFirstTokenOnLine = true;
                continue;
            case 0:
                return  Tokens.EOS_TOKEN;
            default:
                if (CharClass.isDigit(c)) {
                    state = decimalinteger_state;
                    return KEEPSCANNING;
                } else if (isNextIdentifierPart()) { // don't just use c as \ escapes need handling
                    state = identifier_state;
                    return KEEPSCANNING;
                } else {
                    state = error_state;
                    return KEEPSCANNING;
                }
            }
        }
        Debug.assert(false);
        return 0; // don't think this is reachable, but compiler doesn't figure that out
	};

	private function error_state_proc():int {
		var c:int;
        error(ErrorConstants.kError_Lexical_General);
        skiperror();
        state = start_state;
        return KEEPSCANNING;
	};

	private function minus_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_minus) {
            return Tokens.MINUSMINUS_TOKEN;
        } else if (c == CharCode.CHAR_equal) {
            return Tokens.MINUSASSIGN_TOKEN;
        } else {
            retract();
            return Tokens.MINUS_TOKEN;
        }
	};

	private function notequals_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_equal) {
            return Tokens.STRICTNOTEQUALS_TOKEN;
        } else {
            retract();
            return Tokens.NOTEQUALS_TOKEN;
        }
	};

	private function not_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_equal) {
            state = notequals_state;
            return KEEPSCANNING;
        } else {
            retract();
            return Tokens.NOT_TOKEN;
        }
	};

	private function remainder_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_equal) {
            return Tokens.MODULUSASSIGN_TOKEN;
        } else {
            retract();
            return Tokens.MODULUS_TOKEN;
        }
	};

	private function and_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_ampersand) {
            return Tokens.LOGICALAND_TOKEN;
        } else if (c == CharCode.CHAR_equal) {
            return Tokens.BITWISEANDASSIGN_TOKEN;
        } else {
            retract();
            return Tokens.BITWISEAND_TOKEN;
        }
	};

	private function star_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_equal) {
            return Tokens.MULTASSIGN_TOKEN;
        } else {
            retract();
            return Tokens.MULT_TOKEN;
        }
	};

	private function dot_state_proc():int {
		var c:int;
        c = nextchar();
        if (CharClass.isDigit(c)) {
            state = decimal_state;
            return KEEPSCANNING;
        } else if (c == CharCode.CHAR_period) {
            state = doubledot_state;
            return KEEPSCANNING;
        } else {
            retract();
            return Tokens.DOT_TOKEN;
        }
	};

	private function doubledot_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_period) {
            return Tokens.TRIPLEDOT_TOKEN;
        } else {
            retract();
            return Tokens.DOUBLEDOT_TOKEN;
        }
	};

	private function slash_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_slash) {
            if (blockcommentbuf == null) {
                blockcommentbuf = "";
            }
            state = linecomment_state;
            return KEEPSCANNING;
        } else if (c == CharCode.CHAR_asterisk) {
            if (blockcommentbuf == null) {
                blockcommentbuf = "";
            }
            blockcommentbuf += "/*";
            state = blockcomment_state;
            return KEEPSCANNING;
        } else {
            retract();
            if (isSlashDivContext()) {
                state = slashdiv_state;
            } else {
                state = slashregexp_state;
            }
            return KEEPSCANNING;
        }
	};

	private function colon_state_proc():int {
		var c:int = nextchar();
        if (c == CharCode.CHAR_colon) {
            return Tokens.DOUBLECOLON_TOKEN;
        } else {
            retract();
            return Tokens.COLON_TOKEN;
        }
	};

	private function bitwisexor_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_equal) {
            return Tokens.BITWISEXORASSIGN_TOKEN;
        } else {
            retract();
            return Tokens.BITWISEXOR_TOKEN;
        }
	};

	private function or_state_proc():int {
		var c:int;
        c = nextchar();
        switch (c) {
        case CharCode.CHAR_verticalbar:
            return Tokens.LOGICALOR_TOKEN;
        case CharCode.CHAR_equal:
            return Tokens.BITWISEORASSIGN_TOKEN;
        default:
            retract();
            return Tokens.BITWISEOR_TOKEN;
        }
	};

	private function plus_state_proc():int {
		var c:int;
        c = nextchar();
        switch (c) {
        case CharCode.CHAR_plus:
            return Tokens.PLUSPLUS_TOKEN;
        case CharCode.CHAR_equal:
            return Tokens.PLUSASSIGN_TOKEN;
        default:
            retract();
            return Tokens.PLUS_TOKEN;
        }
	};

	private function lessthan_state_proc():int {
		var c:int = nextchar();
        if ( isSlashDivContext() ) {
            switch (c) {
            case CharCode.CHAR_lessthan:
                c = nextchar();
                if (c == CharCode.CHAR_equal) {
                    return Tokens.LEFTSHIFTASSIGN_TOKEN;
                } else {
                    retract();
                    return Tokens.LEFTSHIFT_TOKEN;
                }
            case CharCode.CHAR_equal:
                return Tokens.LESSTHANOREQUALS_TOKEN;
            case CharCode.CHAR_slash:
                return Tokens.XMLTAGSTARTEND_TOKEN;
            case CharCode.CHAR_bang:
                state = xmlcommentorcdatastart_state;
                return KEEPSCANNING;
            case CharCode.CHAR_questionmark:
                state = xmlpi_state;
                return KEEPSCANNING;
            default:
                retract();
                return Tokens.LESSTHAN_TOKEN;
            }
        } else {
            switch (c) {
            case CharCode.CHAR_slash:
                return Tokens.XMLTAGSTARTEND_TOKEN;
            case CharCode.CHAR_bang:
                state = xmlcommentorcdatastart_state;
                return KEEPSCANNING;
            case CharCode.CHAR_questionmark:
                state = xmlpi_state;
                return KEEPSCANNING;
            default:
                retract();
                return Tokens.LESSTHAN_TOKEN;
            }
        }
        return 0; // keep compiler happy
    };

	private function equal_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_equal) {
            c = nextchar();
            if (c == CharCode.CHAR_equal) {
                return Tokens.STRICTEQUALS_TOKEN;
            } else {
                retract();
                return Tokens.EQUALS_TOKEN;
            }
        } else {
            retract();
            return Tokens.ASSIGN_TOKEN;
        }
	};

	private function greaterthan_state_proc():int {
		var c:int;
		c = nextchar();
        if ( isSlashDivContext() ) {
            switch (c) {
            case CharCode.CHAR_greaterthan:
                // right shift
                c = nextchar();
                if (c == CharCode.CHAR_equal) {
                    return Tokens.RIGHTSHIFTASSIGN_TOKEN;
                } else if (c == CharCode.CHAR_greaterthan) {
                    c = nextchar();
                    if (c == CharCode.CHAR_equal) {
                        return Tokens.UNSIGNEDRIGHTSHIFTASSIGN_TOKEN;
                    } else {
                        retract();
                        return Tokens.UNSIGNEDRIGHTSHIFT_TOKEN;
                    }
                } else {
                    retract();
                    return Tokens.RIGHTSHIFT_TOKEN;
                }                        
            case CharCode.CHAR_equal:
                return Tokens.GREATERTHANOREQUALS_TOKEN;
            default:
                retract();
                return Tokens.GREATERTHAN_TOKEN;
            }
        } else {
            return Tokens.GREATERTHAN_TOKEN;
        }
	};

	private function identifier_state_proc():int {
		var c:int;
        while (true) {
        	c = nextchar();
            if (isNextIdentifierPart()) {
                // state = identifier_state;
                continue;
            } else {
                retract();
                return makeTokenInstance(Tokens.IDENTIFIER_TOKEN, input.copy());
            }
       }
       Debug.assert(false);
       return 0;
	};

	private function singlequote_state_proc():int {
		var c:int;
        while (true) {
            c = nextchar_unnormalized();
            // string literals can span multiple lines, which InputBuffer doesn't handle
            //  copy all characters in the string literal into a local buffer instead.
            stringliteralbuf += String.fromCharCode(c);
            switch (c) {
            case CharCode.CHAR_singlequote:
                return makeTokenInstance(Tokens.STRINGLITERAL_TOKEN, input.escapeString(stringliteralbuf, 0, stringliteralbuf.length - 1));
            case CharCode.CHAR_backslash:
                c = nextchar_unnormalized();
                if (c == CharCode.CHAR_newline) {  // escaped newline is the line continuation character.  Continue string with input from next line.   
                    stringliteralbuf = stringliteralbuf.substr(0, stringliteralbuf.length-1); // remove backslash from buffer
                    c = nextchar_unnormalized();
                }
                stringliteralbuf += String.fromCharCode(c);
                // state = singlequote_staqte;
                //RES: make sure this works if quote immediately follows quoted newline
                continue;
            case CharCode.CHAR_newline:
                error(ErrorConstants.kError_Lexical_LineTerminatorInSingleQuotedStringLiteral);
                state = start_state; //RES: added this
                return KEEPSCANNING;
            case 0:
                error(ErrorConstants.kError_Lexical_EndOfStreamInStringLiteral);
                return Tokens.EOS_TOKEN;
            default:
                // state = singlequote_state;
                continue;
            }
        }
       Debug.assert(false);
       return 0;
	};

	private function doublequote_state_proc():int {
		var c:int;
        while (true) {
            c = nextchar_unnormalized();
            // string literals can span multiple lines, which InputBuffer doesn't handle
            //  copy all characters in the string literal into a local buffer instead.
            stringliteralbuf += String.fromCharCode(c);
            switch (c) {
            case CharCode.CHAR_doublequote:
                return makeTokenInstance(Tokens.STRINGLITERAL_TOKEN, input.escapeString(stringliteralbuf, 0, stringliteralbuf.length - 1));
            case CharCode.CHAR_backslash:
                c = nextchar_unnormalized();
                if (c == CharCode.CHAR_newline) {  // escaped newline is the line continuation character.  Continue string with input from next line.   
                    stringliteralbuf = stringliteralbuf.substr(0, stringliteralbuf.length-1); // remove backslash from buffer
                    c = nextchar_unnormalized();
                }
                stringliteralbuf += String.fromCharCode(c);
                // state = doublequote_staqte;
                //RES: make sure this works if quote immediately follows quoted newline
                continue;
            case CharCode.CHAR_newline:
                error(ErrorConstants.kError_Lexical_LineTerminatorInDoubleQuotedStringLiteral);
                state = start_state; //RES: added this
                return KEEPSCANNING;
            case 0:
                error(ErrorConstants.kError_Lexical_EndOfStreamInStringLiteral);
                return Tokens.EOS_TOKEN;
            default:
                // state = doublequote_state;
                continue;
            }
        }
       Debug.assert(false);
       return 0;
	};

	private function zero_state_proc():int {
		var c:int;
        c = nextchar();
        switch (c) {
        case CharCode.CHAR_X:
        case CharCode.CHAR_x:
            if (CharClass.isHex(nextchar())) {
                retract();
                state = hexinteger_state;
                return KEEPSCANNING;
            } else {
                error(ErrorConstants.kError_Lexical_General);
                state = start_state;
                return KEEPSCANNING;
            }
        case CharCode.CHAR_period:
            state = decimalinteger_state;
            return KEEPSCANNING;
        case CharCode.CHAR_E:
        case CharCode.CHAR_e:
            state = exponentstart_state;
            return KEEPSCANNING;
        default:
            if (CharClass.isDigit(c)) {
                state = decimalinteger_state;
                return KEEPSCANNING;
            } else {
                retract();
                return  makeTokenInstance(Tokens.NUMBERLITERAL_TOKEN, input.copy());
            }
        }
	};

	private function decimalinteger_state_proc():int {
		var c:int;
        while (true) {
            c = nextchar();
            if (CharClass.isDigit(c)) {
                // state = decimalinteger_state;
                continue;
            } else if (c == CharCode.CHAR_period) {
                state = decimal_state;
                return KEEPSCANNING;
            } else if ((c == CharCode.CHAR_E) || (c == CharCode.CHAR_e)) {
                state = exponentstart_state;
                return KEEPSCANNING;
            } else {
                retract();
                return  makeTokenInstance(Tokens.NUMBERLITERAL_TOKEN, input.copy());
            }
        }
       Debug.assert(false);
       return 0;
	};

	private function decimal_state_proc():int {
		var c:int;
        while (true) {
            c = nextchar();
            if (CharClass.isDigit(c)) {
                continue;
            } else if ((c == CharCode.CHAR_E) || (c == CharCode.CHAR_e)) {
                state = exponentstart_state;
                return KEEPSCANNING;
            } else {
                retract();
                return makeTokenInstance(Tokens.NUMBERLITERAL_TOKEN, input.copy());
            }
        }
       Debug.assert(false);
       return 0;
	};

	private function exponentstart_state_proc():int {
		var c:int;
        c = nextchar();
        if (CharClass.isDigit(c) || c == CharCode.CHAR_plus || c == CharCode.CHAR_minus) {
            state = exponent_state;
            return KEEPSCANNING;
        } else {
            error(ErrorConstants.kError_Lexical_General);
            state = start_state;
            return KEEPSCANNING;
        }
	};

	private function exponent_state_proc():int {
		var c:int;
        while (true) {
            c = nextchar();
            if (CharClass.isDigit(c)) {
                // state = exponent_state;
                continue;
            } else {
                retract();
                return makeTokenInstance(Tokens.NUMBERLITERAL_TOKEN, input.copy());
            }
        }
       Debug.assert(false);
       return 0;
	};

	private function hexinteger_state_proc():int {
		var c:int;
        while (true) {
            c = nextchar();
            if (CharClass.isHex(c)) {
                // state = hexinteger_state;
                continue;
            } else {
                retract();
                return makeTokenInstance(Tokens.NUMBERLITERAL_TOKEN, input.copy() );
            }
        }
       Debug.assert(false);
       return 0;
	};

	private function slashregexp_state_proc():int {
		var c:int;
        while (true) {
            c = nextchar();
            switch (c) {
            case CharCode.CHAR_backslash:
                nextchar();
                continue;
            case CharCode.CHAR_slash:
                regexp_flags = 0;
                state = regexp_state;
                return KEEPSCANNING;
            case CharCode.CHAR_LF:
            case 0:
                error(ErrorConstants.kError_Lexical_General);
                state = start_state;
                return KEEPSCANNING;
            default:
                continue;
            }
        }
        Debug.assert(false);
       return 0;
    };

	private function slashdiv_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_greaterthan) {
            return Tokens.XMLTAGENDEND_TOKEN;
        } else if (c == CharCode.CHAR_equal) {
            return Tokens.DIVASSIGN_TOKEN;
        } else {
            retract();
            return Tokens.DIV_TOKEN;
        }
	};

	private function regexp_state_proc():int {
        /*
         * tokens: g | i | m | s | x  .  Note that s and x are custom extentions to match perl's functionality
         *   Also note we handle this via an array of boolean flags intead of state change logic
         */

		var c:int;
        while (true) { 
            c = nextchar();
            switch (c) {
            case CharCode.CHAR_g:
                if ((regexp_flags & 0x01) == 0) {
                    regexp_flags |= 0x01;
                    continue;
                }
                error(ErrorConstants.kError_Lexical_General); 
                state = start_state; 
                return KEEPSCANNING;
            case CharCode.CHAR_i:
                if ((regexp_flags & 0x02) == 0) {
                    regexp_flags |= 0x02;
                    continue;
                }
                error(ErrorConstants.kError_Lexical_General); 
                state = start_state; 
                return KEEPSCANNING;
            case CharCode.CHAR_m:
                if ((regexp_flags & 0x04) == 0) {
                    regexp_flags |= 0x04;
                    continue;
                }
                error(ErrorConstants.kError_Lexical_General); 
                state = start_state; 
                return KEEPSCANNING;
            case CharCode.CHAR_s:
                if ((regexp_flags & 0x08) == 0) {
                    regexp_flags |= 0x08;
                    continue;
                }
                error(ErrorConstants.kError_Lexical_General); 
                state = start_state; 
                return KEEPSCANNING;
            case CharCode.CHAR_x:
                if ((regexp_flags & 0x10) == 0) {
                    regexp_flags |= 0x10;
                    continue;
                }
                error(ErrorConstants.kError_Lexical_General); 
                state = start_state; 
                return KEEPSCANNING;
            default:
                if (CharClass.isIdentifierPart(c)) {
                    error(ErrorConstants.kError_Lexical_General); 
                    state = start_state; 
                    return KEEPSCANNING;
                } else {
                    retract(); 
                    return makeTokenInstance( Tokens.REGEXPLITERAL_TOKEN, input.copyWithoutInterpretingEscapedChars() );
                }
            }
        }
       Debug.assert(false);
       return 0;

	};

	private function keywordmatch(key:String):Boolean {
        var i:int;
        var c:int;
        for (i = 0; i < key.length; i++) {
            c = nextchar();
            if (c != key.charCodeAt(i)) {
                retract();
                state = identifier_state;
                return false;
            }
        }
        c = nextchar();
        if ( isNextIdentifierPart() ) {
            state = identifier_state;
            return false;
        }
        retract();
        return true;
    }

               
        
	private function a_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_s) {
        	if (!keywordmatch(""))
        		return KEEPSCANNING;
            return Tokens.AS_TOKEN;
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
 	};

	private function b_state_proc():int {
        var c:int;
		if (!keywordmatch("reak"))
            return KEEPSCANNING;
        return Tokens.BREAK_TOKEN;
        
	};

	private function c_state_proc():int {
		var c:int;
        c = nextchar();
        switch (c) {
        case CharCode.CHAR_a:
            c = nextchar();
            if (c == CharCode.CHAR_s) {
                if (!keywordmatch("e"))
                    return KEEPSCANNING;
                return Tokens.CASE_TOKEN;
            } else if (c == CharCode.CHAR_t) {
                if (!keywordmatch("ch"))
                    return KEEPSCANNING;
                return Tokens.CATCH_TOKEN;
            }
            break;
        case CharCode.CHAR_l:
            if (!keywordmatch("ass"))
                return KEEPSCANNING;
            return Tokens.CLASS_TOKEN;
        case CharCode.CHAR_o:
            c = nextchar();
            if (c == CharCode.CHAR_n) {
                c = nextchar();
                if (c == CharCode.CHAR_s) {
                    if (!keywordmatch("t"))
                        return KEEPSCANNING;
                    return Tokens.CONST_TOKEN;
                } else if (c == CharCode.CHAR_t) {
                    if (!keywordmatch("inue"))
                        return KEEPSCANNING;
                    return Tokens.CONTINUE_TOKEN;
                }
            }
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
    };

	private function d_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_e) {
            c = nextchar();
            if (c == CharCode.CHAR_f) {
                if (!keywordmatch("ault"))
                    return KEEPSCANNING;
                return Tokens.DEFAULT_TOKEN;
            } else if (c == CharCode.CHAR_l) {
                if (!keywordmatch("ete"))
                    return KEEPSCANNING;
                return Tokens.DELETE_TOKEN;
            }
        } else if (c == CharCode.CHAR_o) {
            if (!keywordmatch("")) 
                return KEEPSCANNING;
            return Tokens.DO_TOKEN;
        } else if (c == CharCode.CHAR_y) {
            if (!keywordmatch("namic")) 
                return KEEPSCANNING;
            return Tokens.DYNAMIC_TOKEN;
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
	};

	private function e_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_a) {
            if (!keywordmatch("ch"))
                return KEEPSCANNING;
            return Tokens.EACH_TOKEN;
        } else if (c == CharCode.CHAR_l) {
            if (!keywordmatch("se")) 
                return KEEPSCANNING;
            return Tokens.ELSE_TOKEN;
        } else if (c == CharCode.CHAR_x) {
            if (!keywordmatch("tends")) 
                return KEEPSCANNING;
            return Tokens.EXTENDS_TOKEN;
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
	};

	private function f_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_a) {
            if (!keywordmatch("lse"))
                return KEEPSCANNING;
            return Tokens.FALSE_TOKEN;
        } else if (c == CharCode.CHAR_i) {
            c = nextchar();
            if (c == CharCode.CHAR_n) {
                c = nextchar();
                if (c == CharCode.CHAR_a) {
                    c = nextchar();
                    if (c == CharCode.CHAR_l) {
                        c = nextchar();
                        if (c == CharCode.CHAR_l) {
                            if (!keywordmatch("y")) 
                                return KEEPSCANNING;
                            return Tokens.FINALLY_TOKEN;
                        } else  if (isNextIdentifierPart()) {
                            state = identifier_state;
                            return KEEPSCANNING;
                        } else {
                            retract();
                            return Tokens.FINAL_TOKEN;
                        }
                    }
                }
            } 
        } else if (c == CharCode.CHAR_o) {
            if (!keywordmatch("r")) 
                return KEEPSCANNING;
            return Tokens.FOR_TOKEN;
        } else if (c == CharCode.CHAR_u) {
            if (!keywordmatch("nction")) 
                return KEEPSCANNING;
            return Tokens.FUNCTION_TOKEN;
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
	};

	private function g_state_proc():int {
		var c:int;
        if (!keywordmatch("et"))
            return KEEPSCANNING;
        return Tokens.GET_TOKEN;
	};

	private function i_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_f) {
            if (!keywordmatch(""))
                return KEEPSCANNING;
            return Tokens.IF_TOKEN;
        } else if (c == CharCode.CHAR_m) {
            c = nextchar();
            if (c == CharCode.CHAR_p) {
                c = nextchar();
                if (c == CharCode.CHAR_l) {
                    if (!keywordmatch("ements")) 
                        return KEEPSCANNING;
                    return Tokens.IMPLEMENTS_TOKEN;
                } else if (c == CharCode.CHAR_o) {
                    if (!keywordmatch("rt"))
                        return KEEPSCANNING;
                    return Tokens.IMPORT_TOKEN;
                }
            }
        } else if (c == CharCode.CHAR_n) {
            c = nextchar();
            if (c == CharCode.CHAR_c) {
                if (!keywordmatch("lude"))
                    return KEEPSCANNING;
                return Tokens.INCLUDE_TOKEN;
            } else if (c == CharCode.CHAR_s) {
                if (!keywordmatch("tanceof"))
                    return KEEPSCANNING;
                return Tokens.INSTANCEOF_TOKEN;
            } else if (c == CharCode.CHAR_t) {
                c = nextchar();
                if (c == CharCode.CHAR_e) {
                    c = nextchar();
                    if (c == CharCode.CHAR_r) {
                        c = nextchar();
                        if (c == CharCode.CHAR_f) {
                            if (!keywordmatch("ace"))
                                return KEEPSCANNING;
                            return Tokens.INTERFACE_TOKEN;
                        } else if (c == CharCode.CHAR_n) {
                            if (!keywordmatch("al"))
                                return KEEPSCANNING;
                            return Tokens.INTERNAL_TOKEN;
                        } else if (isNextIdentifierPart()) {
                            state = identifier_state;
                            return KEEPSCANNING;
                        } else {
                            retract()
                            return Tokens.IN_TOKEN;
                        }
                    }
                }
            } else {
            	retract();
                if (!keywordmatch(""))
                    return KEEPSCANNING;
                return Tokens.IN_TOKEN;
            }
        } else if (c == CharCode.CHAR_s) {
            if (!keywordmatch("")) 
                return KEEPSCANNING;
            return Tokens.IS_TOKEN;
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
	};


	private function o_state_proc():int {
		var c:int;
        if (!keywordmatch("verride"))
            return KEEPSCANNING;
        return Tokens.OVERRIDE_TOKEN;
	};

	private function n_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_a) {
            c = nextchar();
            if (c == CharCode.CHAR_m) {
                if (!keywordmatch("espace"))
                    return KEEPSCANNING;
                return Tokens.NAMESPACE_TOKEN;
            } else if (c == CharCode.CHAR_t) {
                if (!keywordmatch("ive"))
                    return KEEPSCANNING;
                return Tokens.NATIVE_TOKEN;
            }
        } else if (c == CharCode.CHAR_e) {
            if (!keywordmatch("w"))
                return KEEPSCANNING;
            return Tokens.NEW_TOKEN;
        } else if (c == CharCode.CHAR_u) {
            if (!keywordmatch("ll"))
                return KEEPSCANNING;
            return Tokens.NULL_TOKEN;
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
	};



	private function p_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_a) {
            if (!keywordmatch("ckage"))
                return KEEPSCANNING;
            return Tokens.PACKAGE_TOKEN;
        } else if (c == CharCode.CHAR_r) {
            c = nextchar();
            if (c == CharCode.CHAR_i) {
                if (!keywordmatch("vate"))
                    return KEEPSCANNING;
                return Tokens.PRIVATE_TOKEN;
            } else if (c == CharCode.CHAR_o) {
                if (!keywordmatch("tected"))
                    return KEEPSCANNING;
                return Tokens.PROTECTED_TOKEN;
            }
        } else if (c == CharCode.CHAR_u) {
            if (!keywordmatch("blic"))
                return KEEPSCANNING;
            return Tokens.PUBLIC_TOKEN;
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
	};

	private function r_state_proc():int {
		var c:int;
        if (!keywordmatch("eturn"))
            return KEEPSCANNING;
        return Tokens.RETURN_TOKEN;
	};

	private function s_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_e) {
            if (!keywordmatch("t"))
                return KEEPSCANNING;
            return Tokens.SET_TOKEN;
        } else if (c == CharCode.CHAR_t) {
            if (!keywordmatch("atic")) 
                return KEEPSCANNING;
            return Tokens.STATIC_TOKEN;
        } else if (c == CharCode.CHAR_u) {
            if (!keywordmatch("per")) 
                return KEEPSCANNING;
            return Tokens.SUPER_TOKEN;
        } else if (c == CharCode.CHAR_w) {
            if (!keywordmatch("itch")) 
                return KEEPSCANNING;
            return Tokens.SWITCH_TOKEN;
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
	};

	private function t_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_h) {
            c = nextchar();
            if (c == CharCode.CHAR_i) {
                if (!keywordmatch("s")) 
                    return KEEPSCANNING;
                return Tokens.THIS_TOKEN;
            } else if (c == CharCode.CHAR_r) {
                if (!keywordmatch("ow"))
                    return KEEPSCANNING;
                return Tokens.THROW_TOKEN;
            }
        } else if (c == CharCode.CHAR_o) {
            if (!keywordmatch("")) 
                return KEEPSCANNING;
            return Tokens.TO_TOKEN;
        } else if (c == CharCode.CHAR_r) {
            c = nextchar();
            if (c == CharCode.CHAR_u) {
                if (!keywordmatch("e")) 
                    return KEEPSCANNING;
                return Tokens.TRUE_TOKEN;
            } else if (c == CharCode.CHAR_y) {
                if (!keywordmatch(""))
                    return KEEPSCANNING;
                return Tokens.TRY_TOKEN;
            }
        } else if (c == CharCode.CHAR_y) {
            if (!keywordmatch("peof")) 
                return KEEPSCANNING;
            return Tokens.TYPEOF_TOKEN;
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
	};


	private function u_state_proc():int {
		if (!keywordmatch("se"))
            return KEEPSCANNING;
        return Tokens.USE_TOKEN;
	};

	private function v_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_a) {
            if (!keywordmatch("r"))
                return KEEPSCANNING;
            return Tokens.VAR_TOKEN;
        } else if (c == CharCode.CHAR_o) {
            if (!keywordmatch("id")) 
                return KEEPSCANNING;
            return Tokens.VOID_TOKEN;
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
	};

	private function w_state_proc():int {
		var c:int;
        c = nextchar();
        if (c == CharCode.CHAR_h) {
            if (!keywordmatch("ile"))
                return KEEPSCANNING;
            return Tokens.WHILE_TOKEN;
        } else if (c == CharCode.CHAR_i) {
            if (!keywordmatch("th")) 
                return KEEPSCANNING;
            return Tokens.WITH_TOKEN;
        }
        retract();
        state = identifier_state;
        return KEEPSCANNING;
	};

	private function blockcomment_state_proc():int {
		var c:int;
        var afterStar:Boolean = false;
        var blocktext:String;
        while (true) {
            c = nextchar();
            blockcommentbuf += String.fromCharCode(c);
            switch (c) {
            case CharCode.CHAR_asterisk:
                afterStar = true;
                continue;
            case CharCode.CHAR_slash:
                if (afterStar) {
                    blocktext = blockcommentbuf.toString();
                    return makeTokenInstance(Tokens.BLOCKCOMMENT_TOKEN, blocktext);
                }
                break;
            case 0:
                error(ErrorConstants.kError_Lexical_General);
                state = start_state;
                return KEEPSCANNING;
            }
            afterStar = false;
        }
        Debug.assert(false);
        return 0; // keep compiler happy
	};

	private function linecomment_state_proc():int {
		var c:int;
        while(true) {
            c = nextchar();
            switch (c) {
            case CharCode.CHAR_newline:
                retract(); // don't return the newline in the comment
                return makeTokenInstance( Tokens.SLASHSLASHCOMMENT_TOKEN, input.copy());
            case 0:
                return Tokens.EOS_TOKEN;
            default:
                continue;
            }
        }
        Debug.assert(false);
        return 0; // keep compiler happy
	};

	private function xmlliteral_state_proc():int {
		var c:int;
        var xmltext:String;
        while (true) {
            c = nextchar();
            switch (c) {
            case CharCode.CHAR_leftbrace:
                xmltext = input.source().substring(startofxml, input.positionOfNext());
                return makeTokenInstance(Tokens.XMLPART_TOKEN, xmltext);
            case CharCode.CHAR_lessthan:
                c = nextchar();
                if ( c == CharCode.CHAR_slash ) {
                    --level;
                    nextchar();
                    input.mark();
                    retract();
                    state = endxmlname_state;
                    return KEEPSCANNING;
                } else {
                    ++level;
                    continue;
                }
            case CharCode.CHAR_slash:
                c = nextchar();
                if ( c == CharCode.CHAR_greaterthan ) {
                    --level;
                    if (level == 0) {
                        xmltext = input.source().substring(startofxml, input.positionOfNext() + 2);
                        return makeTokenInstance(Tokens.XMLLITERAL_TOKEN, xmltext);
                    } else {
                        continue;
                    }
                } else
                    continue;
            case 0:
                retract();
                error(ErrorConstants.kError_Lexical_NoMatchingTag);
                state = start_state;
                return KEEPSCANNING;
            default:
                continue;
            }
        }
        Debug.assert(false);
        return 0;
	};

	private function endxmlname_state_proc():int {
		var c:int;
        var xmltext:String;
        var temp:String;
        while (true) {
            c = nextchar();
            if (CharClass.isIdentifierPart(c) || (c == CharCode.CHAR_colon)) {
                continue;
            } else if (c == CharCode.CHAR_leftbrace) {
                if (xmltagname != null)
                    xmltagname = null;
                xmltext = input.source().substring(startofxml, input.positionOfNext());
                return makeTokenInstance(Tokens.XMLPART_TOKEN, xmltext);
            } else if (c == CharCode.CHAR_greaterthan) {
                retract();
                temp = input.copy();
                nextchar();
                if (level == 0) {
                    if (xmltagname != null) {
                        if (temp == xmltagname) {
                            xmltext = input.source().substring(startofxml, input.positionOfNext() + 2);
                            return makeTokenInstance(Tokens.XMLLITERAL_TOKEN, xmltext);
                        }
                    } else {
                        xmltext = input.source().substring(startofxml, input.positionOfNext() + 2);
                        return makeTokenInstance(Tokens.XMLLITERAL_TOKEN, xmltext);
                    }
                }
                state = xmlliteral_state;
                return KEEPSCANNING;
                        
            } else {
                state = xmlliteral_state;
                return KEEPSCANNING;
            }
        }
        Debug.assert(false);
        return 0;
	};

	private function xmlcommentorcdatastart_state_proc():int {
		var c:int = nextchar();
        switch (c) {
        case CharCode.CHAR_leftbracket:
            state = xmlcdatastart_state;
            return KEEPSCANNING;
        case CharCode.CHAR_leftbracket:
            state = xmlcommentstart_state;
            return KEEPSCANNING;
        default:
            error(ErrorConstants.kError_Lexical_General);
            state = start_state;
            return KEEPSCANNING;
        }
	};

	private function xmlcdatastart_state_proc():int {
		var c:int;
        var target:Array = [CharCode.CHAR_C, CharCode.CHAR_D, CharCode.CHAR_A, 
                            CharCode.CHAR_T, CharCode.CHAR_A, CharCode.CHAR_leftbracket];
        var i:int;
        for (i = 0; i < target.length; i++) {
            c = nextchar();
            if (c != target[i]) {
                error(ErrorConstants.kError_Lexical_General);
                state = start_state;
                return KEEPSCANNING;
            }
        }
        // we've seen <[CDATA[.  Now look for contents of CDATA section
        while (true) {
            c = nextchar();
            switch (c) {
            case 0:
                error(ErrorConstants.kError_Lexical_General);
                state = start_state;
                return KEEPSCANNING;
            case CharCode.CHAR_rightbracket: // we need to see ]]> or continue around the loop
                c = nextchar();
                if (c != CharCode.CHAR_rightbracket)
                    continue;
                if (c != CharCode.CHAR_greaterthan) {
                    return makeTokenInstance(Tokens.XMLMARKUP_TOKEN, getXMLText(startofxml, input.positionOfNext()+1)); 
                }
            default:
                continue;
            }
        }
        Debug.assert(false);
        return 0;
	};

	private function xmlcommentstart_state_proc():int {
		var c:int;
        c = nextchar();
        if (c != CharCode.CHAR_minus) {
            error(ErrorConstants.kError_Lexical_General);
            state = start_state;
            return KEEPSCANNING;
        }
        // we have seen <!--
        while (true) {
            c = nextchar();
            switch (c) {
            case 0:
                error(ErrorConstants.kError_Lexical_General);
                state = start_state;
                return KEEPSCANNING;
            case CharCode.CHAR_minus:
                c = nextchar();
                if (c != CharCode.CHAR_minus)
                    continue;
                c = nextchar();
                if (c != CharCode.CHAR_greaterthan) {
                    error(ErrorConstants.kError_Lexical_General);
                    state = start_state;
                    return KEEPSCANNING;
                }
                return makeTokenInstance(Tokens.XMLMARKUP_TOKEN, getXMLText(startofxml, input.positionOfNext()+1));
            default:
                continue;
            }
        }
        Debug.assert(false);
        return 0;
	};

	private function xmlpi_state_proc():int {
		var c:int;
        while (true) {
            c = nextchar();
            switch (c) {
            case 0:
                error(ErrorConstants.kError_Lexical_General);
                state = start_state;
                return KEEPSCANNING;
            case CharCode.CHAR_questionmark:
                c = nextchar();
                if (c != CharCode.CHAR_greaterthan) {
                    error(ErrorConstants.kError_Lexical_General);
                    state = start_state;
                    return KEEPSCANNING;
                }
                return makeTokenInstance(Tokens.XMLMARKUP_TOKEN, getXMLText(startofxml, input.positionOfNext()+1));
            default:
                continue;
            }
        }
        Debug.assert(false);
        return 0;
	};

	private function xmltext_state_proc():int {
		var c:int;
        var xmltext:String;
        while (true) {
            switch (c) {
            case CharCode.CHAR_lessthan:
            case CharCode.CHAR_leftbrace:
                retract();
                xmltext = getXMLText(startofxml, input.positionOfNext()+1);
                if (xmltext != null) {
                    return makeTokenInstance(Tokens.XMLTEXT_TOKEN, xmltext);
                } else {// if there is no leading text, then just return puncutation token to avoid empty text tokens
                    c = nextchar(); // the < or { we retracted (which was already in c, but we need to un-retract
                    if (c == CharCode.CHAR_leftbrace) {
                        return Tokens.LEFTBRACE_TOKEN;
                    } else { // c == '<'
                        switch ( nextchar() ) {
                        case CharCode.CHAR_slash: 
                            return Tokens.XMLTAGSTARTEND_TOKEN;
                        case CharCode.CHAR_bang:
                            state = xmlcommentorcdatastart_state;
                            return KEEPSCANNING;
                        case CharCode.CHAR_questionmark:
                            state = xmlpi_state;
                            return KEEPSCANNING;
                        default:
                            retract();
                            return Tokens.LEFTBRACE_TOKEN;
                        }
                    }
                }
            case 0:
                return Tokens.EOS_TOKEN;
            default:
                continue;
            }
        }
        Debug.assert(false);
        return 0;
	};

	private function utf8sig_state_proc():int {
		var c:int;
		c = nextchar();
		if (c == 0xffffffbb) {
			c = nextchar();
			if (c == 0xffffffbf) {
				state = start_state;
				return KEEPSCANNING;
			}
		}
		state = error_state;
		return KEEPSCANNING;
	};

	private function eol_state_proc():int {
		var c:int;
        while (true) {
            // RES: the state is only set when in this state.  i.e., not reachable.  Fix this
            isFirstTokenOnLine = true;
            switch (nextchar()) {
            case CharCode.CHAR_newline:
                dt("eating eol");
                // state = eol_state;
                continue;
            default:
                dt("returning eol"); //RES: though it doesn't seem to actually return
                retract();
                state = start_state;
                return KEEPSCANNING;
            };
        }
        Debug.assert(false);
        return 0;
	};

	private var state_proc:Array = [
		error_state_proc,
		start_state_proc,
		minus_state_proc,
		notequals_state_proc,
		not_state_proc,
		remainder_state_proc,
		and_state_proc,
		star_state_proc,
		dot_state_proc,
		doubledot_state_proc,
		slash_state_proc,
		colon_state_proc,
		bitwisexor_state_proc,
		or_state_proc,
		plus_state_proc,
		lessthan_state_proc,
		equal_state_proc,
		greaterthan_state_proc,
		identifier_state_proc,
		singlequote_state_proc,
		doublequote_state_proc,
		zero_state_proc,
		decimalinteger_state_proc,
		decimal_state_proc,
		exponentstart_state_proc,
		exponent_state_proc,
		hexinteger_state_proc,
		slashregexp_state_proc,
		slashdiv_state_proc,
		regexp_state_proc,
		a_state_proc,
		b_state_proc,
		c_state_proc,
		d_state_proc,
		e_state_proc,
		f_state_proc,
		g_state_proc,
		i_state_proc,
		n_state_proc,
        o_state_proc,
		r_state_proc,
		p_state_proc,
		s_state_proc,
		t_state_proc,
		u_state_proc,
		v_state_proc,
		w_state_proc,
		blockcomment_state_proc,
		linecomment_state_proc,
		xmlliteral_state_proc,
		endxmlname_state_proc,
		xmlcommentorcdatastart_state_proc,
		xmlcdatastart_state_proc,
		xmlcommentstart_state_proc,
		xmlpi_state_proc,
		xmltext_state_proc,
		utf8sig_state_proc,
		eol_state_proc 
	]

	/* scanner states */
 
	private static const error_state:int = 0;
	private static const start_state:int = 1;

	private static const minus_state:int = start_state + 1;
	private static const notequals_state:int = minus_state + 1;
	private static const not_state:int = notequals_state + 1;
	private static const remainder_state:int = not_state + 1;
	private static const and_state:int = remainder_state + 1;
	private static const star_state:int = and_state + 1;
	private static const dot_state:int = star_state + 1;
	private static const doubledot_state:int = dot_state + 1;
	private static const slash_state:int = doubledot_state + 1;
	private static const colon_state:int = slash_state + 1;
	private static const bitwisexor_state:int = colon_state + 1;
	private static const or_state:int = bitwisexor_state + 1;
	private static const plus_state:int = or_state + 1;
	private static const lessthan_state:int = plus_state + 1;
	private static const equal_state:int = lessthan_state + 1;
	private static const greaterthan_state:int = equal_state + 1;
	private static const identifier_state:int = greaterthan_state + 1;
	private static const singlequote_state:int = identifier_state + 1;
	private static const doublequote_state:int = singlequote_state + 1;
	private static const zero_state:int = doublequote_state + 1;
	private static const decimalinteger_state:int = zero_state + 1;
	private static const decimal_state:int = decimalinteger_state + 1;
	private static const exponentstart_state:int = decimal_state + 1;
	private static const exponent_state:int = exponentstart_state + 1;
	private static const hexinteger_state:int = exponent_state + 1;
	private static const slashregexp_state:int = hexinteger_state + 1;
	private static const slashdiv_state:int = slashregexp_state + 1;
	private static const regexp_state:int = slashdiv_state + 1;

	private static const a_state:int = regexp_state + 1;
	private static const b_state:int = a_state + 1;
	private static const c_state:int = b_state + 1;
	private static const d_state:int = c_state + 1;
	private static const e_state:int = d_state + 1;
	private static const f_state:int = e_state + 1;
	private static const g_state:int = f_state + 1;
	private static const i_state:int = g_state + 1;
	private static const n_state:int = i_state + 1;
	private static const o_state:int = n_state + 1;
	private static const r_state:int = o_state + 1;
	private static const p_state:int = r_state + 1;
	private static const s_state:int = p_state + 1;
	private static const t_state:int = s_state + 1;
	private static const u_state:int = t_state + 1;
	private static const v_state:int = u_state + 1;
	private static const w_state:int = v_state + 1;

	private static const blockcomment_state:int = w_state + 1;
	private static const linecomment_state:int = blockcomment_state + 1;

	private static const xmlliteral_state:int = linecomment_state + 1;
	private static const endxmlname_state:int = xmlliteral_state + 1;
	private static const xmlcommentorcdatastart_state:int = endxmlname_state + 1;
	private static const xmlcdatastart_state:int = xmlcommentorcdatastart_state + 1;
	private static const xmlcommentstart_state:int = xmlcdatastart_state + 1;
	private static const xmlpi_state:int = xmlcommentstart_state + 1;
	private static const xmltext_state:int = xmlpi_state + 1;

	private static const utf8sig_state:int = xmltext_state + 1;

	private static const eol_state:int = utf8sig_state + 1;

	private function isNextIdentifierPart():Boolean {
        var c:int = input.codeOfNext();
        return CharClass.isIdentifierPart(c);
    }
    
    private static var stateName:Array = [
		"error_state",
		"start_state",
		"minus_state",
		"notequals_state",
		"not_state",
		"remainder_state",
		"and_state",
		"star_state",
		"dot_state",
		"doubledot_state",
		"slash_state",
		"colon_state",
		"bitwisexor_state",
		"or_state",
		"plus_state",
		"lessthan_state",
		"equal_state",
		"greaterthan_state",
		"identifier_state",
		"singlequote_state",
		"doublequote_state",
		"zero_state",
		"decimalinteger_state",
		"decimal_state",
		"exponentstart_state",
		"exponent_state",
		"hexinteger_state",
		"slashregexp_state",
		"slashdiv_state",
		"regexp_state",
		"a_state",
		"b_state",
		"c_state",
		"d_state",
		"e_state",
		"f_state",
		"g_state",
		"i_state",
		"n_state",
		"o_state",
		"r_state",
		"p_state",
		"s_state",
		"t_state",
		"u_state",
		"v_state",
		"w_state",
		"blockcomment_state",
		"linecomment_state",
		"xmlliteral_state",
		"endxmlname_state",
		"xmlcommentorcdatastart_state",
		"xmlcdatastart_state",
		"xmlcommentstart_state",
		"xmlpi_state",
		"xmltext_state",
		"utf8sig_state",
		"eol_state"
 		];
 }
}

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

