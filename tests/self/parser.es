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


namespace parser

{
    use default namespace parser
	use namespace release

    public class Parser 
    {
        const abbrevIfElse_mode         = else_token  // lookahead uses this value. don't change.
        const abbrevDoWhile_mode        = while_token // ditto.
        const abbrevFunction_mode       = function_token
        const abbrev_mode               = last_token-1
        const full_mode                 = abbrev_mode-1

        const allowIn_mode              = full_mode-1
        const noIn_mode                 = allowIn_mode-1

        const catch_parameter_error     = 1
        const syntax_xml_error          = 2

        const syntax_error              = 7
        const expression_syntax_error   = 8
        const syntax_eos_error          = 9

        var scan : Scanner

        public function Parser(src)
        {
            this.scan = new Scanner(src)
        }
        
        function lookahead(kind)
        {
            return scan.tokenKind(scan.token()) === kind
        }
        
        function lookaheadSemicolon(mode)
        {
            if( lookahead(semicolon_token) ||
                lookahead(eos_token) ||
                lookahead(rightbrace_token) ||
                scan.followsNewline() ||
				mode === abbrev_mode )
            {
// print("semicolon found")
                var result = true
            }
            else
            {
// print("semicolon not found")
                var result = false
            }

            return result
        }
        
        function matchSemicolon(mode)
        {
            Debug.enter("matchSemicolon",mode)

            if( lookahead(semicolon_token) )
            {
//print("semicolon found")
                var result = match(semicolon_token)
            }
            else
            if( lookahead(eos_token) ||
                lookahead(rightbrace_token) ||
                scan.followsNewline() ||
                mode == abbrev_mode )
            {
//print("virtual semicolon found")
                var result = semicolon_token
            }
            else
            if( (mode == abbrevIfElse_mode || mode == abbrevDoWhile_mode) &&
                lookahead(mode) )
            {
                var result = semicolon_token
            }
            else
            {
                match(semicolon_token)
            }
            
            Debug.exit("matchSemicolon",result)
            return result
        }
        
        function lookaheadReservedWord()
        {
            var result = true
            switch( scan.token() )
            {
                case as_token:
                case break_token:
                case case_token:
                case cast_token:
                case class_token:
                case const_token:
                case continue_token:
                case debugger_token:
                case default_token:
                case delete_token:
                case do_token:
                case else_token:
                case enum_token:
                case extends_token:
                case false_token:
                case finally_token:
                case for_token:
                case function_token:
                case goto_token:
                case if_token:
                case implements_token:
                case import_token:
                case in_token:
                case instanceof_token:
                case interface_token:
//                case internal_token:
//                case intrinsic_token:
                case is_token:
                case let_token:
                case native_token:
                case new_token:
                case null_token:
                case package_token:
//                case private_token:
//                case protected_token:
//                case public_token:
                case return_token:
                case super_token:
                case switch_token:
                case this_token:
                case throw_token:
                case to_token:
                case true_token:
                case try_token:
                case typeof_token:
                case use_token:
                case var_token:
                case void_token:
                case while_token:
                case with_token:
                case dynamic_token:
                case debugger_token:
                case each_token:
                case final_token:
                case get_token: 
                case goto_token: 
                case include_token: 
                case namespace_token: 
                case native_token: 
                case override_token: 
                case prototype_token: 
                case set_token: 
                case static_token: 
                case type_token: 
                case xml_token:
                case yield_token:
                     break; 
                default: result = false
            }
            return result
        }

        function matchReservedWord()
        {
            var result
            switch( scan.token() )
            {
                case as_token: result = match(as_token); break
                case break_token: result = match(break_token); break
                case case_token: result = match(case_token); break
                case cast_token: result = match(cast_token); break
                case class_token: result = match(class_token); break
                case const_token: result = match(const_token); break
                case continue_token: result = match(continue_token); break
                case debugger_token: result = match(debugger_token); break
                case default_token: result = match(default_token); break
                case delete_token: result = match(delete_token); break
                case do_token: result = match(do_token); break
                case else_token: result = match(else_token); break
                case enum_token: result = match(enum_token); break
                case extends_token: result = match(extends_token); break
                case false_token: result = match(false_token); break
                case finally_token: result = match(finally_token); break
                case for_token: result = match(for_token); break
                case function_token: result = match(function_token); break
                case goto_token: result = match(goto_token); break
                case if_token: result = match(if_token); break
                case implements_token: result = match(implements_token); break
                case import_token: result = match(import_token); break
                case in_token: result = match(in_token); break
                case instanceof_token: result = match(instanceof_token); break
                case interface_token: result = match(interface_token); break
                case internal_token: result = match(internal_token); break
                case intrinsic_token: result = match(intrinsic_token); break
                case is_token: result = match(is_token); break
                case let_token: result = match(let_token); break
                case native_token: result = match(native_token); break
                case new_token: result = match(new_token); break
                case null_token: result = match(null_token); break
                case package_token: result = match(package_token); break
                case private_token: result = match(private_token); break
                case protected_token: result = match(protected_token); break
                case public_token: result = match(public_token); break
                case return_token: result = match(return_token); break
                case super_token: result = match(super_token); break
                case switch_token: result = match(switch_token); break
                case this_token: result = match(this_token); break
                case throw_token: result = match(throw_token); break
                case to_token: result = match(to_token); break
                case true_token: result = match(true_token); break
                case try_token: result = match(try_token); break
                case typeof_token: result = match(typeof_token); break
                case use_token: result = match(use_token); break
                case with_token: result = match(with_token); break
                case var_token: result = match(var_token); break
                case void_token: result = match(void_token); break
                case while_token: result = match(while_token); break
                case with_token: result = match(with_token); break
                case dynamic_token: result = match(dynamic_token); break
                case debugger_token: result = match(debugger_token); break
                case each_token: result = match(each_token); break
                case final_token: result = match(final_token); break
                case get_token: result = match(get_token); break
                case goto_token: result = match(goto_token); break
                case include_token: result = match(include_token); break
                case namespace_token: result = match(namespace_token); break
                case native_token: result = match(native_token); break
                case override_token: result = match(override_token); break
                case prototype_token: result = match(prototype_token); break
                case set_token: result = match(set_token); break
                case static_token: result = match(static_token); break
                case type_token: result = match(type_token); break
                case xml_token: result = match(xml_token); break
                case yield_token: result = match(yield_token); break
                default: result = false
            }
            return result
        }


        function match(kind)
        {
            var tid = scan.token()
            if( kind == scan.tokenKind(tid) )
            {
//print("match =",scan.tokenText(tid))
                //scan.printSuffix()
                scan.next()
                return tid
            }
            throw "syntax error: expecting "+scan.tokenText(kind)+", found "+ scan.tokenText(scan.tokenKind(tid))
        }
        
        function enterSlashContext(goal)
        {
            scan.slash_context.push(goal);
            //print("entering slash context "+scan.slash_context)
        }

        function exitSlashContext(goal)
        {
            if( scan.slash_context[scan.slash_context.length-1] != goal ) throw "slash context out of sync"
            scan.slash_context.pop();
            //print("exiting slash context "+scan.slash_context)
        }

        // Parse rountines

        /*

        Identifier    
            Identifier
            call
            debugger
            dynamic
            each
            final
            get
            goto
            include
            namespace
            native
            override
            prototype
            set
            static
            type
            xml

        */

        public function parseIdentifier()
        {
            Debug.enter("parseIdentifier")

            if( lookahead(call_token) ) var name = scan.tokenText(match(call_token))
            else if( lookahead(debugger_token) ) var name = scan.tokenText(match(debugger_token))
            else if( lookahead(dynamic_token) ) var name = scan.tokenText(match(dynamic_token))
            else if( lookahead(each_token) ) var name = scan.tokenText(match(each_token))
            else if( lookahead(final_token) ) var name = scan.tokenText(match(final_token))
            else if( lookahead(get_token) ) var name = scan.tokenText(match(get_token))
            else if( lookahead(goto_token) ) var name = scan.tokenText(match(goto_token))
            else if( lookahead(include_token) ) var name = scan.tokenText(match(include_token))
            else if( lookahead(namespace_token) ) var name = scan.tokenText(match(namespace_token))
            else if( lookahead(native_token) ) var name = scan.tokenText(match(native_token))
            else if( lookahead(override_token) ) var name = scan.tokenText(match(override_token))
            else if( lookahead(prototype_token) ) var name = scan.tokenText(match(prototype_token))
            else if( lookahead(set_token) ) var name = scan.tokenText(match(set_token))
            else if( lookahead(static_token) ) var name = scan.tokenText(match(static_token))
            else if( lookahead(type_token) ) var name = scan.tokenText(match(type_token))
            else if( lookahead(xml_token) ) var name = scan.tokenText(match(xml_token))
            else var name = scan.tokenText(match(identifier_token))
            
            var result = new Ast.Identifier (name)

            Debug.exit("parseIdentifier",result)
            return result
        }

        /* 
            PropertyIdentifier    
                Identifier
                *
        */

        function parsePropertyIdentifier()
        {
            Debug.enter("parsePropertyIdentifier")

            if( lookahead (mult_token) )
            {
                match (mult_token)
				var result = new Identifier ("*")
            }
            else 
            {
                var result = parseIdentifier ()
            }

            Debug.exit ("parsePropertyIdentifier",result)
            return result
        }

        /*
            Qualifier    
                ReservedNamespace
                PropertyIdentifier
        */
        
        function parseQualifier()
        {
            Debug.enter("parseQualifier")

            if( lookahead(internal_token) || lookahead(intrinsic_token) ||
                lookahead(private_token)  || lookahead(protected_token) || 
                lookahead(public_token) )
            {
                var result = parseReservedNamespace()
            }
            else
            {
                var result = parsePropertyIdentifier()
            }

            Debug.exit("parseQualifier",result)
            return result
        }

        /*
            ReservedNamespace    
                internal
                intrinsic
                private
                protected
                public
        */
        
        function parseReservedNamespace()
			: NAMESPACE
        {
            Debug.enter("parseReservedNamespace")

            if( lookahead(Internal) )
            {
                match( internal_token )
				var result = new Ast.Internal (current_package)
            }
            else 
            if( lookahead( intrinsic_token ) )
            {
                match( intrinsic_token )
                var result = <Namespace kind="intrinsic" name=""/>
            }
            else
            if( lookahead( private_token ) )
            {
                match( private_token )
                var result = <Namespace kind="private" name={current_class}/>
            }
            else 
            if( lookahead( protected_token ) )
            {
                match( protected_token )
                var result = <Namespace kind="protected" name={current_class}/>
            }
            else 
            if( lookahead( public_token ) )
            {
                match( public_token )
                if( inClassBody() )
                {
                    var public_namespace_name = ""
                }
                else
                {
                    var public_namespace_name = current_package
                }
                var result = <Namespace kind="public" name={public_namespace_name}/>
            }

            Debug.exit("parseReservedNamespace",result)
            return result
        }

        /*
            SimpleQualifiedIdentifier    
                PropertyIdentifier
                Qualifier :: PropertyIdentifier
                Qualifier :: Brackets
        */
        
        function parseSimpleQualifiedIdentifier()
        {
            Debug.enter("parseSimpleQualifiedIdentifier")

            var first = parseQualifier()
            if( first.name() == "ReservedNamespace" )
            {
                match(doublecolon_token);
                if( lookahead(leftbracket_token) )  // @ns::[expr]
                {
                    var second = parseBrackets();
                    var result = <QualifiedExpression><Qualifier>{first}</Qualifier><Expr>{second}</Expr></QualifiedExpression>
                }
                else
                {
                    var second = parsePropertyIdentifier()
                    var result = <QualifiedIdentifier><Qualifier>{first}</Qualifier>{second}</QualifiedIdentifier>
                }
            }
            else
            if( lookahead(doublecolon_token) )
            {
                match(doublecolon_token);
                
                first = <Get kind="lexical">{first}</Get>

                if( lookahead(leftbracket_token) )  // @ns::[expr]
                {
                    var second = parseBrackets();
                    var result = <QualifiedExpression><Qualifier>{first}</Qualifier><Expr>{second}</Expr></QualifiedExpression>
                }
                else
                {
                    var second = parsePropertyIdentifier()
                    var result = <QualifiedIdentifier><Qualifier>{first}</Qualifier>{second}</QualifiedIdentifier>
                }
            }
            else
            {
                var result = first;
            }

            Debug.exit("parseSimpleQualifiedIdentifier",result)
            return result            
        }

        /*
            ExpressionQualifiedIdentifier    
                ParenListExpression :: PropertyIdentifier
                ParenListExpression :: Brackets
        */
        
        function parseExpressionQualifiedIdentifier()
        {
            Debug.enter("parseExpressionQualifiedIdentifier")
 
            var first = parseParenListExpression()
            match(doublecolon_token)
            if( lookahead(leftbracket_token) )
            {
                var second = parseBrackets()
                var result = <QualifiedExpression><Qualifier>{first}</Qualifier><Expr>{second}</Expr></QualifiedExpression>
            }
            else
            {
                var second = parsePropertyIdentifier()
                var result = <QualifiedIdentifier><Qualifier>{first}</Qualifier>{second}</QualifiedIdentifier>
            }

            Debug.exit("parseExpressionQualifiedIdentifier",result)
            return result
        }

        /*
            NonAttributeQualifiedIdentifier    
                SimpleQualifiedIdentifier
                ExpressionQualifiedIdentifier
        */
        
        function parseNonAttributeQualifiedIdentifier()
        {
            Debug.enter("parseNonAttributeQualifiedIdentifier")

            if( lookahead(leftparen_token) )
            {
                var result = parseExpressionQualifiedIdentifier()
            }
            else
            {
                var result = parseSimpleQualifiedIdentifier();
            }

            Debug.exit("parseNonAttributeQualifiedIdentifier",result)
            return result
        }

        /*
            AttributeQualifiedIdentifier    
                @ Brackets
                @ NonAttributeQualifiedIdentifier
        */
        
        function parseAttributeIdentifier()
        {
            Debug.enter("parseAttributeIdentifier")

            match(at_token)
            if( lookahead(leftbracket_token) )
            {
                var result = parseBrackets()
            }
            else
            {
                var result = parseNonAttributeQualifiedIdentifier()
            }
            result.@is_attr="true"

            Debug.exit("parseAttributeIdentifier",result)
            return result
        }

        /*
            QualifiedIdentifier    
                PackageIdentifier . Identifier
                NonAttributeQualifiedIdentifier
        */

        function parseQualifiedIdentifier()
        {
            Debug.enter("parseQualifiedIdentifier")

            var is_attr
            if( lookahead(at_token) )
            {
                var result = parseAttributeIdentifier()
            }
            else
            if( lookahead(packageidentifier_token) )
            {
                var pkg_name = scan.tokenText(match(packageidentifier_token))
                var first  = <PackageIdentifier name={pkg_name}/>
                match(dot_token)
                var second = parseIdentifier()
                var def_name = second.@name
                if( !isImported(pkg_name,def_name) )
                {
                    throw "package qualified reference to unimported name "+pkg_name+"."+def_name
                }
                var result   = <QualifiedIdentifier><Qualifier>{first}</Qualifier>{second}</QualifiedIdentifier>
            }
            else
            {
                var result = parseNonAttributeQualifiedIdentifier()
            }

            Debug.exit("parseQualifiedIdentifier",result)
            return result
        }

        /*

        SimpleTypeIdentifier    
            PackageIdentifier  .  Identifier
            NonAttributeQualifiedIdentifier

        ParameterisedTypeIdentifier    
            SimpleTypeIdentifier
            SimpleTypeIdentifier  .<  TypeExpressionList  >
 
        TypeIdentifier
            ParameterisedTypeIdentifier
            ParameterisedTypeIdentifier  !
            ParameterisedTypeIdentifier  ?
            
        TypeIdentifer may occur wherever a PrimaryExpression may occur. This includes as a base of
        a property reference, in a new expression, in a call expression. TypeIdentifiers shall not
        be used as the identifier of an object reference (e.g. o.T)
 
        */
        
        function parseSimpleTypeIdentifier()
        {
            Debug.enter("parseSimpleTypeIdentifier")

            if( lookahead(packageidentifier_token) )
            {
                var pkg_name = scan.tokenText(match(packageidentifier_token))
                var first  = <PackageIdentifier name={pkg_name}/>
                match(dot_token)
                var second = parseIdentifier()
                var def_name = second.@name

                if( !isImported(pkg_name,def_name) )
                {
                    throw "package qualified reference to unimported name "+pkg_name+"."+def_name
                }
                var result = <QualifiedIdentifier><Qualifier>{first}</Qualifier>{second}</QualifiedIdentifier>
            }
            else
            {
                var result = parseNonAttributeQualifiedIdentifier()
            }

            Debug.exit("parseSimpleTypeIdentifier",result)
            return result
        }

        private var rightanglesseen : int = 0
        private var rightanglesneeded : int = 0

        function parseTypeIdentifier()
        {
            Debug.enter("parseTypeIdentifier")    

            var first = parseSimpleTypeIdentifier()
            if( lookahead(leftdotangle_token) )
            {
                match(leftdotangle_token)
                rightanglesneeded++

                var second = parseTypeExpressionList()
                
                if( lookahead(unsignedrightshift_token) )
                {
                    match(unsignedrightshift_token)
                    rightanglesseen += 2
                }
                else 
                if( lookahead(rightshift_token) )
                {
                    match(rightshift_token)
                    rightanglesseen += 1
                }
                else 
                if( lookahead(greaterthan_token) )
                {
                    match(greaterthan_token)
                }
                else 
                if( rightanglesseen <= rightanglesneeded )
                {
                    rightanglesseen--
                }

                rightanglesneeded--

                if( rightanglesneeded < 0 )
                {
                    throw "too few right angle braces in type identifier"
                }
                else
                if( rightanglesseen > rightanglesneeded || 
                    (rightanglesneeded == 0 && (lookahead(greaterthan_token) || lookahead(rightshift_token) || lookahead(unsignedrightshift_token))) )
                {
                    throw "too many right angle braces in type identifier"
                }

                var result = <TypeIdentifier>{first}<TypeArguments>{second}</TypeArguments></TypeIdentifier>
            }
            else
            {
                var result = first
            }

            Debug.exit("parseTypeIdentifier",result)
            return result
        }

        function parseXMLLiteral()
        {
            throw "XMLLiteral not implemented"
        }

        function parseXMLElement()
        {
        }
        
        function parseXMLName(first)
        {
        }
                
        function parseXMLAttributes(first)
        {
        }
                
        function parseXMLAttribute(first)
        {
        }
                
        function parseXMLElementContent(first)
        {
        }
                
        function parseParenExpression()
        {
            Debug.enter("parseParenExpression")

            enterSlashContext(regexpliteral_token)
            match(leftparen_token);
            var result = parseAssignmentExpression(allowIn_mode)
            exitSlashContext(regexpliteral_token)
            match(rightparen_token)
 
            Debug.exit("parseParenExpression",result)
            return result
        }

        function parseParenListExpression()
        {
            Debug.enter("parseParenListExpression")

            enterSlashContext(regexpliteral_token)
            match( leftparen_token ); 
            var result = <ParenList>{parseListExpression(allowIn_mode)}</ParenList>
            exitSlashContext(regexpliteral_token)
            match( rightparen_token )

            Debug.exit("parseParenListExpression",result)
            return result
        }

        /*
            ParenListOrExpressionQualifiedIdentifier    
                ParenListExpression
                ParenListExpression :: PropertyIdentifier
                ParenListExpression :: Brackets
        */
        
        function parseParenListOrExpressionQualifiedIdentifier()
        {
            Debug.enter("parseParenListOrExpressionQualifiedIdentifier")

            var first = parseParenListExpression()
            if( lookahead(doublecolon_token) )
            {
                match(doublecolon_token)
                if( lookahead(leftbracket_token) )
                {
                    var second = parseBrackets()
                    var result = <QualifiedExpression><Qualifier>{first}</Qualified><Expr>{second}</Expr></QualifiedExpression>
                }
                else
                {
                    var second = parsePropertyIdentifier()
                    var result = <QualifiedIdentifier><Qualified>{first}</Qualified>{second}</QualifiedIdentifier>
                }
            }
            else
            {
                var result = first
            }

            Debug.exit("parseParenListOrExpressionQualifiedIdentifier",result)
            return result
        }


        /*
        
        FunctionCommon    
            FunctionSignature
            FunctionSignature Block
        
        */

        function parseFunctionCommon(first)
        {
            Debug.enter("parseFunctionCommon",first)
            
            var prologue = <Prologue/>
            var second = parseFunctionSignature(prologue)

            if( !inInterfaceBody() )
            {
                slot_context_stack.push("function")            
                var third = parseBlockStatement();
                slot_context_stack.pop()
                prologue.* += third.Prologue.*          
                var block = third.Block
            }
            else
            {
                var block = <></>
            }

            var node = <Function>{first}{second}{prologue}{block}</Function>

            if( !inClassBody() )
            {
                node.@factory = "true"
            }
            
            Debug.exit("parseFunctionCommon",node)
            return node
        }

        /*
    
        FunctionSignature    
            TypeParameters  (  Parameters  )  Result
    
        */

        function parseFunctionSignature(prologue)
        {
            Debug.enter("parseFunctionSignature")

            var first = parseTypeParameters()
            match(leftparen_token) 
            var second = parseParameters(prologue)
            match(rightparen_token)
            var third = parseResultType()
            var result = <Signature>{first}{second}{third}</Signature>

            Debug.exit("parseFunctionSignature",result)
            return result
        }

        /*

        TypeParameters    
            «empty»
            .<  TypeParameterList  >  

        */

        function parseTypeParameters()
        {
            if( lookahead(leftdotangle_token) )
            {
                match(leftdotangle_token)
                var first = parseTypeParameterList()
                var result = <TypeParameters>{first}</TypeParameters>
                match(greaterthan_token)
            }
            else
            {
                var result = <TypeParameters/>
            }

            return result
        }

        /*

        TypeParameterList
            Identifier
            Identifier  ,  TypeParameterList

        */

        function parseTypeParameterList()
        {
            Debug.enter("parseTypeParameterList")
            
            var list = <></>
            list += parseIdentifier()
            while( lookahead(comma_token) )
            {
                match(comma_token)
                list += parseIdentifier()
            }
            var result = list

            Debug.exit("parseTypeParameterList",result)
            return result
        }
        
        /*

        */

        function parseParameters(prologue)
        {
            Debug.enter("parseParameters")

            if( lookahead(rightparen_token) )
            {
                var result = <Parameters/>
            }
            else
            {
                var result = parseNonemptyParameters(<></>,prologue)
            }

            Debug.exit("parseParameters",result)
            return result
        }

        /*

        NonemptyParameters    
            ParameterInit
            ParameterInit  ,  NonemptyParameters
            RestParameters
        
        */

		function isLet(node)
		{
			if( node.localName() == "LetExpression" ||
				node.localName() == "YieldExpression" && node.*.length() > 0 ||
				node.localName() == "ConditionalExpression" && isLet(node.*[2]) )
			{
				return true
			}
			return false
		}

        function parseNonemptyParameters(first,prologue)
        {
            Debug.enter("parseNonemptyParameters",first)

            if( lookahead(tripledot_token) )
            {
                first += parseRestParameter()
                var result = first
            }
            else 
            {
                first += parseParameterInit(prologue)
                if( lookahead(comma_token) )
                {
					if( isLet(first) )
					{
						throw "ambiguous syntax, use parens to associate"
					}
                    match(comma_token)
                    var result = parseNonemptyParameters(first,prologue)
                }
                else
                {
                    var result = first
                }
            }

            Debug.exit("parseNonemptyParameters",result)
            return result
        }

        /*

        ParameterInit    
            Parameter
            Parameter  =  NonAssignmentExpressionallowIn

        */

        function parseParameterInit(prologue)
        {
            Debug.enter("parseParameterInit")

            if( lookahead(const_token) )
            {
                var kind = match(const_token);
            }
            else
            {
                var kind = var_token;
            }

            var typedid = parseTypedIdentifier(allowIn_mode)

            var result = <Parameter kind={scan.tokenText(kind)}>{typedid}</Parameter>

            if( lookahead(assign_token) )
            {
                match(assign_token);
                var init = parseNonAssignmentExpression(allowIn_mode);
                result.Init = init
            }

            var temp = makeBinding(<Attributes><parameter/></Attributes>,var_token,typedid,init,prologue)

            Debug.exit("parseParameterInit",result)
            return result
        }

        /*

        */

        function parseParameter()
        {
            Debug.enter("parseParameter")

            Debug.exit("parseParameter",result)
            return result
        }

        /*

        RestParameter    
            ...
            ...  ParameterAttributes Identifier

        */

        function parseRestParameter()
        {
            Debug.enter("parseRestParameter")

            match(tripledot_token)
            if( lookahead(const_token) )
            {
                var first = match(const_token);
            }
            else
            {
                var first = var_token;
            }

            var second = parseIdentifier()
            var result = <RestParameter kind={scan.tokenText(first)}>{second}</RestParameter>

            Debug.exit("parseRestParameter",result)
            return result
        }

        /*

        ResultType    
            «empty»
            :  void
            :  TypeExpression

        */

        function parseResultType()
        {
            Debug.enter("parseResultType")

            if( lookahead(colon_token) )
            {
                match(colon_token)
                if( lookahead(void_token) )
                {
                    match(void_token)
                    var first = <Void/>
                }
                else
                {
                    var first = parseTypeExpression()
                }
                var result = <ResultType>{first}</ResultType>
            }
            else
            {
                var result = <ResultType/>
            }

            Debug.exit("parseResultType",result)
            return result
        }

        /*

        */

        function parseObjectLiteral()
        {
            Debug.enter("parseObjectLiteral")

            enterSlashContext(regexpliteral_token)
            match(leftbrace_token)
            if( lookahead(rightbrace_token) )
            {
                var first = null
            }
            else
            {
                var first = parseFieldListPrime(<>{parseLiteralField()}</>)
            }
            exitSlashContext(regexpliteral_token)
            match(rightbrace_token)
            var result = <LiteralObject>{first}</LiteralObject>

            Debug.exit("parseObjectLiteral",result)
            return result
        }

        /*

        */

        function parseFieldListPrime(first)
        {
            Debug.enter("parseFieldListPrime",first)

            if( lookahead(comma_token) )
            {
				if( isLet(first) )
				{
					throw "ambiguous syntax, use parens to clarify list association"
				}
                match(comma_token)
                var second = parseLiteralField()
                var result = parseFieldListPrime(<>{first}{second}</>)
            }
            else
            {
                var result = first
            }

            Debug.exit("parseFieldListPrime",result)
            return result
        }

        /*

        LiteralField    
            FieldName  :  AssignmentExpressionallowIn

        */

        function parseLiteralField()
        {
            Debug.enter("parseLiteralField")

            var first = parseFieldName()
            match(colon_token)
            var second = parseAssignmentExpression(allowIn_mode)
            var result = <LiteralField>{first}{second}</LiteralField>

            Debug.exit("parseLiteralField",result)
            return result
        }

        /*

        FieldName    
            NonAttributeQualifiedIdentifier
            String
            Number
            ParenExpression
            ReservedIdentifier
            ContextuallyReservedIdentifier

        */

        function parseFieldName()
        {
            Debug.enter("parseFieldName")
        
            if( lookahead(stringliteral_token) )
            {
                result = <LiteralString value={scan.tokenText(match(stringliteral_token))}/>
            }
            else if( lookahead(numberliteral_token) )
            {
                result = <LiteralNumber value={scan.tokenText(match(numberliteral_token))}/>
            }
            else if( lookahead(leftparen_token) )
            {
                var result = parseParenExpression();
            }
            else
            if( lookahead( lookaheadReservedWord) )
            {
                var result = <Identifier>{scan.tokenText(matchReservedWord())}</Identifier>
            }
            else
            {
                var result = parseNonAttributeQualifiedIdentifier();
            }

            Debug.exit("parseFieldName",result)
            return result
        }

        /*

        ArrayLiteral    
            [  ElementList  ]
    
        ElementList    
            «empty»
            LiteralElement
            ,  ElementList
            LiteralElement  ,  ElementList
    
        LiteralElement    
            AssignmentExpressionallowIn

        */

        function parseArrayLiteral()
        {
            Debug.enter("parseArrayLiteral")

            enterSlashContext(regexpliteral_token)
            match(leftbracket_token)
            if( lookahead(rightbracket_token) )
            {
                var first = <></>
            }
            else
            {
                var temp = parseLiteralElement()
                var first = parseElementListPrime(<>{temp}</>)
            }
            exitSlashContext(regexpliteral_token)
            match(rightbracket_token)
            var result = <LiteralArray>{first}</LiteralArray>

            Debug.exit("parseArrayLiteral",result)
            return result
        }

        function parseElementListPrime(first)        
        {
            Debug.enter("parseElementListPrime",first)

            while( lookahead(comma_token) )
            {
				if( isLet(first) )
				{
					throw "ambiguous syntax, use parens to clarify list association"
				}
                match(comma_token)
                var second = parseLiteralElement()
                if( second == null )
                {
                    // do nothing
                }
                else
                {
                    var first = <>{first}{second}</>
                }
            }
            var result = first

            Debug.exit("parseElementListPrime",result)
            return result
        }

        function parseLiteralElement()
        {
            Debug.enter("parseLiteralElement")

            if( lookahead(comma_token) )
            {
                var result = <EmptyElement/>
            }
            else
            if( lookahead(rightbracket_token) )
            {
                var result = null
            }
            else
            {
                var result = parseAssignmentExpression(allowIn_mode)
            }

            Debug.exit("parseLiteralElement",result)
            return result
        }

        function parseCastExpression()
        {
            Debug.enter("parseCastExpression")

            if( lookahead(cast_token) )
            {
                match(cast_token)
                var first = parseTypeExpression()
                var second = parseParenListExpression()
                var result = <Cast>{first}<Expr>{second}</Expr></Cast>
            }
            else
            if( lookahead(to_token) )
            {
                match(to_token)
                var first = parseTypeExpression()
                var second = parseParenListExpression()
                var result = <to>{first}<Expr>{second}</Expr></to>
            }

            Debug.exit("parseCastExpression",result)
            return result
        }

        /*

        PrimaryExpression    
            null
            true
            false
            Number
            String
            this
            RegularExpression
            TypeIdentifier
            AttributeQualifiedIdentifier
            XMLInitialiser
            ParenListExpression
            ArrayLiteral
            ObjectLiteral
            FunctionExpression
            cast  TypeExpression  ParenListExpression
        
        */

        function parsePrimaryExpression()
        {
            Debug.enter("parsePrimaryExpression")
            
            if( lookahead(null_token) )
            {
                match(null_token)
                var result = <LiteralNull/>
            }
            else 
            if( lookahead(true_token) )
            {
                match(true_token)
                var result = <LiteralBoolean value="true"/>
            }
            else 
            if( lookahead(false_token) )
            {
                match(false_token)
                var result = <LiteralBoolean value="false"/>
            }
            else 
            if( lookahead(numberliteral_token) )
            {
                var result = <LiteralNumber value={scan.tokenText(match(numberliteral_token))}/>
            }
            else
            if( lookahead(stringliteral_token) )
            {
                var result = <LiteralString value={scan.tokenText(match(stringliteral_token))}/>
            }
            else 
            if( lookahead(this_token) )
            {
                match(this_token)
                var result = <This/>
            }
            else
            if( lookahead(regexpliteral_token) )
            {
                var result = <LiteralRegExp value={scan.tokenText(match(regexpliteral_token))}/>
            }
            else 
            if( lookahead(function_token) )
            {
                match(function_token);
                var first = null
                if( lookahead(identifier_token) )
                {
                    first = parseIdentifier();
                }
                var result = parseFunctionCommon(first);
            }
            else
            if( lookahead(leftparen_token) )
            {
                var result = parseParenListOrExpressionQualifiedIdentifier()
            }
            else
            if( lookahead(leftbracket_token) )
            {
                var result = parseArrayLiteral()
            }
            else
            if( lookahead(leftbrace_token) )
            {
                var result = parseObjectLiteral()
            }
            else
            if( lookahead(cast_token) || lookahead(to_token) )
            {
                var result = parseCastExpression()
            }
            else
            if( lookahead(at_token) )
            {
                var temp = parseAttributeIdentifier()
                var result = <Get kind="lexical">{temp}</Get>
            }
            else
            {
                var temp = parseTypeIdentifier()
                var result = <Get kind="lexical">{temp}</Get>
            }
            
            Debug.exit("parsePrimaryExpression",result)
            return result
        }

        /*

        SuperExpression    
            super
            super  Arguments

        */

        function parseSuperExpression()
        {
            Debug.enter("parseSuperExpression")

            match(super_token)
            var first = <SuperExpression/>
            if( lookahead(leftparen_token) )
            {
                var result = parseArguments(first)
            }
            else
            {
                var result = first
            }

            Debug.exit("parseSuperExpression",result)
            return result
        }

        /*

        MemberExpression    
            PrimaryExpression
            new  MemberExpression  Arguments
            SuperExpression  PropertyOperator
            MemberExpression  PropertyOperator

        Refactored:

        MemberExpression :    
            PrimaryExpression MemberExpressionPrime
            new MemberExpression Arguments MemberExpressionPrime
            SuperExpression  PropertyOperator  MemberExpressionPrime
    
        MemberExpressionPrime :    
            [ Expression ] MemberExpressionPrime
            . Identifier MemberExpressionPrime
            «empty»

        */

        function parseMemberExpression()
        {
            Debug.enter("parseMemberExpression")
            
            if( lookahead(new_token) )
            {   
                match(new_token)
                var first = parseMemberExpression()
                if( lookahead(leftparen_token) )
                {
                    var first = parseArguments(first)
                    if( first.name() == "Call" )
                    {
                        first.@is_new = true
                    }
                    else
                    {
                        first.Call.@is_new = true
                    }
                    var is_shortnew = false
                }
                else
                {
                    if( first.name() == "Get" )
                    {
                        first.setLocalName("Call")
                        first.args = <Args/>
                        first.@is_new = true
                    }
                    else
                    {
                        first = <Call is_new="true">{first}<Args/></Call>
                    }
                    var is_shortnew = true
                }
            }
            else
            if( lookahead(super_token) )
            {
                var first = parseSuperExpression()
                var first = parsePropertyOperator(first)
            }
            else
            {
                var first = parsePrimaryExpression()
            }

            if( !is_shortnew )
            while( lookahead(leftbracket_token) || 
                   lookahead(dot_token) || 
                   lookahead(doubledot_token) )
            {
                var first = parsePropertyOperator(first)
            }

            var result = first
            
            Debug.exit("parseMemberExpression",result)
            return result
        }

        /*

        NewExpression    
            MemberExpression
            new  NewExpression

        */

        function parseNewExpression()
        {
            Debug.enter("parseNewExpression")
            
            var result = parseMemberExpression()

            Debug.exit("parseNewExpression",result)
            return result
        }

        /*

        CallExpressionPrime :    
            Arguments CallExpressionPrime
            [ Expression ] CallExpressionPrime
            . Identifier CallExpressionPrime
            «empty»

        */

        function parseCallExpressionPrime(first)
        {
            Debug.enter("parseCallExpressionPrime",first)

            if( lookahead(leftparen_token) )
            {
                var second = parseArguments(first)
                var result = parseCallExpressionPrime(second)
            }
            else
            if( lookahead(leftbracket_token) || 
                lookahead(dot_token) || 
                lookahead(doubledot_token) )
            {
                var second = parsePropertyOperator(first)
                var result = parseCallExpressionPrime(second)
            }
            else
            {
                var result = first
            }

            Debug.exit("parseCallExpressionPrime",result)
            return result
        }

        /*

        PropertyOperator    
            .  QualifiedIdentifier
            ..  QualifiedIdentifier
            .  ParenListExpression
            .  ParenListExpression  ::  PropertyIdentifier
            .  ParenListExpression  ::  Brackets
            Brackets

        */

        function parsePropertyOperator(first)
        {
            Debug.enter("parsePropertyOperator",first)

            if( lookahead(dot_token) )
            {
                match(dot_token)
                if( lookahead(leftparen_token) )
                {
                    var second = parseParenListExpression()
                    if( lookahead(doublecolon_token) )
                    {
                        match(doublecolon_token)
                        if( lookahead(leftbracket_token) )
                        {
                            var third = parseBrackets()
                            var result = <Get kind="bracket">{first}<QualifiedExpression><Qualifier>{second}</Qualifier>{third}</QualifiedExpression></Get>
                        }
                        else
                        {
                            var third = parsePropertyIdentifier()
                            var result = <Get kind="dot">{first}<QualifiedIdentifier><Qualifier>{second}</Qualifier>{third}</QualifiedIdentifier></Get>
                        }
                    }
                    else
                    {
                        var result = <FilterExpression>{first}{second}</FilterExpression>
                    }
                }
                else
                if( lookaheadReservedWord() )
                {
                    var second = <Identifier>{scan.tokenText(matchReservedWord())}</Identifier>
                    var result = <Get kind="dot">{first}{second}</Get>
                }
                else
                {
                    var second = parseQualifiedIdentifier()
                    var result = <Get kind="dot">{first}{second}</Get>
                }
            }
            else
            if( lookahead(doubledot_token) )
            {
                match(doubledot_token)
                var second = parseQualifiedIdentifier()
                var result = <DescendExpression>{first}{second}</DescendExpression>
            }
            else
            if( lookahead(leftbracket_token) )
            {
                var second = parseBrackets()
                var result = <Get kind="bracket">{first}{second}</Get>
            }

            Debug.exit("parsePropertyOperator",result)
            return result
        }

        /*

        Brackets    
            [  ]
            [  ListExpressionallowIn  ]
            [  ListExpressionallowIn  :  ]
            [  ListExpressionallowIn  :  ListExpressionallowIn  ]
            [  :  ListExpressionallowIn  ]

        */

        function parseBrackets()
        {
            Debug.enter("parseBrackets")
            
            match(leftbracket_token)
            if( lookahead(rightbracket_token) )
            {
                var first = null
                var second = null
            }
            else
            if( lookahead(colon_token) )
            {
                match(colon_token)
                var first = null
                var second = parseListExpression(allowIn_mode)
            }
            else
            {
                var first = parseListExpression(allowIn_mode)
                if( lookahead(colon_token) )
                {
                    match(colon_token)
                    if( lookahead(rightbracket_token) )
                    {
                        var second = null
                    }
                    else
                    {
                        var second = parseListExpression(allowIn_mode)
                    }
                }
                else
                {
                }
            }
            match(rightbracket_token);
            var result = <Brackets>{first}{second}</Brackets>

            Debug.exit("parseBrackets",result)
            return result
        }

        /*

        Arguments    
            (  )
            (  ListExpressionALLOWIN  )

        */

        function parseArguments(first)
        {        
            Debug.enter("parseArguments",first)
            
            enterSlashContext(regexpliteral_token)
            match(leftparen_token);

            if( lookahead(rightparen_token) )
            {
                var second = <></>
            }
            else
            {
                var second = parseArgumentList()
            }

            exitSlashContext(regexpliteral_token)
            match(rightparen_token);
            if( first.name() == "Get" )
            {
                first.setLocalName("Call")
                first.args = <Args>{second}</Args>
            }
            else
            {
                first = <Call>{first}<Args>{second}</Args></Call>
            }
            var result = first

            Debug.exit("parseArguments",result)
            return result
        }

        /*

        ArgumentList

        */

        function parseArgumentList()
        {
            Debug.enter("parseArgumentList")
            
            var list = <></>
			var first = parseAssignmentExpression(allowIn_mode)
            list += <To>{first}<SlotTypeRef/></To>
            while( lookahead( comma_token ) )
            {
				if( isLet(first) )
				{
					throw "ambiguous syntax, use parens to clarify list association"
				}
                match( comma_token );
                list += <To>{parseAssignmentExpression(allowIn_mode)}<SlotTypeRef/></To>
            }
            var node = list

            Debug.exit("parseArgumentList",node)
            return node
        }



        /*

        LeftHandSideExpression    
            NewExpression
            CallExpression

        Refactored:

        LeftHandSideExpression    
            MemberExpression LeftHandSideExpressionPrime
            new NewExpression
    
        LeftHandSideExpressionPrime    
            Arguments CallExpressionPrime
            «empty»

        */

        function parseLeftHandSideExpression()
        {
            Debug.enter("parseLeftHandSideExpression")
            
            if( lookahead(new_token) )
            {   
                var first = parseNewExpression()
            }
            else
            {
                var first = parseMemberExpression()
            }

            if( lookahead(leftparen_token) )
            {   
                var first = parseArguments(first)
                var result = parseCallExpressionPrime(first)
            }
            else
            {
                var result = first
            }

            Debug.exit("parseLeftHandSideExpression",result)
            return result
        }

        /*

        PostfixExpression    
            LeftHandSideExpression
            LeftHandSideExpression  [no line break]  ++
            LeftHandSideExpression  [no line break]  --

        */

        function parsePostfixExpression()
        {
            Debug.enter("parsePostfixExpression")

            var first = parseLeftHandSideExpression()
            if( lookahead(plusplus_token) )
            {
                match(plusplus_token)
                var result = <Postfix kind="increment">{first}</Postfix>
            }
            else
            if( lookahead(minusminus_token) )
            {
                match(minusminus_token)
                var result = <Postfix kind="decrement">{first}</Postfix>
            }
            else
            {
                var result = first
            }            

            Debug.exit("parsePostfixExpression",result)
            return result
        }

        /*

        UnaryExpression    
            PostfixExpression
            delete  PostfixExpression
            void  UnaryExpression
            typeof  UnaryExpression
            ++   PostfixExpression
            --  PostfixExpression
            +  UnaryExpression
            -  UnaryExpression
            ~  UnaryExpression
            !  UnaryExpression

        */

        function parseUnaryExpression() : Ast.EXPR
        {
            Debug.enter("parseUnaryExpression")

            if( lookahead(delete_token) )
            {
                enterSlashContext(regexpliteral_token)
                match(delete_token)

                var first = parsePostfixExpression()
                if( first.name() == "Get" )
                {
                    first.setLocalName("Delete")
                }
                else
                {
                    first = <Delete>{first}</Delete>
                }

                var result = first

                exitSlashContext(regexpliteral_token)
            }
            else
            if( lookahead(void_token) )
            {
                enterSlashContext(regexpliteral_token)
                match(void_token)
                var first = parsePostfixExpression()
                exitSlashContext(regexpliteral_token)
                var result = <Unary kind={scan.tokenText(void_token)}>{first}</Unary>
            }
            else
            if( lookahead(typeof_token) )
            {
                enterSlashContext(regexpliteral_token)
                match(typeof_token)
                var first = parsePostfixExpression()
                exitSlashContext(regexpliteral_token)
                var result = <Unary kind={scan.tokenText(typeof_token)}>{first}</Unary>
            }
            else
            if( lookahead(plusplus_token) )
            {
                enterSlashContext(regexpliteral_token)
                match(plusplus_token)
                var first = parsePostfixExpression()
                exitSlashContext(regexpliteral_token)
                var result = <Unary kind={scan.tokenText(plusplus_token)}>{first}</Unary>
            }
            else
            if( lookahead(minusminus_token) )
            {
                enterSlashContext(regexpliteral_token)
                match(minusminus_token)
                var first = parsePostfixExpression()
                exitSlashContext(regexpliteral_token)
                var result = <Unary kind={scan.tokenText(minusminus_token)}>{first}</Unary>
            }
            else
            if( lookahead(plus_token) )
            {
                enterSlashContext(regexpliteral_token)
                match(plus_token)
                var first = parsePostfixExpression()
                exitSlashContext(regexpliteral_token)
                var result = <Unary kind={scan.tokenText(plus_token)}>{first}</Unary>
            }
            else
            if( lookahead(minus_token) )
            {
                enterSlashContext(regexpliteral_token)
                match(minus_token)
                var first = parsePostfixExpression()
                exitSlashContext(regexpliteral_token)
                var result = <Unary kind={scan.tokenText(minus_token)}>{first}</Unary>
            }
            else
            if( lookahead(bitwisenot_token) )
            {
                enterSlashContext(regexpliteral_token)
                match(bitwisenot_token)
                var first = parsePostfixExpression()
                exitSlashContext(regexpliteral_token)
                var result = <Unary kind={scan.tokenText(bitwisenot_token)}>{first}</Unary>
            }
            else
            if( lookahead(not_token) )
            {
                enterSlashContext(regexpliteral_token)
                match(not_token)
                var first = parsePostfixExpression()
                exitSlashContext(regexpliteral_token)
                var result = <Unary kind={scan.tokenText(not_token)}>{first}</Unary>
            }
            else
            {
                var result = parsePostfixExpression()
            }

            Debug.exit("parseUnaryExpression",result)
            return result
        }

        /*

        YieldExpression    
            UnaryExpression
            yield  UnaryExpression

        */

        function parseYieldExpression()
        {
            Debug.enter("parseYieldExpression")

            if( lookahead(yield_token) )
            {
                enterSlashContext(regexpliteral_token)

	            match(yield_token)
    	        if( !(lookaheadSemicolon(full_mode) || lookahead(rightparen_token) || lookahead(rightbrace_token) || lookahead(comma_token)) )
        	    {
	            	var first = parseUnaryExpression()
	    	        var result = <YieldExpression>{first}</YieldExpression>
    	        }
				else
				{
					var result = <YieldExpression/>
				}

                exitSlashContext(regexpliteral_token)
            }
            else
            {
                var result = parseUnaryExpression()
            }

            Debug.exit("parseYieldExpression",result)
            return result
        }

        /*

        MultiplicativeExpression    
            UnaryExpression
            MultiplicativeExpression  *  UnaryExpression
            MultiplicativeExpression  /  UnaryExpression
            MultiplicativeExpression  %  UnaryExpression

        */

        function parseMultiplicativeExpression()
        {
            Debug.enter("parseMultiplicativeExpression")

            enterSlashContext(div_token)
            var first = parseYieldExpression()
            exitSlashContext(div_token)
            while( true )
            {
                if( lookahead(mult_token) )
                {
                    match(mult_token)
                    enterSlashContext(div_token)
                    var second = parseYieldExpression()
                    exitSlashContext(div_token)
                    var first = <Binary kind="multiply">{first}{second}</Binary>
                }
                else
                if( lookahead(div_token) )
                {
                    match(div_token)
                    enterSlashContext(div_token)
                    var second = parseYieldExpression()
                    exitSlashContext(div_token)
                    var first = <Binary kind="divide">{first}{second}</Binary>
                }
                else
                if( lookahead(modulus_token) )
                {
                    match(modulus_token)
                    enterSlashContext(div_token)
                    var second = parseYieldExpression()
                    exitSlashContext(div_token)
                    var first = <Binary kind="modulus">{first}{second}</Binary>
                }
                else
                {
                    break  // okay, none found
                }
            }                
            var result = first

            Debug.exit("parseMultiplicativeExpression",result)
            return result
        }

        /*

        AdditiveExpression    
            MultiplicativeExpression
            AdditiveExpression + MultiplicativeExpression
            AdditiveExpression - MultiplicativeExpression

        */

        function parseAdditiveExpression()
        {
            Debug.enter("parseAdditiveExpression")

            var first = parseMultiplicativeExpression()
            while( true )
            {
                if( lookahead(plus_token) )
                {
                    match(plus_token)
                    var second = parseMultiplicativeExpression()
                    var first = <Binary kind="plus">{first}{second}</Binary>
                }
                else
                if( lookahead(minus_token) )
                {
                    match(minus_token)
                    var second = parseMultiplicativeExpression()
                    var first = <Binary kind="minus">{first}{second}</Binary>
                }
                else
                {
                    break  // okay, none found
                }
            }                

            var result = first

            Debug.exit("parseAdditiveExpression",result)
            return result
        }

        /*

        ShiftExpression    
            AdditiveExpression
            ShiftExpression << AdditiveExpression
            ShiftExpression >> AdditiveExpression
            ShiftExpression >>> AdditiveExpression


        */

        function parseShiftExpression()
        {
            Debug.enter("parseShiftExpression")

            var first = parseAdditiveExpression()
            while( true )
            {
                if( lookahead(leftshift_token) )
                {
                    match(leftshift_token)
                    var second = parseAdditiveExpression()
                    var first = <Binary kind="leftshift">{first}{second}</Binary>
                }
                else
                if( lookahead(rightshift_token) )
                {
                    match(rightshift_token)
                    var second = parseAdditiveExpression()
                    var first = <Binary kind="rightshift">{first}{second}</Binary>
                }
                else
                if( lookahead(unsignedrightshift_token) )
                {
                    match(unsignedrightshift_token)
                    var second = parseAdditiveExpression()
                    var first = <Binary kind="unsignedrightshift">{first}{second}</Binary>
                }
                else
                {
                    break  // okay, none found
                }
            }                
            var result = first

            Debug.exit("parseShiftExpression",result)
            return result
        }

        /*

        RelationalExpressionallowIn    
            ShiftExpression
            RelationalExpressionallowIn < ShiftExpression
            RelationalExpressionallowIn > ShiftExpression
            RelationalExpressionallowIn <= ShiftExpression
            RelationalExpressionallowIn >= ShiftExpression
            RelationalExpressionallowIn in ShiftExpression
            RelationalExpressionallowIn instanceof ShiftExpression
            RelationalExpressionallowIn is ShiftExpression
            RelationalExpressionallowIn to ShiftExpression
            RelationalExpressionallowIn cast ShiftExpression

        */

        function parseRelationalExpression(mode)
        {
            Debug.enter("parseRelationalExpression")

            var first = parseShiftExpression()
            while( true )
            {
                if( lookahead(lessthan_token) )
                {
                    match(lessthan_token)
                    var second = parseShiftExpression()
                    var first = <Binary kind="lessthan">{first}{second}</Binary>
                }
                else
                if( lookahead(greaterthan_token) )
                {
                    match(greaterthan_token)
                    var second = parseShiftExpression()
                    var first = <Binary kind="greaterthan">{first}{second}</Binary>
                }
                else
                if( lookahead(lessthanorequals_token) )
                {
                    match(lessthanorequals_token)
                    var second = parseShiftExpression()
                    var first = <Binary kind="lessthanorequals">{first}{second}</Binary>
                }
                else
                if( lookahead(greaterthanorequals_token) )
                {
                    match(greaterthanorequals_token)
                    var second = parseShiftExpression()
                    var first = <Binary kind="greaterthanorequals">{first}{second}</Binary>
                }
                else
                if( mode === allowIn_mode && lookahead(in_token) )
                {
                    match(in_token)
                    var second = parseShiftExpression()
                    var first = <Binary kind="in">{first}{second}</Binary>
                }
                else
                if( lookahead(instanceof_token) )
                {
                    match(instanceof_token)
                    var second = parseShiftExpression()
                    var first = <Binary kind="instanceof">{first}{second}</Binary>
                }
                else
                if( lookahead(is_token) )
                {
                    match(is_token)
                    var second = parseShiftExpression()
                    var first = <Binary kind="is">{first}{second}</Binary>
                }
                else
                if( lookahead(to_token) )
                {
                    match(to_token)
                    var second = parseShiftExpression()
                    var first = <Binary kind="to">{first}{second}</Binary>
                }
                else
                if( lookahead(cast_token) )
                {
                    match(cast_token)
                    var second = parseShiftExpression()
                    var first = <Binary kind="cast">{first}{second}</Binary>
                }
                else
                {
                    break  // okay, none found
                }
            }                
            var result = first

            Debug.exit("parseRelationalExpression",result)
            return result
        }

        /*

        EqualityExpressionb    
            RelationalExpressionb
            EqualityExpressionb == RelationalExpressionb
            EqualityExpressionb != RelationalExpressionb
            EqualityExpressionb === RelationalExpressionb
            EqualityExpressionb !== RelationalExpressionb


        */

        function parseEqualityExpression(mode)
        {
            Debug.enter("parseEqualityExpression")

            var first = parseRelationalExpression(mode)
            while( true )
            {
                if( lookahead(equals_token) )
                {
                    match(equals_token)
                    var second = parseRelationalExpression(mode)
                    var first = <Binary kind="equals">{first}{second}</Binary>
                }
                else
                if( lookahead(notequals_token) )
                {
                    match(notequals_token)
                    var second = parseRelationalExpression(mode)
                    var first = <Binary kind="notequals">{first}{second}</Binary>
                }
                else
                if( lookahead(strictequals_token) )
                {
                    match(strictequals_token)
                    var second = parseRelationalExpression(mode)
                    var first = <Binary kind="strictequals">{first}{second}</Binary>
                }
                else
                if( lookahead(strictnotequals_token) )
                {
                    match(strictnotequals_token)
                    var second = parseRelationalExpression(mode)
                    var first = <Binary kind="strictnotequals">{first}{second}</Binary>
                }
                else
                {
                    break  // okay, none found
                }
            }                
            var result = first

            Debug.exit("parseEqualityExpression",result)
            return result
        }

        /*

        BitwiseAndExpressionb    
            EqualityExpressionb
            BitwiseAndExpressionrb & EqualityExpressionb

        */

        function parseBitwiseAndExpression(mode)
        {
            Debug.enter("parseBitwiseAndExpression")

            var first = parseEqualityExpression(mode)
            while( lookahead(bitwiseand_token) )
            {
                match(bitwiseand_token)
                var second = parseEqualityExpression(mode)
                var first = <Binary kind="bitwiseand">{first}{second}</Binary>
            }
            var result = first

            Debug.exit("parseBitwiseAndExpression",result)
            return result
        }

        /*

        BitwiseXorExpressionb    
            BitwiseAndExpressionb
            BitwiseXorExpressionb ^ BitwiseAndExpressionb

        */

        function parseBitwiseXorExpression(mode)
        {
            Debug.enter("parseBitwiseXorExpression")

            var first = parseBitwiseAndExpression(mode)
            while( lookahead(bitwisexor_token) )
            {
                match(bitwisexor_token)
                var second = parseBitwiseAndExpression(mode)
                var first = <Binary kind="bitwisexor">{first}{second}</Binary>
            }
            var result = first

            Debug.exit("parseBitwiseXorExpression",result)
            return result
        }

        /*

        BitwiseOrExpressionb
            BitwiseXorExpressionb
            BitwiseOrExpressionb | BitwiseXorExpressionb

        */

        function parseBitwiseOrExpression(mode)
        {
            Debug.enter("parseBitwiseOrExpression")

            var first = parseBitwiseXorExpression(mode)
            while( lookahead(bitwiseor_token) )
            {
                match(bitwiseor_token)
                var second = parseBitwiseXorExpression(mode)
                var first = <Binary kind="bitwiseor">{first}{second}</Binary>
            }
            var result = first

            Debug.exit("parseBitwiseOrExpression",result)
            return result
        }

        /*

        LogicalAndExpressionb    
            BitwiseOrExpressionb
            LogicalAndExpressionb && BitwiseOrExpressionb

        */

        function parseLogicalAndExpression(mode)
        {
            Debug.enter("parseLogicalAndExpression")

            var first = parseBitwiseOrExpression(mode)
            while( lookahead(logicaland_token) )
            {
                match(logicaland_token)
                var second = parseBitwiseOrExpression(mode)
                var first = <Binary kind="bitwiseand">{first}{second}</Binary>
            }
            var result = first

            Debug.exit("parseLogicalAndExpression",result)
            return result
        }

        /*

        LogicalXorExpressionb    
            LogicalAndExpressionb
            LogicalXorExpressionb ^^ LogicalAndExpressionb

        */

        function parseLogicalXorExpression(mode)
        {
            Debug.enter("parseLogicalXorExpression")

            var first = parseLogicalAndExpression(mode)
            while( lookahead(logicalxor_token) )
            {
                match(logicalxor_token)
                var second = parseLogicalAndExpression(mode)
                var first = <Binary kind="logicalxor">{first}{second}</Binary>
            }
            var result = first

            Debug.exit("parseLogicalXorExpression",result)
            return result
        }

        /*

        LogicalOrExpressionb    
            LogicalXorExpressionb
            LogicalOrExpressionallowIn || LogicalXorExpressionb

        */

        function parseLogicalOrExpression(mode)
        {
            Debug.enter("parseLogicalOrExpression")

            var first = parseLogicalXorExpression(mode)
            while( lookahead(logicalor_token) )
            {
                match(logicalor_token)
                var second = parsePostfixExpression() //parseLogicalXorExpression(mode)
                var first = <Binary kind="logicalor">{first}{second}</Binary>
            }
            var result = first

            Debug.exit("parseLogicalOrExpression",result)
            return result
        }

        /*



        */

        function parseConditionalExpression(mode)
        {
            Debug.enter("parseConditionalExpression",mode)

            var result
            var first

            first = parseLogicalOrExpression(mode)

            if( lookahead(questionmark_token) )
            {
                match(questionmark_token);
                var second;
                var third;
                second = parseAssignmentExpression(mode);
                match(colon_token);
                third = parseAssignmentExpression(mode);
                result = <ConditionalExpression>{first}{second}{third}</ConditionalExpression>
            }
            else
            {
                result = first
            }

            Debug.exit("parseConditionalExpression",result)
            return result
        }

        /*



        */

        function parseNonAssignmentExpression(mode)
        {
            Debug.enter("parseNonAssignmentExpression",mode)

            //var first = parseLogicalOrExpression(mode)
            var first = parsePostfixExpression()

            if( lookahead(questionmark_token) )
            {
                match(questionmark_token);
                var second = parseNonAssignmentExpression(mode);
                match(colon_token);
                var third = parseNonAssignmentExpression(mode);
                var result = <ConditionalExpression>{first}{second}{third}</ConditionalExpression>
            }
            else
            {
                var result = first
            }

            Debug.exit("parseNonAssignmentExpression",result)
            return result
        }

        /*

        AssignmentExpressionb    
            ConditionalExpressionb
            LetExpressionb
            YieldExpressionb
            PostfixExpression  =  AssignmentExpressionb
            PostfixExpression CompoundAssignment AssignmentExpressionb
            PostfixExpression LogicalAssignment AssignmentExpressionb

            CompoundAssignment    
                *=
                /=
                %=
                +=
                -=
                <<=
                >>=
                >>>=
                &=
                ^=
                |=
    
            LogicalAssignment    
                &&=
                ^^=
                ||=
        */

        function parseAssignmentExpression(mode)
        {
            Debug.enter("parseAssignmentExpression",mode)
            
            if( lookahead(let_token) )
            {
                var result = parseLetExpression(mode)
            }
            else
            {
                var first = parseConditionalExpression(mode)
                var found = lookahead(assign_token) ? match(assign_token) :
                            lookahead(multassign_token) ? match(multassign_token) : 
                            lookahead(divassign_token) ? match(divassign_token) : 
                            lookahead(modulusassign_token) ? match(modulusassign_token) : 
                            lookahead(plusassign_token) ? match(plusassign_token) : 
                            lookahead(minusassign_token) ? match(minusassign_token) : 
                            lookahead(leftshiftassign_token) ? match(leftshiftassign_token) : 
                            lookahead(rightshiftassign_token) ? match(rightshiftassign_token) : 
                            lookahead(unsignedrightshiftassign_token) ? match(unsignedrightshiftassign_token) : 
                            lookahead(bitwiseandassign_token) ? match(bitwiseandassign_token) : 
                            lookahead(bitwisexorassign_token) ? match(bitwisexorassign_token) : 
                            lookahead(bitwiseorassign_token) ? match(bitwiseorassign_token) : 
                            lookahead(logicalandassign_token) ? match(logicalandassign_token) : 
                            lookahead(logicalxorassign_token) ? match(logicalxorassign_token) : 
                            lookahead(logicalorassign_token) ? match(logicalorassign_token) : empty_token;

                if( found != empty_token )
                {
                    if( first.name() != "Get" &&
                        first.name() != "Call" &&
                        first.name() != "Set" )
                    {
                        throw "invalid expression on left side of assignment"
                    }

                    var second = parseAssignmentExpression(mode)

                    if( first.name() == "Get" )
                    {
                        first.setLocalName("Set")
                        first.value = <To>{second}<SlotTypeRef/></To>
                    }
                    else
                    {
                        first = <Set kind="lexical">{first}<To>{second}<SlotTypeRef/></To></Set>
                    }
                    var result = first
                }
                else
                {
                    var result = first
                }
            }
            
            Debug.exit("parseAssignmentExpression",result)
            return result
        }

        function parseAssignmentExpressionSuffix(mode, first)
        {
        }
        
        /*

        LetExpressionb    
            let  (  LetBindingList  )  AssignmentExpressionb
    
        LetBindingList    
            «empty»
            NonemptyLetBindingList
    
        NonemptyLetBindingList    
            VariableBinding
            VariableBinding , NonemptyLetBindingList

        */

        function parseLetExpression(mode)
        {
            Debug.enter("parseLetExpression")

            var prologue = <Prologue/>
            match(let_token)
            match(leftparen_token)
            if( lookahead(rightparen_token) )
            {
                var first = <></>
            }
            else
            {
                var first = <></>
                first += parseVariableBinding(<Attributes><Let/></Attributes>,var_token,allowIn_mode,prologue)
                while( lookahead(comma_token) )
                {
                    match(comma_token)
                    first += parseVariableBinding(<Attributes><Let/></Attributes>,var_token,allowIn_mode,prologue)
                }
                prologue.* += first
            }
            match(rightparen_token)
            var second = parseAssignmentExpression(mode)
            var result = <LetExpression>{prologue}{second}</LetExpression>

            Debug.exit("parseLetExpression",result)
            return result
        }

        /*

        YieldExpressionb
            yield  AssignmentExpressionb
    
        */

/*
        function parseYieldExpression(mode)
        {
            Debug.enter("parseYieldExpression")

            Debug.exit("parseYieldExpression",result)
            return result
        }
*/
        /*
        
        DestructuringAssignmentExpression
            DestructuringPattern  =  AssignmentExpressionb

        */

        function parseDestructuringAssignmentExpression(mode)
        {
            Debug.enter("parseDestructuringAssignmentExpression",mode)
            
            var first = parseDestructuringPattern()
            match(assign_token)
            var second = parseAssignmentExpression(mode)
            var result = <AssignmentExpression>{first}{second}</AssignmentExpression>
            
            Debug.exit("parseDestructuringAssignmentExpression",result)
            return result
        }

        /*

        DestructuringPattern    
            DestructuringObjectPattern
            DestructuringArrayPattern

        */

        function parseDestructuringPattern()
        {
            Debug.enter("parseDestructuringPattern")

            if( lookahead(leftbrace_token) )
            {
                var result = parseDestructuringObjectPattern()
            }
            else
            if( lookahead(leftbracket_token ) ) 
            {
                var result = parseDestructuringArrayPattern()
            }
            else
            {
                throw "expecting destrcturing pattern"
            }

            Debug.exit("parseDestructuringPattern",result)
            return result
        }

        /*

        DestructuringObjectPattern    
            {  DestructuringFieldList  }

        DestructuringFieldList    
            DestructuringField
            DestructuringFieldList  ,  DestructuringField
    
        */

        function parseDestructuringObjectPattern()
        {
            Debug.enter("parseDestructuringObjectPattern")

            enterSlashContext(regexpliteral_token)
            match(leftbrace_token)
            var first = <></>
            first += parseDestructuringField()
            while( lookahead(comma_token) )
            {
                match(comma_token)
                first += parseDestructuringField()
            }
            match(rightbrace_token)
            exitSlashContext(regexpliteral_token)
            var result = <DestructuringObjectPattern>{first}</DestructuringObjectPattern>

            Debug.exit("parseDestructuringObjectPattern",result)
            return result
        }

        /*

        DestructuringField    
            NonAttributeQualifiedIdentifier  :  DestructuringPattern
            NonAttributeQualifiedIdentifier  :  PostfixExpression

        */

        function parseDestructuringField()
        {
            Debug.enter("parseDestructuringField")

            var first = parseNonAttributeQualifiedIdentifier()
            match(colon_token)
            if( lookahead(leftbrace_token) || lookahead(leftbracket_token) )
            {
                var second = parseDestructuringPattern()
                var result = <DestructuringField>{first}{second}</DestructuringField>
            }
            else
            {
                var second = parsePostfixExpression()
                var result = <DestructuringField>{first}{second}</DestructuringField>
            }

            Debug.exit("parseDestructuringField",result)
            return result
        }

        /*

        DestructuringArrayPattern    
            [  DestructuringElementList  ]
    
        DestructuringElementList    
            «empty»
            DestructuringElement
            , DestructuringElementList
            DestructuringElement , DestructuringElementList
    
        */

        function parseDestructuringArrayPattern()
        {
            Debug.enter("parseDestructuringArrayPattern")

            enterSlashContext(regexpliteral_token)
            match(leftbracket_token)
            if( lookahead(rightbracket_token) )
            {
                var first = <></>
            }
            else
            {
                var first = <></>
                first += parseDestructuringElement()
                while( lookahead(comma_token) )
                {
                    match(comma_token)
                    if( lookahead(rightbracket_token) )
                    {
                        // do nothing, we're done
                    }
                    else
                    {
                        first += parseDestructuringElement()
                    }
                }
            }
            match(rightbracket_token)
            exitSlashContext(regexpliteral_token)
            var result = <DestructuringArrayPattern>{first}</DestructuringArrayPattern>

            Debug.exit("parseDestructuringArrayPattern",result)
            return result
        }

        /*

        DestructuringElement    
            «empty»
            DestructuringPattern
            PostfixExpression

        */

        function parseDestructuringElement()
        {
            Debug.enter("parseDestructuringElement")

            if( lookahead(comma_token) )
            {
                var result = <EmptyElement/>
            }
            else
            if( lookahead(leftbrace_token) || lookahead(leftbracket_token) )
            {
                var result = parseDestructuringPattern()
            }
            else
            {
                var result = parseAssignmentExpression(allowIn_mode)
            }

            Debug.exit("parseDestructuringElement",result)
            return result
        }

        /*

        ListExpression

        */

        function parseListExpression(mode)
        {
            Debug.enter("parseListExpression",mode)
            
            var list = <></>
            list += parseAssignmentExpression(mode)
            while( lookahead( comma_token ) )
            {
                match( comma_token );
                list += parseAssignmentExpression(mode)
            }
            var node = list

            Debug.exit("parseListExpression",node)
            return node
        }

        function parseListExpressionPrime(first,mode)
        {
            Debug.enter("parseListExpressionPrime",mode)
            
            var list = <></>
            list += first
            while( lookahead( comma_token ) )
            {
                match( comma_token );
                list += parseAssignmentExpression(mode)
            }
            var node = list

            Debug.exit("parseListExpressionPrime",node)
            return node
        }

         // TYPE EXPRESSIONS

        /*

        TypeExpression    
            TypeIdentifier
            function  FunctionSignature
            (  TypeExpressionList  )
            {  FieldTypeList  }
            [  ElementTypeList  ]

        */

        function parseTypeExpression()
        {
            Debug.enter("parseTypeExpression")

            var prologue = <Prologue/>

            if( lookahead(function_token) )
            {
                match(function_token)
                var result = parseFunctionSignature(prologue)
            }
            else
            if( lookahead(leftparen_token) )
            {
                var result = parseUnionType()
            }
            else
            if( lookahead(leftbrace_token) )
            {
                var result = parseRecordType()
            }
            else
            if( lookahead(leftbracket_token) )
            {
                var result = parseArrayType()
            }
            else
            {
                var result = parseTypeIdentifier()
                if( lookahead(not_token) )
                {
                    match(not_token)
                    result.@nullable="false"
                }
                else
                if( lookahead(questionmark_token) )
                {
                    match(questionmark_token)
                    result.@nullable="true"
                }
            }

            Debug.exit("parseTypeExpression",result.toXMLString())
            return result
        }

        /*

        UnionType    
            (  TypeExpressionList  )

        */

        function parseUnionType()
        {
            Debug.enter("parseUnionType")

            match(leftparen_token)
            var first = parseTypeExpressionList()
            var result = <UnionType>{first}</UnionType>
            match(rightparen_token)

            Debug.exit("parseUnionType",result)
            return result
        }

        /*

        RecordType
            {  FieldTypeList  }

        */

        function parseRecordType()
        {
            Debug.enter("parseRecordType")

            match(leftbrace_token)
            if( lookahead(rightbrace_token) )
            {
                var first = <></>
            }
            else
            {
                var first = parseFieldTypeListPrime(<>{parseFieldType()}</>)
            }
            var result = <RecordType>{first}</RecordType>
            match(rightbrace_token)

            Debug.exit("parseRecordType",result)
            return result
        }

        /*

        NonemptyFieldTypeList    
            FieldType
            FieldType  ,  NonemptyFieldTypeList

        */

        function parseFieldTypeListPrime(first)
        {
            Debug.enter("parseNonemptyFieldTypeList",first)

            if( lookahead(comma_token) )
            {
                match(comma_token)
                var second = parseFieldType()
                var result = parseFieldTypeListPrime(<>{first}{second}</>)
            }
            else
            {
                var result = first
            }

            Debug.exit("parseFieldListPrime",result)
            return result
        }

        /*
            FieldType    
                FieldName  :  TypeExpression
        */
                  
        function parseFieldType()
        {
            Debug.enter("parseFieldType")

            var first = parseFieldName()
            match(colon_token)
            var second = parseTypeExpression()
            var result = <FieldType>{first}{second}</FieldType>

            Debug.exit("parseFieldType",result)
            return result
        }

        /*
        
        ArrayType    
            [  ElementTypeList  ]
    
        ElementTypeList    
            «empty»
            TypeExpression
            ,  ElementTypeList
            TypeExpression  ,  ElementTypeList
        
        */

        function parseArrayType()
        {
            Debug.enter("parseArrayType")

            enterSlashContext(regexpliteral_token)
            match(leftbracket_token)
            if( lookahead(rightbracket_token) )
            {
                var first = <></>
            }
            else
            {
                var temp = parseElementType()
                var first = parseElementTypeListPrime(<>{temp}</>)
            }

            exitSlashContext(regexpliteral_token)
            match(rightbracket_token)
            var result = <LiteralType>{first}</LiteralType>

            Debug.exit("parseArrayLiteral",result)
            return result
        }

        function parseElementTypeListPrime(first)        
        {
            Debug.enter("parseElementTypeListPrime",first)

            while( lookahead(comma_token) )
            {
                match(comma_token)
                var second = parseElementType()
                if( second == null )
                {
                    // do nothing
                }
                else
                {
                    var first = <>{first}{second}</>
                }
            }
            var result = first

            Debug.exit("parseElementTypeListPrime",result)
            return result
        }

        function parseElementType()
        {
            Debug.enter("parseElementType")

            if( lookahead(comma_token) )
            {
                var result = <EmptyElementType/>
            }
            else
            if( lookahead(rightbracket_token) )
            {
                var result = null
            }
            else
            {
                var result = parseTypeExpression()
            }

            Debug.exit("parseElementType",result)
            return result
        }


        // STATEMENTS

        /*

        Statement

        */

        function parseStatement(mode)
        {
            Debug.enter("parseStatement",mode)

            if( lookahead(super_token) )
            {
                var node = parseSuperStatement()
                matchSemicolon(mode)
            }
            else
            if( lookahead(leftbrace_token) )
            {
                var node = parseBlockStatement()
            }
            else
            if( lookahead(if_token) )
            {
                var node = parseIfStatement(mode)
            }
            else
            if( lookahead(switch_token) )
            {
                var node = parseSwitchStatement()  //includes 'switch type'
            }
            else
            if( lookahead(do_token) )
            {
                var node = parseDoStatement()
                matchSemicolon(mode)
            }
            else
            if( lookahead(while_token) )
            {
                var node = parseWhileStatement(mode)
            }
            else
            if( lookahead(for_token) )
            {
                var node = parseForStatement(mode)
            }
            else
            if( lookahead(let_token) )
            {
                match(let_token) // because other context do
                var node = parseLetStatement(mode)
            }
            else
            if( lookahead(with_token) )
            {
                var node = parseWithStatement(mode)
            }
            else
            if( lookahead(continue_token) )
            {
                var node = parseContinueStatement()
                matchSemicolon(mode)
            }
            else
            if( lookahead(break_token) )
            {
                var node = parseBreakStatement()
                matchSemicolon(mode)
            }
            else
            if( lookahead(return_token) )
            {
                var node = parseReturnStatement()
                matchSemicolon(mode)
            }
            else
            if( lookahead(throw_token) )
            {
                var node = parseThrowStatement()
                matchSemicolon(mode)
            }
            else
            if( lookahead(try_token) )
            {
                var node = parseTryStatement()
            }
            else
            if( lookahead(default_token) )
            {
                var node = parseDefaultXMLNamespaceStatement()
                matchSemicolon(mode)
            }
            else
            {
                var node = parseLabeledOrExpressionStatement(mode)
                matchSemicolon(mode)
            }

            Debug.exit("parseStatement",node)
            return node
        }

        /*
        */

        function parseSubstatement(mode)
        {
            Debug.enter("parseSubstatement")

            var node = parseStatement(mode)

            Debug.exit("parseSubstatement",node)
            return node
        }

        function parseBlockStatement()
        {
            Debug.enter("parseSubstatement")

            var prologue = <Prologue/>
            var stmts = parseBlock(prologue)
            //var slots = stmts.Slot  // hoist let slots
            //delete stmts.Slot
            var node = <BlockStatement>{prologue}{stmts}</BlockStatement>

            Debug.exit("parseBlockStatement",node)
            return node
        }

        /*

        SuperExpression    
            super
            super  Arguments

        */

        function parseSuperStatement()
        {
            Debug.enter("parseSuperStatement")

            match(super_token)
            var first = <SuperStatement/>
            if( lookahead(leftparen_token) )
            {
                var result = parseArguments(first)
            }
            else
            {
                var result = first
            }

            Debug.exit("parseSuperStatement",result)
            return result
        }

        function parseLabeledOrExpressionStatement(mode)
        {
            Debug.enter("parseLabeledOrExpressionStatement",mode)

            var first = parseListExpression(allowIn_mode)
            if( lookahead(colon_token) )
            {
                if( first.length() == 1 || first.Get.identifier != void 0 )
                {
                    first = first.Get.identifier
                }
                else
                {
                    throw "invalid label"
                }
                match(colon_token)
                var second = parseSubstatement(mode)
                var result = <LabeledStatement>{first}{second}</LabeledStatement>
            }
            else
            {
                var result = <ExpressionStatement>{first}</ExpressionStatement>
                // leave matchSemicolon(mode) for caller
            }
            
            Debug.exit("parseLabeledOrExpressionStatement",result)
            return result
        }

        function parseBlock(prologue)
        {
            Debug.exit("parseBlock")

            match(leftbrace_token)
            var node = parseDirectives(void 0,prologue)
            match(rightbrace_token)

            Debug.exit("parseBlock",node)
            return node
        }

        function parseMetaData()
        {
        }

        function parseIfStatement(mode)
        {
        }

        function parseSwitchStatement()
        {
        }

        function parseCaseStatement(mode)
        {
        }

        function parseCaseLabel()
        {
        }

        function parseCaseStatements()
        {
        }

        function parseCaseStatementsPrefix(first)
        {
        }

        function parseDoStatement()
        {
        }

        function parseWhileStatement(mode)
        {
        }

        function parseForStatement(mode)
        {
        }

        function parseLetStatement(mode)
        {
            Debug.enter("parseLetStatement")

            // already ate 'let'

            var prologue = <Prologue/>
            var block = <Block/>
            match(leftparen_token)
            if( lookahead(rightparen_token) )
            {
            }
            else
            {
                block.* += parseVariableBinding(<Attributes><Let/></Attributes>,var_token,allowIn_mode,prologue)
                while( lookahead(comma_token) )
                {
                    match(comma_token)
                    block.* += parseVariableBinding(<Attributes><Let/></Attributes>,var_token,allowIn_mode,prologue)
                }
            }
            match(rightparen_token)
            var second = parseSubstatement(mode)
            if( second.name() == "BlockStatement" )
            {
                prologue.* += second.Prologue.*
                block.* += second.Block.* 
            }
            else
            {
                block.* += second 
            }

            var node = <BlockStatement kind="let">{prologue}{block}</BlockStatement>

            Debug.exit("parseLetStatement",node)
            return node
        }

        function parseWithStatement(mode)
        {
            throw "WithStatement not implemented"
        }

        function parseContinueStatement()
        {
            throw "ContinueStatement not implemented"
        }

        function parseBreakStatement()
        {
            throw "BreakStatement not implemented"
        }

        /*

        ReturnStatement    
            return
            return [no line break] ListExpressionallowIn
        
        */

        function parseReturnStatement()
        {
            Debug.enter("parseReturnStatement")

            match(return_token)

            if( !inFunctionBody(true) )
            {
                throw "return statement is not allowed outside of function body"
            }

            var node = <Return/>

            if( !lookaheadSemicolon(full_mode) )
            {
                node.* = parseListExpression(allowIn_mode)
            }

            Debug.exit("parseReturnStatement",node)
            return node
        }

        function parseThrowStatement()
        {
            throw "ThrowStatement not implemented"
        }

        function parseTryStatement()
        {
            throw "TryStatement not implemented"
        }

        function parseCatchClauses()
        {
            throw "CatchClauses not implemented"
        }

        function parseCatchClause()
        {
            throw "CatchClause not implemented"
        }

        function parseFinallyClause()
        {
            throw "FinallyClause not implemented"
        }

        function parseDefaultXMLNamespaceStatement()
        {
            throw "DefaultXMLNamespaceStatement not implemented"
        }

        function parseAnnotatedDirective(mode)
        {
            throw "AnnotatedDirective not implemented"
        }

        function parseAnnotatedSubstatementsOrStatement(mode)
        {
            throw "not implemented"
        }

        function parseAnnotatableDirective(attrs,mode,prologue)
        {
            Debug.enter("parseAnnotatableDirective",attrs,mode)

            if( lookahead(let_token) )
            {
                match(let_token)
                attrs.* += <Let/>  // the let attribute
                if( lookahead(function_token) )
                {
                    var node = parseFunctionDefinition(attrs,prologue)
                }
                else
                {
                    var node = parseVariableDefinition(attrs,allowIn_mode,prologue)
                    matchSemicolon(mode)
                }
            }
            else
            if( lookahead(var_token) )
            {
                match(var_token) // eat 'var' before calling parseVar...
                var node = parseVariableDefinition(attrs,allowIn_mode,prologue)
            }
            else
            if( lookahead(const_token) )
            {
                var node = parseVariableDefinition(attrs,allowIn_mode,prologue)
            }
            else
            if( lookahead(function_token) )
            {
                var node = parseFunctionDefinition(attrs,prologue)
            }
            else
            if( lookahead(class_token) )
            {
                var node = parseClassDefinition(attrs,prologue)
            }
            else
            if( lookahead(interface_token) )
            {
                var node = parseInterfaceDefinition(attrs,prologue)
            }
            else
            if( lookahead(namespace_token) )
            {
                var node = parseNamespaceDefinition(attrs,prologue)
            }
            else
            if( lookahead(type_token) )
            {
                var node = parseTypeDefinition(attrs,prologue)
            }
            else
            {
                throw "not implemented yet"
            }

            Debug.exit("parseAnnotatableDirective",node)
            return node
        }

        function parseAnnotatableDirectiveOrLetStatement(attrs,mode,prologue)  // actually only need to handle let bindings and let statements
        {
            Debug.enter("parseAnnotatableDirectiveOrLetStatement",attrs,mode)

            match(let_token)

            if( lookahead(leftparen_token ) )  // Let statement
            {
                var node = parseLetStatement(mode)
            }
            else  // Let binding
            {
                attrs.* += <Let/>  // the let attribute
                if( lookahead(function_token) )
                {
                    var node = parseFunctionDefinition(attrs,prologue)
                }
                else
                {
                    var node = parseVariableDefinition(attrs,allowIn_mode,prologue)
                }
            }

            Debug.exit("parseAnnotatableDirectiveOrLetStatement",node)
            return node
        }

        function parseIncludeDirective()
        {
        }

        function parseVariableDefinition(first,mode,prologue)
        {
            Debug.enter("parseVariableDefinition",first,mode)

            // already ate 'let' if there was one, so will see 'const' or nothing (which means 'var')
            // if there is no 'let' then caller must eat 'var' before calling to avoid 'let var'

            var second = lookahead(const_token) ? match(const_token) : var_token 
            if( inInterfaceBody() )
            {
                throw "variable definition is not allowed in an interface"
            }
            var first = parseVariableBindingList(first,second,mode,prologue);
            var node = first

            Debug.exit("parseVariableDefinition",node)
            return node
        }

        function parseVariableBindingList(attrs,kind,mode,prologue)
        {
            Debug.enter("parseVariableBindingList",attrs,kind,mode)

            var node = <></>
            node += parseVariableBinding(attrs,kind,mode,prologue)

            while( lookahead( comma_token ) )
            {
                match( comma_token );
                node += parseVariableBinding(attrs,kind,mode,prologue);
            }

            Debug.exit("parseVariableBindingList",node)
            return node
        }

        /*

        VariableBindingb    
            TypedIdentifierb VariableInitialisationb
            DestructuringPattern VariableInitialisationb
    
        VariableInitialisationb    
            «empty»
            =  VariableInitialiserb
    
        VariableInitialiserb    
            AssignmentExpressionb

        */

        function isNamespaceAttribute(attr)
        {
            Debug.enter("isNamespaceAttribute",attr.toXMLString())

            var result = 
                    ( attr.name()=="Get" &&
                      attr.Identifier != undefined ) ? true :
                      attr.name()=="Namespace" ? true : false

            Debug.exit("isNamespaceAttribute",result)
            return result
        }

        function inFunctionBody(recurse=false)
        {
            Debug.enter("inFunctionBody")

            if( recurse )
            {
                var result = false
                for each( var item in slot_context_stack )
                {
                    if( item=="function" ) 
                    { 
                        result = true
                        break
                    }
                }
            }
            else
            {
                var context = slot_context_stack[slot_context_stack.length-1]
                var result = context=="function"
            }
            
            Debug.exit("inFunctionBody",result)
            return result
        }

        function inClassBody()
        {
            Debug.enter("inClassBody")
            var context = slot_context_stack[slot_context_stack.length-1]
            var result = context=="class"
            Debug.exit("inClassBody",result)
            return result
        }

        function inInterfaceBody()
        {
            Debug.enter("inInterfaceBody")
            var context = slot_context_stack[slot_context_stack.length-1]
            var result = context=="interface"
            Debug.exit("inInterfaceBody",result)
            return result
        }

        function inClassOrInterfaceBody()
        {
            Debug.enter("inClassOrInterfaceBody")
            var context = slot_context_stack[slot_context_stack.length-1]
            var result = context=="class" || context=="interface"
            Debug.exit("inClassOrInterfaceBody",result)
            return result
        }

        function parseVariableBinding(attrs,kind,mode,prologue)
        {
            Debug.enter("parseVariableBinding",attrs,kind,mode)

            if( lookahead(leftbrace_token) || lookahead(leftbracket_token) )
            {
                var first = parseDestructuringPattern()
                match(assign_token)
                var second = parseAssignmentExpression(mode)
            }
            else
            {
                var first  = parseTypedIdentifier(mode)
                if( lookahead(assign_token) )
                {
                    match(assign_token)
                    var second = parseAssignmentExpression(mode);
                }
                else
                {
                    var second
                }
                var node = makeBinding(attrs,kind,first,second,prologue)
            }

            Debug.exit("parseVariableBinding",node)
            return node
        }

        /*

        Make a slot

        For some kinds of bindings we hoist the intialiser to the prologue along with
        the slot (instance slots, function slots). The value of a function slot
        initialiser is moved by the Definer to derive an ExpressionStatement inserted 
        at the beginning of the corresponding Block. The Definer also hoists some
        slots (var,const,function) to the inner most enclosing variable object 
        (global,class,function)

        */

        function makeBinding(attrs,kind,typedid,value,prologue)
        {
            Debug.enter("makeBinding",attrs,kind,typedid,value)
                
            // See if there is one namespace attribute

            var ns = null
            for each( var attr in attrs.* )
            {
                if( isNamespaceAttribute(attr) )
                {
                    if( ns === null )
                    {
                        ns = attr
                    }
                    else
                    {
                        throw "only one namespace attribute allowed"
                    }
                }
            }

            // Make a qualified identifier

            if( ns != null )
            {
                var name = 
                    <QualifiedIdentifier>
                        <Qualifier>{ns}</Qualifier>
                        {typedid.Identifier}
                    </QualifiedIdentifier>
            }
            else   // use the default namespace
            {
                var name = 
                    <QualifiedIdentifier>
                        <Qualifier>
                            {default_namespace}
                        </Qualifier>
                        {typedid.Identifier}
                    </QualifiedIdentifier>
            }

            // Get the type if it has one

            if( typedid.name() == "TypedIdentifier" )
            {
                var type = typedid.Type.*
            }
			else
			if( kind == class_token )
			{
				var type = <Identifier name="Class"/>
			}
            else
            {
				var type = <Identifier name="Object"/>
            }


            // Make the slot and initialiser

            if( kind == class_token || 
                kind == interface_token || 
                kind == function_token || 
                kind == namespace_token ||
                kind == type_token ||
                inClassBody() && attrs.Let == void 0 && attrs.Static == void 0 )
            {
                var slot =
                    <Slot kind={scan.tokenText(kind)}>
                        <Name>{name}</Name>
                        <Type>{type}</Type>
                        <Init>{value}</Init>
                    </Slot>

                if( kind == function_token && inClassOrInterfaceBody() )
                {
                    slot.@method="true"
                }
                var init = <></>
            }
            else
            {
                var slot =
                    <Slot kind={scan.tokenText(kind)}>
                        <Name>{name}</Name>
                        <Type>{type}</Type>
                    </Slot>

				if( value != void 0 )
				{
	                var init = <>
                        <ExpressionStatement>
                	        <Set kind="lexical">
            	                {name}<To>{value}{typedid.Type}</To>
        	                </Set>
    	                </ExpressionStatement></>
				}
				else
				{
					var init = <></>
				}
            }

            // Apply attributes to the slot

            applyAttributesToSlot(attrs,slot)

            // Return the results

            var node = init

            if( slot.@static == "true" )
            {
                prologue.Static.* += slot
            }
            else
            if( inClassBody() && slot.@let != "true" )
            {
                prologue.Instance.* += slot
            }
            else
            {
                prologue.* += slot
            }

            Debug.exit("makeBinding",node,slot,prologue)
            return node
        }

        var slot_context_stack = ["global"]

        function applyAttributesToSlot(attrs,slot)
        {
            Debug.enter("applyAttributesToSlot",attrs.toXMLString(),slot)

            var slot_context = slot_context_stack[slot_context_stack.length-1]
            var slot_kind = slot.@kind

            if( attrs.Let != void 0 )
            {
                slot.@let = true
            }
            if( attrs.Dynamic != void 0 )
            {
                if( slot_kind == "class" )
                {
                    slot.@dynamic = true
                }
                else
                {
                    throw "'dynamic' must only be used on class definitions"
                }
            }
            if( attrs.Final != void 0 )
            {
                if( slot_kind == "class" || 
                    (slot_context == "class" && slot_kind == "function" && attrs.Static == void 0 ) )
                {
                    slot.@final = true
                }
                else
                {
                    throw "'final' must only be used on class and non-static method definitions"
                }
            }
            if( attrs.Native != void 0 )
            {
                if( slot_kind == "function" && slot_context != "function" )
                {
                    slot.@native = true
                }
                else
                {
                    throw "'native' must only be used on non-nested function definitions"
                }
            }
            if( attrs.Override != void 0 )
            {
                if( slot_context == "class" && slot_kind == "function" && attrs.Static == void 0 )
                {
                    slot.@override = true
                }
                else
                {
                    throw "'override' must only be used on non-static method definitions"
                }
            }
            if( attrs.Prototype != void 0 )
            {
                if( slot_context == "class" && attrs.Static == void 0 )
                {
                    slot.@prototype = true
                }
                else
                {
                    throw "'prototype' must only be used on non-static class variable and method definitions"
                }
            }
            if( attrs.Static != void 0 )
            {
                if( slot_context == "class" )
                {
                    slot.@static = true
                }
                else
                {
                    throw "'static' must only be used on non-static class variable and method definitions"
                }
            }
            if( attrs.get != void 0 )
            {
                if( slot.@kind == "function" )
                {
                    slot.@kind = "function get"
                }
                else
                {
                    throw "'get' must be used on function bindings only"
                }
            }
            if( attrs.set != void 0 )
            {
                if( slot.@kind == "function" )
                {
                    slot.@kind = "function set"
                }
                else
                {
                    throw "'set' must be used on function bindings only"
                }
            }
            if( attrs.call != void 0 )
            {
                if( slot.@kind == "function" )
                {
                    slot.@kind = "function call"
                }
                else
                {
                    throw "'call' must be used on function bindings only"
                }
            }
            if( attrs.to != void 0 )
            {
                if( slot.@kind == "function" )
                {
                    slot.@kind = "function to"
                }
                else
                {
                    throw "'to' must be used on function bindings only"
                }
            }
            if( attrs.operator != void 0 )
            {
                if( slot.@kind == "function" )
                {
                    slot.@kind = "function operator"
                }
                else
                {
                    throw "internal error"
                }
            }
            if( attrs.parameter != void 0 )
            {
                if( slot.@kind == "var" || slot.@kind == "const" )
                {
                    slot.@is_param = "true"
print("parameter slot found",slot)
                }
                else
                {
                    throw "internal error"
                }
            }

            Debug.exit("applyAttributesToSlot",slot)
            return
        }
            
        function parseTypedIdentifier(mode)
        {
            Debug.enter("parseTypedIdentifier",mode)

            var first =    parseIdentifier()
            if( lookahead(colon_token) )
            {
                match(colon_token)
                if( lookahead(mult_token) )
                {
                    match(mult_token);
                    var second = <Type><Identifier name="*"/></Type>  // same as no annotation
                }
                else
                if( lookahead(multassign_token) )
                {
                    var nexttoken=assign_token; // morph into an assign token
                    var second = <Type><Identifier name="*"/></Type>  // same as no annotation
                }
                else
                {
                    var second = <Type>{parseTypeExpression()}</Type>
                }
                var result = <TypedIdentifier>{first}{second}</TypedIdentifier>
            }
            else
            {
                var result = <TypedIdentifier>{first}<Type><Identifier name="*"/></Type></TypedIdentifier>
            }
            
            Debug.exit("parseTypedIdentifier",result)
            return result
        }

        function parseSimpleVariableDefinition()
        {
        }

        function parseUntypedVariableBindingList()
        {
        }

        function parseUntypedVariableBinding()
        {
        }
            
        function parseFunctionDefinition(attrs,prologue)
        {
            Debug.enter("parseFunctionDefinition",attrs)

            var kind  = match(function_token)
            var name  = parseFunctionName()
            var value = parseFunctionCommon(<></>)
            attrs.* += <{name.@kind}/>  // add functionname kind to attrs
            var node = makeBinding(attrs,kind,name,value,prologue)

            Debug.exit("parseFunctionDefinition",node)
            return node
        }

        function parseFunctionName()
        {
            Debug.enter("parseFunctionName")

            if( lookahead(identifier_token) )
            {
                var kind = "empty"
                var first = parseIdentifier()
            }
            else
            if( lookahead(to_token) )
            {
                var kind = scan.tokenText(match(to_token))
                var first = parseIdentifier()                
            }
            else
            if( lookahead(get_token) )
            {
                var kind = scan.tokenText(match(get_token))
                var first = parsePropertyIdentifier()
            }
            else
            if( lookahead(set_token) )
            {
                var kind = scan.tokenText(match(set_token))
                var first = parsePropertyIdentifier()                
            }
            else
            if( lookahead(call_token) )
            {
                var kind = scan.tokenText(match(call_token))
                var first = parsePropertyIdentifier()                
            }
            else
            {
                var found = lookahead(mult_token) ? match(mult_token) : 
                            lookahead(div_token) ? match(div_token) : 
                            lookahead(modulus_token) ? match(modulus_token) : 
                            lookahead(plus_token) ? match(plus_token) : 
                            lookahead(minus_token) ? match(minus_token) : 
                            lookahead(leftshift_token) ? match(leftshift_token) : 
                            lookahead(rightshift_token) ? match(rightshift_token) : 
                            lookahead(unsignedrightshift_token) ? match(unsignedrightshift_token) : 
                            lookahead(bitwiseand_token) ? match(bitwiseand_token) : 
                            lookahead(bitwisexor_token) ? match(bitwisexor_token) : 
                            lookahead(bitwiseor_token) ? match(bitwiseor_token) : 
                            lookahead(strictequals_token) ? match(strictequals_token) :
                            lookahead(notequals_token) ? match(notequals_token) : 
                            lookahead(strictnotequals_token) ? match(strictnotequals_token) : empty_token

                if( found != empty_token )
                {
                    var kind = "operator"
                    var first = <Identifier name={scan.tokenText(found)}/>
                }
            }
    
            var node = <FunctionName kind={kind}>{first}</FunctionName>

            Debug.exit("parseFunctionName",node)
            return node
        }

        var current_class = null

        /*

        class A { function A(){} var x = 10; function m() {}; print("hello") }

        class 'A'
          prologue
            slot 'iinit' function
              prologue
                slot 'construct' function ...
                slot 'x' 10
                slot 'm' function ...
              block
          block
            print("hello")

        bindings: class field initialisers are not hoisted
        with their slots but instance var initialisers are

        */

        function parseClassDefinition(attrs,hoisted)
        {
            Debug.enter("parseClassDefinition",attrs)
            
            match(class_token)
            var name = parseClassName()

            current_class = name
            slot_context_stack.push("class")   // use to determine if inits are hoisted

            var inherits = parseInheritance()
            var stmt = parseBlockStatement()

            // Move the static and instance slots out of the embedded block statement
            var prologue = <Prologue>{stmt.Prologue.Static.*}<Instance>{stmt.Prologue.Instance.*}</Instance></Prologue>
            delete stmt.Prologue.Static
            delete stmt.Prologue.Instance

            slot_context_stack.pop()

            current_class = null

            var value = <Class>{name}{inherits}{prologue}<Block>{stmt}</Block></Class>
            delete value.Block.BlockStatement.Prologue.Static

            var node = makeBinding(attrs,class_token,name,value,hoisted)

            Debug.exit("parseClassDefinition",node)
            return node
        }

        function parseClassName()
        {

            var first = parseIdentifier()

            if( lookahead(leftdotangle_token) )
            {
                var second = parseTypeParameters()
            }
            else
            {
                var second = <></>
            }

            if( lookahead(not_token) )
            {
                match(not_token)
                var node = <ClassName not_nullable="true">{first}{second}</ClassName>
            }
            else
            {
                var node = <ClassName>{first}{second}</ClassName>
            }

            return node
        }

        /*

        Inheritance    
            «empty»
            extends TypeName
            implements TypeNameList
            extends TypeName implements TypeNameList

        */

        function parseInheritance()
        {
            Debug.enter("parseInheritance")

            var node = <Inheritance/>

            if( lookahead(extends_token) )
            {
                match(extends_token)
                var first = parseTypeName()
                node.Extends.* = first
                if( lookahead(implements_token) )
                {
                    match(implements_token)
                    var second = parseTypeNameList()
                    node.Implements.* = second
                }
            }
            else
            if( lookahead(implements_token) )
            {
                match(implements_token)
                var second = parseTypeNameList()
                node.Implements.* = second
            }
    
            Debug.exit("parseInheritance",node)
            return node
        }

        function parseTypeName()
        {
            return parseTypeIdentifier()
        }

        function parseTypeNameList()
        {
            var node = <></>
            node += parseTypeIdentifier()
            while( lookahead(comma_token) )
            {
                match(comma_token)
                node += parseTypeIdentifier()                
            }
            return node
        }


        function parseInterfaceDefinition(attrs,hoisted)
        {
            Debug.enter("parseInterfaceDefinition",attrs)
            
            match(interface_token)
            var name = parseClassName()

            current_class = name
            slot_context_stack.push("interface")

            var inherits = parseExtendsList()

            var last_default_namespace = default_namespace
            default_namespace = <Namespace kind="interface" name={name.Identifier.@name}/>

            var stmt = parseBlockStatement()

            default_namespace = last_default_namespace

            slot_context_stack.pop()

            current_class = null

            var value = <Interface>{name}{inherits}{stmt.Prologue}{stmt.Block}</Interface>
            var node = makeBinding(attrs,interface_token,name,value,hoisted)

            Debug.exit("parseInterfaceDefinition",node)
            return node
        }

        /*

        ExtendsList
            «empty»
            extends TypeNameList

        */

        function parseExtendsList()
        {
            Debug.enter("parseExtendsList")

            var node = <Inheritance/>

            if( lookahead(extends_token) )
            {
                match(extends_token)
                var first = parseTypeNameList()
                node.Extends.* = first
            }
    
            Debug.exit("parseExtendsList",node)
            return node
        }


        /*

        TypeExpressionList    
            TypeExpression
            TypeExpressionList  ,  TypeExpression
        
        */
        
        function parseTypeExpressionList()
        {
            Debug.enter("parseTypeExpressionList")
            
            var list = <></>
            list += parseTypeExpression()
            while( lookahead( comma_token ) )
            {
                match( comma_token );
                list += parseTypeExpression()
            }
            var result = list

            Debug.exit("parseTypeExpressionList",result)
            return result
        }

        function parseNamespaceDefinition(attrs,prologue)
        {
            Debug.enter("parseNamespaceDefinition",attrs)

            match(namespace_token)
            var first = parseTypedIdentifier(allowIn_mode)
            if( lookahead(assign_token) )
            {
                match(assign_token)
                if( lookahead(stringliteral_token) )
                {
                    var second = <LiteralString value={scan.tokenText(match(stringliteral_token))}/>
                }
                else
                {
                    var second = <Get kind="lexical">{parseSimpleTypeIdentifier()}</Get>
                }
            }
            else
            {
                var second = <UniqueNamespaceName/>
            }

            if( inClassBody() )
            {
                attrs.* += <Static/>
            }

            var node = makeBinding(attrs,namespace_token,first,second,prologue)

            Debug.exit("parseNamespaceDefinition",node)
            return node
        }

        /*

        */

        function parseTypeDefinition(attrs,hoisted)
        {
            Debug.enter("parseTypeDefinition",attrs)

            match(type_token)
            var first = parseTypedIdentifier(allowIn_mode)
            match(assign_token)
            var second = parseTypeExpression()
            var node = makeBinding(attrs,type_token,first,second,hoisted)

            Debug.exit("parseTypeDefinition",node)
            return node
        }

        /*

        PackageDefinition    
            PackageAttributes package PackageNameOpt Block
    
        PackageAttributes    
            private
            «empty»
    
        PackageNameOpt    
            «empty»
            PackageName
    
        PackageName [create a lexical PackageIdentifier with the sequence of characters that make a PackageName]    
            Identifier
            PackageName  .  Identifier

        */

        var current_package

        function parsePackageDefinition(attr)
        {
            Debug.enter("parsePackageDefinition")

            enterSlashContext(div_token)
            match(package_token)
            var name = parsePackageName()
            exitSlashContext(div_token)

            current_package = name
            default_namespace = <Namespace kind="internal" name={name}/>
            var stmt = parseBlockStatement()
            var block = stmt.Block
            var prologue = stmt.Prologue
            current_package = null
            stmt.@name=name
            stmt.@kind="package"
            var node = stmt

//            prologue.insertChildBefore(prologue.*[0], 
            stmt.Prologue.* +=  
                    <OpenNamespaces ident="*">
                        <Namespace kind="public" name={name}/>
                        <Namespace kind="internal" name={name}/>
                    </OpenNamespaces>


            Debug.exit("parsePackageDefinition",node)
            return node
        }

        function parsePackageName()
        {
            Debug.enter("parsePackageName")
            
            var name = ""
            if( lookahead(leftbrace_token) )
            {
            }
            else
            {
                name += scan.tokenText(match(identifier_token))
                while( lookahead(dot_token) )
                {
                    match(dot_token)
                    name += "."
                    name += scan.tokenText(match(identifier_token))
                }

                scan.addPackageName(name)
            }

            Debug.exit("parsePackageName",name)
            return name
        }

        /*

        Attributes    
            Attribute
            Attribute [no line break] Attributes
    
        Attribute
            SimpleTypeIdentifier
            ReservedNamespace
            dynamic
            final
            native
            override
            prototype
            static
            [  AssignmentExpressionallowIn  ]

        */

        function parseAttributes(first)
        {
            Debug.enter("parseAttributes",first)

            while( !lookaheadSemicolon(full_mode) &&
                   ( lookahead(public_token) || 
                   lookahead(private_token) ||
                   lookahead(internal_token)  || 
                   lookahead(intrinsic_token) ||
                   lookahead(protected_token) || 
                   lookahead(dynamic_token) ||
                   lookahead(final_token) || 
                   lookahead(native_token) ||
                   lookahead(override_token) || 
                   lookahead(prototype_token) ||
                   lookahead(static_token) || 
                   lookahead(leftbracket_token) ||
                   lookahead(packageidentifier_token) ||
                   lookahead(identifier_token) ) )
            {
                first += parseAttribute()
            }

            var node = <Attributes>{first}</Attributes>

            // todo: check for duplicates

            Debug.exit("parseAttributes",node)
            return node
        }

        function parseAttribute()
        {
            Debug.enter("parseAttribute")

            var found = lookahead(public_token) ? match(public_token) :
                        lookahead(private_token) ? match(private_token) :
                        lookahead(internal_token) ? match(internal_token) :
                        lookahead(intrinsic_token) ? match(intrinsic_token) :
                        lookahead(protected_token) ? match(protected_token) :
                        lookahead(dynamic_token) ? match(dynamic_token) :
                        lookahead(final_token) ? match(final_token) :
                        lookahead(native_token) ? match(native_token) :
                        lookahead(override_token) ? match(override_token) :
                        lookahead(prototype_token) ? match(prototype_token) :
                        lookahead(static_token) ? match(static_token) : empty_token


            if( found != empty_token )
            {
                if( lookahead(doublecolon_token) )  // todo: look for other tokens that indicate an expression
                {
                    throw "attribute names cannot start a labeled or expression statement"
                }

                var slot_context = slot_context_stack[slot_context_stack.length-1]
                switch(found)
                {
                    case internal_token:
                        if( slot_context == "function" )
                        {
                            throw "'internal' shall not be used in local definitions"
                        }
                        var node = <Namespace kind="internal" name={current_package}/>
                        break
                    case intrinsic_token:
                        throw "'intrinsic' shall only be used by implementations"
                        var node = <Namespace kind={scan.tokenText(found)}/>
                        break
                    case private_token:
                        if( slot_context != "class" )
                        {
                            throw "'private' must only be used on class variable and method definitions"
                        }
                        var node = <Namespace kind="private" name={current_class}/>
                        break
                    case protected_token:
                        if( slot_context != "class" )
                        {
                            throw "'protected' must only be used on class variable and method definitions"
                        }
                        var node = <Namespace kind="protected" name={current_class}/>
                        break
                    case public_token:
                        if( slot_context == "function" )
                        {
                            throw "'public' shall not be used in local definitions"
                        }
                        if( inClassBody() )
                        {
                            var public_namespace_name = ""
                        }
                        else
                        {
                            var public_namespace_name = current_package
                        }
                        var node = <Namespace kind="public" name={public_namespace_name}/>
                        break
                    case dynamic_token:
                        var node = <Dynamic/>
                        break
                    case final_token:
                        var node = <Final/>
                        break
                    case native_token:
                        var node = <Native/>
                        break
                    case override_token:
                        var node = <Override/>
                        break
                    case prototype_token:
                        var node = <Prototype/>
                        break
                    case static_token:
                        var node = <Static/>
                        break
                    default:
                        throw "invalid attribute kind"
                }

            }
            else
            if( lookahead(leftbracket_token) )
            {
                var node = parseArrayLiteral()  // not quite right but close enough for now
            }
            else
            {
                var node = parseSimpleTypeIdentifier()
            }

            Debug.exit("parseAttribute",node.toXMLString())
            return node
        }

        /*

        Directivew    
            EmptyStatement
            Statementw
            AnnotatableDirectivew
            Attributes [no line break] AnnotatableDirectivew
            IncludeDirective Semicolonw
    
        AnnotatableDirectivew    
            VariableDefinitionallowIn Semicolonw
            FunctionDefinition
            ClassDefinition
            InterfaceDefinition
            NamespaceDefinition Semicolonw
            TypeDefinition Semicolonw
    
        */

        public function parseDirective(attr,mode,prologue)
        {
            Debug.enter("parseDirective",mode)

            if( lookahead(semicolon_token) )
            {
                match(semicolon_token)
                node = <></>
            }
            else
            if( lookahead(include_token) )
            {
//                var node = parseIncludeDirective()
            }
            else
            if( lookahead(public_token) ||
                lookahead(private_token) ||
                lookahead(internal_token) ||
                lookahead(intrinsic_token) ||
                lookahead(protected_token) ||
                lookahead(dynamic_token) ||
                lookahead(final_token) ||
                lookahead(native_token) ||
                lookahead(override_token) ||
                lookahead(prototype_token) ||
                lookahead(static_token) ||
                lookahead(leftbracket_token) ||
                attr != null )
            {
                if( attr != null )
                {
                    var first = attr   // passed in from parseDirectives
                }
                else
                {
                    var first = parseAttribute()
                }
                if( !lookaheadSemicolon(abbrev_mode) )
                {

                    var first = parseAttributes(<>{first}</>)
                }
                else
                {
                    throw "attributes cannot be used at expression statements"
                }
                if( lookahead(var_token) ||
                    lookahead(const_token) ||
                    lookahead(let_token) ||
                    lookahead(function_token) ||
                    lookahead(class_token) ||
                    lookahead(interface_token) ||
                    lookahead(namespace_token) ||
                    lookahead(type_token) )
                {
                    var node = parseAnnotatableDirective(first,mode,prologue)
                }
                else
                {
                    var node = first
                }
            }
            else
            if( lookahead(var_token) ||
                lookahead(const_token) ||
                lookahead(function_token) ||
                lookahead(class_token) ||
                lookahead(interface_token) ||
                lookahead(namespace_token) ||
                lookahead(type_token) )
            {
                var first = <Attributes/>
                var node = parseAnnotatableDirective(first,mode,prologue)                
            }
            else
            if( lookahead(let_token) )
            {
                var first = <Attributes/>
                var node = parseAnnotatableDirectiveOrLetStatement(first,mode,prologue)
            }
            else
            if( lookahead(identifier_token) )  // labeled, attribute or expression
            {
                var first = parseAssignmentExpression(mode)
                if( lookaheadSemicolon(abbrev_mode) )
                {
                    matchSemicolon(abbrev_mode)
                    var node = <ExpressionStatement>{first}</ExpressionStatement>
                }
                else
                if( lookahead(colon_token) )  // labeled statement
                {
                    match(colon_token)
                    if( first.name() != "Get" ||
                        first.Get.Identifier == undefined )  // don't allow qualified identifiers
                    {
                        throw "invalid label identifier"
                    }
                    else
                    {
                        first = first.Get.Identifier.@name
                    }
                       var second = parseSubstatement(mode)
                    var node = <Label label={first}>{second}</Label>
                }
                else
                if( first.name() == "Get" &&
                    ( lookahead(public_token) ||
                      lookahead(private_token) ||
                      lookahead(internal_token) ||
                      lookahead(intrinsic_token) ||
                      lookahead(protected_token) ||
                      lookahead(dynamic_token) ||
                      lookahead(final_token) ||
                      lookahead(native_token) ||
                      lookahead(override_token) ||
                      lookahead(prototype_token) ||
                      lookahead(static_token) ||
                      lookahead(leftbracket_token) ||
                      lookahead(identifier_token) ) )   // attribute list
                {
                    var first = parseAttributes(first)
                    if( lookahead(var_token) ||
                        lookahead(const_token) ||
                        lookahead(let_token) ||
                        lookahead(function_token) ||
                        lookahead(class_token) ||
                        lookahead(interface_token) ||
                        lookahead(namespace_token) ||
                        lookahead(type_token) )
                    {
                        var node = parseAnnotatableDirective(first,mode,prologue)
                    }
                    else
                    {
                        throw "looks like an attribute list but there's no definition to go with it"
                    }
                }
                else
                if( lookahead(var_token) ||
                    lookahead(const_token) ||
                    lookahead(let_token) ||
                    lookahead(function_token) ||
                    lookahead(class_token) ||
                    lookahead(interface_token) ||
                    lookahead(namespace_token) ||
                    lookahead(type_token) )
                {
                    var first = <Attributes>{first}</Attributes>
                    var node = parseAnnotatableDirective(first,mode,prologue)
                }
                else  // must be a list expression resume parsing as such
                {
                    var node = <ExpressionStatement>{parseListExpressionPrime(first,mode)}</ExpressionStatement>
                }
            }
            else
            {
                var node = parseStatement(mode)
            }

            Debug.exit("parseDirective",node)
            return node
        }

        /*

        Directives    
            «empty»
            DirectivesPrefix Directiveabbrev
    
        DirectivesPrefix    
            «empty»
            Pragmas
            DirectivesPrefix Directivefull

        */
    
        public function parseDirectives(attr,prologue)
        {
            Debug.enter("parseDirectives")

            // look for pragmas

            var node = <Block/>

            if( !lookahead(eos_token) && 
                (lookahead(use_token) || lookahead(import_token)) )
            {
                prologue.* += parsePragmas()
            }

            // look for statements and definitions

            while( !lookahead(eos_token) && !lookahead(rightbrace_token) )
            {
                 node.* += parseDirective(attr,full_mode,prologue)
            }

            Debug.exit("parseDirectives",node)
            return node
        }

        public function parsePragmas()
        {
            Debug.enter("parsePragmas")

            var node = <></>

            while( !lookahead(eos_token) && 
                (lookahead(use_token) || lookahead(import_token)) )
            {
                node += parsePragma()
            }

            Debug.exit("parsePragmas",node)
            return node
        }

        public function parsePragma()
        {
            Debug.enter("parsePragma")

            var node = <></>

            if( lookahead(use_token) )
            {
                node += parseUsePragma()
                matchSemicolon(full_mode)
            }
            else
            if( lookahead(import_token) )
            {
                node += parseImportPragma()
                matchSemicolon(full_mode)
            }

            Debug.exit("parsePragma",node)
            return node
        }

        public function parseImportPragma()
        {
            Debug.enter("parseImportPragma")

            match(import_token)

            if( lookahead(identifier_token) )
            {
                var first = parseIdentifier()
                if( lookahead(assign_token) )
                {
                    match(assign_token)
                }
                else
                {
                    throw "import name '"+first.@name+"' is not a known package identifier"
                }
                var second = parseImportName()
                if( second.@def == "*" )
                {
                    throw "wildcard not allowed in aliasing import pragmas"
                }
                second.@alias=first.@name
                var node = second
            }
            else
            if( lookahead(packageidentifier_token) )
            {
                var second = parseImportName()
                var node = second
            }
            else
            {
                throw "invalid import name"
            }

            var pkg_name = node.@pkg
            var def_name = node.@def

            if( def_name == "*" )
            {
                scopes[scopes.length-1].Imports.*+=node
            }
            else
            {
                scopes[scopes.length-1].Imports[def_name].*+=node
            }

            Debug.exit("parseImportPragma",node.toXMLString())
            return node
        }

        var scopes = [<Scope/>]

        function isImported(pkg_name,def_name)
        {
            var scope   = scopes[scopes.length-1]
            for each( var def in scope.Imports[def_name] )
            {
                if( def.Import.@pkg == pkg_name )
                {
                    return true
                }
            }

            for each( var def in scope.Imports.* )
            {
                if( def.@pkg == pkg_name )
                {
                    return true
                }
            }
            return false
        }
        
        function parseImportName()
        {
            Debug.enter("parseImportName")
            
            var pkg_part = scan.tokenText(match(packageidentifier_token))
            match(dot_token)
            if( lookahead(mult_token) )
            {
                match(mult_token)
                var def_part = "*"
            }
            else
            if( lookaheadReservedWord() )
            {
                var def_part = scan.tokenText(matchReservedWord())
            }
            else
            {
                var def_part = scan.tokenText(match(identifier_token))
            }

            var node = <Import pkg={pkg_part} def={def_part}/>

            Debug.exit("parseImportName",node.toXMLString())
            return node
        }

        public function parseUsePragma()
        {
            Debug.enter("parseUsePragma")

            match(use_token)
            var node = <></>
            node += parsePragmaItem()
            while( lookahead(comma_token) )
            {
                match(comma_token)
                node += parsePragmaItem()
            }

            Debug.exit("parseUsePragma",node)
            return node
        }

        function parsePragmaItem()
        {
            Debug.enter("parsePragmaItem")

            if( lookaheadReservedWord() ||
                lookahead(identifier_token) )
            {
                if( lookaheadReservedWord() )
                {
                    var first = matchReservedWord()
                }
                else
                {
                    var first = match(identifier_token)
                }
                var ident = scan.tokenText(first)

                switch(ident)
                {
                    case 'decimal':
                    case 'double':
                    case 'int':
                    case 'rounding':
                    case 'standard':
                    case 'strict':
                    case 'uint':
                        var node = <{ident}/>
                        break
                    case 'default':
                        match(namespace_token)
                        var node = <DefaultNamespace/>
                        node.* = <Get kind="lexical">{parseSimpleTypeIdentifier()}</Get>
                        break
                    case 'namespace':
                        var node = <UseNamespace/>
                        node.* = <Get kind="lexical">{parseSimpleTypeIdentifier()}</Get>
                        break
                    default:
                        throw "invalid pragma identifier:"+ident
                        break
                }
            }

            Debug.exit("parsePragmaItem",node)
            return node
        }

        var default_namespace

        public function parseProgram()
        {
            Debug.enter("parseProgram")

            var blocks = <></>
            
            while( lookahead(internal_token) || lookahead(package_token) )
            {
                if( lookahead(internal_token) )
                {
                    var attr = parseAttribute()
                }
                if( lookahead(package_token) )
                {
                    blocks += parsePackageDefinition(attr)
                    attr = null  // clear it
                }
                else
                {
                    break
                }
            }
            current_package = "global"
            var prologue = <Prologue/>
            default_namespace = <Namespace kind="internal" name="global"/>

            var block  = parseDirectives(attr,prologue)
            var global = <BlockStatement kind="package" name="">{prologue}{block}</BlockStatement>

            blocks += global
            var node = <Program><Prologue/><Block>{blocks}</Block></Program>
            node.Prologue.OpenNamespaces =
                    <OpenNamespaces ident="*">
                        <Namespace kind="public" name=""/>
                        <Namespace kind="internal" name="global"/>
                    </OpenNamespaces>


            Debug.exit("parseProgram",node)
            return node
        }
    }

    function testParser() 
    {
        var programs = 
        [
            //primary expressions
            
            "x",
            "q::id",
            "q::[expr]",
            "(expr)::id",
            "(expr)::[expr]",
            "@a",
            "@q::id",
            "@q::[expr]",
            "@(expr)::id",
            "@(expr)::[expr]",
            "@[expr]",
            "/abcdefg/g",
            "/abcdefg/",
            "/abcdefg/i",
            "/abcdefg/x",
            "true",
            "false",
            "null",
            "(a)::x",
            "(function(a,b,c){})",
            "{x:a,y:b,z:c}",
            "[a,b,c]",
            "{(x):y}",
            "(function(){})",
            "(function f(a:A,b:B){})",            
            "(function f.<T,U,V>(a:T,b:U,c:V){})",
            
            // type expressions
            
            "T",
            "?T",
            "T!",
            "T~",
            "T.<U>",
            "T.<U.<V>>",
            "T.<{a:A,t:{i:I,s:S}}>",
            "T.<{x:[A,B,C]}>",
            "T.<{x:(A,B,C)}>",
            "T.<U.<V.<W.<[,,,]>>>>",
            "T.<U>!",
            "?T.<U>",

            // Postfix expressions
            
            "x.y",
            "new x",
            "new x()",
            "x()",
            "x.y()",
            "x++",
            "x--",
            "x.y++",
            "x.y()++",
            "new x.y++",
        ]

        var n = 0
        for each ( var p in programs )
        {
            n++
            try {
                var parser = new Parser(p)
                var node = parser.parseProgram()

                print("> "+p)
                print(node.toXMLString())
                //print("---")
            }
            catch(x)
            {
                print(x)
            }
        }
    }

//    testParser()
}

