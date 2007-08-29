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

/* ast.es */

namespace Ast

{
    use default namespace Ast;
    //    use namespace intrinsic;

    // POS

    type POS =
       { file: String
       , span: int //StreamPos.span
       , sm: int // StreamPos.sourcemap
       , post_newline: Boolean }

    // BASIC TYPES

    type IDENT = String;   // unicode string
    type IDENTS = [IDENT];

    type HEAD = Head;

    class Head {
        use default namespace public;  // TRAC should default namespace nest
        const fixtures: FIXTURES;  
        const exprs // : EXPRS;
        function Head (fixtures,exprs)
            : fixtures = fixtures
            , exprs = exprs { }
    }

    type FIXTURE_NAME =
       ( TempName
       , PropName )

    class TempName {
        const index : int;
        function TempName (index)
            : index = index {}
    }

    class PropName {
        const name /*: NAME*/;
        function PropName(name) 
            : name=name { }
    }

    type FIXTURE_BINDING = [FIXTURE_NAME,FIXTURE];
    type FIXTURES = [FIXTURE_BINDING];

    type INIT_BINDING = [FIXTURE_NAME,EXPR]
    type INITS = [INIT_BINDING];

    type NAMES = [NAME];
    type NAME =
       { ns: NAMESPACE
       , id: IDENT }

    type MULTINAME =
       { nss: [[NAMESPACE]]
       , id: IDENT }

    // NAMESPACE

    type NAMESPACES = [NAMESPACE];

    type NAMESPACE =
       ( IntrinsicNamespace
       , PrivateNamespace
       , ProtectedNamespace
       , PublicNamespace
       , InternalNamespace
       , UserNamespace
       , AnonymousNamespace
       , ImportNamespace );

    type RESERVED_NAMESPACE =
       ( IntrinsicNamespace
       , PrivateNamespace
       , ProtectedNamespace
       , PublicNamespace
       , InternalNamespace );

    class IntrinsicNamespace {
        function hash () { return "intrinsic"; }
    }

    class OperatorNamespace {
        function hash () { return "operator"; }
    }

    class PrivateNamespace {
        const name : IDENT
        function PrivateNamespace (name)
            : name = name { }
        function hash () { return "private " + name; }
    }

    class ProtectedNamespace {
        const name : IDENT
        function ProtectedNamespace (name)
            : name = name { }
        function hash () { return "protected " + name; }
    }

    class PublicNamespace {
        const name : IDENT;
        function PublicNamespace (name)
            : name = name { }
        function hash () { return "public " + name; }
    }

    class InternalNamespace {
        const name : IDENT;
        function InternalNamespace (name)
            : name = name { }
        function hash () { return "internal " + name; }
    }

    class UserNamespace {
        const name : IDENT;
        function UserNamespace (name)
            : name = name { }
        function hash () { return "use " + name; }
    }

    class AnonymousNamespace {
        const name : IDENT;
        function AnonymousNamespace (name)
            : name = name { }
        function hash () { return "anon " + name; }
    }

    class ImportNamespace {
        const ident : IDENT
        const ns : PublicNamespace
        function hash () { return "import " + ns.hash; }
    }

    var noNS = new PublicNamespace ("");   // FIXME find better way to express

    function test () {
        print (new EmptyStmt)
    }

    test()
}
