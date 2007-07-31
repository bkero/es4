/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */
// use module ast "tests/self/ast.es";
// module ast_encoder {
{
    use default namespace Ast;
    use namespace intrinsic;
    use namespace Debug;

    function indent (n:int)
        : string {
        let str = "\n";
        for ( ; n > 0; n-- ) {
            str += " ";
        }
        return str;
    }

    function encodeProgram (nd : PROGRAM, nesting : int = 0)
        : string {
        enter ("encodeProgram ", nesting);
        var str = "";
        switch type (nd): PROGRAM {
        case (p: Program) {
            var str =
                indent(nesting) + "{ 'ast::class': 'Program'"
              + indent(nesting) + ", 'packages': " + encodePackages (p.packages,nesting+", 'packages': ".length+1)
              + indent(nesting) + ", 'fixtures': " + encodeFixtures (p.fixtures,nesting+", 'fixtures': ".length+1)
              + indent(nesting) + ", 'block': " + encodeBlock (p.block,nesting+", block': ".length+1)
              + " }";
        }
        case (x: *) {
            throw "internalError: encodeProgram";
        }
        }
        exit ("encodeProgram");
        return str;
    }

    function encodePackages (packages: [PACKAGE], nesting: int = 0)
        : string {
        var str : string = "[]";
        return str;
    }

    function encodeFixtures (nd /*: FIXTURES*/, nesting: int = 0)
        : string {
        enter ("encodeFixtures nd.length=",nd.length);
        var str;

        if (nd.length == 0) {
            var str = "";
        }
        else
        {
            var str =
                  encodeFixtureBinding (nd[0], nesting)
                + indent(nesting-2)
                + ", "
                + encodeFixtures (nd.slice (1,nd.length), nesting);
        }
        exit ("encodeFixtures ",str);
        return str;
    }

    function encodeFixtureBinding (nd /*: FIXTURE_BINDING*/, nesting: int = 0)
        : string {
        enter ("encodeFixtureBinding ",nesting);

            var str =
                "[ " + encodeFixtureName (nd[0],nesting+"[ ".length)
              + indent(nesting) + ", " + encodeFixture (nd[1],nesting+", ".length)
              + " ]";

        exit ("encodeFixtureBinding ",str);
        return str;
    }

    function encodeFixtureName (nd /*: FIXTURE_NAME*/, nesting: int = 0)
        : string {
        enter ("encodeFixtureName ",nesting);

        var str = "";
        switch type (nd): FIXTURE_NAME {
        case (nd:Ast::PropName) {
            var str =
                "{ 'ast::class': 'PropName'"
              + indent(nesting) + ", 'name': " + encodeName (nd.name,nesting+", 'name': ".length+1)
              + " }";
        }
        case (nd:Ast::TempName) {
            var str =
                "{ 'ast::class': 'TempName'"
              + indent(nesting) + ", 'index': " + nd.index
              + " }";
        }
        case (nd: *) {
            var str = "**encodeFixtureName, unhandled ast: " + nd + "]]";
        }
        }

        exit ("encodeFixtureName ",str);
        return str;
    }

    function encodeFixture (nd /*: FIXTURE*/, nesting: int = 0)
        : string {
        enter ("encodeFixture ",nesting);

        var str = "";

        switch type (nd): FIXTURE {
        case (nd:Ast::ValFixture) {
            var str =
                "{ 'ast::class': 'ValFixture'"
              + indent(nesting) + ", 'type': " /*+ encodeTypeExpr (nd.type,nesting+", 'type': ".length+1) */
              + indent(nesting) + ", 'isReadOnly': " + nd.isReadOnly
              + " }";
        }
        case (nd:Ast::MethodFixture) {
            var str =
                "{ 'ast::class': 'MethodFixture'"
              + indent(nesting) + ", 'func': " + encodeFunc (nd.func,nesting+", 'func': ".length+1)
              + indent(nesting) + ", 'isReadOnly': " + nd.isReadOnly
              + indent(nesting) + ", 'isOverride': " + nd.isOverride
              + indent(nesting) + ", 'isFinal': " + nd.isFinal
              + " }";
        }
        case (nd: *) {
            var str = "[[encodeFixture, unhandled ast: " + nd + "]]";
        }
        }

        exit ("encodeFixture ",str);
        return str;
    }

    function encodeHead (nd /*: HEAD*/, nesting: int = 0)
        : string {
        enter ("encodeHead ",nesting);
        var str =
              "{ 'fixtures': [ " + encodeFixtures (nd.fixtures,nesting+"{ 'fixtures': [ ".length)
            + indent(nesting) + ", 'inits': [ " /*+ encodeInits (nd.inits,nesting+", 'inits': [ ".length) */
            + " ] ] }";
        exit ("encodeHead");
        return str;
    }

    function encodeBlock (nd: BLOCK, nesting: int = 0)
        : string {
        enter ("encodeBlock ",nesting);
        var str;
        switch type (nd) : BLOCK {
        case (nd:Block) {
            var str =
                  "{ 'ast::class': 'Block'"
                + indent(nesting) + ", 'pragmas': " + "[]" //encodePragmas (nd.pragmas)
                + indent(nesting) + ", 'defns': [ " + encodeDefns (nd.Ast::defns,nesting+", 'defns': [ ".length)
                + indent(nesting) + ", 'head': " + "[]" //encodeHead (nd.head)
                + indent(nesting) + ", 'stmts': [ " + encodeStmts (nd.Ast::stmts,nesting+", 'stmts': [ ".length) +" ]";
                + indent(nesting) + ", 'pos': " + "null" //encodePos (nd.pos)
                + " }";
        }
        case (x: *) {
            throw "internalError: encodeBlock";
        }
        }
        exit ("encodeBlock");
        return str;
    }

    function encodeStmts (nd /*: [STMT]*/, nesting: int = 0)
        : string {
        enter ("encodeStmts nd.length=",nd.length);
        var str;
        if (nd.length == 0) {
            var str = "";
        }
        else
        {
            var str =
                  encodeStmt (nd[0], nesting)
                + indent(nesting-2)
                + ", "
                + encodeStmts (nd.slice (1,nd.length), nesting);
        }
        exit ("encodeStmts ",str);
        return str;
    }

    function encodeStmt (nd : STMT, nesting: int = 0)
        : string {
        var str = "";
        enter ("encodeStmt");

        if (nd == null) {
            var str = "null";
        }
        else {
        switch type (nd): STMT {
        case (nd: ExprStmt) {
            var str =
                "{ 'ast::class': 'ExprStmt'"
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + " }";
        }
        case (nd: ReturnStmt) {
            var str =
                "{ 'ast::class': 'ReturnStmt'"
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + " }";
        }
        case (nd: IfStmt) {
            var str =
                "{ 'ast::class': 'IfStmt'"
              + indent(nesting)
              + ", 'cnd': "
              + encodeExpr (nd.cnd,nesting+", 'cnd': ".length)
              + indent(nesting)
              + ", 'thn': "
              + encodeStmt (nd.thn,nesting+", 'thn': ".length)
              + indent(nesting)
              + ", 'els': "
              + encodeStmt (nd.els,nesting+", 'els': ".length)
              + " }";
        }
        case (x: *) {
            throw "internalError: encodeStmt";
        }
        }
        }
        exit ("encodeStmt");
        return str;
    }

    function encodeExprs (nd /*: [EXPR]*/, nesting: int = 0)
        : string {
        enter ("encodeExprs nd.length=",nd.length);

        var str;
        if (nd == null) {
            var str = "null";
        }
        else
        if (nd.length == 0) {
            var str = "";
        }
        else
        {
            var str =
                  encodeExpr (nd[0],nesting)
                + indent(nesting-2)
                + ", "
                + encodeExprs (nd.slice (1,nd.length), nesting);
        }

        exit ("encodeExprs ",str);
        return str;
    }

    function encodeExprOption (nd : EXPR?, nesting: int = 0)
        : string {
        enter ("encodeExprOption");
        var str = "";
        if( nd === null ) {
            var str = "'null'";
        }
        else {
            var str = encodeExpr (nd,nesting);
        }
        exit ("encodeExprOption ",str);
        return str;
    }

    function encodeExpr (nd : EXPR, nesting: int = 0)
        : string {
        enter ("encodeExpr");
        var str = "";
        switch type (nd): EXPR {
        case (le: LiteralExpr) {
            var str =
                "{ 'ast::class': 'LiteralExpr'"
              + indent(nesting)
              + ", 'literal': "
              + encodeLiteral (le.literal,nesting+", 'literal': ".length)
              + " }";
        }
        case (ex: ListExpr) {
            var str =
                "{ 'ast::class': 'ListExpr'"
              + indent(nesting)
              + ", 'exprs': [ "
              + encodeExprs (ex.exprs,nesting+", 'exprs': [ ".length)
              + " ] }";
        }
        case (ex: CallExpr) {
            var str =
                "{ 'ast::class': 'CallExpr'"
              + indent(nesting)
              + ", 'func': "
              + encodeExpr (ex.func,nesting+", 'func': ".length)
              + indent(nesting)
              + ", 'args': [ "
              + encodeExprs (ex.args,nesting+", 'args': [ ".length)
              + " ] }";
        }
        case (ex: NewExpr) {
            enter ("newexpr");
            var str =
                "{ 'ast::class': 'NewExpr'"
              + indent(nesting)
              + ", 'func': "
              + encodeExpr (ex.func,nesting+", 'func': ".length)
              + indent(nesting)
              + ", 'args': [ "
              + encodeExprs (ex.args,nesting+", 'args': [ ".length)
              + " ] }";
            exit ("newexpr");
        }
        case (ex: LexicalRef) {
            var str =
                "{ 'ast::class': 'LexicalRef'"
              + indent(nesting)
              + ", 'ident': "
              + encodeIdentExpr (ex.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (ex: ObjectRef) {
            var str =
                "{ 'ast::class': 'ObjectRef'"
              + indent(nesting)
              + ", 'base': "
              + encodeExpr (ex.base,nesting+", 'base': ".length)
              + indent(nesting)
              + ", 'ident': "
              + encodeIdentExpr (ex.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (ex: BinaryExpr) {
            var str =
                "{ 'ast::class': 'BinaryExpr'"
              + indent(nesting)
              + ", 'op': "
              + encodeBinOp (ex.op,nesting,", 'op': ".length)
              + indent(nesting)
              + ", 'e1': "
              + encodeExpr (ex.e1,nesting+", 'e1': ".length)
              + indent(nesting)
              + ", 'e2': "
              + encodeExpr (ex.e2,nesting+", 'e2': ".length)
              + " }";
        }
        case (x: *) {
            var str = "[[unknown EXPR value "+nd+"]]";
        }
        }
        exit ("encodeExpr ",str);
        return str;
    }

    function encodeIdentExpr (nd : IDENT_EXPR, nesting: int = 0)
        : string {
        enter ("encodeIdentExpr");
        var str = "";
        switch type (nd): EXPR {
        case (ie: Identifier) {
            var str =
                "{ 'ast::class': 'Identifier'"
              + indent(nesting)
              + ", 'ident': "
              + ie.ident
              + " }";
        }
        case (ie: UnresolvedPath) {
            var str =
                "{ 'ast::class': 'UnresolvedPath'"
              + indent(nesting)
              + ", 'path': [ "
              + encodePath (ie.path,nesting+", 'path': [ ".length)
              + " ]"
              + indent(nesting)
              + ", 'ident': "
              + encodeIdentExpr (ie.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (x: *) {
            throw "internalError: encodeExpr: "+nd;
        }
        }
        exit ("encodeIdentExpr ",str);
        return str;
    }

    function encodeLiteral (nd : LITERAL, nesting: int = 0)
        : string {
        enter ("encodeLiteral")
        var str = "";
        switch type (nd): LITERAL {
        case (nd: LiteralString) {
            var str =
                "{ 'ast::class': 'LiteralString'"
              + indent(nesting)
              + ", 'strValue': "
              + nd.strValue
              + " }";
        }
        case (nd: LiteralDecimal) {
            var str =
                "{ 'ast::class': 'LiteralDecimal'"
              + indent(nesting)
              + ", 'decimalValue': "
              + nd.decimalValue
              + " }";
        }
        case (nd: LiteralNamespace) {
            var str =
                "{ 'ast::class': 'LiteralNamespace'"
              + indent(nesting)
              + ", 'namespaceValue': "
              + encodeNamespace (nd.namespaceValue,nesting+", 'namespaceValue': ".length)
              + " }";
        }
        case (nd: LiteralBoolean) {
            var str =
                "{ 'ast::class': 'LiteralBoolean'"
              + indent(nesting)
              + ", 'booleanValue': "
              + nd.booleanValue
              + " }";
        }
        case (x: *) {
            throw "internalError: encodeLiteral "+nd;
        }
        }
        exit ("encodeLiteral ",str);
        return str;
    }

    function encodeNamespace (nd : NAMESPACE, nesting: int = 0)
        : string {
        enter ("encodeNamespace ",nesting)
        var str = "";
        switch type (nd): NAMESPACE {
        case (nd: PublicNamespace) {
            var str =
                "{ 'ast::class': 'PublicNamespace'"
              + indent(nesting-1) + ", 'name': '" + nd.name
              + "' }";
        }
        case (x: *) {
            throw "internalError: encodeNamespace "+nd;
        }
        }
        exit ("encodeNamespace ",str);
        return str;
    }

    function encodeName (nd /*: NAME*/, nesting: int = 0)
        : string {
        enter ("encodeName ",nesting)
        var str =
                "{ 'ns': '"+encodeNamespace (nd.ns,nesting+"{ 'ns': '".length)
              + indent(nesting-1) + ", 'id': '"+ nd.id+"' }";

        exit ("encodeName ",str);
        return str;
    }

    function encodeBinOp (nd : BINOP, nesting: int = 0)
        : string {
        enter ("encodeLiteral")
        var str = "";
        switch type (nd): BINOP {
        case (op: Plus) {
            var str = "Plus";
        }
        case (op: Minus) {
            var str = "Minus";
        }
        case (op: Times) {
            var str = "Times";
        }
        case (op: Divide) {
            var str = "Divide";
        }
        case (op: Remainder) {
            var str = "Remainder";
        }
        case (op: LeftShift) {
            var str = "LeftShift";
        }
        case (op: RightShift) {
            var str = "RightShift";
        }
        case (op: RightShiftUnsigned) {
            var str = "RightShiftUnsigned";
        }
        case (op: BitwiseAnd) {
            var str = "BitwiseAnd";
        }
        case (op: BitwiseOr) {
            var str = "BitwiseOr";
        }
        case (op: BitwiseXor) {
            var str = "BitwiseXor";
        }
        case (op: LogicalAnd) {
            var str = "LogicalAnd";
        }
        case (op: LogicalOr) {
            var str = "LogicalOr";
        }
        case (op: InstanceOf) {
            var str = "InstanceOf";
        }
        case (op: In) {
            var str = "In";
        }
        case (op: Equal) {
            var str = "Equal";
        }
        case (op: NotEqual) {
            var str = "NotEqual";
        }
        case (op: StrictEqual) {
            var str = "StrictEqual";
        }
        case (op: StrictNotEqual) {
            var str = "StrictNotEqual";
        }
        case (op: Less) {
            var str = "Less";
        }
        case (op: LessOrEqual) {
            var str = "LessOrEqual";
        }
        case (op: Greater) {
            var str = "Greater";
        }
        case (op: GreaterOrEqual) {
            var str = "GreaterOrEqual";
        }
        case (x: *) {
            throw "internalError: encodeLiteral";
        }
        }
        var str = "{ 'ast::class': '" + str + "' }"
        exit ("encodeLiteral ",str);
        return str;
    }

    function encodePath (nd /*: [IDENT]*/, nesting: int = 0)
        : string {
        enter ("encodePath nd.length=",nd.length);

        var str;
        if (nd.length == 0) {
            var str = "";
        }
        else
        {
            var str =
                  nd[0]
                + indent(nesting-2)
                + ", "
                + encodePath (nd.slice (1,nd.length), nesting);
        }
        exit ("encodePath ",str);
        return str;
    }

    function encodeDefns (nd /*: [DEFN]*/, nesting: int = 0)
        : string {
        enter ("encodeDefns nd.length=",nd.length);
        var str;
        if (nd.length == 0) {
            var str = "";
        }
        else
        {
            var str =
                  encodeDefn (nd[0], nesting)
                + indent(nesting-2)
                + ", "
                + encodeDefns (nd.slice (1,nd.length), nesting);
        }
        exit ("encodeDefns ",str);
        return str;
    }

    function encodeDefn (nd : DEFN, nesting: int = 0)
        : string {
        var str = "";
        enter ("encodeDefn");
        switch type (nd): DEFN {
        case (nd: VariableDefn) {
            var str =
                "{ 'ast::class': 'VariableDefn'"
              + indent(nesting)
              + ", 'ns': "
              + encodeExpr (nd.ns, nesting+", 'ns': ".length)
              + indent(nesting)
              + ", 'bindings': [ [ "
              + encodeBindings (nd.bindings[0], nesting+", 'bindings': [ [ ".length)
              + " ]"
              + indent(nesting+", 'bindings': ".length)
              + ", [ "
              + encodeInitSteps (nd.bindings[1], nesting+", 'bindings': [ [ ".length)
              + " ] ] }";
        }
        case (nd: FunctionDefn) {
            var str =
                "{ 'ast::class': 'FunctionDefn'"
              + indent(nesting)
              + ", 'ns': "
              + encodeExpr (nd.ns, nesting+", 'ns': ".length)
              + indent(nesting)
              + ", 'func': "
              + encodeFunc (nd.func, nesting+", 'func': ".length)
                /*
              + indent(nesting+", 'bindings': ".length)
              + ", [ "
              + encodeInitSteps (nd.bindings[1], nesting+", 'bindings': [ [ ".length)
                */
              + " }";
        }
        case (x: *) {
            throw "internalError: encodeDefn";
        }
        }
        exit ("encodeDefn");
        return str;
    }

    function encodeFunc (nd : FUNC, nesting: int = 0)
        : string {
        enter ("encodeFunc");

        var str =
            "{ 'ast::class': 'Func'"
          + indent(nesting)
          + ", 'name': "
          + encodeFuncName (nd.name,nesting+", 'name': ".length)
          + indent(nesting)
          + ", 'isNative': " + nd.isNative
          + indent(nesting)
          + ", 'block': "
          + encodeBlock (nd.block,nesting+", 'block': ".length)
          + indent(nesting)
          + ", 'params': "
          + encodeHead (nd.params,nesting+", 'params': ".length)
          + indent(nesting)
          + ", 'defaults': "
          + encodeExprs (nd.defaults,nesting+", 'defaults': ".length)
            /*
          + indent(nesting)
          + ", 'type': "
          + encodeTypeExpr (nd.type,nesting+", 'ident': ".length)
            */
          + " }";

        exit ("encodeBinding ",str);
        return str;
    }

    function encodeFuncName (nd /*: FUNC_NAME*/, nesting: int = 0)
        : string {
        enter ("encodeFuncName");

        var str =
            "{ 'kind': "
          + encodeFuncNameKind (nd.kind,nesting+"{ 'kind': ".length)
          + indent(nesting)
          + ", 'ident': " + nd.ident
          + " }";

        exit ("encodeFuncName ",str);
        return str;
    }

    function encodeFuncNameKind (nd /*: FUNC_NAME_KIND*/, nesting: int = 0)
        : string {
        enter ("encodeFuncNameKind");

        switch type (nd) {
        case (nd:Ordinary) {
            var str = "Ordinary";
        }
        case (nd: *) {
            throw "internal error: encodeFuncNameKind " + nd;
        }
        }

        exit ("encodeFuncNameKind ",str);
        return str;
    }

    function encodeBindings (nd /*: [BINDING]*/, nesting: int = 0)
        : string {
        enter ("encodeBindings nd.length=",nd.length);

        var str;
        if (nd == null) {
            var str = "null";
        }
        else
        if (nd.length == 0) {
            var str = "";
        }
        else
        {
            var str =
                  encodeBinding (nd[0],nesting)
                + indent(nesting-2)
                + ", "
                + encodeBindings (nd.slice (1,nd.length), nesting);
        }
        exit ("encodeBindings ",str);
        return str;
    }

    function encodeBinding (nd : BINDING, nesting: int = 0)
        : string {
        enter ("encodeBinding");

        var str =
            "{ 'ast::class': 'Binding'"
          + indent(nesting)
          + ", 'ident': "
          + encodeBindingIdent (nd.ident,nesting+", 'ident': ".length)
            /*
          + indent(nesting)
          + ", 'type': "
          + encodeTypeExpr (nd.type,nesting+", 'ident': ".length)
            */
          + " }";

        exit ("encodeBinding ",str);
        return str;
    }

    function encodeBindingIdent (nd : BINDING_IDENT, nesting: int = 0)
        : string {
        enter ("encodeBindingIdent");

        var str = "";

        switch type (nd) {
        case (nd:TempIdent) {
            str = "{ 'ast::class': 'TempIdent'"
                + indent(nesting)
                + ", 'n': "
                + nd.n
                + " }";
        }
        case (nd:ParamIdent) {
            str = "{ 'ast::class': 'ParamIdent'"
                + indent(nesting)
                + ", 'n': "
                + nd.n
                + " }";
        }
        case (nd:PropIdent) {
            str = "{ 'ast::class': 'PropIdent'"
                + indent(nesting)
                + ", 'ident': '"
                + nd.ident
                + "' }";
        }
        }


        exit ("encodeBindingIdent ",str);
        return str;
    }

    function encodeInitSteps (nd /*: [INIT_STEP]*/, nesting: int = 0)
        : string {
        enter ("encodeInitSteps nd.length=",nd.length);

        var str;
        if (nd == null) {
            var str = "null";
        }
        else
        if (nd.length == 0) {
            var str = "";
        }
        else
        {
            var str =
                  encodeInitStep (nd[0],nesting)
                + indent(nesting-2)
                + ", "
                + encodeInitSteps (nd.slice (1,nd.length), nesting);
        }
        exit ("encodeBindings ",str);
        return str;
    }

    function encodeInitStep (nd : INIT_STEP, nesting: int = 0)
        : string {
        enter ("encodeInitStep");

        var str;
        switch type (nd) {
        case (nd:InitStep) {
        var str =
            "{ 'ast::class': 'InitStep'"
          + indent(nesting)
          + ", 'ident': "
          + encodeBindingIdent (nd.ident,nesting+", 'ident': ".length)
          + indent(nesting)
          + ", 'expr': "
          + encodeExpr (nd.expr,nesting+", 'expr': ".length)
          + " }";
        }
        case (nd:*) {
            throw "internal error: encodeInitStep";
        }
        }

        exit ("encodeInitStep ",str);
        return str;
    }

}
// } // end module
