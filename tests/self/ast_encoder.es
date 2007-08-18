/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */
// use module ast "tests/self/ast.es";
// module ast_encoder {

namespace Encode;

{
    use default namespace Encode;
    use namespace intrinsic;
    use namespace Ast;

    use namespace Release;

    function indent (n:int)
        : string {
        let str = "\n";
        for ( ; n > 0; n-- ) {
            str += " ";
        }
        return str;
    }

    function program (nd : PROGRAM, nesting : int = 0)
        : string {
        enter ("Encode::program ", nesting);
        var str = "";
        switch type (nd): PROGRAM {
        case (nd: Program) {
            var str =
                indent(nesting) + "{ 'ast_class': 'Program'"
              + indent(nesting) + ", 'packages': " + encodePackages (nd.packages,nesting+", 'packages': ".length)
              + indent(nesting) + ", 'head': " + head (nd.head,nesting+", 'head': ".length)
              + indent(nesting) + ", 'block': " + block (nd.block,nesting+", 'block': ".length)
              + " }";
        }
        case (nd: *) {
            throw "error Encode::program " + nd;
        }
        }
        exit ("Encode::program");
        return str;
    }

    function encodePackages (packages: [PACKAGE], nesting: int = 0)
        : string {
        var str : string = "[]";
        return str;
    }

    function encodeFixtures (nd /*: FIXTURES*/, nesting: int = 0)
        : string {
        enter ("encodeFixtures nd=",nd);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + encodeFixtureBinding (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("encodeFixtures ",str);
        return str;
    }

    function encodeFixtureBinding (nd /*: FIXTURE_BINDING*/, nesting: int = 0)
        : string {
        enter ("encodeFixtureBinding ",nd);

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

            // print ("prop ",nd.name.id);

            var str =
                "{ 'ast_class': 'PropName'"
              + indent(nesting) + ", 'name': " + encodeName (nd.name,nesting+", 'name': ".length)
              + " }";
        }
        case (nd:Ast::TempName) {
            var str =
                "{ 'ast_class': 'TempName'"
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

    function encodeFixture (nd: FIXTURE, nesting: int = 0)
        : string {
        enter ("encodeFixture ",nesting);

        var str = "";

        switch type (nd): FIXTURE {
        case (nd:Ast::ValFixture) {
            var str =
                "{ 'ast_class': 'ValFixture'"
              + indent(nesting) 
              + ", 'type': " 
              + encodeTypeExpr (nd.type,nesting+", 'type': ".length)
              + indent(nesting) + ", 'isReadOnly': " + nd.isReadOnly
              + " }";
        }
        case (nd:Ast::MethodFixture) {
            var str =
                "{ 'ast_class': 'MethodFixture'"
              + indent(nesting) + ", 'func': " + encodeFunc (nd.func,nesting+", 'func': ".length)
              + indent(nesting) + ", 'type': " + encodeTypeExpr (nd.type,nesting+", 'type': ".length)
              + indent(nesting) + ", 'isReadOnly': " + nd.isReadOnly
              + indent(nesting) + ", 'isOverride': " + nd.isOverride
              + indent(nesting) + ", 'isFinal': " + nd.isFinal
              + " }";
        }
        case (nd:Ast::ClassFixture) {
            var str =
                "{ 'ast_class': 'ClassFixture'"
              + indent(nesting) + ", 'cls': " + encodeCls (nd.cls,nesting+", 'cls': ".length)
              + " }";
        }
        case (nd:Ast::NamespaceFixture) {
            var str =
                "{ 'ast_class': 'NamespaceFixture'"
              + indent(nesting) + ", 'ns': " + encodeNamespace (nd.ns,nesting+", 'ns': ".length)
              + " }";
        }
        case (nd:Ast::TypeFixture) {
            var str =
                "{ 'ast_class': 'TypeFixture'"
              + indent(nesting) + ", 'type': " + encodeTypeExpr (nd.type,nesting+", 'type': ".length)
              + " }";
        }
        case (nd: *) {
            var str = "**encodeFixture, unhandled ast: " + nd + "**";
        }
        }

        exit ("encodeFixture ",str);
        return str;
    }

    function encodeInits (nd /*: INITS*/, nesting: int = 0)
        : string {
        enter ("encodeInits nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + encodeInitBinding (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("encodeInits ",str);
        return str;
    }

    function encodeInitBinding (nd /*: INIT_BINDING*/, nesting: int = 0)
        : string {
        enter ("encodeInitBinding ",nesting);

            var str =
                "[ " + encodeFixtureName (nd[0],nesting+"[ ".length)
              + indent(nesting) + ", " + encodeExpr (nd[1],nesting+", ".length)
              + " ]";

        exit ("encodeInitBinding ",str);
        return str;
    }

    function head (nd /*: HEAD*/, nesting: int = 0)
        : string {
        enter ("Encode::head ",nesting);

        var str =
              "{ 'fixtures': [ " + encodeFixtures (nd.fixtures,nesting+"{ 'fixtures': [ ".length) + " ]"
            + indent(nesting) + ", 'inits': [ " + encodeInits (nd.inits,nesting+", 'inits': [ ".length)
            + " ] }";

        exit ("Encode::head");
        return str;
    }

    function block (nd: BLOCK, nesting: int = 0)
        : string {
        enter ("Encode::block ",nesting);
        var str;
        switch type (nd) : BLOCK {
        case (nd:Block) {
            var str =
                  "{ 'ast_class': 'Block'"
                + indent(nesting) + ", 'head': " + head (nd.head,nesting+", 'head': ".length)
                + indent(nesting) + ", 'stmts': [ " + stmts (nd.Ast::stmts,nesting+", 'stmts': [ ".length)
                + " ] }";
        }
        case (x: *) {
            throw "internalError: Encode::block";
        }
        }
        exit ("Encode::block");
        return str;
    }

    function stmts (nd /*: [STMT]*/, nesting: int = 0)
        : string {
        enter ("Encode::stmts ", nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + stmt (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode::stmts ",str);
        return str;
    }

    function stmt (nd : STMT, nesting: int = 0)
        : string {
        var str = "";
        enter ("stmt");
        print ("  stmt");
        if (nd == null) {
            var str = "null";
        }
        else {
        switch type (nd): STMT {
        case (nd: ExprStmt) {
            var str =
                "{ 'ast_class': 'ExprStmt'"
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + " }";
        }
        case (nd: ReturnStmt) {
            var str =
                "{ 'ast_class': 'ReturnStmt'"
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + " }";
        }
        case (nd: ThrowStmt) {
            var str =
                "{ 'ast_class': 'ThrowStmt'"
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + " }";
        }
        case (nd: BreakStmt) {
            var str =
                "{ 'ast_class': 'BreakStmt'"
              + indent(nesting)
              + ", 'ident': "
              + identOpt (nd.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (nd: ContinueStmt) {
            var str =
                "{ 'ast_class': 'ContinueStmt'"
              + indent(nesting)
              + ", 'ident': "
              + identOpt (nd.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (nd: IfStmt) {
            var str =
                "{ 'ast_class': 'IfStmt'"
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'then': "
              + stmt (nd.then,nesting+", 'then': ".length)
              + indent(nesting)
              + ", 'elseOpt': "
              + stmt (nd.elseOpt,nesting+", 'elseOpt': ".length)
              + " }";
        }
        case (nd: WhileStmt) {
            var str =
                "{ 'ast_class': 'WhileStmt'"
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'stmt': "
              + stmt (nd.stmt,nesting+", 'stmt': ".length)
              + indent(nesting)
              + ", 'labels': "
              + "[]"  // for now: stmt (nd.stmt,nesting+", 'stmt': ".length)
              + " }";
        }
        case (nd: SwitchStmt) {
            var str =
                "{ 'ast_class': 'SwitchStmt'"
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'cases': [ "
              + cases (nd.cases,nesting+", 'cases': ".length) + " ]"
              + indent(nesting)
              + ", 'labels': "
              + "[]"  // for now: stmt (nd.stmt,nesting+", 'stmt': ".length)
              + " }";
        }
        case (nd: SwitchTypeStmt) {
            var str =
                "{ 'ast_class': 'SwitchTypeStmt'"
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'type': "
              + encodeTypeExpr (nd.type,nesting+", 'type': ".length)
              + indent(nesting)
              + ", 'cases': [ "
              + catches (nd.cases,nesting+", 'cases': ".length) + " ]"
              + " }";
        }
        case (nd: ForStmt) {
            var str =
                "{ 'ast_class': 'ForStmt'"
              + indent(nesting)
              + ", 'vars': "
              + head (nd.vars,nesting+", 'vars': ".length)
              + indent(nesting)
              + ", 'init': "
              + exprOpt (nd.init,nesting+", 'init': ".length)
              + indent(nesting)
              + ", 'cond': "
              + exprOpt (nd.cond,nesting+", 'cond': ".length)
              + indent(nesting)
              + ", 'incr': "
              + exprOpt (nd.incr,nesting+", 'incr': ".length)
              + indent(nesting)
              + ", 'stmt': "
              + stmt (nd.stmt,nesting+", 'stmt': ".length)
              + indent(nesting)
              + ", 'labels': "
              + "[]"  // for now: stmt (nd.stmt,nesting+", 'stmt': ".length)
              + " }";
        }
        case (nd: BlockStmt) {
            var str =
                "{ 'ast_class': 'BlockStmt'"
              + indent(nesting)
              + ", 'block': "
              + block (nd.block,nesting+", 'block': ".length)
              + " }";
        }
        case (nd: ClassBlock) {
            var str =
                "{ 'ast_class': 'ClassBlock'"
              + indent(nesting) + ", 'name': " + encodeName (nd.name,nesting+", 'name': ".length)
              + indent(nesting) + ", 'block': " + block (nd.block,nesting+", 'block': ".length)
              + " }";
        }
        case (nd: TryStmt) {
            var str =
                "{ 'ast_class': 'TryStmt'"
              + indent(nesting)
              + ", 'block': "
              + block (nd.block,nesting+", 'block': ".length)
              + indent(nesting)
              + ", 'catches': [ "
              + catches (nd.catches,nesting+", 'catches': ".length) + " ]"
              + indent(nesting)
              + ", 'finallyBlock': "
              + "null"  // for now: blockOpt (nd.finallyBlock,nesting+", 'finallyBlock': ".length)
              + " }";
        }
        case (x: *) {
            throw "error stmt " + nd;
        }
        }
        }
        exit ("stmt");
        return str;
    }

    function catches (nd /*: CATCHES*/, nesting: int = 0)
        : string {
        enter ("Encode::catches ", nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + catchClause (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode::catches ",str);
        return str;
    }

    function catchClause (nd : CATCH, nesting: int = 0)
        : string {
        enter ("Encode::catchClause ");

            var str =
                "{ 'ast_class': 'Catch'"
              + indent(nesting)
              + ", 'param': "
              + head (nd.param,nesting+", 'param': ".length)
              + indent (nesting)
              + ", 'block': "
              + block (nd.block,nesting+", 'block': ".length)
              + " }";

        exit ("Encode::catchClause ",str);
        return str;
    }

    function cases (nd /*: CASES*/, nesting: int = 0)
        : string {
        enter ("Encode::cases ", nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + caseElement (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode::cases ",str);
        return str;
    }

    function caseElement (nd : CASE, nesting: int = 0)
        : string {
        enter ("Encode::caseElement ");

            var str =
                "{ 'ast_class': 'Case'"
              + indent(nesting)
              + ", 'expr': "
              + exprOpt (nd.expr,nesting+", 'expr': ".length)
              + indent (nesting)
              + ", 'stmts': [ "
              + stmts (nd.stmts,nesting+", 'stmts': ".length)
              + " ] }";

        exit ("Encode::caseElement ",str);
        return str;
    }

    function exprs (nd /*: [EXPR]*/, nesting: int = 0)
        : string {
        enter ("Encode::exprs nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str
                + encodeExpr (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("Encode::exprs ",str);
        return str;
    }

    function exprOpt (nd : EXPR?, nesting: int = 0)
        : string {
        enter ("Encode::exprOpt");
        var str = "";
        if( nd === null ) {
            var str = "null";
        }
        else {
            var str = encodeExpr (nd,nesting);
        }
        exit ("Encode::exprOpt ",str);
        return str;
    }

    function identOpt (nd : IDENT?, nesting: int = 0)
        : string {
        enter ("Encode::identOpt");
        var str = "";
        if( nd === null ) {
            var str = "null";
        }
        else {
            var str = nd;
        }
        exit ("Encode::identOpt ",str);
        return str;
    }

    function encodeExpr (nd : EXPR, nesting: int = 0)
        : string {
        enter ("encodeExpr ",nd);
        var str = "";
        switch type (nd): EXPR {
        case (nd: LiteralExpr) {
            var str =
                "{ 'ast_class': 'LiteralExpr'"
              + indent(nesting)
              + ", 'literal': "
              + encodeLiteral (nd.literal,nesting+", 'literal': ".length)
              + " }";
        }
        case (nd: ListExpr) {
            var str =
                "{ 'ast_class': 'ListExpr'"
              + indent(nesting)
              + ", 'exprs': [ "
              + exprs (nd.exprs,nesting+", 'exprs': [ ".length)
              + " ] }";
        }
        case (nd: CallExpr) {
            var str =
                "{ 'ast_class': 'CallExpr'"
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'args': [ "
              + exprs (nd.args,nesting+", 'args': [ ".length)
              + " ] }";
        }
        case (nd: NewExpr) {
            enter ("newexpr");
            var str =
                "{ 'ast_class': 'NewExpr'"
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + indent(nesting)
              + ", 'args': [ "
              + exprs (nd.args,nesting+", 'args': [ ".length)
              + " ] }";
            exit ("newexpr");
        }
        case (nd: LexicalRef) {
            var str =
                "{ 'ast_class': 'LexicalRef'"
              + indent(nesting)
              + ", 'ident': "
              + encodeIdentExpr (nd.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (nd: ObjectRef) {
            var str =
                "{ 'ast_class': 'ObjectRef'"
              + indent(nesting)
              + ", 'base': "
              + encodeExpr (nd.base,nesting+", 'base': ".length)
              + indent(nesting)
              + ", 'ident': "
              + encodeIdentExpr (nd.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (nd: SetExpr) {
            var str =
                "{ 'ast_class': 'SetExpr'"
              + indent(nesting)
              + ", 'op': "
              + encodeAssignOp (nd.op,nesting,", 'op': ".length)
              + indent(nesting)
              + ", 'le': "
              + encodeExpr (nd.le,nesting+", 'le': ".length)
              + indent(nesting)
              + ", 're': "
              + encodeExpr (nd.re,nesting+", 're': ".length)
              + " }";
        }
        case (nd: BinaryExpr) {
            var str =
                "{ 'ast_class': 'BinaryExpr'"
              + indent(nesting)
              + ", 'op': "
              + encodeBinOp (nd.op,nesting,", 'op': ".length)
              + indent(nesting)
              + ", 'e1': "
              + encodeExpr (nd.e1,nesting+", 'e1': ".length)
              + indent(nesting)
              + ", 'e2': "
              + encodeExpr (nd.e2,nesting+", 'e2': ".length)
              + " }";
        }
        case (nd: UnaryExpr) {
            var str =
                "{ 'ast_class': 'UnaryExpr'"
              + indent(nesting)
              + ", 'op': "
              + encodeUnOp (nd.op,nesting,", 'op': ".length)
              + indent(nesting)
              + ", 'e1': "
              + encodeExpr (nd.e1,nesting+", 'e1': ".length)
              + " }";
        }
        case (nd: InitExpr) {
            var str =
                "{ 'ast_class': 'InitExpr'"
              + indent(nesting)
              + ", 'target': "
              + encodeInitTarget (nd.target,nesting+", 'target': ".length)
              + indent(nesting)
              + ", 'head': "
              + head (nd.head,nesting+", 'head': ".length)
              + indent(nesting)
              + ", 'inits': [ "
              + encodeInits (nd.inits,nesting+", 'inits': [ ".length)
              + " ] }";
        }
        case (nd: LetExpr) {
            var str =
                "{ 'ast_class': 'LetExpr'"
              + indent(nesting)
              + ", 'head': "
              + head (nd.head,nesting+", 'head': ".length)
              + indent(nesting)
              + ", 'expr': "
              + encodeExpr (nd.expr,nesting+", 'expr': ".length)
              + " }";
        }
        case (nd: GetTemp) {
            var str =
                "{ 'ast_class': 'GetTemp'"
              + indent(nesting)
              + ", 'n': "
              + nd.n
              + " }";
        }
        case (nd: GetParam) {
            var str =
                "{ 'ast_class': 'GetParam'"
              + indent(nesting)
              + ", 'n': "
              + nd.n
              + " }";
        }
        case (x: *) {
            var str = "**unknown node in encodeExpr "+nd+"**";
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
                "{ 'ast_class': 'Identifier'"
              + indent(nesting)
              + ", 'ident': "
              + "'" + ie.ident + "'"
              + indent(nesting)
              + ", 'nss': [ "
              + encodeNamespacesList (ie.nss,nesting+", 'nss': [ ".length)
              + " ] }";
        }
        case (ie: QualifiedIdentifier) {
            var str =
                "{ 'ast_class': 'QualifiedIdentifier'"
              + indent(nesting) + ", 'qual': " + encodeExpr (nd.qual,nesting+", 'qual':".length)
              + indent(nesting) + ", 'ident': '" + ie.ident + "'"
              + " }";
        }
        case (ie: UnresolvedPath) {
            var str =
                "{ 'ast_class': 'UnresolvedPath'"
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

    function encodeTypeExprs (nd /*: [TYPE_EXPR]*/, nesting: int = 0)
        : string {
        enter ("encodeTypeExprs nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + encodeTypeExpr (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("encodeTypeExprs ",str);
        return str;
    }

    function encodeTypeExpr (nd : TYPE_EXPR, nesting: int = 0)
        : string {
        enter ("encodeTypeExpr ",nd);
        var str = "";
        switch type (nd): TYPE_EXPR {
        case (nd: TypeName) {
            var str =
                "{ 'ast_class': 'TypeName'"
              + indent(nesting)
              + ", 'ident': "
              + encodeIdentExpr (nd.ident,nesting+", 'ident': ".length)
              + " }";
        }
        case (nd: SpecialType) {
            var str =
                "{ 'ast_class': 'SpecialType'"
              + indent(nesting)
              + ", 'kind': "
                + encodeSpecialTypeKind (nd.kind,nesting+", 'kind': ".length)
              + " }";
        }
        case (nd: UnionType) {
            var str =
                "{ 'ast_class': 'UnionType'"
              + indent(nesting)
              + ", 'types': [ "
              + encodeTypeExprs (nd.types,nesting+", 'types': [ ".length)
              + " ] }";
        }
        case (nd: ObjectType) {
            var str =
                "{ 'ast_class': 'ObjectType'"
              + indent(nesting)
              + ", 'fields': [ "
              + encodeFieldTypes (nd.fields,nesting+", 'fields': [ ".length)
              + " ] }";
        }
        case (nd: ArrayType) {
            var str =
                "{ 'ast_class': 'ArrayType'"
              + indent(nesting)
              + ", 'types': [ "
              + encodeTypeExprs (nd.types,nesting+", 'types': [ ".length)
              + " ] }";
        }
        case (nd: *) {
            var str = "** unknown type in encodeTypeExpr: " + nd;
        }
        }
        exit ("encodeTypeExpr ",str);
        return str;
    }

    function encodeSpecialTypeKind (nd : SPECIAL_TYPE_KIND, nesting: int = 0)
        : string {
        enter ("encodeSpecialTypeKind")
        var str = "";
        switch type (nd) {
        case (op: AnyType) {
            var str = "AnyType";
        }
        case (op: NullType) {
            var str = "NullType";
        }
        case (op: UndefinedType) {
            var str = "UndefinedType";
        }
        case (op: VoidType) {
            var str = "VoidType";
        }
        case (x: *) {
            throw "internalError: encodeLiteral";
        }
        }
        var str = "{ 'ast_class': '" + str + "' }"
        exit ("encodeSpecialTypeKind ",str);
        return str;
    }

    function encodeFieldTypes (nd /*: [FIELD_TYPE]*/, nesting: int = 0)
        : string {
        enter ("encodeFieldTypes nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str
                + encodeFieldType (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("encodeFieldTypes ",str);
        return str;
    }

    function encodeFieldType (nd : FIELD_TYPE, nesting: int = 0)
    {
        enter ("encode FieldType");

        var str = "";
            var str =
                "{ 'ast_class': 'FieldType'"
              + indent(nesting) + ", 'ident': '" + nd.ident + "'"
              + indent(nesting) + ", 'type': " + encodeTypeExpr (nd.type,nesting+", 'type': ".length)
                + " }";

        exit ("encodeFieldType");
        return str;
    }

    function encodeLiteral (nd : LITERAL, nesting: int = 0)
        : string {
        enter ("encodeLiteral")
        var str = "";
        switch type (nd): LITERAL {
        case (nd: LiteralString) {
            var str =
                "{ 'ast_class': 'LiteralString'"
              + indent(nesting)
              + ", 'strValue': '"
              + nd.strValue
              + "' }";
        }
        case (nd: LiteralDecimal) {
            var str =
                "{ 'ast_class': 'LiteralDecimal'"
              + indent(nesting)
              + ", 'decimalValue': '"
              + nd.decimalValue
              + "' }";
        }
        case (nd: LiteralNamespace) {
            var str =
                "{ 'ast_class': 'LiteralNamespace'"
              + indent(nesting)
              + ", 'namespaceValue': "
              + encodeNamespace (nd.namespaceValue,nesting+", 'namespaceValue': ".length)
              + " }";
        }
        case (nd: LiteralBoolean) {
            var str =
                "{ 'ast_class': 'LiteralBoolean'"
              + indent(nesting)
              + ", 'booleanValue': "
              + nd.booleanValue
              + " }";
        }
        case (nd: LiteralNull) {
            var str =
                "{ 'ast_class': 'LiteralNull'"
              + " }";
        }
        case (nd: LiteralUndefined) {
            var str =
                "{ 'ast_class': 'LiteralUndefined'"
              + " }";
        }
        case (x: *) {
            throw "internalError: encodeLiteral "+nd;
        }
        }
        exit ("encodeLiteral ",str);
        return str;
    }

    function encodeNamespacesList (nd /*: [[NAMESPACE]]*/, nesting: int = 0)
        : string {
        enter ("encodeNamespacesList nd.length=",nd.length);

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
            var str = " [ "
                + encodeNamespaces (nd[0],nesting+" [ ".length)
                + " ]"
                + indent(nesting-2)
                + ", "
                + encodeNamespacesList (nd.slice (1,nd.length), nesting);
        }

        exit ("encodeNamespacesList ",str);
        return str;
    }

    function encodeNamespaces (nd /*: [NAMESPACE]*/, nesting: int = 0)
        : string {
        enter ("encodeNamespaces nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str
                + encodeNamespace (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("encodeNamespaces ",str);
        return str;
    }

    function encodeNamespace (nd : NAMESPACE, nesting: int = 0)
        : string {
        enter ("encodeNamespace ",nesting)
        var str = "";
        switch type (nd): NAMESPACE {
        case (nd: PublicNamespace) {
            var str =
                "{ 'ast_class': 'PublicNamespace'"
              + indent(nesting) + ", 'name': '" + nd.name
              + "' }";
        }
        case (nd: PrivateNamespace) {
            var str =
                "{ 'ast_class': 'PrivateNamespace'"
              + indent(nesting) + ", 'name': '" + nd.name
              + "' }";
        }
        case (nd: ProtectedNamespace) {
            var str =
                "{ 'ast_class': 'ProtectedNamespace'"
              + indent(nesting) + ", 'name': '" + nd.name
              + "' }";
        }
        case (nd: InternalNamespace) {
            var str =
                "{ 'ast_class': 'InternalNamespace'"
              + indent(nesting) + ", 'name': '" + nd.name
              + "' }";
        }
        case (nd: AnonymousNamespace) {
            var str =
                "{ 'ast_class': 'AnonymousNamespace'"
              + indent(nesting) + ", 'name': '" + nd.name
              + "' }";
        }
        case (nd: IntrinsicNamespace) {
            var str =
                "{ 'ast_class': 'IntrinsicNamespace'"
              + " }";
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
                "{ 'ns': "+encodeNamespace (nd.ns,nesting+"{ 'ns': ".length)
              + indent(nesting) + ", 'id': '"+ nd.id+"' }";

        exit ("encodeName ",str);
        return str;
    }

    function encodeAssignOp (nd : ASSIGNOP, nesting: int = 0)
        : string {
        enter ("encodeAssignOp")
        var str = "";
        switch type (nd): ASSIGNOP {
        case (op: Assign) {
            var str = "Assign";
        }
        case (op: AssignPlus) {
            var str = "AssignPlus";
        }
        case (op: AssignMinus) {
            var str = "AssignMinus";
        }
        case (op: AssignTimes) {
            var str = "AssignTimes";
        }
        case (op: AssignDivide) {
            var str = "AssignDivide";
        }
        case (op: AssignRemainder) {
            var str = "AssignRemainder";
        }
        case (op: AssignLeftShift) {
            var str = "AssignLeftShift";
        }
        case (op: AssignRightShift) {
            var str = "AssignRightShift";
        }
        case (op: AssignRightShiftUnsigned) {
            var str = "AssignRightShiftUnsigned";
        }
        case (op: AssignBitwiseAnd) {
            var str = "AssignBitwiseAnd";
        }
        case (op: AssignBitwiseOr) {
            var str = "AssignBitwiseOr";
        }
        case (op: AssignBitwiseXor) {
            var str = "AssignBitwiseXor";
        }
        case (op: AssignLogicalAnd) {
            var str = "AssignLogicalAnd";
        }
        case (op: AssignLogicalOr) {
            var str = "AssignLogicalOr";
        }
        case (x: *) {
            throw "internalError: encodeLiteral";
        }
        }
        var str = "{ 'ast_class': '" + str + "' }"
        exit ("encodeAssignOp ",str);
        return str;
    }

    function encodeBinOp (nd : BINOP, nesting: int = 0)
        : string {
        enter ("encodeBinOp")
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
        var str = "{ 'ast_class': '" + str + "' }"
        exit ("encodeBinOp ",str);
        return str;
    }

    function encodeUnOp (nd : UNOP, nesting: int = 0)
        : string {
        enter ("encodeUnOp")
        var str = "";
        switch type (nd): UNOP {
        case (op: Delete) {
            var str = "Delete";
        }
        case (op: Void) {
            var str = "Void";
        }
        case (op: Typeof) {
            var str = "Typeof";
        }
        case (op: PreIncr) {
            var str = "PreIncr";
        }
        case (op: PreDecr) {
            var str = "PreDecr";
        }
        case (op: PostIncr) {
            var str = "PostIncr";
        }
        case (op: PostDecr) {
            var str = "PostDecr";
        }
        case (op: UnaryPlus) {
            var str = "UnaryPlus";
        }
        case (op: UnaryMinus) {
            var str = "UnaryMinus";
        }
        case (op: BitwiseNot) {
            var str = "BitwiseNot";
        }
        case (op: LogicalNot) {
            var str = "LogicalNot";
        }
        case (op: Type) {
            var str = "Type";
        }
        case (x: *) {
            throw "internalError: encodeUnOp";
        }
        }
        var str = "{ 'ast_class': '" + str + "' }"
        exit ("encodeUnOp ",str);
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

    function encodeFunc (nd : FUNC, nesting: int = 0)
        : string {
        enter ("encodeFunc");

        var str =
            "{ 'ast_class': 'Func'"
          + indent(nesting) + ", 'name': " + encodeFuncName (nd.name,nesting+", 'name': ".length)
          + indent(nesting) + ", 'isNative': " + nd.isNative
          + indent(nesting) + ", 'block': " + block (nd.block,nesting+", 'block': ".length)
          + indent(nesting) + ", 'params': " + head (nd.params,nesting+", 'params': ".length)
          + indent(nesting) + ", 'vars': " + head (nd.vars,nesting+", 'vars': ".length)
          + indent(nesting) + ", 'defaults': [" + exprs (nd.defaults,nesting+", 'defaults': ".length) + " ]"
          + indent(nesting) + ", 'type': " + encodeTypeExpr (nd.type,nesting+", 'type': ".length)
          + " }";

        exit ("encodeFunc ",str);
        return str;
    }

    function encodeCls (nd : CLS, nesting: int = 0)
        : string {

        enter ("encodeCls ",nd);

        print ("cls ",nd.name.id);
        var str =
            "{ 'ast_class': 'Cls'"
          + indent(nesting) + ", 'name': " + encodeName (nd.name,nesting+", 'name': ".length)
          + indent(nesting) + ", 'baseName': " + encodeName (nd.baseName,nesting+", 'baseName': ".length)
          + indent(nesting) + ", 'interfaceNames': " + "[]" //encodeNames (nd.interfaceNames,nesting+", 'interfaceNames': ".length)
          + indent(nesting) + ", 'constructor': " + encodeCtor (nd.constructor,nesting+", 'constructor': ".length)
          + indent(nesting) + ", 'classHead': " + head (nd.classHead,nesting+", 'classHead': ".length)
          + indent(nesting) + ", 'instanceHead': " + head (nd.instanceHead,nesting+", 'instanceHead': ".length)
          + indent(nesting) + ", 'classType': " + encodeTypeExpr (nd.classType,nesting+", 'classType': ".length)
          + indent(nesting) + ", 'instanceType': " + encodeTypeExpr (nd.instanceType,nesting+", 'instanceType': ".length)
          + " }";

        exit ("encodeCls ",str);
        return str;
    }

    function encodeCtor (nd : CTOR, nesting: int = 0)
        : string {
        enter ("encodeCtor");

        var str =
            "{ 'ast_class': 'Ctor'"
          + indent(nesting) + ", 'settings': [ " + exprs (nd.settings,nesting+", 'settings': [ ".length) + "]"
          + indent(nesting) + ", 'superArgs': [ " + exprs (nd.superArgs,nesting+", 'superArgs': ".length) + " ]" 
          + indent(nesting) + ", 'func': " + encodeFunc (nd.func,nesting+", 'func': ".length)
          + " }";

        exit ("encodeCtor ",str);
        return str;
    }

    function encodeFuncName (nd /*: FUNC_NAME*/, nesting: int = 0)
        : string {
        enter ("encodeFuncName");

        var str =
            "{ 'kind': "
          + encodeFuncNameKind (nd.kind,nesting+"{ 'kind': ".length)
          + indent(nesting)
          + ", 'ident': '" + nd.ident + "'"
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
            var str = "** encodeFuncNameKind " + nd + "**";
        }
        }

        var str = "{ 'ast_class': '" + str + "' }"
        exit ("encodeFuncNameKind ",str);
        return str;
    }

    function encodeBindings (nd /*: [BINDING]*/, nesting: int = 0)
        : string {
        enter ("encodeBindings nd.length=",nd.length);

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str
                + encodeBinding (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("encodeBindings ",str);
        return str;
    }

    function encodeBinding (nd : BINDING, nesting: int = 0)
        : string {
        enter ("encodeBinding");

        var str =
            "{ 'ast_class': 'Binding'"
          + indent(nesting)
          + ", 'ident': "
          + encodeBindingIdent (nd.ident,nesting+", 'ident': ".length)
          + indent(nesting)
          + ", 'type': "
          + encodeTypeExpr (nd.type,nesting+", 'type': ".length)
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
            str = "{ 'ast_class': 'TempIdent'"
                + indent(nesting)
                + ", 'n': "
                + nd.n
                + " }";
        }
        case (nd:ParamIdent) {
            str = "{ 'ast_class': 'ParamIdent'"
                + indent(nesting)
                + ", 'n': "
                + nd.n
                + " }";
        }
        case (nd:PropIdent) {
            str = "{ 'ast_class': 'PropIdent'"
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

        var str = "";
        var len = nd.length;
        for (var i = 0; i < len; ++i) 
        {
            str = str 
                + encodeInitStep (nd[i], nesting)
                + indent (nesting-2)
                + ", ";
        }

        exit ("encodeInitSteps ",str);
        return str;
    }

    function encodeInitStep (nd : INIT_STEP, nesting: int = 0)
        : string {
        enter ("encodeInitStep");

        var str;
        switch type (nd) {
        case (nd:InitStep) {
        var str =
            "{ 'ast_class': 'InitStep'"
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

    function encodeInitTarget (nd : INIT_TARGET, nesting: int = 0)
        : string {
        enter ("encodeInitTarget")
        var str = "";
        switch type (nd) {
        case (op: HoistedInit) {
            var str = "HoistedInit";
        }
        case (op: LocalInit) {
            var str = "LocalInit";
        }
        case (op: PrototypeInit) {
            var str = "PrototypeInit";
        }
        case (op: InstanceInit) {
            var str = "InstanceInit";
        }
        case (x: *) {
            throw "internalError: encodeLiteral";
        }
        }
        var str = "{ 'ast_class': '" + str + "' }"
        exit ("encodeInitTarget ",str);
        return str;
    }

}
// } // end module
