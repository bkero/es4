/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */

/*

This ast decoder takes JSON encoded objects and transforms them into
ast nodes. it should be possible to read ast_encoder generated asts,
decode them using the decoder here, and rewrite them with ast_encoder 
to get the same text form back.

*/

use namespace Release;
use namespace Ast;
use namespace intrinsic;
public namespace Decode;

{
    use default namespace Decode;

    function program (ob) 
        : PROGRAM
    {
        enter ("Decode::program ", ob.ast_class);

        var nd1 = packages (ob.packages);
        var nd2 = block (ob.block);
        var nd3 = head (ob.head);

        exit ("Decode::program ");
        return new Program (nd1,nd2,nd3);
    }

    function packages (obj) 
        : PACKAGES
    {
        enter ("Decode::packages");

        // FIXME todo

        exit ("Decode::packages");
        return [];
    }

    function block (ob) 
        : BLOCK
    {
        enter ("Decode::block ",ob.ast_class);

        var nd1 = head (ob.head);
        var nd2 = stmts (ob.stmts);

        exit ("Decode::block");
        return new Block (nd1,nd2);
    }

    function blockOpt (ob) 
        : BLOCK?
    {
        enter ("Decode::blockOpt ", ob);

        if (ob === null) {
            var nd1 = null;
        }
        else {
            var nd1 = block (ob);
        }

        exit ("Decode::blockOpt");
        return nd1;
    }

    function head (ob) 
        : HEAD
    {
        enter ("Decode::head " + ob.fixtures);

        var nd1 = fixtures (ob.fixtures);
        var nd2 = exprs (ob.exprs);

        exit ("Decode::head");
        return new Head (nd1,nd2);
    }

    function fixtures (ob) 
        : FIXTURES
    {
        enter ("Decode::fixtures ", ob.length);

        var nd1 = [];
        for (var i = 0; i < ob.length; ++i) {
            var nd = fixtureBinding (ob[i]);
            nd1.push (nd);
        }

        exit ("Decode::fixtures");
        return nd1;
    }

    function fixtureBinding (ob) 
        : FIXTURE_BINDING
    {
        enter ("Decode::fixtureBinding ", ob.length);

        var nd1 = fixtureName (ob[0]);
        var nd2 = fixture (ob[1]);
        var ndx = [nd1,nd2];

        exit ("Decode::fixtureBinding");
        return ndx;
    }

    function fixtureName (ob) 
        : FIXTURE_NAME
    {
        enter ("Decode::fixtureName ", ob.ast_class);

        switch (ob.ast_class) {
        case 'TempName':
            let index = ob.index;
            var ndx = new Ast::TempName (index);
            break;
        case 'PropName':
            var nd1 = name (ob.name);
            var ndx = new Ast::PropName (nd1);
            break;
        default:
            throw "error Decode::fixtureName " + ob.ast_class;
        }

        exit ("Decode::fixtureName");
        return ndx;
    }

    function fixture (ob) 
        : FIXTURE
    {
        enter ("Decode::fixture ", ob.ast_class);

        switch (ob.ast_class) {
        case 'NamespaceFixture':
            let nd1 = namespace (ob.ns);
            var ndx = new NamespaceFixture (nd1);
            break;
        case 'ClassFixture':
            let nd1 = cls (ob.cls);
            var ndx = new ClassFixture (nd1);
            break;
        case 'TypeFixture':
            let nd1 = typeExpr (ob.type);
            var ndx = new TypeFixture (nd1);
            break;
        case 'MethodFixture':
            let nd1 = func (ob.func);
            let nd2 = typeExpr (ob.type);
            let nd3 = ob.isReadOnly;
            let nd4 = ob.isOverride;
            let nd5 = ob.isFinal;
            var ndx = new MethodFixture (nd1,nd2,nd3,nd4,nd5);
            break;
        case 'ValFixture':
            let nd1 = typeExpr (ob.type);
            let nd2 = ob.isReadOnly;
            var ndx = new ValFixture (nd1,nd2);
            break;
        case 'VirtualValFixture':
        case 'InterfaceFixture':
        case 'TypeVarFixture':
        default:
            throw "error Decode::fixture " + ob.ast_class;
        }

        exit ("Decode::fixture");
        return ndx;
    }

    function inits (ob) 
        : INITS
    {
        enter ("Decode::inits ", ob.length);

        var nd1 = [];
        for (var i = 0; i < ob.length; ++i) {
            var nd = initBinding (ob[i]);
            nd1.push (nd);
        }

        exit ("Decode::inits");
        return nd1;
    }

    function initBinding (ob) 
        : INIT_BINDING
    {
        enter ("Decode::initBinding ", ob.ast_class);

        var nd1 = fixtureName (ob[0]);
        var nd2 = expr (ob[1]);
        var ndx = [nd1,nd2];

        exit ("Decode::initBinding");
        return ndx;
    }

    function cls (ob)
        : CLS
    {
        enter ("Decode::cls ", ob.ast_class);

        switch (ob.ast_class) {
        case 'Cls':
            var nd1 = name (ob.name);
            var nd2 = name (ob.baseName);
            var nd3 = names (ob.interfaceNames);
            var nd4 = ctor (ob.constructor);
            var nd5 = head (ob.classHead);
            var nd6 = head (ob.instanceHead);
            var nd7 = typeExpr (ob.classType);
            var nd8 = typeExpr (ob.instanceType);
            var ndx = new Cls (nd1,nd2,nd3,nd4,nd5,nd6,nd7,nd8);
            break;
        default:
            throw "error: Decode::cls " + ob.ast_class;
        }

        exit ("Decode::cls");
        return ndx;
    }

    function name (ob)
        : NAME
    {
        enter ("Decode::name ", ob.id);

        let nd1 = namespace (ob.ns);
        var ndx = {ns:nd1,id:ob.id};

        exit ("Decode::name");
        return ndx;
    }

    function names (ob) 
        : NAMES
    {
        enter ("Decode::names ", ob.length);

        var nd1 = [];
        for (var i = 0; i < ob.length; ++i) {
            var nd = name (ob[i]);
            nd1.push (nd);
        }

        exit ("Decode::names");
        return nd1;
    }

    function ctor (ob)
        : CTOR
    {
        enter ("Decode::ctor ", ob.ast_class);

        let nd1 = exprs (ob.settings);
        let nd2 = exprs (ob.superArgs);
        let nd3 = func (ob.func);
        let ndx = new Ctor (nd1,nd2,nd3);

        exit ("Decode::ctor");
        return ndx;
    }

    function func (ob)
        : FUNC
    {
        enter ("Decode::func ", ob.ast_class);

        let nd1 = funcName (ob.name);
        let nd2 = ob.isNative;
        let nd3 = block (ob.block);
        let nd4 = head (ob.params);
        let nd5 = head (ob.vars);
        let nd6 = exprs (ob.defaults);
        let nd7 = typeExpr (ob.type);
        let ndx = new Func (nd1,nd2,nd3,nd4,nd5,nd6,nd7);

        exit ("Decode::func");
        return ndx;
    }

    function funcName (ob)
        : FUNC_NAME
    {
        enter ("Decode::funcName ");

        let nd1 = funcNameKind (ob.kind);
        let nd2 = ob.ident;
        let ndx = {kind:nd1, ident:nd2};

        exit ("Decode::funcName");
        return ndx;
    }

    function funcNameKind (ob)
        : FUNC_NAME_KIND
    {
        enter ("Decode::funcNameKind ", ob.ast_class);

        switch (ob.ast_class) {
        case 'Ordinary':
            var ndx = new Ordinary;
            break;
        case 'Get':
            var ndx = new Get;
            break;
        case 'Set':
            var ndx = new Set;
            break;
        default:
            throw "error Decoder::funcNameKind " + ob;
        }

        exit ("Decode::funcNameKind");
        return ndx;
    }

    function stmts (ob) 
        : STMTS
    {
        enter ("Decode::stmts ", ob.length);

        var nd1 = [];
        for (var i = 0; i < ob.length; ++i) {
            var nd = stmt (ob[i]);
            nd1.push (nd);
        }

        exit ("Decode::stmts");
        return nd1;
    }

    function stmtOpt (ob) 
        : STMT
    {
        enter ("Decode::stmtOpt ", ob);

        if (ob !== null) {
            var nd1 = stmt (ob);
        }
        else {
            var nd1 = null;
        }

        exit ("Decode::stmtOpt ");
        return nd1;
    }

    function stmt (ob) 
        : STMT
    {
        enter ("Decode::stmt ", ob.ast_class);

        switch (ob.ast_class) {
        case 'EmptyStmt':
            var ndx = new EmptyStmt ();
            break;
        case 'ExprStmt':
            var nd1 = expr (ob.expr);
            var ndx = new ExprStmt (nd1);
            break;
        case 'ClassBlock':
            var nd1 = name (ob.name);
            let nd2 = block (ob.block);
            var ndx = new ClassBlock (nd1,nd2);
            break;
        case 'ThrowStmt':
            let nd1 = expr (ob.expr);
            var ndx = new ThrowStmt (nd1);
            break;
        case 'ReturnStmt':
            let nd1 = exprOpt (ob.expr);
            var ndx = new ReturnStmt (nd1);
            break;
        case 'BreakStmt':
            var ndx = new BreakStmt (ob.ident);
            break;
        case 'ContinueStmt':
            var ndx = new ContinueStmt (ob.ident);
            break;
        case 'BlockStmt':
            let nd1 = block (ob.block);
            var ndx = new BlockStmt (nd1);
            break;
        case 'LabeledStmt':
            let nd1 = ob.label;
            let nd2 = stmt (ob.stmt);
            var ndx = new LabeledStmt (nd1,nd2);
            break;
        case 'LetStmt':
            let nd1 = block (ob.block);
            var ndx = new LetStmt (nd1);
            break;
        case 'WhileStmt':
            let nd1 = expr (ob.expr);
            let nd2 = stmt (ob.stmt);
            let nd3 = idents (ob.labels);
            var ndx = new WhileStmt (nd1,nd2,nd3);
            break;
        case 'DoWhileStmt':
            let nd1 = expr (ob.expr);
            let nd2 = stmt (ob.stmt);
            let nd3 = idents (ob.labels);
            var ndx = new WhileStmt (nd1,nd2,nd3);
            break;
        case 'ForStmt':
            let nd1 = head (ob.vars);
            let nd2 = exprOpt (ob.init);
            let nd3 = exprOpt (ob.cond);
            let nd4 = exprOpt (ob.incr);
            let nd5 = stmt (ob.stmt);
            let nd6 = idents (ob.labels);
            var ndx = new ForStmt (nd1,nd2,nd3,nd4,nd5,nd6);
            break;
        case 'IfStmt':
            let nd1 = expr (ob.expr);
            let nd2 = stmt (ob.then);
            let nd3 = stmtOpt (ob.elseOpt);
            var ndx = new IfStmt (nd1,nd2,nd3);
            break;
        case 'WithStmt':
            let nd1 = expr (ob.expr);
            let nd2 = stmt (ob.stmt);
            var ndx = new WithStmt (nd1,nd2);
            break;
        case 'SwitchStmt':
            let nd1 = expr (ob.expr);
            let nd2 = cases (ob.cases);
            let nd3 = cases (ob.labels);
            var ndx = new SwitchStmt (nd1,nd2,nd3);
            break;
        case 'TryStmt':
            let nd1 = block (ob.block);
            let nd2 = catches (ob.catches);
            let nd3 = blockOpt (ob.finallyBlock);
            var ndx = new TryStmt (nd1,nd2,nd3);
            break;
        case 'SwitchTypeStmt':
            let nd1 = expr (ob.expr);
            let nd2 = typeExpr (ob.type);
            let nd3 = catches (ob.cases);
            var ndx = new SwitchTypeStmt (nd1,nd2,nd3);
            break;
        default:
            throw "error Decoder::stmt " + ob.ast_class;
        }

        exit ("Decode::stmt");
        return ndx;
    }

    function cases (ob) 
        : CASES
    {
        enter ("Decode::cases ", ob.length);

        var nd1 = [];
        for (var i = 0; i < ob.length; ++i) {
            var nd = switchCase (ob[i]);
            nd1.push (nd);
        }

        exit ("Decode::cases");
        return nd1;
    }

    function switchCase (ob) 
        : CASE
    {
        enter ("Decode::switchCase ", ob.ast_class);

        let nd1 = exprOpt (ob.expr);
        let nd2 = stmts (ob.stmts);
        var ndx = new Case (nd1,nd2);

        exit ("Decode::switchCase");
        return ndx;
    }

    function catches (ob) 
        : CATCHES
    {
        enter ("Decode::catches ", ob.length);

        var nd1 = [];
        for (var i = 0; i < ob.length; ++i) {
            var nd = catchClause (ob[i]);
            nd1.push (nd);
        }

        exit ("Decode::catches");
        return nd1;
    }

    function catchClause (ob) 
        : CATCH
    {
        enter ("Decode::catchClause ", ob.ast_class);

        let nd1 = head (ob.param);
        let nd2 = block (ob.block);
        var ndx = new Catch (nd1,nd2);

        exit ("Decode::catchClause");
        return ndx;
    }

    function idents (ob) 
        : IDENTS
    {
        enter ("Decode::idents ", ob.length);

        var nd1 = [];
        for (var i = 0; i < ob.length; ++i) {
            var nd = ob[i];
            nd1.push (nd);
        }

        exit ("Decode::idents");
        return nd1;
    }

    function exprs (ob) 
        : EXPRS
    {
        enter ("Decode::exprs ", ob.length);

        var nd1 = [];
        for (var i = 0; i < ob.length; ++i) {
            var nd = expr (ob[i]);
            nd1.push (nd);
        }

        exit ("Decode::exprs");
        return nd1;
    }

    function exprOpt (ob) 
        : EXPR?
    {
        enter ("Decode::exprOpt ", ob);

        if (ob !== null) {
            var nd1 = expr (ob);
        }
        else {
            var nd1 = null;
        }

        exit ("Decode::exprOpt ");
        return nd1;
    }


    function expr (ob) 
        : EXPR
    {
        enter ("Decode::expr ", ob.ast_class);

        switch (ob.ast_class) {
        case 'ThisExpr':
            var ndx = new ThisExpr;
            break;
        case 'GetParam':
            var ndx = new GetParam (ob.n);
            break;
        case 'GetTemp':
            var ndx = new GetTemp (ob.n);
            break;
        case 'LiteralExpr':
            var nd1 = literal (ob.literal);
            var ndx = new LiteralExpr (nd1);
            break;
        case 'ListExpr':
            var nd1 = exprs (ob.exprs);
            var ndx = new ListExpr (nd1);
            break;
        case 'CallExpr':
            var nd1 = expr (ob.expr);
            var nd2 = exprs (ob.args);
            var ndx = new CallExpr (nd1,nd2);
            break;
        case 'NewExpr':
            var nd1 = expr (ob.expr);
            var nd2 = exprs (ob.args);
            var ndx = new NewExpr (nd1,nd2);
            break;
        case 'LexicalRef':
            var nd1 = identExpr (ob.ident);
            var ndx = new LexicalRef (nd1);
            break;
        case 'ObjectRef':
            let nd1 = expr (ob.base);
            let nd2 = identExpr (ob.ident);
            var ndx = new ObjectRef (nd1,nd2);
            break;
        case 'InitExpr':
            let nd1 = initTarget (ob.target);
            let nd2 = head (ob.head);
            let nd3 = inits (ob.inits);
            var ndx = new InitExpr (nd1,nd2,nd3);
            break;
        case 'LetExpr':
            let nd1 = head (ob.head);
            let nd2 = expr (ob.expr);
            var ndx = new LetExpr (nd1,nd2);
            break;
        case 'UnaryExpr':
            let nd1 = unOp (ob.op);
            let nd2 = expr (ob.e1);
            var ndx = new UnaryExpr (nd1,nd2);
            break;
        case 'BinaryTypeExpr':
            let nd1 = binTyOp (ob.op);
            let nd2 = expr (ob.e1);
            let nd3 = typeExpr (ob.e2);
            var ndx = new BinaryTypeExpr (nd1,nd2,nd3);
            break;
        case 'BinaryExpr':
            let nd1 = binOp (ob.op);
            let nd2 = expr (ob.e1);
            let nd3 = expr (ob.e2);
            var ndx = new BinaryExpr (nd1,nd2,nd3);
            break;
        case 'TernaryExpr':
            let nd1 = expr (ob.e1);
            let nd2 = expr (ob.e2);
            let nd3 = expr (ob.e3);
            var ndx = new TernaryExpr (nd1,nd2,nd3);
            break;
        case 'SetExpr':
            let nd1 = assignOp (ob.op);
            let nd2 = expr (ob.le);
            let nd3 = expr (ob.re);
            var ndx = new SetExpr (nd1,nd2,nd3);
            break;
        default:
            throw "error Decode::expr " + ob.ast_class;
        }

        exit ("Decode::expr");
        return ndx;
    }

    function initTarget (ob) 
        : INIT_TARGET
    {
        enter ("Decode::initTarget ", ob.ast_class);

        switch (ob.ast_class) {
        case 'VarInit':
            var ndx = new VarInit;
            break;
        case 'LetInit':
            var ndx = new LetInit;
            break;
        case 'PrototypeInit':
            var ndx = new PrototypeInit;
            break;
        case 'InstanceInit':
            var ndx = new InstanceInit;
            break;
        default:
            throw "error Decode::initTarget " + ob.ast_class;
        }

        exit ("Decode::initTarget");
        return ndx;
    }

    function assignOp (ob) 
        : ASSIGNOP
    {
        enter ("Decode::assignOp ", ob.ast_class);

        switch (ob.ast_class) {
        case 'Assign':
            var ndx = new Assign;
            break;
        case 'AssignPlus':
            var ndx = new AssignPlus;
            break;
        case 'AssignMinus':
            var ndx = new AssignMinus;
            break;
        case 'AssignTimes':
            var ndx = new AssignTimes;
            break;
        case 'AssignDivide':
            var ndx = new AssignDivide;
            break;
        case 'AssignRemainder':
            var ndx = new AssignRemainder;
            break;
        case 'AssignLeftShift':
            var ndx = new AssignLeftShift;
            break;
        case 'AssignRightShift':
            var ndx = new AssignRightShift;
            break;
        case 'AssignRightShiftUnsigned':
            var ndx = new AssignRightShiftUnsigned;
            break;
        case 'AssignBitwiseAnd':
            var ndx = new AssignBitwiseAnd;
            break;
        case 'AssignBitwiseOr':
            var ndx = new AssignBitwiseOr;
            break;
        case 'AssignBitwiseXor':
            var ndx = new AssignBitwiseXor;
            break;
        case 'AssignLogicalAnd':
            var ndx = new AssignLogicalAnd;
            break;
        case 'AssignLogicalOr':
            var ndx = new AssignLogicalOr;
            break;
        default:
            throw "error Decode::assignOp " + ob.ast_class;
        }

        exit ("Decode::assignOp");
        return ndx;
    }

    function binOp (ob) 
        : BINOP
    {
        enter ("Decode::binOp ", ob.ast_class);

        switch (ob.ast_class) {
        case 'Plus':
            var ndx = new Plus;
            break;
        case 'Minus':
            var ndx = new Minus;
            break;
        case 'Times':
            var ndx = new Times;
            break;
        case 'Divide':
            var ndx = new Divide;
            break;
        case 'Remainder':
            var ndx = new Remainder;
            break;
        case 'LeftShift':
            var ndx = new LeftShift;
            break;
        case 'RightShift':
            var ndx = new RightShift;
            break;
        case 'RightShiftUnsigned':
            var ndx = new RightShiftUnsigned;
            break;
        case 'BitwiseAnd':
            var ndx = new BitwiseAnd;
            break;
        case 'BitwiseOr':
            var ndx = new BitwiseOr;
            break;
        case 'BitwiseXor':
            var ndx = new BitwiseXor;
            break;
        case 'LogicalAnd':
            var ndx = new LogicalAnd;
            break;
        case 'LogicalOr':
            var ndx = new LogicalOr;
            break;
        case 'InstanceOf':
            var ndx = new InstanceOf;
            break;
        case 'In':
            var ndx = new In;
            break;
        case 'Equal':
            var ndx = new Equal;
            break;
        case 'NotEqual':
            var ndx = new NotEqual;
            break;
        case 'StrictEqual':
            var ndx = new StrictEqual;
            break;
        case 'StrictNotEqual':
            var ndx = new StrictNotEqual;
            break;
        case 'Less':
            var ndx = new Less;
            break;
        case 'LessOrEqual':
            var ndx = new LessOrEqual;
            break;
        case 'Greater':
            var ndx = new Greater;
            break;
        case 'GreaterOrEqual':
            var ndx = new GreaterOrEqual;
            break;
        default:
            throw "error Decode::binOp " + ob.ast_class;
        }

        exit ("Decode::binOp");
        return ndx;
    }

    function binTyOp (ob) 
        : BINTYOP
    {
        enter ("Decode::binTyOp ", ob.ast_class);

        switch (ob.ast_class) {
        case 'CastOp':
            var ndx = new CastOp;
            break;
        case 'IsOp':
            var ndx = new IsOp;
            break;
        case 'ToOp':
            var ndx = new ToOp;
            break;
        default:
            throw "error Decode::binTyOp " + ob.ast_class;
        }

        exit ("Decode::binTyOp");
        return ndx;
    }

    function unOp (ob) 
        : UNOP
    {
        enter ("Decode::unOp ", ob.ast_class);

        switch (ob.ast_class) {
        case 'Delete':
            var ndx = new Delete;
            break;
        case 'Void':
            var ndx = new Void;
            break;
        case 'Typeof':
            var ndx = new Typeof;
            break;
        case 'PreIncr':
            var ndx = new PreIncr;
            break;
        case 'PreDecr':
            var ndx = new PreDecr;
            break;
        case 'PostIncr':
            var ndx = new PostIncr;
            break;
        case 'PostDecr':
            var ndx = new PostDecr;
            break;
        case 'UnaryPlus':
            var ndx = new UnaryPlus;
            break;
        case 'UnaryMinus':
            var ndx = new UnaryMinus;
            break;
        case 'BitwiseNot':
            var ndx = new BitwiseNot;
            break;
        case 'LogicalNot':
            var ndx = new LogicalNot;
            break;
        case 'Type':
            var ndx = new Type;
            break;
        default:
            throw "error Decode::unOp " + ob.ast_class;
        }

        exit ("Decode::unOp");
        return ndx;
    }

    function typeExprs (ob) 
        : TYPE_EXPRS
    {
        enter ("Decode::typeExprs ", ob.length);

        var nd1 = [];
        for (var i = 0; i < ob.length; ++i) {
            var nd = typeExpr (ob[i]);
            nd1.push (nd);
        }

        exit ("Decode::typeExprs");
        return nd1;
    }

    function typeExpr (ob) 
        : TYPE_EXPR
    {
        enter ("Decode::typeExpr ", ob.ast_class);

        switch (ob.ast_class) {
        case 'SpecialType':
            var nd1 = specialTypeKind (ob.kind);
            var ndx = new SpecialType (nd1);
            break;
        case 'UnionType':
            var nd1 = typeExprs (ob.types);
            var ndx = new UnionType (nd1);
            break;
        case 'ArrayType':
            var nd1 = typeExprs (ob.types);
            var ndx = new ArrayType (nd1);
            break;
        case 'ObjectType':
            var nd1 = fieldTypes (ob.fields);
            var ndx = new ObjectType (nd1);
            break;
        case 'AppType':
            var nd1 = typeExpr (ob.type);
            var nd2 = typeExprs (ob.args);
            var ndx = new AppType (nd1,nd2);
            break;
        case 'NullableType':
            var nd1 = typeExpr (ob.type);
            var nd2 = ob.isNullable;
            var ndx = new NullableType (nd1,nd2);
            break;
        case 'TypeName':
            var nd1 = identExpr (ob.ident);
            var ndx = new TypeName (nd1);
            break;
        case 'ElementTypeRef':
            var nd1 = typeExpr (ob.base);
            var nd2 = ob.index;
            var ndx = new ElementTypeRef (nd1,nd2);
            break;
        case 'FieldTypeRef':
            var nd1 = typeExpr (ob.base);
            var nd2 = identExpr (ob.ident);
            var ndx = new FieldTypeRef (nd1,nd2);
            break;
        default:
            throw "error Decode::typeExpr " + ob.ast_class;
        }

        exit ("Decode::typeExpr");
        return ndx;
    }

    function fieldTypes (ob) 
        : FIELD_TYPES
    {
        enter ("Decode::fieldTypes ", ob.length);

        var nd1 = [];
        for (var i = 0; i < ob.length; ++i) {
            var nd = fieldType (ob[i]);
            nd1.push (nd);
        }

        exit ("Decode::fieldTypes");
        return nd1;
    }

    function fieldType (ob) 
        : FIELD_TYPE
    {
        enter ("Decode::fieldType ", ob.ast_class);

        let nd1 = ob.ident;
        let nd2 = typeExpr (ob.type);
        var ndx = new FieldType (nd1,nd2);

        exit ("Decode::fieldType");
        return ndx;
    }

    function specialTypeKind (ob) 
        : SPECIAL_TYPE_KIND
    {
        enter ("Decode::specialTypeKind ", ob.ast_class);

        switch (ob.ast_class) {
        case 'AnyType':
            var ndx = new AnyType;
            break;
        case 'VoidType':
            var ndx = new VoidType;
            break;
        default:
            throw "error Decode::specialTypeKind " + ob.ast_class;
        }

        exit ("Decode::specialTypeKind");
        return ndx;
    }

    function identExpr (ob) 
        : IDENT_EXPR
    {
        enter ("Decode::identExpr ", ob.ast_class);

        switch (ob.ast_class) {
        case 'Identifier':
            var nd1 = ob.ident;
            var nd2 = namespaces (ob.nss);
            var ndx = new Identifier (nd1,nd2);
            break;
        case 'ExpressionIdentifier':
            var nd1 = expr (ob.expr);
            var nd2 = namespaces (ob.nss);
            var ndx = new ExpressionIdentifier (nd1,nd2);
            break;
        case 'QualifiedIdentifier':
            var nd1 = expr (ob.qual);
            var nd2 = ob.ident;
            var ndx = new QualifiedIdentifier (nd1,nd2);
            break;
        case 'ReservedNamespace':
            var nd1 = ob.ns;
            var nd2 = namespace (ob.ns);
            var ndx = new ReservedNamespace (nd1);
            break;
        default:
            throw "Decode::identExpr error";
        }

        exit ("Decode::identExpr");
        return ndx;
    }

    function namespaces (ob) 
        : NAMESPACES
    {
        enter ("Decode::namespaces ", ob.length);

        var nd1 = [];
        var ob1 = ob;
        for (var i = 0; i < ob1.length; ++i) {
            var ob2 = ob1[i];
            var nd2 = [];
            for (var j = 0; j < ob2.length; ++j) {
                var nd = namespace (ob2[j]);
                nd2.push (nd);
            }
            nd1.push (nd2);
        }

        exit ("Decode::namespaces");
        return nd1;
    }

    function namespace (ob) 
        : NAMESPACE
    {
        enter ("Decode::namespace ", ob.ast_class);

        switch (ob.ast_class) {
        case 'IntrinsicNamespace':
            var ndx = new IntrinsicNamespace ();
            break;
        case 'PrivateNamespace':
            var ndx = new PrivateNamespace (ob.name);
            break;
        case 'ProtectedNamespace':
            var ndx = new ProtectedNamespace (ob.name);
            break;
        case 'PublicNamespace':
            var ndx = new PublicNamespace (ob.name);
            break;
        case 'InternalNamespace':
            var ndx = new InternalNamespace (ob.name);
            break;
        case 'UserNamespace':
            var ndx = new UserNamespace (ob.name);
            break;
        case 'AnonymousNamespace':
            var ndx = new AnonymousNamespace (ob.name);
            break;
        case 'ImportNamespace':
        default:
            throw "Decode::namespace unimplemented " + ob;
            break;
        }

        exit ("Decode::namespace");
        return ndx;
    }

    function literal (ob) 
        : LITERAL
    {
        enter ("Decode::literal ", ob.ast_class);

        switch (ob.ast_class) {
        case 'LiteralNamespace':
            let nd1 = namespace (ob.namespaceValue);
            var ndx = new LiteralNamespace (nd1);
            break;
        case 'LiteralString':
            var ndx = new LiteralString (ob.strValue);
            break;
        case 'LiteralDecimal':
            var ndx = new LiteralDecimal (ob.decimalValue);
            break;
        case 'LiteralBoolean':
            var ndx = new LiteralBoolean (ob.booleanValue);
            break;
        case 'LiteralNull':
            var ndx = new LiteralNull;
            break;
        case 'LiteralUndefined':
            var ndx = new LiteralUndefined;
            break;
        case 'LiteralArray':
            var nd1 = exprs (ob.exprs);
            var nd2 = typeExpr (ob.type);
            var ndx = new LiteralArray (nd1,nd2);
            break;
        case 'LiteralObject':
            var nd1 = literalFields (ob.fields);
            var nd2 = typeExpr (ob.type);
            var ndx = new LiteralObject (nd1,nd2);
            break;
        default:
            throw "error Decode::literal " + ob.ast_class;
        }

        exit ("Decode::literal");
        return ndx;
    }

    function literalFields (ob) 
        : LITERAL_FIELDS
    {
        enter ("Decode::literalField ", ob.length);

        var nd1 = [];
        for (var i = 0; i < ob.length; ++i) {
            var nd = literalField (ob[i]);
            nd1.push (nd);
        }

        exit ("Decode::fieldTypes");
        return nd1;
    }

    function literalField (ob) 
        : LITERAL_FIELD
    {
        enter ("Decode::literalField ", ob.ast_class);

        let nd1 = varKind (ob.kind);
        let nd2 = identExpr (ob.ident);
        let nd3 = expr (ob.expr);
        var ndx = new LiteralField (nd1,nd2,nd3);

        exit ("Decode::literalField");
        return ndx;
    }

    function varKind (ob) 
        : VAR_DEFN_TAG
    {
        enter ("Decode::varKind ", ob.ast_class);

        switch (ob.ast_class) {
        case 'Const':
            var ndx = constTag;
            break;
        case 'Var':
            var ndx = varTag;
            break;
        case 'LetVar':
            var ndx = letVarTag;
            break;
        case 'LetConst':
            var ndx = letConstTag;
            break;
        default:
            throw "error Decode::varKind " + ob.ast_class;
        }

        exit ("Decode::varKind");
        return ndx;
    }
}
