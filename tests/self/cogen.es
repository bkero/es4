/* The structure ctx has the following fields:

      asm: emitter.Method
      lexical: Object 
      ...

   Think of it as a "multi-stack": it's a data structure that
   encapsulates several stacks, and the data structure is handled by
   extension, not update.

   But it uses the prototype chain extensively, so there is no
   structural type to describe it.

   It will tend to be shallow.

   In *practice* it may be faster to have instances with all the
   fields and copy the fields in when a new structure is
   created... depends on how big the structure becomes.
*/

function CTX() {}

function bindAsm(ctx, asm) {
    CTX.prototype = ctx;
    var v = new CTX();
    v.asm = asm;
    return v;
}

function bindWith(ctx, c) { ... }
function bindBreak(ctx, c) { ... }
function bindContinue(ctx, c) { ... }
function bindCatch(ctx, c) { ... }

var baseCTX = {
    bindAsm: bindAsm,
    bindWith: bindWith,
    bindBreak: bindBreak,
    bindContinue: bindContinue,
    bindCatch: bindCatch
};

function verifyProgram(e:ABCEmitter, p) {
    var program = e.newScript();
    var newctx = ctx.bindAsm(baseCTX, ctx.program.getInit());
    // for each fixture, define it in newctx
    // ...
    verifyStmt(newctx, p.body);
}

function verifyStmt(ctx, s) {
    if (s is WhileStmt)
        verifyWhile(ctx, w);
    else if (s is IfStmt)
        verifyIf(ctx, w);
}

function verifyIf(ctx, s) {
    var asm = ctx.asm;
    verifyExpr(ctx, s.expr);
    var L0 = asm.I_iffalse();
    verifyStmt(ctx, s.consequent);
    var L1 = asm.I_jump();
    asm.I_label(L0);
    verifyStmt(ctx, s.alternate);
    asm.I_label(L1);
}

function verifyWhile(ctx, s) {
    var asm = ctx.asm;
    var L0 = asm.I_jump();
    var L1 = asm.I_label();
    verifyStmt(ctx, s.body);
    asm.I_label(L0);
    verifyExpr(ctx, s.expr);
    asm.I_iftrue(L1);
}

function verifyExpr(ctx, e) {
    if (s is LiteralExpr)
        verifyLiteralExpr(ctx, e);
    else if (s is BinaryExpr)
        verifyBinaryExpr(ctx, e);
    else if (s is ObjectRef) 
        ...;
    else if (s is LexicalRef)
        ...;
    else if (s is NewExpr)
        ...;
    else if (s is CallExpr)
        ...;
    else
        throw "Unimplemented expression type";
}

function verifyLiteralExpr(ctx, e) {
    if (e is LiteralNull)
        ctx.asm.I_pushnull();
    else if (e is LiteralUndefined)
        ctx.asm.I_pushundefined();
    else if (e is LiteralInt) 
        ctx.asm.I_pushint(ctx.cp.int32(e.intValue));
    else if (e is LiteralUint)
        ctx.asm.I_pushuint(ctx.cp.uint32(e.uintValue));
    else if (e is LiteralBoolean) {
        if (e.booleanValue)
            ctx.asm.I_pushtrue();
        else
            ctx.asm.I_pushfalse();
    }
    else if (e is LiteralString)
        ctx.asm.I_pushstring(ctx.cp.stringUtf8(e.strValue));
    else
        throw "Unimplemented literal";
}
