desugarSubPattern (null,null,null,0);

            function desugarSubPattern (p: PATTERN, t: Ast::TYPE_EXPR, e: Ast::EXPR, n: int) 
                : [Ast::FIXTURES, Ast::EXPR]
            {
                switch type (p) : PATTERN {
                case (p:IdentifierPattern) {
                    let nm = new Ast::PropName ({ns:ns,id:p.ident});
                    let fx = new Ast::ValFixture (t,ro);
                    var fxtrs = [[nm,fx]];
                    if (e !== null) {
                        var inits = [[nm,e]];
                    }
                    else {
                        var inits = [];
                    }
                    var expr = new Ast::InitExpr (it, new Ast::Head ([],[]), inits);
                }
                case (p:SimplePattern) {
                    if (e === null) throw "simple pattern without initializer";
                    var fxtrs = [];
                    if (it != null) { // we have an init target so must be an init
                        var ie = identExprFromExpr (p.expr);
                        var nm = cx.resolveIdentExpr (ie,it);
                        var expr = new Ast::InitExpr (it, new Ast::Head ([],[]), [[nm,e]]);
                    }
                    else {
                        var expr = new Ast::SetExpr (op,p.expr,e);
                    }
                }
                //case (p: (ArrayPattern, ObjectPattern)) {
                case (x: *) {
                    let tn = new Ast::TempName (n);
                    var fxtrs = [];
                    let exprs = [];
                    let ptrns = p.ptrns;
                    for (let i=0; i<ptrns.length; ++i) {
                        let sub = ptrns[i];
                        switch type (sub) {
                        case (pat: FieldPattern) {
                            var typ = new Ast::FieldTypeRef (t,sub.ident);
                            var exp = new Ast::ObjectRef (new Ast::GetTemp (n), sub.ident);
                            var pat = sub.ptrn;
                        }
                        case (pat: *) {
                            var typ = new Ast::ElementTypeRef (t,i);
                            var exp = new Ast::ObjectRef (new Ast::GetTemp (n), new Ast::Identifier (i,[[Ast::noNS]]));
                                      // FIXME what is the ns of a temp and how do we refer it
                            var pat = sub;
                        }
                        case (x: *) {
                            throw "internal error: desugarPattern " + p;
                        }
                        }

                        let [fx,ex] = desugarSubPattern (pat,typ,exp,n+1);
                        for (let j=0; j<fx.length; ++j) fxtrs.push(fx[j]);
                        exprs.push(ex);
                    }
                    let head = new Ast::Head ([[tn,new Ast::ValFixture (Ast::anyType,false)]],[new Ast::InitExpr (Ast::letInit,new Ast::Head([],[]),[[tn,e]])]);
                    var expr = new Ast::LetExpr (head, new Ast::ListExpr (exprs));
                }
                }
                return [fxtrs,expr];
            }
