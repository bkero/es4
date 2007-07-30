class GenericMethod 
{
    function GenericMethod(types, preds, body) 
        : types=types
        , preds=preds
        , body=body
    {        
    }

    var types;  // type objects
    var preds;  // type predicates
    var body;   // body
}

class GenericFunction
{
    function GenericFunction(name, ...formals) 
        : name=name
        , formals=formals
    {
    }

    function addMethod(body, ...sign) {
        let types = sign.map(function(x) x[0]);
        let preds = sign.map(function(x) x[1]);
        let meth = new GenericMethod(types, preds, body);
        methods.push(meth);
        try {
            methods.sort(...);
        }
        catch (e) {
            // incommensurable
            // back out:
            methods.remove(meth);
            throw e;
        }

        x = new Function(...);
        x.__generic__ = this;
        return x;
    }

    var name;
    var formals;
    var methods = [];
}

