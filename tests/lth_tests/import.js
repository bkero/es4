package com.opera.ecma4 
{
    public var x = "ecma4";
    public var y = "ecma4";
    public var z = "ecma4";
}
package com.opera.ecma3 
{
    public var x = "ecma3";  // present to ensure that it's not visible
}

{
    import com.opera.ecma4.*;
    let y = "ecma5"; // local var shadows imported vars with same name
    var z = "ecma5"; // import shadows hoisted 'var' with same name
    x + y + z;
}