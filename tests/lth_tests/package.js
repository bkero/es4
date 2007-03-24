// Just see if the parser groks it
package com.opera.ecma4 
{
    public var x = "ecma4";
}
package com.opera.ecma3 
{
    public var x = "ecma3";
}
this.x;  // should be undefined
