// Anonymous packages are imported everywhere
package 
{
    public var z = "ecma4";
}
package foo
{
    public var x = z;
}
import foo.*;
x + z;
