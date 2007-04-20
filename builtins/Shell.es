package
{
    // non-standard helpers that a variety of tests in our testsuite use.

    intrinsic native function print(x);
    intrinsic native function assert(x);
    intrinsic native function typename(x);
    intrinsic native function inspect(x, depth);
    intrinsic native function proto(x);

    public const print = intrinsic::print;
    public var gVersion = 0;    
    public function version(v) {
        if (v) { 
            gVersion = v; 
        } 
        return gVersion; 
    }   
    public function toString() { return "[object global]" }

}