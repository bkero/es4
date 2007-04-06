package
{
    // non-standard helpers that a variety of tests in our testsuite use.

    intrinsic native function print(x);
    intrinsic native function assert(x);
    intrinsic native function typename(x);
    public const print = intrinsic::print;
    public var gVersion = 0;    
    public function version(v) {
        if (v) { 
            gVersion = v; 
        } 
        return gVersion; 
    }   
}