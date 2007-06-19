/* Utilities adapted to the ECMAScript 4 reference implementation */

package util
{
    /*
    intrinsic native function explodeDouble(d:double);

    public function explodeDouble(d) // : [uint,uint,uint,uint,uint,uint,uint,uint,*]
        intrinsic::explodeDouble(double(d));
    */

    public function explodeNumber(d)
        [0,0,0,0,0,0,0,0];

    public function writeStringToFile(s, filename)
        print(s);

    public function toUint(x)
        intrinsic::uint(x)
}
