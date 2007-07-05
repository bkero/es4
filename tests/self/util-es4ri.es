/* Utilities adapted to the ECMAScript 4 reference implementation */

package util
{
    use namespace intrinsic;

    intrinsic native function explodeDouble(d:double, idx:uint);

    public function explodeNumber(d) {
        d = double(d);
        let hi = explodeDouble(d, 0u);
        let lo = explodeDouble(d, 1u);
        return [lo & 255, (lo >> 8) & 255, (lo >> 16) & 255, (lo >> 24) & 255,
                hi & 255, (hi >> 8) & 255, (hi >> 16) & 255, (hi >> 24) & 255];
    }

    intrinsic native function writeFile(s:string, filename:string);

    public function writeStringToFile(s, filename) {
        writeFile(string(s), string(filename));
    }

    public function toUint(x)
        uint(x);
}
