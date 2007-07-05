/* Utilities adapted to the FlashPlayer libraries */

package util
{
    public function explodeNumber(d) {
        import flash.utils.ByteArray;

        var ba = new ByteArray;
        ba.endian = "littleEndian";
        ba.writeDouble(d);
        ba.position = 0;
        return [ba.readByte(), ba.readByte(), ba.readByte(), ba.readByte(),
                ba.readByte(), ba.readByte(), ba.readByte(), ba.readByte()];
    }

    public function writeStringToFile(s, filename) {
        import avmplus.File;
        File.write(filename, s);
    }

    public function toUint(x) {
        return uint(x);
    }
}
