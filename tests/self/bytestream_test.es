package bytestream
{
    import util.*;

    public function runTests() {
        print("--------------------------------------------");
        print("Testing ABCByteStream");
        print("");
            
        var bytes = new ABCByteStream;
        
        bytes.uint8(0x0A);
        bytes.uint16(0x010B);
        bytes.int16(-2);
        bytes.int24(257);
        bytes.uint30(7);
        bytes.int30(128);
        bytes.int30(-128);
        bytes.int32(10);
        bytes.uint32(0x0fffabcd);
        bytes.float64(1.0);
        bytes.utf8("foo");
        var b2 = new ABCByteStream;

        b2.uint8(0x0A);
        b2.uint8(0xFA);
        bytes.byteStream(b2);

        var result = "0A 0B 01 FE FF 01 01 00 07 80 01 80 01 0A CD D7 FE 7F 00 00 00 00 00 00 FF 3F 66 6F 6F 0A FA".split(" ");
        dumpByteStream( bytes, result );
    }
}
