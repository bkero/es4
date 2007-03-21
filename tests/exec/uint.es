use uint;

intrinsic::assert((0x9c9c9c9c & 0x35353535) == 0x14141414)
intrinsic::assert((0xffff << 16 | 0xffff) == 0xffffffff)
intrinsic::assert(0xffffffff > 0)
