let x = {a:10, b:20}
intrinsic::assert("a" in x);
intrinsic::assert("b" in x);
intrinsic::assert(! ("z" in x));
