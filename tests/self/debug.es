namespace Debug;
namespace Release;

Debug function trace (s) {
    print (s);
}

Debug function enter (s,a="") {
    print (">> ", s, a);
}

Debug function exit (s,a="") {
    print ("<< ", s, a);
}

Debug function assert (e) {
    if (!e)
        throw "Assertion failed!";
}


Release function enter (s,a) { }
Release function exit (s,a) { }
Release function trace (s) { }
Release function assert (e) { }
