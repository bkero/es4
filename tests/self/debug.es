public namespace Debug;
public namespace Release;

Debug function trace (s) {
    print (s);
}

Debug function enter (s,a="") {
    print (">> ", s, a);
}

Debug function exit (s,a="") {
    print ("<< ", s, a);
}

Release function enter (s,a="") { }
Release function exit (s,a="") { }
Release function trace (s) { }
