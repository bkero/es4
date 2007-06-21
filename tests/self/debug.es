//module debug 
{
    use namespace intrinsic;

    namespace Debug;
    namespace Release;

    Debug function enter (s,a) {
        print (">> ", s, a);
    }

    Debug function exit (s,a) {
        print ("<< ", s, a);
    }

    Release function enter (s,a) { }
    Release function exit (s,a) { }
} // end module debug

