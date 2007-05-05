package p.q {}
package {
    use default namespace public;
    use namespace intrinsic;
//    use strict;
    import p.q.*
    class A extends String {
        override function get length() : uint undefined
    }
}