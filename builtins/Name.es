/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Name" object
 */

use namespace intrinsic;

final class Name extends String {

    type NS = (Name, Namespace, string);
    type ID = (undefined, string);

    function Name(ns : NS, id : ID = undefined) {
        if (id is undefined) {
            if (ns is Name) {
                let n : Name = ns;
                identifier = n.identifier;
                qualifier = n.qualifier;
            } else {
                identifier = ns;
            }
        } else {
            qualifier = ns;
            identifier = id;
        }
    }
   
    meta static function invoke(ns : NS, id : ID = undefined) : Name
        new Name(ns, id);
   
    meta static function convert(v : *)
        Name(v);
   
    prototype function toString(this : Name)
        this.intrinsic::toString();
   
    intrinsic override function toString() : string {
        if (qualifier === null)
            return identifier;
        return qualifier + "::" + identifier;
    }
   
    prototype function valueOf(this : Name)
        this.intrinsic::valueOf();
   
    intrinsic override function valueOf() : string
        intrinsic::toString();
   
    public const qualifier  : Namespace,
                 identifier : string;
}
