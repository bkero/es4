var res = "fell through";
try {
    var x : Object = null;  // Object is nullable
}
catch (e : TypeError) {
    res = "threw exception";
}
res; // should be "fell through"
