var res = "fell right through";
try {
    var x : Number = "abracadabra";
}
catch (e : ReferenceError) {
    res = "wrong catch";
}
catch (e : TypeError) {
    res = "threw exception";
}
res;
