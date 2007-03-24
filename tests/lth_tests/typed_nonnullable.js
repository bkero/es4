var res = "fell right through";
try {
    var x : Number = null;  // Number is not nullable
}
catch (e : TypeError) {
    res = "threw exception";
}
res;
