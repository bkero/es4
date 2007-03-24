var s = "";
try {
  throw "thrown";
  s += "bar";
}
catch (exn) {
  s += exn;
}
finally {
  s += "finally";
}
s;
