var s = 0;
var t = "";
var o = [[1,"a"],[2,"b"],[3,"c"],[4,"d"],[5,"e"]];
var i = 0;
var j = "";
for each ( let [i,j] in o ) {
    s += i;
    t += j;
}
(s + i) + (t + j);
