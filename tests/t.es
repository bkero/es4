use namespace intrinsic
var x,y
L1: L2: for ( {i:x,j:y} = {i:20,j:10}; x > y; --x ) {
L3: if (x==15) break L1
print(x,"\n")
continue L2
print(x,"\n")
}