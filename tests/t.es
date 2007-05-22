use namespace intrinsic
function f() 
    : [int,int]
{ 
    let [x,y]:[int,int] = [10,20]
    return [x,y]
}
print(f())