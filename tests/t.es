var {i:x,s:y} : {i:int,s:String} = o;
({i:x,s:y} = o)

//t = o
//x=t.i
//y=t.s

var {i:x,a:{j:y,k:z}}:{i:t,a:{j:u,k:v}} = o

/*
  t = o
  x = t.i
  a = t.a
  y = a.j
  z = a.k
*/
