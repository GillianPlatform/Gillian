"use strict";

/**
	@toprequires	emp
	@topensures		scope(c1 : 2) * scope(c2 : 2) * scope(c3 : 3) * scope(c4 : 2)
*/

var c1 = 0;

try {
  c1 += 1;
  y;
}
catch (e) {
  c1 *= 2;
}

var c2=0;
try{
  c2 += 1;
}
finally{
  c2 *= 2;
}

var c3=0;
try{
  c3 = 1;
  z;
}
catch(err){
  c3 *= 2;
}
finally{
  c3 += 1;
}	

var c4 = 0;
try {
  c4 = 1;
}
catch(err){
  c4 *= 3;
}
finally{
  c4 += 1;
}