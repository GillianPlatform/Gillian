"use strict";

/**
	@toprequires 	emp
	@topensures		scope(x : 0) * scope(x1 : 1) * scope(c1 : 1) * scope(x2 : 1) * scope(c2 : 1)
*/

try {
	var x = 0;
}
catch (e) {
	throw new Error()
}

var c1 = 0;

try{
	var x1 = 1;
}
finally {
	c1 = 1;
}

var c2 = 0;

try {
	var x2 = 1;
}
catch(e) {
	throw new Error()
}
finally {
	c2 = 1;
}