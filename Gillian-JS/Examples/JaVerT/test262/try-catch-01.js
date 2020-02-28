"use strict";

/**
	@toprequires emp
	@topensures  scope(x : 42)
*/

/**
	@id f
	
	@pre	emp
	@post	(ret == 42)
*/
function f(o) {
	var x = 42;
	
	/**
		@id innerf
		
		@pre	scope(x : #x)
		@post	scope(x : #x) * (ret == #x)
	*/
	function innerf(o) {
		try {
			throw o;
		}
		catch (e) {
			return x;
		}
	}
	
	return innerf(o);
}

var x = f({})