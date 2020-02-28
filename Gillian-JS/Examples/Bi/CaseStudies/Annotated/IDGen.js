'use strict';

/* 
  Success specs:
    1. Creation of ID Generator

  @id makeIdGen 
*/
var makeIdGen = function (prefix) { 

	var count = 0; 

    /* 
      Success specs:
        1. prefix is a string

	  @id getId 
	*/
	var getId = function () { 
        /* Annotation */ isString(prefix);
		return prefix + "_id_" + (count++) 
	}; 

	/* 
      Success specs:
        1. count is set to 0

	  @id reset 
	*/
	var reset = function () { 
		count = 0 
	}; 

	return { 
		getId: getId, 
		reset: reset 
	}
};