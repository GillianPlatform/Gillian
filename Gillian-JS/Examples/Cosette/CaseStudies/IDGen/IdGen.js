
var makeIdGen = function (prefix) { 

	var count = 0; 

	var getId = function () { 
		return prefix + "_id_" + (count++) 
	}; 

	var reset = function () { 
		count = 0 
	}; 

	return { 
		getId: getId, 
		reset: reset 
	}
};

var prefix = symb_string();

var idgen = makeIdGen (prefix);
var id  = idgen.getId();
var eid = prefix + "_id_0";

Assert(id = eid);

var id  = idgen.getId();
var eid = prefix + "_id_1"; 
Assert(id = eid);

idgen.reset();
var id  = idgen.getId();
var eid = prefix + "_id_0";

Assert(id = eid);
