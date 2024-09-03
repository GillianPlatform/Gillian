"use strict";

/*
		@pred ValidKey(k) : 
				types(k : Str) * (! (k == "hasOwnProperty"));

		@pred InvalidKey(ik) :
				types (ik : Undefined),
				types (ik : Null),
				types (ik : Bool),
				types (ik : Num),
				types (ik : Str) * (ik == "hasOwnProperty");

		@pred Map (+m, mp, kvs, keys) :
				JSObjWithProto(m, mp) *
				DataProp(m, "_contents", #c) * JSObject(#c) *
				((m, "get") -> none) * ((m, "put") -> none) * ((m, "validKey") -> none) *
				((#c, "hasOwnProperty") -> none) * KVPairs(#c, kvs, keys) * empty_fields(#c : -u- (keys, -{ "hasOwnProperty" }-));

		@pred KVPairs (+o, kvs : Set, keys : Set) :
		[def1] (kvs == -{ }-) * (keys == -{ }-),
		[def2: #k, #v] 
					 (kvs == -u- (-{ {{ #k, #v }} }-, #rkvs)) * (keys == -u- (-{ #k }-, #rkeys)) *
					 (! (#k --e-- #rkeys)) * 
					 ValidKey(#k) * DataProp(o, #k, #v) * KVPairs(o, #rkvs, #rkeys) *
					 types (#rkvs: Set, #rkeys: Set);

		@pred MapProto(mp) :
				JSObject(mp) *
				DataProp(mp, "get", #gl) * JSFunctionObject(#gl, "mapGet", _, _, _) *
				DataProp(mp, "put", #pl) * JSFunctionObject(#pl, "mapPut", _, _, _) *
				DataProp(mp, "validKey", #vKl) * JSFunctionObject(#vKl, "isValidKey", _, _, _) *
				((mp, "_contents") -> none);
*/


/**
		@id  map

		@pre (
			initialHeapPostWeak() * 
			JSObjWithProto(this, #mp) *
				((this, "_contents") -> none) *
				((this, "get") -> none) *
				((this, "put") -> none) *
				((this, "validKey") -> none) *
				MapProto(#mp)
		)
	  
		@post (
			initialHeapPostWeak() * 
			Map(this, #mp, #kvs, #keys) * (#kvs == -{ }-) * (#keys == -{ }-) * 
			MapProto(#mp) * (ret == this)
		)
*/
function Map() {
	this._contents = {};
	return this;
}

/**
	@id isValidKey
	
	@pre  ((key == #key) * ValidKey(#key))
	@post (ret == true)
		
	@pre ((key == #key) * InvalidKey(#key))
	@post (ret == false)
*/
Map.prototype.validKey = function (key) {
	return (typeof (key) === "string" && key !== "hasOwnProperty")
}

/**
	@id mapGet
	
	@pre     (k == #k) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * InvalidKey(#k) * GlobalObject() * ObjectPrototype($lobj_proto) * BI_ErrorObject()
	@posterr Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ErrorObjectWithMessage(ret, "Invalid Key") * GlobalObject() * ObjectPrototype($lobj_proto) * BI_ErrorObject()

	@pre  (k == #k) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ValidKey(#k) * (! (#k --e-- #keys)) * GlobalObject() * ObjectPrototype($lobj_proto)
	@post Map(this, #mp, #kvs, #keys) * MapProto(#mp) * (ret == null) * GlobalObject() * ObjectPrototype($lobj_proto)
	
	@pre [existent_key: #v] 
				 (k == #k) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ValidKey(#k) * (#k --e-- #keys) * ({{ #k, #v }} --e-- #kvs) * GlobalObject() * ObjectPrototype($lobj_proto)
	@post Map(this, #mp, #kvs, #keys) * MapProto(#mp) * (ret == #v) * GlobalObject() * ObjectPrototype($lobj_proto)
*/
Map.prototype.get = function (k) {
	/* @tactic assert ( DataProp(this, "_contents", #c) ) [bind: #c] */
	if (this.validKey(k)) {
		/* @tactic if (#k -e- #keys) then { unfold KVPairs(#c, #kvs, #keys) [bind: (#k := #k) and (#v := #v)] } */
		if (this._contents.hasOwnProperty(k)) {
			var result = this._contents[k];
			/* @tactic fold KVPairs(#c, #kvs, #keys) [def2 with (#k := #k)] */
			return result
		} else { return null }
	} else
		throw new Error("Invalid Key")
}

/**
	@id mapPut
	
	@pre     (k == #k) * (v == #v) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * InvalidKey(#k) * GlobalObject() * ObjectPrototype($lobj_proto) * BI_ErrorObject()
	@posterr Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ErrorObjectWithMessage(ret, "Invalid Key") * GlobalObject() * ObjectPrototype($lobj_proto) * BI_ErrorObject()

	@pre  (k == #k) * (v == #v) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ValidKey(#k) * (! (#k --e-- #keys)) * GlobalObject() * ObjectPrototype($lobj_proto)
		@post Map(this, #mp, -u- (-{ {{ #k, #v }} }-, #kvs), -u- (-{ #k }-, #keys)) * MapProto(#mp) * GlobalObject() * ObjectPrototype($lobj_proto)

	  
	@pre  [existent_key: #w, #rkvs]
					(k == #k) * (v == #v) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ValidKey(#k) * (#k --e-- #keys) * 
				(#kvs == -u- (-{ {{ #k, #w }} }-, #rkvs)) * GlobalObject() * ObjectPrototype($lobj_proto)
	@post Map(this, #mp, -u- (-{ {{ #k, #v }} }-, #rkvs), #keys) * MapProto(#mp) * GlobalObject() * ObjectPrototype($lobj_proto)
*/
Map.prototype.put = function (k, v) {
	/* @tactic assert( DataProp(this, "_contents", #c) * scope (v : #v) ) [bind: #c] */
	if (this.validKey(k)) {
		/* @tactic if (#k -e- #keys) then { unfold KVPairs(#c, #kvs, #keys) [bind: (#k := #k) and (#v := #w) and (#rkvs := #rkvs)] } */
		this._contents[k] = v;
		/* @tactic if (#k -e- #keys) 
			then { fold KVPairs(#c, -u- (-{ {{ #k, #v }} }-, #rkvs), #keys) [def2 with (#k := #k) and (#v := #v) and (#rkvs := #rkvs) and (#rkeys := (#keys -d- -{ #k }-))] } 
			else { fold KVPairs(#c, -u- (-{ {{ #k, #v }} }-, #kvs), -u- ( -{ #k }-, #keys)) [def2 with (#k := #k) and (#v := #v) and (#rkvs := #kvs)] } */
		return v;
	} else
		throw new Error("Invalid Key")
}