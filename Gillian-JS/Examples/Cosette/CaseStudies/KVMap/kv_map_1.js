function Map () {
  this._contents = {};
  return this;
}

/* @id validKey */
Map.prototype.validKey = function (key) {
  return (typeof(key) === "string" && (key !== "hasOwnProperty"))
}

/* @id mapGet */
Map.prototype.get = function (k) {
  if (this.validKey(k)) { 
      if (this._contents.hasOwnProperty(k)) { 
        var result = this._contents[k];
          return result
      } else { return null }
  } else
  throw new Error("Invalid Key")
}

/* @id mapPut */
Map.prototype.put = function (k, v) {
  if (this.validKey(k)) { 
    this._contents[k] = v; 
    return v;
  } else
    throw new Error("Invalid Key")
}

var k1 = symb_string();
var k2 = symb_string();

var v1 = symb_number();

var map = new Map();
var cts = map._contents;
var props = Object.keys(cts);
var lprps = props.length;
Assert(lprps = 0);

var vk1 = map.validKey(k1);
var vk2 = map.validKey(k2);
Assume (vk1 and vk2);

map.put(k1, v1);
var props = Object.keys(cts);
var lprps = props.length;
var elmnt = props[0];
Assert((lprps = 1) and (elmnt = k1));

var v = map.get(k2);
Assert(((k1 = k2) and (v = v1)) or (v = null))