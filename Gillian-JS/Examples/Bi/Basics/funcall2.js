/** Jose: works
  Bug specs:  
    h - 2 (o null, o undefined)
  Succ specs:
    h - 4 (see plus2.js)
    f - 2 (o.banana str, o.banana num)
*/ 


/** @id h */
var h = function (o, y) {
  var z = o.banana;  
  return z + y; 
}


/** @id f */
var f = function (o) {
  var w  = "xuxu";  
  var z = h(o, w); 
  return z; 
}  