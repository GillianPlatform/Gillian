/** Jose: works
  Bug specs:  2 (o null, o undefined)
  Succ specs: 2  (o obj, o.banana str, y str; o obj, o.banana str, y num; o obj, o.banana num, y str; o obj, o.banana num, y num)
*/ 


/** @id h */
var h = function (o, y) {
  var z = o.banana;  
  return z + y; 
}
