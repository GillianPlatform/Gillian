/** Jose: works
  Bug specs:  0
  Succ specs: 
    - h - 2 (y num, y str)
    - f - 2 (y num, y str)
*/ 


/** @id h */
var h = function (y) {
  return "banana" + y; 
}

/** @id f */
var f = function (y) {
  var z = h(y); 
  return z; 
}  

