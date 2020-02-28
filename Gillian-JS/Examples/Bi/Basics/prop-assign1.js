/** Jose: works
  Bug specs:  2 (y null, y undefined)
  Succ specs: 2  (y obj, w num; y obj, w string)
*/ 

/** @id f */
var f = function (y) {
  y[w] = 2;
  return y; 
}  

