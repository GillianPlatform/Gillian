/** Jose: works
  Bug specs:  1
  Succ specs: 2 
 */ 

/** @id listCount */
function listCount (lst) { 
  if (lst === null) {
    return 0; 
  } else {
    var next  = lst.next; 
    var count = listCount(next);
    return count + 1; 
  }
}