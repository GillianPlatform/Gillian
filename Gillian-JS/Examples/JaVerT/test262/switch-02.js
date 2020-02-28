"use strict";

/**
	@id SwitchTest
	
	@pre	(value == 0)
	@post	(ret   == 6)
*/
function SwitchTest(value){
  var result = 0;
  
  switch(value) {
    case 0:
      switch(value) {
        case 0:
         result += 3;
        break;
        default:
          result += 32;
          break;
        }
      result *= 2;
      break;
      result=3;
    default:
      result += 32;
      break;
  }
  return result;
}