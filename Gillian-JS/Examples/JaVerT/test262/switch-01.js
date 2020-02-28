"use strict";

/**
	@id SwitchTest
	
	@pre	(value == 0)
	@post	(ret   == 6)
	
	@pre	(value == 1)
	@post	(ret   == 4)
	
	@pre	(value == 2)
	@post	(ret   == 56)
	
	@pre	(value == 3)
	@post	(ret   == 48)
	
	@pre	(value == 4)
	@post	(ret   == 64)
	
	@pre	(value == true)
	@post	(ret   == 32)
	
	@pre	(value == false)
	@post	(ret   == 32)
	
	@pre	(value == null)
	@post	(ret   == 32)
	
	@pre	(value == undefined)
	@post	(ret   == 32)
	
	@pre	(value == "0")
	@post	(ret   == 32)
	
*/
	
function SwitchTest(value){
  var result = 0;
  
  switch(value) {
    case 0:
      result += 2;
    case 1:
      result += 4;
      break;
    case 2:
      result += 8;
    case 3:
      result += 16;
    default:
      result += 32;
      break;
    case 4:
      result += 64;
  }
  
  return result;
}