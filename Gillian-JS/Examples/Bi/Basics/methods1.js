/** Jose: works
  Bug specs:  
    talk - 
  Succ specs: 4 (str, str; str, num; num, str; num, num)
*/ 


/** @id talk */ 
function talk (o, i) {
   switch (i) {
   	  case 0:  return "Hello! I am " + this.name + "!\n" 
      case 1:  return "Nice to meet you " + o.name + "\n"
      case 2:  return "I have nothing else to say\n"
      default: return "I really need to go now\n"
   } 
}

/** @id Person */ 
function Person (name) { 
   this.name = name
}

Person.prototype.talk = talk; 

/** @id conversation */ 
function conversation (p1, p2) { 
   var text = ""; 
   for (var i=0; i<3; i++) { 
     var text1 = p1.talk(p2, i); 
     var text2 = p2.talk(p1, i); 
     text += text1; 
     text += text2;      
   }
   return text; 
}
