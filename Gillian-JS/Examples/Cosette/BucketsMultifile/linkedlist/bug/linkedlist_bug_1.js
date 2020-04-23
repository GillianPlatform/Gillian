var buckets = require('../../../buckets_with_bugs');

var list = new buckets.LinkedList()

var x1 = symb_number(x1);
var x2 = symb_number(x2);
var x3 = symb_number(x3);

list.add(x1)
list.add(x2)

var res = list.elementAtIndex(x3);
Assert( (((x3 = 0) and (res = x1)) or ((x3 = 1) and (res = x2))) or (((not (x3 = 0)) and (not (x3 = 1))) and (res = undefined)) );
