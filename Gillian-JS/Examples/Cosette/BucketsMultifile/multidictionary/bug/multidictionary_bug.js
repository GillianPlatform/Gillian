var buckets = require('../../../buckets_with_bugs');

var dict = new buckets.MultiDictionary()

var s = symb_string(s);
var x1 = symb_number(x1);
var x2 = symb_number(x2);

dict.set(s, x1);
dict.set(s, x2);

dict.remove(s, x1);
var res = dict.remove(s, x2);
Assert(((not (x1 = x2)) and (res = true)) or ((x1 = x2) and (res = false)));
