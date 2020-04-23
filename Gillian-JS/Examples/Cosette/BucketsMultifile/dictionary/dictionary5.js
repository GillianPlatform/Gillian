var buckets = require('../../buckets');

var dict = new buckets.Dictionary();

var x1 = symb_number(x1); //1
var x2 = symb_number(x2); //2
var s1 = symb_string(s1); // "2"
var s2 = symb_string(s2); // "foo"

dict.set(s1, x1);
dict.set(s2, x2);

dict.remove(s2);

var res = dict.containsKey(s1);
Assert((not (s1 = s2)) and res or ((s1 = s2) and (not res)));
