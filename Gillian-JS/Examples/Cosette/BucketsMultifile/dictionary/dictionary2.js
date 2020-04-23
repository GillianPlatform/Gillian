var buckets = require('../../buckets');

var dict = new buckets.Dictionary();

var x1 = symb_number(x1); //1
var x2 = symb_number(x2); //2
var s1 = symb_string(s1); // "2"
var s2 = symb_string(s2); // "foo"

dict.set(s1, x1);
dict.set(s2, x2);

var res1 = dict.remove(s1);
Assert(((s1 = s2) and (res1 = x2)) or ((not (s1 = s2)) and (res1 = x1)));
var res2 = dict.remove(s1);
Assert(res2 = undefined);
