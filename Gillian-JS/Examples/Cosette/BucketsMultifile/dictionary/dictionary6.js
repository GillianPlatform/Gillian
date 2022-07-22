var buckets = require('../../buckets');

var dict = new buckets.Dictionary();

var x1 = symb_number(); //1
var x2 = symb_number(); //2
var s1 = symb_string(); // "2"
var s2 = symb_string(); // "foo"

dict.set(s1, x1);
dict.set(s2, x2);

var res1 = dict.size();
Assert(((s1 = s2) and (res1 = 1)) or ((not (s1 = s2)) and (res1 = 2)));
var res2 = dict.isEmpty();
Assert(not res2);

dict.clear();
var res3 = dict.isEmpty();
Assert(res3);
var res4 = dict.size();
Assert(res4 = 0);
