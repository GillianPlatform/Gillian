var buckets = require('../../buckets');

var dict = new buckets.Dictionary();

var x1 = symb_number(x1); //1
var x2 = symb_number(x2); //2
var s1 = symb_string(s1); // "2"
var s2 = symb_string(s2); // "foo"

Assume(not (s1 = s2));

dict.set(s1, x1);
dict.set(s2, x2);

var dict2;
var res2 = dict.equals(dict2);
Assert(not res2);
dict2 = new buckets.Dictionary();

var res1 = dict.equals(dict2);
Assert(not res1);

var keys = dict.keys();
var vals = dict.values();

var i = 0;
for (i = 0; i < 2; i++) {
  dict2.set(keys[i], vals[i]);
}

var res3 = dict2.equals(dict);
Assert(res3);
