var buckets = require('../../buckets');

var dict = new buckets.Dictionary();

var x1 = symb_number(); //1
var x2 = symb_number(); //2
var s1 = symb_string(); // "2"
var s2 = symb_string(); // "foo"

Assume(not (s1 = s2));

dict.set(s1, x1);
dict.set(s2, x2);

var keys = dict.keys();
var l1 = keys.length;
Assert(l1 = 2);
var t1 = keys[0];
var t2 = keys[1];
Assert(((t1 = s1) and (t2 = s2)) or ((t1 = s2) and (t2 = s1)));

var values = dict.values();
var l2 = keys.length;
Assert(l2 = 2);
var y1 = values[0];
var y2 = values[1];
Assert(((y1 = x1) and (y2 = x2)) or ((y1 = x2) and (y2 = x1)));
