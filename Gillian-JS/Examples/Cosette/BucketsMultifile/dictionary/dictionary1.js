var buckets = require('../../buckets');

var dict = new buckets.Dictionary();

var x1 = symb_number(); //1
var x2 = symb_number(); //2
var s1 = symb_string(); // "2"
var s2 = symb_string(); // "foo"

dict.set(s1, x1);
var res1 = dict.set(s2, x2);
Assert(((s1 = s2) and (res1 = x1)) or ((not (s1 = s2)) and (res1 = undefined)));

var res2 = dict.set(s1, undefined);
Assert(res2 = undefined);

var s3 = symb_string();
Assume(not (s3 = s1));
Assume(not (s3 = s2));

var res3 = dict.get(s3);
Assert(res3 = undefined);

var res = dict.get(s1);

Assert(((s1 = s2) and (x1 = x2) and (res = x1)) or ((s1 = s2) and (not (x1 = x2)) and (res = x2)) or ((not (s1 = s2)) and (res = x1)));
