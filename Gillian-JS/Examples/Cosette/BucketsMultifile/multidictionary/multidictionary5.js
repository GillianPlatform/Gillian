var buckets = require('../../buckets');

var dict = new buckets.MultiDictionary()

var s1 = symb_string();
var s2 = symb_string();
var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

Assume (not (x1 = x2));
Assume (not (x1 = x3));
Assume (not (x2 = x3));

dict.set(s1, x1);
dict.set(s1, x2);
dict.set(s2, x2);
dict.set(s2, x3);

var res1 = 0;
var sum1 = x1 + x2 + x3;
var sum2 = x1 + x2 + x2 + x3;
dict.forEach(function (k, vs) {
  var l = vs.length;
  var i;
  for (i = 0; i < l; i++) {
    res1 += vs[i];
  }
});

Assert(((s1 = s2) and (res1 = sum1)) or ((not (s1 = s2)) and (res1 = sum2)));
