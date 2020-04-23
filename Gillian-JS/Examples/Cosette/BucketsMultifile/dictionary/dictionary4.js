var buckets = require('../../buckets');

var dict = new buckets.Dictionary();

var x1 = symb_number(x1); //1
var x2 = symb_number(x2); //2
var s1 = symb_string(s1); // "2"
var s2 = symb_string(s2); // "foo"

Assume(not (s1 = s2));

dict.set(s1, x1);
dict.set(s2, x2);

var res1 = "";
var res2 = 0;
var s3 = symb_string(s3);

dict.forEach(function(k, v) {
  if (k === s3) {
    return false;
  }
  res1 = res1 + k;
  res2 = res2 + v;
});

var c1 = s1 + s2;
var c2 = s2 + s1;
Assert(((not ((s3 = s1) or (s3 = s2))) and ((res1 = c1) or (res1 = c2))) or (((s3 = s1) or (s3 = s2)) and ((res1 = s1) or (res1 = s2) or (res1 = ""))));

var y = x1 + x2;
Assert(((not ((s3 = s1) or (s3 = s2))) and (res2 = y)) or (((s3 = s1) or (s3 = s2)) and ((res2 = x1) or (res2 = x2) or (res2 = 0))));
