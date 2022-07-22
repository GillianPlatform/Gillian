var buckets = require('../../buckets');

// init
var bag = new buckets.Bag();

// size
var n1 = symb_number();
var n2 = symb_number();
var n3 = symb_number();
var n4 = symb_number();

bag.add(n1);
bag.add(n1, n2);
bag.remove(n1);
var res1 = bag.remove(n3, n4);
var res2 = bag.count(n1);
Assert(((n3 = n1) and (((n2 > 0) and (n4 > 0) and (n4 <= n2) and res1 and (res2 = (n2 - n4))) or ((n2 > 0) and (n4 > 0) and (n4 > n2) and res1 and (res2 = 0)) or ((n2 >= 0) and (n4 <= 0) and (not res1) and (res2 = n2)) or ((n2 <= 0) and (not res1) and (res2 = 0)))) or ((not (n3 = n1)) and (not res1) and (((n2 >= 0) and (res2 = n2)) or ((n2 < 0) and (res2 = 0)))));
