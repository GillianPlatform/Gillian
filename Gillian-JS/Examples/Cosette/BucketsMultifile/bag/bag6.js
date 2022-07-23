var buckets = require('../../buckets');

// init
var bag = new buckets.Bag();

// size
var n1 = symb_number();
var n2 = symb_number();
var n3 = symb_number();
var n4 = symb_number();

var res1 = bag.isEmpty();
Assert(res1);
var res2 = bag.size();
Assert(res2 = 0);

bag.add(n1);
bag.add(n1, n2);
var res3 = bag.isEmpty();
Assert(not res3);
bag.remove(n1);
bag.remove(n3, n4);
bag.clear();
var res4 = bag.isEmpty();
Assert(res4);
