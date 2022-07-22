var buckets = require('../../buckets');

// init
var bag = new buckets.Bag();

// size
var n1 = symb_number();
var n2 = symb_number();

bag.add(n1, n2);
var res1 = bag.size();
Assert(((n2 >= 0) and (res1 = n2)) or ((n2 < 0) and (res1 = 0)));
bag.add(n1);
var res2 = bag.size();
Assert(((n2 >= 0) and (res2 = (n2 + 1))) or ((n2 < 0) and (res2 = 1)));
