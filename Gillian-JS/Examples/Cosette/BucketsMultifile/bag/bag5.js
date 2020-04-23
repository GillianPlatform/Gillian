var buckets = require('../../buckets');

// init
var bag = new buckets.Bag();

// size
var n1 = symb_number(n1);
var n2 = symb_number(n2);

bag.add(n1);
bag.add(n2);

var set = bag.toSet();
var l = set.size();
Assert(((n1 = n2) and (l = 1)) or ((not (n1 = n2)) and (l = 2)));
