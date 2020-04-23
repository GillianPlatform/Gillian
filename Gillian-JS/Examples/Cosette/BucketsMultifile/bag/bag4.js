var buckets = require('../../buckets');

// init
var bag = new buckets.Bag();

// size
var n1 = symb_number(n1);
var n2 = symb_number(n2);
//var n3 = symb_number(n3);
//var n4 = symb_number(n4);
//Assume(n3 > 0);
//Assume(n4 > 0);

bag.add(n1, 7);
bag.add(n2, 8);

var ar = bag.toArray();

var res1 = buckets.arrays.frequency(ar, n1);
var res2 = buckets.arrays.frequency(ar, n2);
Assert((((n1 = n2) and (res1 = 15) and (res2 = 15))) or (((not (n1 = n2)) and (res1 = 7) and (res2 = 8))));
