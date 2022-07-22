var buckets = require('../../buckets');

// init
var bag = new buckets.Bag();

// size
var n1 = symb_number();
var n2 = symb_number();
var n3 = symb_number();

bag.add(n1);
bag.add(n2);

var bag2 = new buckets.Bag();

var res1 = bag.equals(bag2);
Assert(not res1);

var res3 = bag.equals([n1, n2, n3]);
Assert(not res3);

bag.forEach(function(x) {
  if (x == n3) {
    return false;
  }
  bag2.add(x);
});

var res2 = bag.equals(bag2);
Assert(((not (n3 = n1)) and (not (n3 = n2)) and res2) or (((n3 = n1) or (n3 = n2)) and (not res2)));
