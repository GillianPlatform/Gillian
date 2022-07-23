var n1 = symb_number(), n2 = symb_number();

Assume((0 <= n1) and (0 <= n2) and (not (n1 = n2)));

var res = n1 + n2;

Assert((n1 <= res) and (n2 <= res));

var x = symb();
Assume(not (typeOf x = Obj));

var tx = typeof(x);