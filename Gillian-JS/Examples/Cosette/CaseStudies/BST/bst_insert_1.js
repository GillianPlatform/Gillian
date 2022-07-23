/** @id makeNode */
function make_node(v)
{
  var node = {
    value : v,
    left  : null,
    right : null
  };
  return node;
}

/** @id insert */
function insert(v, t)
{
  var result;
  
  if (t === null) {
    return make_node(v);
  }

  if (v < t.value)
    t.left = insert(v, t.left);
  else if (v > t.value) 
    t.right = insert(v, t.right);

  return t;
}

/* Test function */
function isNode(n, v, l, r) {
  var nv = n.value, nl = n.left, nr = n.right;
  return ((nv === v) && (nl === l) && (nr === r));
}


/* Test 1 */
var v = symb_number();

var res = insert(v, null);
var ret = isNode(res, v, null, null);
Assert(ret);