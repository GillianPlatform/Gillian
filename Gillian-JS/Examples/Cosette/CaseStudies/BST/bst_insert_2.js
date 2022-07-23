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

/* Test 2 */
var v = symb_number();
var w = symb_number();

var root = insert(v, null);
insert(w, root);

var teq = isNode(root, v, null, null);
var tvl = root.left && isNode(root, v, root.left, null) && isNode(root.left, w, null, null);
var tvg = root.right && isNode(root, v, null, root.right) && isNode(root.right, w, null, null);

Assert (((v = w) and teq) or ((w < v) and tvl) or ((w > v) and tvg))