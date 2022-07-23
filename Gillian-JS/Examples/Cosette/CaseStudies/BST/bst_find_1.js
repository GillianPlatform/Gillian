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

/** @id find */
function find (v, t)
{
  var result;

  if (t === null)
    result = false;
  else if (v === t.value)
    result = true;
  else {
    if (v < t.value)
      result = find(v, t.left) 
    else
      result = find(v, t.right);
  }
  
  return result;
}

/* Test function */
function isNode(n, v, l, r) {
  var nv = n.value, nl = n.left, nr = n.right;
  return ((nv === v) && (nl === l) && (nr === r));
}

/* Test 1 */
var v1 = symb_number();
var v2 = symb_number();
var v3 = symb_number();
var v4 = symb_number();

Assume((v1 < v2) and (v2 < v3));
Assume((not (v4 = v1)) and (not (v4 = v2)) and (not (v4 = v3)));

var root = insert(v2, null);
insert(v1, root); insert(v3, root);

var f1 = find(v1, root), f2 = find(v2, root), f3 = find(v3, root), f4 = find(v4, root);
Assert (f1 and f2 and f3 and (not f4))
