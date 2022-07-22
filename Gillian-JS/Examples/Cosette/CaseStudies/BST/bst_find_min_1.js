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

/** @id findMin */
function find_min(t)
{
  var result;
    
  if (t.left === null)
    result = t.value;
  else
    result = find_min(t.left);
    
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

Assume((v1 < v2) and (v2 < v3));

var root = insert(v2, null);
insert(v1, root); insert(v3, root);

var fm  = find_min(root);
var fml = find_min(root.left);
var fmr = find_min(root.right);

Assert ((fm = v1) and (fml = v1) and (fmr = v3));
