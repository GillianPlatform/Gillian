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

/** @id remove */
function remove(v, t)
{
  if (t === null)
    return null;

  if (v === t.value) {
    if (t.left === null) {  
        return t.right;
      }
    else 
    if (t.right === null) {
          return t.left;
      }
    else {
      var min = find_min(t.right);
      t.right = remove(min, t.right);
      t.value = min;
    }
  }
  else if (v < t.value)
    t.left = remove(v, t.left);
  else
    t.right = remove(v, t.right); 

  return t;
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

var rnull = remove (v1, null);
Assert (rnull = null);

var root = insert(v2, null);
insert(v1, root); insert(v3, root);

var r1 = remove(v1, root);
var t1 = isNode(r1, v2, null, root.right);
var r2 = remove(v3, root);
var t2 = isNode(r1, v2, null, null);

insert(v1, root);
var r3 = remove(v2, root);
var t3 = isNode(r3, v1, null, null);

insert(v2, r3);
var r4 = remove(v1, r3);
var t4 = isNode(r4, v2, null, null);

insert(v1, r4); insert(v3, r4);
var r5 = remove(v2, r4);
var t5 = isNode(r5, v3, r5.left, null) && isNode(r5.left, v1, null, null);

Assert (t1 and t2 and t3 and t4 and t5)
