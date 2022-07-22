/* @id insert */
function insert(node, value) {
    
    if (node === null) {
        return { next: null, value: value }
    } else if (node.value === value) {
        return node;
    } else if (node.value < value) {
        var rec = insert(node.next, value);
        return { next: rec, value: node.value }
    } else {
        return { next: node, value: value }
    }
}

/* Test function */
function isNode(n, v, nxt) {
  var nv = n.value, nx = n.next;
  return ((nv === v) && (nx === nxt));
}

var v1 = symb_number();
var v2 = symb_number();
var v3 = symb_number();

Assume((v1 < v2) and (v2 < v3));

var n1 = insert(null, v2);
var in1 = isNode(n1, v2, null);
Assert(in1);

var n2 = insert(n1, v2);
Assert(n1 = n2);

var n3 = insert(n1, v3);
var in3 = isNode(n3, v2, n3.next) && isNode(n3.next, v3, null);
Assert (in3)

var n4 = insert(n3, v1);
var in4 = isNode(n4, v1, n3) && isNode(n3, v2, n3.next) && isNode(n3.next, v3, null);
Assert (in4)