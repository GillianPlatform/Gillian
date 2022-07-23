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

function sort(head) {
    var result;

    if (head === null) {
        result = null
    } else {
        var rec = sort(head.next);
        result = insert(rec, head.value)
    }
    return result;
}

/* Test function */
function isNode(n, v, nxt) {
  var nv = n.value, nx = n.next;
  return ((nv === v) && (nx === nxt));
}

var s1 = sort(null);
Assert(s1 = null);

var v1 = symb_number();
var v2 = symb_number();

var n2 = { value: v2, next : null};
var n1 = { value: v1, next: n2 };

Assume (v2 < v1);

var s2 = sort(n1);
var in1 = isNode(s2, v2, s2.next) && isNode(s2.next, v1, null);
Assert(in1);