'use strict';

/* 
  @id a_make_node 
*/
function a_make_node(v)
{
  var node = {
    value : v,
    left  : null,
    right : null
  };
  return node;
}

/**
	@id b_insert
*/
function b_insert(v, t)
{

  if (t === null) {
  	return a_make_node(v);
  }

  if (v < t.value)
    t.left = b_insert(v, t.left);
  else if (v > t.value) 
    t.right = b_insert(v, t.right);

  return t;
}

/**
    6 specs, 2 annots

	@id c_find
*/
function c_find (v, t)
{
	if (t === null)
		return false;
	else if (v === t.value)
		return true;
	else {
		if (v < t.value)
		  return c_find(v, t.left) 
		else
		  return c_find(v, t.right);
	}
	
	return result;
}

/**
    2 specs, 1 annots
	@id d_findMin
*/
function d_findMin(t)
{
	if (t.left === null)
		return t.value;
	else
		return d_findMin(t.left);
}

/**
    12 specs, 2 annots
	@id e_remove
*/
function e_remove(v, t)
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
			var min = d_findMin(t.right);
			t.right = e_remove(min, t.right);
			t.value = min;
		}
	}
	else if (v < t.value)
		t.left = e_remove(v, t.left);
	else
		t.right = e_remove(v, t.right);	

  return t;
}
