'use strict';

/*
  @id a_make_node
*/
function a_make_node(v)
{
  /* Annotation */ isNumber(v);

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

  /* Annotation */ isNumber(v);
  /* Annotation */ isNullableObject(t);

  if (t === null) {
  	return a_make_node(v);
  }

  /* Annotation */ isNumber(t.value);

  if (v < t.value) {
    t.left = b_insert(v, t.left);
  }
  else if (v > t.value) {
    t.right = b_insert(v, t.right);
  }

  return t;
}

/* @id c_find */
function c_find (v, t)
{
  /* Annotation */ isNumber(v);
  /* Annotation */ isNullableObject(t);

	if (t === null)
    return false;

  /* Annotation */ isNumber(t.value);

  if (v === t.value)
    return true;

  if (v < t.value)
    return c_find(v, t.left);

  return c_find(v, t.right);
}

/* @id d_findMin */
function d_findMin(t)
{
  /* Annotation */ isObject(t);
  /* Annotation */ isNumber(t.value);
  /* Annotation */ isNullableObject(t.left);

  if (t.left === null)
    return t.value;

	return d_findMin(t.left);
}

/* @id e_remove */
function e_remove(v, t)
{
  /* Annotation */ isNumber(v);
  /* Annotation */ isNullableObject(t);

	if (t === null)
    return null;

  /* Annotation */ isNumber(t.value);

	if (v === t.value) {
      /* Annotation */ isNullableObject(t.left);
		  if (t.left === null) {
				return t.right;
			}
    else
      /* Annotation */ isNullableObject(t.right);
	  	if (t.right === null) {
	  			return t.left;
			}
		else {
			var min = d_findMin(t.right);
			t.right = e_remove(min, t.right);
			t.value = min;
		}
	}
  else if (v < t.value) {
    /* Annotation */ isNullableObject(t.left);
    t.left = e_remove(v, t.left)
  }
  else {
    /* Annotation */ isNullableObject(t.left);
    t.right = e_remove(v, t.right)
  }

  return t;
}
