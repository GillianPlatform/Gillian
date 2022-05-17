/**
  @id missingResource

  @pre  JSObject(o)

  @post ret == 0
*/
function missingResource(o) {
  var x = o["foo"];
  return x
}