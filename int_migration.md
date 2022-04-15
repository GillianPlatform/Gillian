# Int migration

-> Check all calls to lexpr_is_num


| X                | GillianCore | Gillian-C | wisl | Gillian-JS |
| ---------------- | ----------- | --------- | ---- | ---------- |
| LstNth           | X           |           |      |            |
| LstLen           |             |           |      |            |
| LstSub           |             |           |      |            |
| Expr.list_nth    |             |           |      |            |
| Expr.list_nth_e  |             |           |      |            |
| Expr.list_sub    |             |           |      |            |
| Expr.list_length |             |           |      |            |


-> Check typing rules


-> Maybe error:
`Reduction.substitute_in_int_expr`: does scaling on ints, might end up, if number are not divs, this might fail.
I added a check just in case