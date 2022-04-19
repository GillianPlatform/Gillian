# Int migration

|                  | GillianCore | Gillian-C | wisl | Gillian-JS |
| ---------------- | ----------- | --------- | ---- | ---------- |
| LstNth           | X           |           |      |            |
| LstLen           | X           |           |      |            |
| LstSub           | X           |           |      |            |
| Expr.list_nth    | X           |           |      |            |
| Expr.list_nth_e  | X           |           |      |            |
| Expr.list_sub    | X           |           |      |            |
| Expr.list_length | X           |           |      |            |
| FPlus            | X           |           |      |            |
| FMinus           | X           |           |      |            |
| FUnaryMinus      | X           |           |      |            |
| FTimes           | X            |           |      |            |
| Other FOps       |             |           |      |            |

-> wisl ints everywhere
-> Check typing rules
-> Look for "INTEGER BYTE-BY-BYTE BREAKDOWN
-> Maybe error:
`Reduction.substitute_in_int_expr`: does scaling on ints, might end up, if number are not divs, this might fail.
I added a check just in case