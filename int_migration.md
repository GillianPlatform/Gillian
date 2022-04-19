# Int migration

|                  | GillianCore | Gillian-C | wisl | Gillian-JS |
| ---------------- | ----------- | --------- | ---- | ---------- |
| LstNth           | X           |           | X    |            |
| LstLen           | X           |           | X    |            |
| LstSub           | X           |           | X    |            |
| Expr.list_nth    | X           |           | X    |            |
| Expr.list_nth_e  | X           |           |      |            |
| Expr.list_sub    | X           |           |      |            |
| Expr.list_length | X           |           |      |            |
| FPlus            | X           |           |      |            |
| FMinus           | X           |           |      |            |
| FUnaryMinus      | X           |           |      |            |
| FTimes           | X           |           |      |            |
| FLess            |             |           |      |            |
| FLessEq          |             |           |      |            |
| #<=              |             |           |      |            |
| #<               |             |           |      |            |
| #>               |             |           |      |            |
| #>=              |             |           |      |            |
| Other FOps       |             |           |      |            |
| Expr.num_int     |             |           |      |            |


-> Check typing rules
-> Look for "INTEGER BYTE-BY-BYTE BREAKDOWN
-> StrNth


-> Maybe error:
`Reduction.substitute_in_int_expr`: does scaling on ints, might end up, if number are not divs, this might fail.
I added a check just in case