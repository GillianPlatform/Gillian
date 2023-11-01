## In and outs

-> Consuming wands:
  - What are the ins of a wand which has the shape P(+x, y) -* Q(+z, t) ?
  => In order to infer it, we would first need to package it.
     In order to package it, we'd need to start by producing P, therefore we need to know all its parameters.
     So x and y are ins.
  => Worst case scenario, P is emp, and we need to consume Q entirely from the current state, in which case we need to know all the ins of Q
  
  - Another argument: Take the wand `x ↦ m −* x ↦ m * m ↦ 0`. The `m ↦ 0` will be consumed from the current state, when the `x ↦ m` will be consumed from the created memory.
    We need to be able to connect the `m` in the current memory, so it cannot be just some existential, hence we need to know the outs of the lhs in advance.
  
  I would conclude that the is of a magic wand are
  - all args of the lhs
  - ins of the rhs


## Fractioning core predicates

In Rust, core predictes are not atomic like they would be in linear memory/wisl/viper.
This leads to situations akin to what happens in Viper if you try to prove that 
```
acc(x.f, 1/2) => acc(x.f, 1/2) --* acc(x.f)
```
When consuming the rhs, half of the desired state will be in the lhs and half in the current state.
We need some kind of `partial` consumption function that says "I've successfully consumed that share of memory,
but you would need to consume that much more to make things work.