# First order functional language
p ::= {define {d1 . . . dk} for e} <br />
d := [fun f (x1 . . . xl) = e] <br />
e ::= n | x <br />
| {e1 ⊕ e2} <br />
| {ifz e0 then e1 else e2} <br />
| {let x be e1 in e2} <br />
| {f (e1 . . . el)} <br />
⊕ ::= + | - | * | <= <br />
# program p consists of global function definitions (d) that allow us to compute the value of expression e.
