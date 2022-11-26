# First order functional language
p ::= {define {d1 . . . dk} for e} <br />
d := [fun f (x1 . . . xl) = e] <br />
e ::= n | x | {e1 ⊕ e2} | {ifz e0 then e1 else e2} | {let x be e1 in e2} | {f (e1 . . . el)} <br />
⊕ ::= + | - | * | <= <br />
# program p consists of global function definitions (d) that allow us to compute the value of expression e.
