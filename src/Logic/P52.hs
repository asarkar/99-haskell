module Logic.P52 where

{-
Problem 52: (***) Conjunctive normal form.

It is known that any boolean function can be represented in conjunctive normal form.
These are conjunctions of disjunctions of literals, where literals are one of boolean
values, variables, or the complement of values or variables.

Return the conjunctive normal form of a boolean formula. The value returned should
always be a conjunction of disjunctions.

data Formula
  Constructors:

  Value Bool: A constant value.

  Variable String: A variable with given name.

  Complement Formula: Logical complement. I.e., it is true only if its clause is false.

  Disjoin [Formula]: Disjunction. I.e., it is true if any of its clauses are true.

  Conjoin [Formula]: toConjunctiveNormalForm :: Formula -> Formula

Examples:
  >>> toConjunctiveNormalForm $ Value True
  Conjoin [Disjoin [Value True]]

  >>> toConjunctiveNormalForm $ Complement $ Disjoin [Variable "X", Variable "Y"]
  Conjoin [Disjoin [Complement (Variable "X")],Disjoin [Complement (Variable "Y")]]

ANSWER: TODO.
-}
