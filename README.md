# Rock

1. `MonadSupply` is basically `Reader` except every time you ask you get a unique new value.

2. The type checker (`infer`) traverses the AST, keeping `Gamma (Map Name {definition :: Term, type :: Term})`, like a usual type checker.

3. When it needs to compare two types, it checks their beta-equivalence. To do this:

  1. It beta-reduces them until it terminates or reaches a recursion depth of 128, whichever happens first.

  2. It checks alpha-equivalence.

If it reaches the recursion depth it reports that it possibly diverges, which is possible because of `Type : Type` and because of fix, which I haven't yet implemented.
