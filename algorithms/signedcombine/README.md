# Heuristics for Genome Rearrangement with Replicated Genes

A collection of heuristics to find rearrangement distances between genomes with multiple genes.

## Dependencies

- [ghc](https://www.haskell.org/ghc/)
- [cabal](https://www.haskell.org/cabal/)
- [stack](https://docs.haskellstack.org/en/stable/README/)

## Execution

To build the aplication just run the folowing command on the root:
```bash
stack build
```

This aplication has 3 diferent executables:
- HeurGRDG: the heuristics to solve rearrangement programs
- MCSP: algorithms for the MCSP (Minimmum Common String Partition) problem, and some variations
- DB: generator of instances

To see the options for each of these executables run:
```bash
stack exec HeurGRDG -- -h
stack exec DB -- -h
stack exec MCSP -- -h
```

## Distance Between Permutations

The heuristics depend upon a black box to calculate the distances between permutations.

As default, we added our own implementation of some know algorithms from literature:

- For signed reversal we use the algorithm describe in the following work:
    - Anne Bergeron, Julia Mixtacki and Jens Stoye. “Reversal Distance without Hurdles and Fortresses”. In: Combinatorial Pattern Matching. Berlin, Heidelberg: Springer
- For unsigned reversal we use the algorithm describe in the following work:
    - John Kececioglu and David Sankoff. “Exact and approximation algorithms for sorting by reversals, with application to genome rearrangement”. In: Algorithmica 13.1-2, pp. 180–210.
- For transposition we use the algorithm describe in the following works:
    - Jianxing Feng and Daming Zhu. “Faster algorithms for sorting by transpositions and sorting by block interchanges”. In: ACM Transactions on Algorithms 3.3, p. 25.
    - Tzvika Hartman and Ron Shamir. “A simpler and faster 1.5-approximation algorithm for sorting by transpositions”. In: Information and Computation 204.2, pp. 275–290.
- For signed reversal and transposition we use the algorithm describe in the following work (the unsigned case uses an adaptation of the same algorithm):
    - Maria Emília M.T. Walter, Zanoni Dias and João Meidanis. “Reversal and transposition distance of linear chromosomes”. In: Proceedings of the String Processing and Information Retrieval: A South American Symposium (SPIRE’1998). Los Alamitos, CA, USA, 1998, pp. 96–102.

For a custom algorithm two methods are available. The simplest one is provide a custom executables with the `-p` flag. That executable should read permutations from the standard input (the other permutation is the identity) and print distances in the standard output. In the end of execution, a character `%` will be printed in the standard input.

The more efficient approach is to provide your own implementation for the `perm_rearrange.h` header. To select that option, just pass the flag `-c` with a value bigger than 0.
