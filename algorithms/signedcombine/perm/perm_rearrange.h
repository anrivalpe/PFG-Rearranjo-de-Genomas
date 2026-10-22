/* Interface to calculate the distance between permutations.
 * Arguments:
 *     g1: vector containing the first permutation
 *     g1: vector containing the second permutation
 *     size: size of the permutations
 *     with_sign: whether the permutations have signs
 *     alg: number passed to the heuristics with the -c flag
 *     mod: which rearrangement model should be consider
 *         0 - Reversal
 *         1 - Transposition 
 *         2 - Reversal and Transposition
 **/
int dist(int *g1, int *g2, int size, int with_sign, int alg, int mod);
