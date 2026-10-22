#ifndef H_CICLE_GRAPH
#define H_CICLE_GRAPH

#include "../misc/perm.h"

typedef struct cicle_graph cicle_graph;

cicle_graph *build_cicle_graph(perm *pi);
void print_cicle_graph(cicle_graph *g, bool as_perm);
void clear_cicle_graph(cicle_graph *g_free);

int length_cicle_graph(cicle_graph *g);
int get_label_cicle_graph(cicle_graph *g, int v);

// black edges are represented by the number of the source vertex
// previous black edge following the order of the permutation
int prev_in_cicle_graph(cicle_graph *g, int e);
// next black edge in the cicle containing e
int next_black_in_cicle_graph(cicle_graph *g, int e);

perm *cicle_graph_to_perm(cicle_graph *g);
void transposition_cicle_graph(cicle_graph *g_mut, int ei, int ej, int ek);
void simple_perm(cicle_graph *g_mut);

void print_ops_cicle_graph(cicle_graph *g);

#endif
