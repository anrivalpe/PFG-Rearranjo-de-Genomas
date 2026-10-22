#ifndef H_PERM_TREE
#define H_PERM_TREE

#include "../misc/perm.h"
#include "../misc/util.h"

typedef struct perm_tree perm_tree;
typedef struct pair_of_trees {
    perm_tree *l;
    perm_tree *r;
} pair_of_trees;

perm_tree* build_perm_tree(perm *pi);
void clear_perm_tree(perm_tree *t_free);
void print_perm_tree(perm_tree *t);

int get_root_perm_tree(perm_tree *t);
int pos_to_label(perm_tree *t, int pos);
int label_to_pos(perm_tree *t, int label);

int query_perm_tree(perm_tree *t_mut, int i, int j);
void transposition_perm_tree(perm_tree *t_mut, int i, int j, int k);

#endif
