#include <stdlib.h>
#include <stdio.h>
#include "perm_tree.h"
#include "../misc/util.h"
#include "../misc/perm.h"

typedef struct perm_node {
    struct perm_node *l;
    struct perm_node *r;
    struct perm_node *p;
    int height;
    int label;
    int size;
} perm_node;

struct perm_tree {
    perm_node *roots[4];
    perm_node **leafs;
    int size;
    int *pos;
    long int *pos_version;
    long int current_version;
};

perm_node* empty_node() {
    perm_node *t = malloc(sizeof(perm_node));
    t->l = NULL;
    t->r = NULL;
    t->p = NULL;
    t->height = 0;
    t->label = EMPTY;
    t->size = 0;
    return t;
}

perm_node* node(int label) {
    perm_node *t = malloc(sizeof(perm_node));
    t->l = NULL;
    t->r = NULL;
    t->p = NULL;
    t->height = 0;
    t->label = label;
    t->size = 1;
    return t;
}

perm_node* combine_nodes(perm_node *l, perm_node *r) {
    perm_node *t = node(max(l->label,r->label));
    t->l = l;
    t->r = r;
    l->p = t;
    r->p = t;
    t->height = max(l->height,r->height) + 1;
    t->size = l->size + r->size;
    return t;
}

perm_node* build_perm_tree_(perm_tree *t, perm *pi, int i, int n) {
    int label;
    perm_node *l,*r,*p;

    if(n == 1) {
        label = get_perm(pi,i);
        t->leafs[label] = node(label);
        t->leafs[label]->l = empty_node();
        t->leafs[label]->r = empty_node();
        return t->leafs[label];
    }
    l = build_perm_tree_(t, pi, i, n/2);
    r = build_perm_tree_(t, pi, i + n/2, div_ceil(n,2));
    p = combine_nodes(l,r);

    return p;
}

perm_tree *build_perm_tree(perm *pi) {
    perm_tree *t = malloc(sizeof(perm_tree));
    t->leafs = malloc( length_perm(pi) * sizeof(perm_node*));
    t->roots[0] = build_perm_tree_(t, pi, 0, length_perm(pi));
    t->size = length_perm(pi);
    t->pos = malloc( length_perm(pi) * sizeof(int));
    t->pos_version = calloc(length_perm(pi), sizeof(long int));
    t->current_version = 1;
    return t;
}

void clear_perm_tree_(perm_node *t) {
    if(t->label != EMPTY) {
        clear_perm_tree_(t->l);
        clear_perm_tree_(t->r);
    }
    free(t);
}

void clear_perm_tree(perm_tree *t_free) {
    clear_perm_tree_(t_free->roots[0]);
    free(t_free->leafs);
    free(t_free->pos);
    free(t_free->pos_version);
    free(t_free);
}

perm_node* join_perm_tree_(perm_node *t1, perm_node *t2) {
    perm_node *t,*t_,*t__;

    if(t2->label == EMPTY) {
        free(t2);
        return t1;
    }
    if(t1->label == EMPTY) {
        free(t1);
        return t2;
    }
    if (abs(t1->height - t2->height) <= 1) {
        t = combine_nodes(t1,t2);
    } else if (t1->height - t2->height == 2) {
        if (t1->l->height >= t1->r->height) {
            t_ = combine_nodes(t1->r,t2);
            t = combine_nodes(t1->l,t_);
        } else {
            t_ = combine_nodes(t1->l,t1->r->l);
            t__ = combine_nodes(t1->r->r,t2);
            t = combine_nodes(t_,t__);
            free(t1->r);
        }
        free(t1);
    } else if (t1->height - t2->height > 2) {
        t = join_perm_tree_(t1->l, join_perm_tree_(t1->r,t2));
        free(t1);
    } else if (t2->height - t1->height == 2) {
        if (t2->r->height >= t2->l->height) {
            t_ = combine_nodes(t1,t2->l);
            t = combine_nodes(t_,t2->r);
        } else {
            t_ = combine_nodes(t1,t2->l->l);
            t__ = combine_nodes(t2->l->r,t2->r);
            t = combine_nodes(t_,t__);
            free(t2->l);
        }
        free(t2);
    } else if (t2->height - t1->height > 2) {
        t = join_perm_tree_(join_perm_tree_(t1,t2->l), t2->r);
        free(t2);
    }

    return t;
}

void join_perm_tree(perm_tree *t, int idx1, int idx2) {
    t->roots[idx1] = join_perm_tree_(t->roots[idx1], t->roots[idx2]);
}

void split_perm_tree(perm_tree *t, int label, int idx_l) {
    perm_node *node = t->leafs[label], *parent;
    int idx_r = idx_l + 1;
    bool leaf = True;

    t->roots[idx_r] = node;
    t->roots[idx_l] = empty_node();
    if(node->p == NULL) return;
    while(node->p != NULL) {
        parent = node->p;
        if(parent->l->label == node->label)
            t->roots[idx_r] = join_perm_tree_(t->roots[idx_r], parent->r);
        else
            t->roots[idx_l] = join_perm_tree_(parent->l, t->roots[idx_l]);
        if(leaf) leaf = False;
        else free(node);
        node = parent;
    }
    free(node);
}

void print_perm_tree_(perm_node *t, int level) {
    if(t->label == EMPTY) return;
    print_perm_tree_(t->l, level+1);
    print_spaces(level*4 - 1);
    printf("-%d-+\n",t->label);
    print_perm_tree_(t->r, level+1);
}

void print_perm_tree_idx(perm_tree *t, int idx) {
    printf("\n");
    if(t->roots[idx]->label == EMPTY) {
        printf("EMPTY TREE\n");
        return;
    }
    print_perm_tree_(t->roots[idx]->l, 1);
    printf("%d,%d-+\n",t->roots[idx]->label,t->roots[idx]->size);
    print_perm_tree_(t->roots[idx]->r, 1);
}

void print_perm_tree(perm_tree *t) {
    print_perm_tree_idx(t,0);
}

int pos_to_label(perm_tree *t, int pos) {
    perm_node *node = t->roots[0];

    while(node->size > 1) {
        if(pos < node->l->size) {
            node = node->l;
        } else {
            pos -= node->l->size;
            node = node->r;
        }
    }

    return node->label;
}

int label_to_pos(perm_tree *t, int label) {
    perm_node *node;
    int pos = 0;

    if(t->pos_version[label] == t->current_version) {
        return t->pos[label];
    }
    node = t->leafs[label];
    while(node->p != NULL) {
        if(node->p->r->label == node->label)
            pos += node->p->l->size;
        node = node->p;
    }
    t->pos[label] = pos;
    t->pos_version[label] = t->current_version;

    return pos;
}

int query_perm_tree(perm_tree *t, int i, int j) {
    int k,li,lj;

    if (i > j) SWAP(int,i,j);
    li = pos_to_label(t,i);
    lj = pos_to_label(t,j-1);

    split_perm_tree(t,li,0);
    split_perm_tree(t,lj,1);
    k = t->roots[1]->label;

    join_perm_tree(t,1,2);
    join_perm_tree(t,0,1);
    return k+1;
}

void transposition_perm_tree(perm_tree *t, int i, int j, int k) {
    int li,lj,lk;

    li = pos_to_label(t,i);
    lj = pos_to_label(t,j);
    lk = pos_to_label(t,k);

    split_perm_tree(t,li,0);
    split_perm_tree(t,lj,1);
    split_perm_tree(t,lk,2);
    join_perm_tree(t,0,2);
    join_perm_tree(t,0,1);
    join_perm_tree(t,0,3);
    t->current_version++;
}
