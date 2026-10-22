#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "cicle_graph.h"
#include "../misc/perm.h"
#include "../misc/util.h"
#include "../misc/list.h"

// #define RECORD

struct cicle_graph {
    int *blacks;
    int *grays;
    int size;
#ifdef RECORD
    List *ops;
#endif
};

inline int length_cicle_graph(cicle_graph *g) { return g->size; }

inline int next_black_in_cicle_graph(cicle_graph *g, int e) { return g->grays[g->blacks[e]]; }

inline int prev_in_cicle_graph(cicle_graph *g, int e) { return g->blacks[e]; };

cicle_graph *create_cicle_graph(int size) {
    cicle_graph *g = malloc(sizeof(cicle_graph));
    g->size = size;
    g->blacks = malloc(g->size * sizeof(int));
    g->grays = malloc(g->size * sizeof(int));
#ifdef RECORD
    g->ops = create_list();
#endif
    return g;
}

void fill_cicle_graph(cicle_graph *g, perm *pi) {
    g->blacks[0] = END;
    for(int i = 1; i < g->size; i++) {
        g->blacks[get_perm(pi,i)] = get_perm(pi,i-1);
    }

    for(int i = 0; i < g->size - 1; i++) {
        g->grays[i] = i+1;
    }
    g->grays[g->size - 1] = END;
}

cicle_graph *build_cicle_graph(perm *pi) {
    cicle_graph *g = create_cicle_graph(length_perm(pi));
    fill_cicle_graph(g,pi);
    return g;
}

void clear_cicle_graph(cicle_graph *g) {
    free(g->blacks);
    free(g->grays);
#ifdef RECORD
    clear_list(g->ops);
#endif
    free(g);
}

void print_cicle_graph(cicle_graph *g, bool as_perm) {
    if(as_perm) {
        perm *pi = cicle_graph_to_perm(g);
        print_perm(pi);
        clear_perm(pi);
        return;
    }
    printf("B: ");
    for(int i = 0; i < g->size; i++) {
        if(g->blacks[i] != END)
            printf("(%d,%d) ",i,g->blacks[i]);
    }
    printf("\n");
    printf("G: ");
    for(int i = 0; i < g->size; i++) {
        if(g->grays[i] != END)
            printf("(%d,%d) ",i,g->grays[i]);
    }
    printf("\n");
}

perm *cicle_graph_to_perm(cicle_graph *g) {
    int *labels = malloc(g->size * sizeof(int));
    perm *pi = create_perm(g->size, PUnsign, Trans);

    int v,w,i;

    v = 0;
    i = 0;
    while(v != END) {
        labels[v] = i;
        i++;
        w = v;
        v = g->grays[v];
    }

    v = w;
    i = g->size-1;
    while(v != END) {
        set_perm(pi,i,labels[v]);
        i--;
        v = g->blacks[v];
    }

    free(labels);
    return pi;
}

void increase_cicle_graph_max_size(cicle_graph *g, int max_size) {
    int *blacks = malloc(max_size * sizeof(int));
    int *grays = malloc(max_size * sizeof(int));
    memcpy(blacks, g->blacks, g->size * sizeof(int));
    memcpy(grays, g->grays, g->size * sizeof(int));
    free(g->blacks);
    free(g->grays);
    g->blacks = blacks;
    g->grays = grays;
}

void gb_split(cicle_graph *g, int black, int gray) {
    int v = g->size;
    int w = g->size + 1;
    g->size += 2;
    g->blacks[v] = w;
    g->grays[w] = v;

    g->blacks[w] = g->blacks[black];
    g->blacks[black] = v;
    g->grays[v] = g->grays[gray];
    g->grays[gray] = w;
}

void simple_perm(cicle_graph *g) {
    int v,w,black,gray,count;
    bool *in_simple = calloc(g->size*2, sizeof(bool));
    perm *pi;

    increase_cicle_graph_max_size(g, 2*g->size);
    v = g->size - 1;
    while(v != 0) {
        if(in_simple[v]) {
            v--;
        } else {
            black = v;
            count = 0;
            w = v;
            do {
                gray = g->blacks[w];
                w = g->grays[gray];
                in_simple[w] = True;
                count++;
            } while(w != v && count < 3);
            if(w != v) {
                in_simple[w] = False;
                gb_split(g,black,gray);
            }
        }
    }

    free(in_simple);
    pi = cicle_graph_to_perm(g);
    fill_cicle_graph(g,pi);
    clear_perm(pi);
}

void transposition_cicle_graph(cicle_graph *g, int ei, int ej, int ek) {
    int aux;
    aux = g->blacks[ej];
    g->blacks[ej] = g->blacks[ei];
    g->blacks[ei] = g->blacks[ek];
    g->blacks[ek] = aux;
#ifdef RECORD
    char *str = malloc(20*sizeof(char));
    sprintf(str,"(%d,%d,%d)",ei,ej,ek);
    push_back(g->ops,str);
#endif
}

void print_ops_cicle_graph(cicle_graph *g) {
#ifdef RECORD
  print_list(g->ops);
#endif
  return;
}
