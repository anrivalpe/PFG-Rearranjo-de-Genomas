#include <stdlib.h>
#include <stdio.h>

#include "../misc/util.h"
#include "hartman_shamir.h"
#include "cicle_graph.h"
#include "perm_tree.h"
#include "../misc/perm.h"

#define CICLE_2_012 cicle1.pos[0] < cicle2.pos[0] && cicle1.pos[1] < cicle2.pos[1]
#define CICLE_2_021 cicle1.pos[0] < cicle2.pos[0] && cicle2.pos[1] < cicle1.pos[1]
#define CICLE_2_102 cicle2.pos[0] < cicle1.pos[0] && cicle1.pos[1] < cicle2.pos[1]
#define CICLE_2_201 cicle2.pos[1] < cicle1.pos[0]
#define CICLE_3_120 cicle3.pos[0] < cicle1.pos[0] && cicle1.pos[0] < cicle3.pos[1]
#define CICLE_3_210 cicle3.pos[1] < cicle1.pos[0]
#define CICLE_3_012 cicle1.pos[1] < cicle3.pos[1]
#define CICLE_3_021 cicle3.pos[1] < cicle1.pos[1] && cicle1.pos[1] < cicle3.pos[2]

#define ONE_CICLE(e) next_black_in_cicle_graph(g,e) == e
#define IS_ORIENTED(c) (c.pos[1] < c.pos[0])

#define TRANSP_ORIENTED(g,t,c) {                                            \
    transposition_cicle_graph(g, c.e[1], c.e[0], c.e[2]);                   \
    transposition_perm_tree(t, c.pos[1], c.pos[0], c.pos[2]);               \
}                                                                           \

#define TRANSP_UNORIENTED(g,t,c) {                                          \
    transposition_cicle_graph(g, c.e[0], c.e[1], c.e[2]);                   \
    transposition_perm_tree(t, c.pos[0], c.pos[1], c.pos[2]);               \
}                                                                           \

#define TRANSP_INTERLEAVE(g,t,c1,c2) {                                      \
    TRANSP_UNORIENTED(g,t,c1);                                              \
    c2 = update_cicle(c2,c1.pos[0],c1.pos[1],c1.pos[2]);                    \
    c1.pos[1] = c1.pos[0] + c1.pos[2] - c1.pos[1];                          \
    SWAP(int,c1.e[0],c1.e[1]);                                              \
    c2 = sort_cicle(c2);                                                    \
    TRANSP_ORIENTED(g,t,c2);                                                \
    c1 = update_cicle(c1,c2.pos[1],c2.pos[0],c2.pos[2]);                    \
    c1 = sort_cicle(c1);                                                    \
    TRANSP_ORIENTED(g,t,c1);                                                \
}                                                                           \

#define TRANSP_SHAT(g,t,c1,i1,c2,i2,c3,i3) {                                \
    transposition_cicle_graph(g, c1.e[i1], c2.e[i2], c3.e[i3]);             \
    transposition_perm_tree(t, c1.pos[i1], c2.pos[i2], c3.pos[i3]);         \
    cicle1 = update_cicle(cicle1,c1.pos[i1],c2.pos[i2],c3.pos[i3]);         \
}                                                                           \

#define IS_INTERLEAVE(c1,c2)                                                \
((c1.pos[0] < c2.pos[0] && c2.pos[0] < c1.pos[1] && c1.pos[1] < c2.pos[1] &&\
 c2.pos[1] < c1.pos[2] && c1.pos[2] < c2.pos[2]) ||                         \
(c2.pos[0] < c1.pos[0] && c1.pos[0] < c2.pos[1] && c2.pos[1] < c1.pos[1] && \
 c1.pos[1] < c2.pos[2] && c2.pos[2] < c1.pos[2]))                           \

typedef struct three_cicle {
    int e[3], pos[3];
} three_cicle;

void print_cicle(three_cicle c) {
    printf("%d,%d,%d (%d,%d,%d)\n", c.e[0], c.e[1], c.e[2], c.pos[0], c.pos[1], c.pos[2]);
}

three_cicle get_cicle(cicle_graph *g, perm_tree *t, int e) {
    three_cicle c;
    c.e[2] = e;
    c.e[1] = next_black_in_cicle_graph(g, c.e[2]);
    c.e[0] = next_black_in_cicle_graph(g, c.e[1]);
    c.pos[2] = label_to_pos(t, c.e[2]);
    c.pos[1] = label_to_pos(t, c.e[1]);
    c.pos[0] = label_to_pos(t, c.e[0]);
    return c;
}

three_cicle update_cicle(three_cicle c, int i, int j, int k) {
    if(i < c.pos[2] && c.pos[2] < j)
        c.pos[2] += k - j;
    else if(j < c.pos[2] && c.pos[2] < k)
        c.pos[2] -= j - i;
    if(i < c.pos[1] && c.pos[1] < j)
        c.pos[1] += k - j;
    else if(j < c.pos[1] && c.pos[1] < k)
        c.pos[1] -= j - i;
    if(i < c.pos[0] && c.pos[0] < j)
        c.pos[0] += k - j;
    else if(j < c.pos[0] && c.pos[0] < k)
        c.pos[0] -= j - i;
    return c;
}

three_cicle sort_cicle(three_cicle c) {
    while (c.pos[2] < c.pos[1] || c.pos[2] < c.pos[0]) {
        SWAP(int,c.pos[2], c.pos[1]);
        SWAP(int,c.pos[1], c.pos[0]);
        SWAP(int,c.e[2], c.e[1]);
        SWAP(int,c.e[1], c.e[0]);
    }
    return c;
}

void get_five_cicle(cicle_graph *g, perm_tree *t, int e, int cicle[]) {
    int max = label_to_pos(t,e), e_ = e, max_e = e;
    do {
        e_ = next_black_in_cicle_graph(g,e_);
        if(label_to_pos(t,e_) > max) {
            max = label_to_pos(t,e_);
            max_e = e_;
        }
    } while (e != e_);
    e_ = max_e;
    for(int i = 0; i < 5; i++) {
        cicle[i] = e_;
        e_ = next_black_in_cicle_graph(g,e_);
    }
}

int transposition_shattered(cicle_graph *g, perm_tree *t, three_cicle cicle1, three_cicle cicle2, three_cicle cicle3) {
    bool intersect = True;
    int five_cicle[5],i,j,k,ei,ej,ek,e,prev,next,pos_e,pos_prev,pos_next;

    if(cicle2.pos[2] < cicle3.pos[2]) {
        TRANSP_SHAT(g,t,cicle2,0,cicle3,0,cicle3,2)
    } else if(cicle1.pos[0] < cicle2.pos[0]) {
        if(cicle2.pos[0] < cicle3.pos[2])
            TRANSP_SHAT(g,t,cicle3,0,cicle2,0,cicle2,2)
        else
            intersect = False;
    } else if(cicle3.pos[0] < cicle2.pos[0]) {
        if(cicle2.pos[1] < cicle1.pos[0])
            TRANSP_SHAT(g,t,cicle2,1,cicle3,2,cicle2,2)
        else
            TRANSP_SHAT(g,t,cicle2,0,cicle3,2,cicle2,1)
    } else if(cicle2.pos[1] < cicle1.pos[0]) {
        if(cicle3.pos[0] < cicle2.pos[1])
            TRANSP_SHAT(g,t,cicle3,0,cicle3,2,cicle2,2)
        else
            intersect = False;
    } else {
        if(cicle2.pos[1] < cicle3.pos[2])
            TRANSP_SHAT(g,t,cicle2,0,cicle3,0,cicle2,1)
        else
            intersect = False;
    }

    if(!intersect) {
        if(CICLE_3_120 && CICLE_2_012) {
            TRANSP_SHAT(g,t,cicle3,0,cicle3,2,cicle2,2);
        } else if(CICLE_3_210 && CICLE_2_012) {
            TRANSP_SHAT(g,t,cicle3,1,cicle2,0,cicle2,2);
        } else if(CICLE_3_210 && CICLE_2_021) {
            TRANSP_SHAT(g,t,cicle3,0,cicle2,0,cicle2,2);
        } else if(CICLE_3_120 && CICLE_2_021) {
            TRANSP_SHAT(g,t,cicle3,0,cicle3,2,cicle2,2);
        } else if(CICLE_3_012 && CICLE_2_102) {
            TRANSP_SHAT(g,t,cicle2,0,cicle3,0,cicle3,2);
        } else if(CICLE_3_021 && CICLE_2_102) {
            TRANSP_SHAT(g,t,cicle2,0,cicle3,0,cicle3,2);
        } else if(CICLE_3_021 && CICLE_2_201) {
            TRANSP_SHAT(g,t,cicle2,1,cicle3,0,cicle3,2);
        } else if(CICLE_3_120 && CICLE_2_102) {
            TRANSP_SHAT(g,t,cicle2,0,cicle3,1,cicle2,1);
        } else if(CICLE_3_210 && CICLE_2_102) {
            TRANSP_SHAT(g,t,cicle3,0,cicle3,2,cicle2,2);
        } else if(CICLE_3_120 && CICLE_2_201) {
            TRANSP_SHAT(g,t,cicle2,1,cicle3,1,cicle2,2);
        } else if(CICLE_3_012 && CICLE_2_201) {
            TRANSP_SHAT(g,t,cicle2,0,cicle3,0,cicle3,2);
        } else if(CICLE_3_210 && CICLE_2_201) {
            TRANSP_SHAT(g,t,cicle2,1,cicle3,2,cicle2,2);
        }
    }

    TRANSP_ORIENTED(g,t,cicle1);

    if(ONE_CICLE(cicle2.e[0])) get_five_cicle(g,t,cicle2.e[1],five_cicle);
    else get_five_cicle(g,t,cicle2.e[0],five_cicle);

    prev = five_cicle[4];
    i = j = k = -1;
    for(int c_idx = 0; c_idx < 5; c_idx++) {
        e = five_cicle[c_idx];
        next = five_cicle[(c_idx + 1) % 5];

        pos_e = label_to_pos(t,e);
        pos_prev = label_to_pos(t,prev);
        pos_next = label_to_pos(t,next);
        if(pos_next < pos_prev) {
            if(pos_e < pos_next && pos_e < pos_prev) {
                i = pos_e; j = pos_next; k = pos_prev;
                ei = e; ej = next; ek = prev;
                break;
            }
            if(pos_next < pos_e && pos_prev < pos_e) {
                i = pos_next; j = pos_prev; k = pos_e;
                ei = next; ej = prev; ek = e;
                break;
            }
        } else if(pos_prev < pos_e && pos_e < pos_next) {
            i = pos_prev; j = pos_e; k = pos_next;
            ei = prev; ej = e; ek = next;
            break;
        }

        prev = e;
    }
    if(i == -1) error("Five cicle is not oriented.");

    transposition_cicle_graph(g, ei, ej, ek);
    transposition_perm_tree(t, i, j, k);
    return 3;
}

int remove_2_cicles(cicle_graph *g) {
    int count_trans = 0,e1,e2,e3,val;
    int e,e_,count,num_two_cicles=0;
    int size = length_cicle_graph(g);
    bool *two_cicles = calloc(size, sizeof(bool));
    int *es = malloc(size * sizeof(int));
    int *pos = malloc(size * sizeof(int));

    pos[0] = 0;
    e = val = size-1;
    while(e > 0) {
        pos[e] = val;
        val--;
        count = 0;
        e_ = e;
        do {
            e_ = next_black_in_cicle_graph(g,e_);
            count++;
        } while(e_ != e);
        if(count == 2) {
            if(!two_cicles[next_black_in_cicle_graph(g,e)]) {
                two_cicles[e] = True;
                two_cicles[next_black_in_cicle_graph(g,e)] = True;
                es[num_two_cicles] = e;
                num_two_cicles++;
            }
        }
        e = prev_in_cicle_graph(g,e);
    }

    for(int idx = 0; idx < num_two_cicles; idx+=2) {
        e1 = es[idx];
        e2 = next_black_in_cicle_graph(g,e1);
        e3 = es[idx+1];
        if(pos[e1] > pos[e2]) SWAP(int,e1,e2);
        if(pos[e3] < pos[e1]) transposition_cicle_graph(g,e3,e1,e2);
        else if(pos[e3] > pos[e2]) transposition_cicle_graph(g,e1,e2,e3);
        else transposition_cicle_graph(g,e1,e3,e2);
        count_trans++;
    }

    free(two_cicles);
    free(es);
    free(pos);
    return count_trans;
}

int remove_3_cicles(cicle_graph *g) {
    int e,e_, count_trans = 0, size = length_cicle_graph(g);
    three_cicle cicle1,cicle2,cicle3;
    perm *pi = cicle_graph_to_perm(g);
    perm_tree *t = build_perm_tree(pi);

    e = size - 1;
    while(e > 0) {

        if(ONE_CICLE(e)) {
            e = prev_in_cicle_graph(g,e);
            continue;
        }

        cicle1 = get_cicle(g,t,e);
        if(IS_ORIENTED(cicle1)) {
            TRANSP_ORIENTED(g,t,cicle1);
            count_trans++;
            continue;
        }

        e_ = query_perm_tree(t, cicle1.pos[1], cicle1.pos[2]);
        cicle2 = sort_cicle(get_cicle(g,t,e_));
        if(IS_ORIENTED(cicle2)) {
            TRANSP_ORIENTED(g,t,cicle2);
            count_trans++;
            continue;
        }
        if(IS_INTERLEAVE(cicle1,cicle2)) {
            TRANSP_INTERLEAVE(g,t,cicle1,cicle2);
            count_trans += 3;
            continue;
        }

        e_ = query_perm_tree(t, cicle1.pos[0], cicle1.pos[1]);
        cicle3 = sort_cicle(get_cicle(g,t,e_));
        if(IS_ORIENTED(cicle3)) {
            TRANSP_ORIENTED(g,t,cicle3);
            count_trans++;
            continue;
        }
        if(IS_INTERLEAVE(cicle1,cicle3)) {
            TRANSP_INTERLEAVE(g,t,cicle1,cicle3);
            count_trans += 3;
            continue;
        }
        if(IS_INTERLEAVE(cicle2,cicle3)) {
            TRANSP_INTERLEAVE(g,t,cicle2,cicle3);
            count_trans += 3;
            continue;
        }

        count_trans += transposition_shattered(g,t,cicle1,cicle2,cicle3);
    }

    clear_perm(pi);
    clear_perm_tree(t);
    return count_trans;
}

int hartman_shamir(perm *pi) {
    int dist=0;

    cicle_graph *g = build_cicle_graph(pi);
    simple_perm(g);
    dist += remove_2_cicles(g);
    dist += remove_3_cicles(g);

    perm *pi_ = cicle_graph_to_perm(g);
    if(!is_sorted(pi_)) {
        print_perm(pi_);
        error("Not Sorted");
    }
    clear_perm(pi_);

    print_ops_cicle_graph(g);
    clear_cicle_graph(g);
    return dist;
}
