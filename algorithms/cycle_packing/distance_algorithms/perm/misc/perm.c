#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "perm.h"
#include "../misc/util.h"
#include "../misc/list.h"

// #define RECORD

struct perm {
    int *vet;
    int *inv_vet;
    bool valid_inv;
    int size;
    PermType type;
    Model model;
#ifdef RECORD
    List *ops;
#endif
};

inline int length_perm(perm *pi) {return pi->size;}
inline int get_perm(perm *pi, int i) {return pi->vet[i];}
inline void set_perm(perm *pi, int i, int val) {pi->vet[i] = val; pi->valid_inv = False;}
inline Model get_model(perm *pi) {return pi->model;}

int get_perm_idx_ignoring_sing(perm *pi, int val) {
    int idx;

    if(!pi->valid_inv) {
        for(int i = 0; i < length_perm(pi); i++)
            pi->inv_vet[abs(get_perm(pi,i))] = i;
        pi->valid_inv = True;
    }
    idx = pi->inv_vet[abs(val)];
	return idx;
}

inline int get_perm_idx(perm *pi, int val) {
    int idx;

	idx = get_perm_idx_ignoring_sing(pi,val);
    if (pi->vet[idx] == val) {
        return idx;
    } else {
        return EMPTY;
    }
}

perm *create_perm(int size, PermType type, Model mod) {
    perm *pi = malloc(sizeof(perm));
    pi->vet = malloc(size * sizeof(int));
    pi->inv_vet = malloc(size * sizeof(int));
    pi->size = size;
    pi->type = type;
    pi->model = mod;
    pi->valid_inv = False;
#ifdef RECORD
    pi->ops = create_list();
#endif
    return pi;
}

perm *build_and_rename_perm(int *vet1, int *vet2, int n, PermType type, Model mod) {
    perm *pi = create_perm(n+2, type, mod);
    int *labels = malloc((n+2) * sizeof(int));

    for(int i = 0; i < n; i++) {
        labels[abs(vet2[i])] = (vet2[i] < 0) ? -i-1 : i+1;
    }

    pi->vet[0] = 0;
    for(int i = 1; i <= n; i++) {
        pi->vet[i] = (vet1[i-1] < 0) ? - labels[abs(vet1[i-1])] : labels[abs(vet1[i-1])];
    }
    pi->vet[n+1] = n+1;

    free(labels);
    return pi;
}

perm *build_perm(int *vet, int n, PermType type, Model mod) {
    perm *pi = create_perm(n+2, type, mod);
    pi->vet[0] = 0;
    for(int i = 1; i <= n; i++) {
        pi->vet[i] = vet[i-1];
    }
    pi->vet[n+1] = n+1;
    return pi;
}

void clear_perm(perm *pi) {
    free(pi->vet);
    free(pi->inv_vet);
#ifdef RECORD
    clear_list(pi->ops);
#endif
    free(pi);
}

void print_perm(perm *pi) {
    print_vec(pi->vet, pi->size);
}

bool is_sorted(perm *pi) {
    for(int i = 1; i < pi->size; i++) {
        if(pi->vet[i-1] > pi->vet[i]) return False;
    }
    return True;
}

bool break_point(perm *pi, int x, int y) {
    if( (pi->model == Rev || pi->model == TransRev) && !is_signed(pi) ) {
        return abs(pi->vet[y] - pi->vet[x]) != 1;
    } else {
        return pi->vet[y] - pi->vet[x] != 1;
    }
}

void rev_(perm *pi, int i, int j) {
    for(int k = 0; k < (j-i+1) / 2; k++) {
        SWAP(int, pi->vet[i+k], pi->vet[j-k]);
    }
    if(is_signed(pi)) {
        for(int k = i; k <= j; k++) {
            pi->vet[k] = - pi->vet[k];
        }
    }
}

void rev(perm *pi, int i, int j) {
    pi->valid_inv = False;
    switch(pi->model) {
      case Rev:
      case TransRev:
          rev_(pi, i, j);
          break;
      default:
          error("Invalid Operation (Rev) in Permutation");
    }
#ifdef RECORD
    char *str = malloc(20*sizeof(char));
    sprintf(str,"(%d,%d)",i,j);
    push_back(pi->ops,str);
#endif
}

void trans(perm *pi, int i, int j, int k) {
    pi->valid_inv = False;
    switch(pi->model) {
    case Trans:
    case TransRev:
        rev_(pi, i, k - 1);
        rev_(pi, i, i + k - j - 1);
        rev_(pi, i + k - j, k - 1);
        break;
    default:
        error("Invalid Operation (Trans) in Permutation");
    }
#ifdef RECORD
    char *str = malloc(20*sizeof(char));
    sprintf(str,"(%d,%d,%d)",i,j,k);
    push_back(pi->ops,str);
#endif
}

int end_strip(perm *pi, int i) {
    while(i < pi->size-1 && !break_point(pi,i,i+1)) i++;
    return i;
}

bool is_signed(perm *pi) {
    switch(pi->type) {
    case PSign:
        return True;
    case PUnsign:
        return False;
    }
    return False;
}

inline bool singleton_strip(perm *pi, int i) {return (break_point(pi,i-1,i) && break_point(pi,i,i+1));}

void print_ops(perm *pi) {
#ifdef RECORD
  print_list(pi->ops);
#endif
  return;
}

void rotate_left(perm *pi, int r) {
    int *tmp = malloc(pi->size * sizeof(int));
    int *labels = malloc(pi->size * sizeof(int));

	for(int i = 0; i < pi->size; i++) {
		tmp[i] = pi->vet[(i+r) % pi->size];
        labels[(i+r) % pi->size] = i+1;
	}

	/* Rename perm */
	free(pi->vet);
    pi->valid_inv = False;
	pi->size += 2;
	pi->vet = malloc(pi->size * sizeof(int));
	pi->vet[0] = 0; 
    for(int i = 0; i < pi->size - 2; i++) {
        pi->vet[i+1] = (tmp[i] < 0) ? - labels[abs(tmp[i])] : labels[abs(tmp[i])];
    }
	pi->vet[pi->size - 1] = pi->size - 1; 

    free(labels);
	free(tmp);
}

void shuffle(perm *pi) {
	/* srand(time(NULL)); */
	for (int i = pi->size - 2; i >= 1; i--){
		/* j is a random index in [1, i] */
		int j = 1 + rand() % i;
		SWAP(int,pi->vet[i],pi->vet[j])
		if(rand() % 2) {
			pi->vet[i] = - pi->vet[i];
		}
	}
}

int *to_list(perm *pi) {
	return pi->vet + 1;
}
