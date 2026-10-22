#include "../misc/perm.h"
#include <unistd.h>
#include <stdio.h>
#include "../misc/bp_greedy.h"
#include "kacecioglu_sankoff.h"

int smallest_dec_strip(perm *pi) {
	int y = 0, x = 0, k = length_perm(pi)-1;

	while(x < length_perm(pi)-1) {
		y = end_strip(pi, x);
		if(x > 0 && get_perm(pi,x) >= get_perm(pi,y)) {
			if(get_perm(pi,y) < get_perm(pi,k)) { k = y; }
		}
		x = y + 1;
	}

	return k;
}

static int largest_dec_strip(perm *pi) {
	int y = 0, x = 0, l = 0;

	while(x < length_perm(pi)-1) {
		y = end_strip(pi, x);
        if(x > 0 && get_perm(pi,x) >= get_perm(pi,y)) {
			if(get_perm(pi,x) > get_perm(pi,l)) { l = x; }
		}
		x = y + 1;
	}

	return l;
}

void apply_1bp_rev_approx(perm *pi) {
	int i = 0, j = 0, k = 0, l = 0;

    k = smallest_dec_strip(pi);

    if(k < length_perm(pi) - 1) {

        i = k;
        for(int x = 0; x < length_perm(pi); x++) {
            if(get_perm(pi,x) == get_perm(pi,k) - 1) {
                j = x;
                break;
            }
        }

        if(i < j) { i += 1; } else { j += 1; }

        if(i > j) { SWAP(int, i , j); }

        rev(pi,i,j);

        if(!is_sorted(pi) && smallest_dec_strip(pi) == length_perm(pi) - 1) {

            rev(pi,i,j);
            l = largest_dec_strip(pi);
            i = l;

            for(int x = 0; x < length_perm(pi); x++) {
                if(get_perm(pi,x) == get_perm(pi,l) + 1) {
                    j = x;
                    break;
                }
            }

            if(i < j) { j -= 1; } else { i -= 1; }

            if(i > j) { SWAP(int, i , j); }

            rev(pi,i,j);
        }

    } else {
        i = end_strip(pi, 0) + 1;
        j = end_strip(pi, i);
        rev(pi,i,j);
    }
}

int kacecioglu_sankoff(perm *pi) {
    int dist=0;

    if(is_signed(pi))
      error("Algoritm does not work on sign permutation");

    while(!is_sorted(pi)) {
        if(!apply_2bp_rev(pi)) {
            apply_1bp_rev_approx(pi);
        }
        dist++;
    }

    return dist;
}
