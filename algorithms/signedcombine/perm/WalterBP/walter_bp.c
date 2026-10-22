#include <stdlib.h>
#include "../misc/perm.h"
#include "../misc/bp_greedy.h"
#include "../misc/util.h"
#include "walter_bp.h"

void apply_1bp_op(perm *pi) {
    int i,j,k;

    i = end_strip(pi,0) + 1;
    for(j = i; abs(get_perm(pi,j)) != get_perm(pi,i-1) + 1; j++);
    k = end_strip(pi,j) + 1;
    if(singleton_strip(pi, j)) {
        if(get_perm(pi,j) < 0) {
            rev(pi,i,j);
        } else {
            trans(pi,i,j,k);
        }
    } else if(j == k - 1){
        rev(pi,i,j);
    } else {
        trans(pi,i,j,k);
    }
}

int walter_bp(perm *pi) {
    int dist=0;

    if(get_model(pi) == Trans) {
      while(!is_sorted(pi)) {
          if(!apply_3bp_trans(pi) && !apply_2bp_trans(pi)) {
              apply_1bp_op(pi);
          }
          dist++;
      }
    } else {
      while(!is_sorted(pi)) {
          if(!apply_3bp_trans(pi) && !apply_2bp_rev(pi) && !apply_2bp_trans(pi)) {
              apply_1bp_op(pi);
          }
          dist++;
      }
    }

    return dist;
}
