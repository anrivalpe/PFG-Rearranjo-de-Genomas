#include <stdlib.h>
#include <stdio.h>
#include "perm_rearrange.h"
#include "misc/util.h"
#include "misc/perm.h"
#include "misc/list.h"
#include "WalterBP/walter_bp.h"
#include "Bergeron/bergeron.h"

int dist(int *g1, int *g2, int size, int mod_) {
  Model mod = mod_;
  PermType type = PSign;
  int dist = -1;
  perm *pi = build_and_rename_perm(g1,g2,size,type, mod);
   
  if(mod == Rev) {
        dist = bergeron(pi);
    } else {
        dist = walter_bp(pi);
    }

  print_ops(pi);
  clear_perm(pi);
  return(dist);
}

// int main() {
  // int g1[5] = {5,4,3,2,1};
  // int g2[5] = {1,2,3,4,5};
//
  // printf("%d\n",dist(g1, g2, 5, 0, 2));
//
  // return 0;
// }
