#include <stdlib.h>
#include <stdio.h>
#include "perm_rearrange.h"
#include "misc/util.h"
#include "misc/perm.h"
#include "misc/list.h"
#include "HartmanShamir/hartman_shamir.h"
#include "KaceciogluSankoff/kacecioglu_sankoff.h"
#include "WalterBP/walter_bp.h"
#include "Bergeron/bergeron.h"

int dist(int *g1, int *g2, int size, int with_sign, int mod_, int alg) {
	Model mod = mod_;
	PermType type = (with_sign) ? PSign : PUnsign;
	int dist = -1;
	perm *pi = build_and_rename_perm(g1,g2,size,type, mod);

	if(is_signed(pi) && mod == Rev) {
		dist = bergeron(pi);
	} else {
	  switch(mod) {
		case Rev:
			dist = kacecioglu_sankoff(pi);
			break;
		case Trans:
			if(is_signed(pi))
				error("Algorithm not implemented for that case.");
			dist = hartman_shamir(pi);
			break;
		case TransRev:
			dist = walter_bp(pi);
			break;
		}
	}

  print_ops(pi);
  clear_perm(pi);
  return(dist);
}
