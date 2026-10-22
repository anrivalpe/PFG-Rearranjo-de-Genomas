#include <stdlib.h>
#include "bp_greedy.h"
#include "perm.h"

#define TEST_APPLY_REV(test)                                            \
    if(i != EMPTY && 0 < i && i <= j && j < length_perm(pi) - 1         \
       && test) {                                                       \
        rev(pi,i,j);                                                    \
        ok = True;                                                      \
        break;                                                          \
    }                                                                   \

#define TEST_APPLY_TRANS(test)                                          \
    if(i != EMPTY && k != EMPTY && 0 < i && i < j && j < k &&           \
       k < length_perm(pi) && break_point(pi,i-1,i) &&                  \
       break_point(pi,k-1,k) && test) {                                 \
        trans(pi,i,j,k);                                                \
        ok = True;                                                      \
        break;                                                          \
    }                                                                   \

#define TEST_APPLY_TRANS_3BP TEST_APPLY_TRANS(!break_point(pi,k-1,i))

#define TEST_APPLY_TRANS_2BP                                            \
    TEST_APPLY_TRANS(True);                                             \
    if(i != EMPTY && 0 < i && i < j) {                                  \
        k = get_perm_idx(pi, get_perm(pi,i) - 1) + 1;                   \
        TEST_APPLY_TRANS(True); \
        if(!is_signed(pi)) {                              \
            k = get_perm_idx(pi, get_perm(pi,i) + 1) + 1;               \
            TEST_APPLY_TRANS(True);                                     \
        }                                                               \
    }                                                                   \
    if(k != EMPTY && j < k && k < length_perm(pi)) {                    \
        i = get_perm_idx(pi, get_perm(pi,k-1) + 1);                     \
        TEST_APPLY_TRANS(True);                                         \
        if(!is_signed(pi)) {                              \
            i = get_perm_idx(pi, get_perm(pi,k-1) - 1);                 \
            TEST_APPLY_TRANS(True);                                     \
        }                                                               \
    }                                                                   \

#define TEST_APPLY_TRANS_BP(bp)                 \
    if(bp == 2) {                               \
        TEST_APPLY_TRANS_2BP;                   \
    } else {                                    \
        TEST_APPLY_TRANS_3BP;                   \
    }                                           \

bool apply_2bp_rev(perm *pi) {
	bool ok = False;
	int j = 0, i = 0;

    for(j = end_strip(pi, 1); j < length_perm(pi)-1; j = end_strip(pi, j+1)) {
        if(!is_signed(pi)) {
            i = get_perm_idx(pi, get_perm(pi,j) - 1) + 1;
            TEST_APPLY_REV(!break_point(pi,j+1,i) && break_point(pi,i-1,i));
            i = get_perm_idx(pi, get_perm(pi,j) + 1) + 1;
            TEST_APPLY_REV(!break_point(pi,j+1,i) && break_point(pi,i-1,i));
        } else {
            i = get_perm_idx(pi, - get_perm(pi,j) - 1) + 1;
            TEST_APPLY_REV(get_perm(pi, j+1) == - get_perm(pi, i) + 1);
        }
    }

    return ok;
}

bool apply_nbp_trans(perm *pi, int bp) {
		bool ok = False;
		int j = 0, i = 0, k = 0;

    for(j = end_strip(pi, 0)+1; j < length_perm(pi)-1; j = end_strip(pi, j) + 1) {
        i = get_perm_idx(pi, get_perm(pi,j) - 1) + 1;
        k = get_perm_idx(pi, get_perm(pi,j-1) + 1);
        TEST_APPLY_TRANS_BP(bp);
        if(!is_signed(pi)) {
            i = get_perm_idx(pi, get_perm(pi,j) + 1) + 1;
            TEST_APPLY_TRANS_BP(bp);
            k = get_perm_idx(pi, get_perm(pi,j-1) - 1);
            TEST_APPLY_TRANS_BP(bp);
            i = get_perm_idx(pi, get_perm(pi,j) - 1) + 1;
            TEST_APPLY_TRANS_BP(bp);
        }
    }

    return ok;
}

bool apply_2bp_trans(perm *pi) {
    return apply_nbp_trans(pi,2);
}

bool apply_3bp_trans(perm *pi) {
    return apply_nbp_trans(pi,3);
}
