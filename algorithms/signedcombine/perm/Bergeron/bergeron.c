#include <stdlib.h>
#include <stdio.h>
#include "../misc/util.h"
#include "../misc/stack.h"
#include "../misc/perm.h"

/* #define DEBUG */

#define TEST_HURDLE\
	/* An unoriented component is a hurdle if it is the first one or
	 * if its span does not contain the span of the previous unoriented component. */\
	if (prev_uncomp.fst == EMPTY || !(s <= prev_uncomp.fst && prev_uncomp.snd <= i)) {\
		if(first_pass) hurdles++;\
		prev_hurdle = hurdle;\
		hurdle.fst = s;\
		hurdle.snd = i;\
		/* Next unoriented component contains directly the hurdle. */\
		directly = True;\
		if(s == 0 && i == n+1) {\
			single_hurdle = True;\
		}\
	} else {\
		if(hurdle.fst != EMPTY && s <= hurdle.fst && hurdle.snd >= i && directly) {\
			if(prev_hurdle.fst != EMPTY && s <= prev_hurdle.fst && prev_hurdle.snd >= i) {\
				/* If the span of the unoriented component contains directly the current hurdle and
				 * contains the previous one, this hurdle is not a super hurdle
				 * (consequently we do not have a  fortress). */\
				fortress = False;\
			}\
		}\
		/* Next unoriented component do not contains directly the hurdle. */\
		directly = False;\
		if(!first_pass) {\
			done = True;\
			break;\
		}\
	}\

#define TEST_GRATEST\
	if(possible_greatest_hurdle) {\
		/* The greatest hurdle can not have elements between hurdles. */\
		if (prev_comp.fst != EMPTY && !(s <= prev_comp.fst && prev_comp.snd <= i)) {\
			if(prev_ind_comp.fst != EMPTY && prev_ind_comp.snd < prev_comp.fst - 1) {\
				possible_greatest_hurdle = False;\
			}\
			/* Update last component not contained in the current */\
			prev_ind_comp = prev_comp;\
		}\
		/* Update last found component */\
		if(s != 0 || i != n+1) {\
			prev_comp.fst = s;\
			prev_comp.snd = i;\
		}\
	}\

int count_cycles(perm *pi) {
	int n = length_perm(pi) - 2, cycles = 0, a, k;
	int num_vert = 2*n + 2;
	bool *done = calloc(num_vert + 1,  sizeof(bool)); // indicate visited vertices

	/* 2*a and 2*(a+1) are vertices correspondent to position a of pi. */
	for(int i = 1; i <= num_vert; i++) {
		if(!done[i]) {
			cycles++;
			/* Visit cycle: */
			int j = i;
			do {
				/* black edge */
				if(j % 2 == 1) j++;
				else j--;
				done[j] = True;
				/* gray edge */
				a = get_perm(pi,j / 2);
				if(j % 2 == 1) {
					if(a >= 0) {
						k = get_perm_idx_ignoring_sing(pi,a+1);
						j = (get_perm(pi,k) >= 0) ? 2*k : 2*k + 1;
					} else {
						k = get_perm_idx_ignoring_sing(pi,a+1);
						j = (get_perm(pi,k) < 0) ? 2*k : 2*k + 1;
					}
				} else {
					if(a >= 0) {
						k = get_perm_idx_ignoring_sing(pi,a-1);
						j = (get_perm(pi,k) < 0) ? 2*k : 2*k + 1;
					} else {
						k = get_perm_idx_ignoring_sing(pi,a-1);
						j = (get_perm(pi,k) >= 0) ? 2*k : 2*k + 1;
					}
				}
				done[j] = True;
			} while(j != i);
		}
	}

	free(done);
	return cycles;
}

pair count_hurdles_and_fortress(perm *pi) {
	int tmp, pi_i, hurdles = 0, s, t;
	bool fortress = True, positive;
	bool greatest_uncomp = False;
	bool possible_greatest_hurdle = True;
	bool single_hurdle = False;
	bool directly = False, done = False, first_pass = True;
	int n = length_perm(pi) - 2;

	while(!done) {
		Stack *stack_M = create_stack(n+3);
		push(stack_M, n+2);
		Stack *stack_m = create_stack(n+3);
		push(stack_m, 0);
		Stack *S1 = create_stack(n+3);
		push(S1, 0);
		Stack *S2 = create_stack(n+3);
		push(S2, 0);
		pair prev_uncomp = EMPTY_PAIR;
		pair hurdle = EMPTY_PAIR;
		pair prev_hurdle = EMPTY_PAIR;
		pair prev_comp = EMPTY_PAIR;
		pair prev_ind_comp = EMPTY_PAIR;

		int *M = malloc((n+3) * sizeof(int));
		int *m = malloc((n+3) * sizeof(int));
		int *maximum = malloc((n+3) * sizeof(int));
		int *minimum = malloc((n+3) * sizeof(int));
		int *mark1 = malloc((n+3) * sizeof(int));
		int *mark2 = malloc((n+3) * sizeof(int));
		M[0] = n+1;
		m[0] = 0;
		maximum[0] = 0;
		minimum[0] = n+1;
		mark1[0] = False;
		mark2[0] = False;

		for(int i = 1; i < n+2; i++) {
			pi_i = get_perm(pi,i);
			positive = pi_i >= 0;
			pi_i = abs(pi_i);

			/* Compute the M_i */
			if((tmp = abs(get_perm(pi,i-1))) > pi_i) {
				M[i] = tmp;
				push(stack_M,tmp);
			} else {
				while(top(stack_M) < pi_i) {
					pop(stack_M);
				}
				M[i] = top(stack_M);
			}

			/* Find connected components of type (m ... M) */
			s = top(S1);
			while(pi_i < abs(get_perm(pi,s)) || pi_i > M[s]) {
				pop(S1);
				t = top(S1);
				maximum[t] = max(maximum[t],maximum[s]);
				mark1[t] |= mark1[s];
				s = t;
			}
			if(positive && pi_i == maximum[s] + 1 && i-s == pi_i - abs(get_perm(pi,s)) && i-s > 1) {
				TEST_GRATEST

				/* Unoriented component */
				if(mark1[s] == False) {
					TEST_HURDLE
					#ifdef DEBUG
					printf("unori positive [%d,%d], h = %d \n",s,i,hurdles);
					#endif
					if(s == 0 && i == n+1) {
						greatest_uncomp = True;
					}
					prev_uncomp.fst = s;
					prev_uncomp.snd = i;

				/* Oriented component */
				} else {
					#ifdef DEBUG
					printf("ori positive [%d,%d], h = %d \n",s,i,hurdles);
					#endif
					mark1[s] = False;
				}
			}

			/* And now the "reverse" algorithm */
			/* Compute the m_i */
			if((tmp = abs(get_perm(pi,i-1))) < pi_i) {
				m[i] = tmp;
				push(stack_m,tmp);
			} else {
				while(top(stack_m) > pi_i)
					pop(stack_m);
				m[i] = top(stack_m);
			}

			/* Find connected components of type (-M ... -m) */
			s = top(S2);
			while((pi_i > abs(get_perm(pi,s)) || pi_i < m[s]) && s > 0) {
				pop(S2);
				t = top(S2);
				minimum[t] = min(minimum[t],minimum[s]);
				mark2[t] |= mark2[s];
				s = t;
			}
			if(!positive && pi_i == minimum[s] - 1 && i-s == abs(get_perm(pi,s)) - pi_i && i-s > 1) {
				TEST_GRATEST

				/* Unoriented component */
				if(mark2[s] == False) {
					TEST_HURDLE
					#ifdef DEBUG
					printf("unori negative [%d,%d], h = %d \n",s,i,hurdles);
					#endif
					prev_uncomp.fst = s;
					prev_uncomp.snd = i;
				/* Oriented component */
				} else {
					#ifdef DEBUG
					printf("ori negative [%d,%d], h = %d \n",s,i,hurdles);
					#endif
					mark2[s] = False;
				}
			}

			/* Update stacks and marks */
			if(positive) {
				push(S1,i);
			} else {
				push(S2,i);
			}
			maximum[i] = pi_i;
			minimum[top(S2)] = min(minimum[top(S2)], pi_i);
			minimum[i] = pi_i;
			maximum[top(S1)] = max(maximum[top(S1)], pi_i);
			mark1[top(S1)] = !positive;
			mark2[top(S2)] = positive;
		}

		if(first_pass) {
			/* One test remaining for the greatest hurdle */
			if(prev_ind_comp.fst != EMPTY && prev_ind_comp.snd < prev_comp.fst - 1) {
				possible_greatest_hurdle = False;
			}

			done = True;
			if (!single_hurdle && greatest_uncomp && possible_greatest_hurdle) {
				/* We have the greatest hurdle. */
				hurdles++;
				/* A fortress exists if the number of hurdles is odd and bigger than two.*/
				fortress = fortress && hurdles != 1 && (hurdles % 2) == 1;
				if (fortress) {
					/* We may have a fortress, more work to do. */
					rotate_left(pi,hurdle.snd);
					#ifdef DEBUG
					printf("rotaded perm: ");
					print_perm(pi);
					#endif
					first_pass = False;
					done = False;
				}
			} else {
				/* A fortress exists if the number of hurdles is odd and bigger than two.*/
				fortress = fortress && hurdles != 1 && (hurdles % 2) == 1;
			}
		}

		clear_stack(stack_M);
		clear_stack(stack_m);
		clear_stack(S1);
		clear_stack(S2);
		free(M);
		free(m);
		free(maximum);
		free(minimum);
		free(mark1);
		free(mark2);
	}

	pair hf;
	hf.fst = hurdles;
	hf.snd = fortress;
	return hf;
}

int bergeron(perm *pi) {
	int n = length_perm(pi) - 1;
	int c = count_cycles(pi);
	pair hf = count_hurdles_and_fortress(pi);
	int h = hf.fst;
	int f = hf.snd;
	int d = n - c + h + f;
	#ifdef DEBUG
	printf("n:%d - c:%d + h:%d + f:%d = %d\n",n,c,h,f,d);
	#endif
	return d;
}
