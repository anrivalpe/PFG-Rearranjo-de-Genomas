#ifndef H_UTIL
#define H_UTIL

typedef enum bool {False=0,True} bool;
typedef struct pair {int fst; int snd;} pair;

#define END -1
#define EMPTY -2
#define EMPTY_PAIR {EMPTY, EMPTY}

#define SWAP(T, x, y) { T SWAP = x; x = y; y = SWAP; }

void print_spaces(int n);
int max(int x, int y);
int min(int x, int y);
int div_ceil(int x, int y);
void print_vec(int *v, int n);
void error(char *s);

#endif
