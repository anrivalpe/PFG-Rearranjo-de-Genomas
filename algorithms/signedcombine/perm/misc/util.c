#include <stdio.h>
#include <stdlib.h>
#include "util.h"

void print_spaces(int n) {
    for (int i = 0; i < n; i++) {
        printf(" ");
    }
}

int max(int x, int y) {return (x >= y) ? x : y;}

int min(int x, int y) {return (x <= y) ? x : y;}

int div_ceil(int x, int y) {return x / y + (x % y ? 1 : 0);}

void print_vec(int *v, int n) {
    for(int i = 0; i < n; i++) {
        printf("%d ",v[i]);
    }
    printf("\n");
}

void error(char *s) {
    fprintf(stderr, "ERROR: ");
    fprintf(stderr, "%s\n", s);
    exit(-1);
}
