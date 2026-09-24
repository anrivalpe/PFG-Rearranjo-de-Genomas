#include <stdlib.h>
#include <stdio.h>
#include "stack.h"
#include "../misc/util.h"

typedef struct Stack {
	int *els;
	int top;
} Stack;

inline Stack *create_stack(int n) {
	Stack *s = malloc(sizeof(Stack));
	s->top = -1;
	s->els = malloc(n * sizeof(int));
	return s;
}

inline void clear_stack(Stack *s_free) {
	free(s_free->els);
	free(s_free);
}

inline bool empty_stack(Stack *s) {return s->top == -1;}

inline int top(Stack *s_mut) { return s_mut->els[s_mut->top]; }

inline void pop(Stack *s_mut) { s_mut->top--; }

inline void push(Stack *s_mut, int val) {
	s_mut->top++;
	s_mut->els[s_mut->top] = val;
}

void print_stack(Stack *s) {
	printf("[");
	for(int i = 0; i <= s->top; i++) {
		printf(" %d ", s->els[i]);
	}
	printf("]\n");
}
