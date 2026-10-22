#ifndef H_STACK
#define H_STACK

#include "util.h"

typedef struct Stack Stack;

Stack *create_stack(int n); // create a stack to hold a maximmum of n elements
void clear_stack(Stack *s_free);
bool empty_stack(Stack *s);

int top(Stack *s);
void pop(Stack *s_mut);
void push(Stack *s_mut, int val);

void print_stack(Stack *s);

#endif
