#ifndef H_LIST
#define H_LIST

#include "util.h"

typedef struct List List;

List *create_list();
void clear_list(List *l_free);
bool empty_list(List *l);

void *pop_front(List *l_mut);
void *pop_back(List *l_mut);
void push_front(List *l_mut, void *val);
void push_back(List *l_mut, void *val);

void print_list(List *l); // when the value is a string

#endif
