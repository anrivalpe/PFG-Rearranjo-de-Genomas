#include <stdlib.h>
#include <stdio.h>
#include "util.h"
#include "list.h"

typedef struct Node {
  void *val;
  struct Node *next;
  struct Node *prev;
} Node;

struct List {
  Node *head;
  Node *tail;
  int size;
};

List *create_list() {
  List *l = malloc(sizeof(List));
  l->head = NULL;
  l->tail = NULL;
  l->size = 0;
  return l;
}

void clear_list(List *l) {
  while(!empty_list(l)) {
    void *val = pop_front(l);
    free(val);
  }
  free(l);
}

bool empty_list(List *l) {
  return l->size == 0;
}

void *pop_front(List *l) {
  if(empty_list(l)) return NULL;
  Node *h = l->head;
  Node *n = h->next;
  void *val = h->val;

  free(h);
  l->size -= 1;
  if(l->head == l->tail) {
    l->head = NULL;
    l->tail = NULL;
  } else {
    l->head = n;
    n->prev = NULL;
  }

  return val;
}

void *pop_back(List *l) {
  if(empty_list(l)) return NULL;
  Node *t = l->tail;
  Node *n = t->prev;
  void *val = t->val;

  free(t);
  l->size -= 1;
  if(l->head == l->tail) {
    l->head = NULL;
    l->tail = NULL;
  } else {
    l->tail = n;
    n->prev = NULL;
  }

  return val;
}

Node *create_node(void *val) {
  Node *n = malloc(sizeof(Node));
  n->val = val;
  n->next = NULL;
  n->prev = NULL;
  return n;
}

void push_front(List *l, void *val) {
  Node *n = create_node(val);
  if(empty_list(l)) {
    l->head = n;
    l->tail = n;
  } else {
    l->head->prev = n;
    n->next = l->head;
    l->head = n;
  }
  l->size += 1;
}

void push_back(List *l, void *val) {
  Node *n = create_node(val);
  if(empty_list(l)) {
    l->head = n;
    l->tail = n;
  } else {
    l->tail->next = n;
    n->prev = l->tail;
    l->tail = n;
  }
  l->size += 1;
}

void print_list(List *l) {
  for(Node *n = l->head; n != NULL; n = n->next) {
    printf(" %s ",(char*) n->val);
  }
  printf("\n");
}
