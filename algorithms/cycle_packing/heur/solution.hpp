#pragma once

#include <iostream>
#include <set>
#include <utility>
using namespace std;

template <class E>
class Solution {
 public:
  virtual ~Solution<E>() {}
  virtual Solution<E> *copy() = 0;
  virtual Solution<E> *empty_sol() = 0;
  virtual bool add(E) = 0;
  virtual bool check(E) = 0;
  virtual void remove(E) = 0;
  virtual int delta_insertion(E) = 0;
  virtual int delta_deletion(E) = 0;
  virtual int eval() = 0;
  virtual typename set<E>::const_iterator begin() const = 0;
  virtual typename set<E>::const_iterator end() const = 0;
  virtual void serialize(ostream &) const = 0;
};

template <class E>
ostream &operator<<(ostream &os, const Solution<E> &sol) {
  sol.serialize(os);
  return os;
}
