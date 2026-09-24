#pragma once
#include "genome.hpp"
#include "../heur/ga.hpp"
#include "../cycle/cycles.hpp"
#include <iostream>

template <class T> void print_vec(ostream &os, vector<T> vec) {
  for (auto a : vec) {
    os << a << " ";
  }
}

struct InputData {
  unique_ptr<Genome> g;
  unique_ptr<Genome> h;
};

vector<string> *read_lines(istream &is);
InputData input(string &line1, string &line2, bool extend);
InputData input(string &line1, string &line2, string &line3, string &line4, bool extend);
void output(ostream &os, int dist, double time);
void output(ostream &os, const CycleGraph &, double time);
void output(ostream &os, InputData &data);
void output(ostream &os, PermsIrs);
