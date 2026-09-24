#pragma once

#include <iostream>
#include <memory>
#include <vector>
#include <cassert>
#include <unordered_map>
using namespace std;

typedef int Gene;
typedef pair<Gene,bool> Genea; // Gene with an indicator whether it is alpha or not (alpha genes must be deleted/inserted)
typedef int IR;

/* String representing a unichromossomal and linear genome (first gane has index 1) */
class Genome {
protected:
  unique_ptr<vector<Genea>> genes;
  unique_ptr<vector<IR>> intergenic_regions;
  unique_ptr<vector<vector<Gene>>> positions;
  vector<Gene> empty_vec;
  Gene op_max;
  Genome(){};
  void record_positions();

public:
  Genome(string str_g, bool extend);
  Genome(string str_g, string str_i, bool extend);
  Genome(vector<Genea> gs, vector<IR> irs);
  Genome(const Genome &g);
  size_t size() const { return genes->size(); }
  /* Only read access to the elements. */
  Gene operator[](int i) const { return (*genes)[i - 1].first; }
  /* Only read access to intergenic regions. */
  IR get_ir(int i) const { return (*intergenic_regions)[i - 1]; }
  /* Check if the gene in position i is alpha (must be deleted) */
  IR check_alpha(int i) const { return (*genes)[i - 1].second; }
  /* Get maximum occurrence of a label */
  int occ(Gene label) const;
  /* Get maximum occurrence */
  int occ_max() const;
  /* Get positions of a given label (read only). */
  const vector<int> &pos(Gene label) const;
  /* Insertion operation
  (insert each genes of the gene vector followed by each ir from the ir vector) */
  void insertion(int, vector<Gene>&);
  void insertion(int, vector<Gene>&, vector<IR>&);
  /* Deletion operation */
  void deletion(int, int, IR);
  /* Reversal operation */
  void reversal(int, int, IR, IR);
  /* Transposition operation */
  void transposition(int, int, int, IR, IR, IR);
  /* Change the label of a gene */
  void replace_label(int, Gene);
  /* Pretty print genome */
  virtual void serialize(ostream &) const;
  /* Get and set a value bigger then all the labels */
  Gene get_op_max() const { return op_max; }
  /* Get the genes alphabet */
  void alphabet(unordered_map<int,int> &alf, bool negative) const;
  /* Check if two genomes hava the same genes */
  bool balanced(Genome&) const;
  /* Get the permutation as a c array */
  int *get_perm() const;
};

ostream &operator<<(ostream &os, const Genome &g);
