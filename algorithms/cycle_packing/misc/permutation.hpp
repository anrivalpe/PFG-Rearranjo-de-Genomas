#pragma once
#include "../misc/io.hpp"
#include "genome.hpp"
using namespace std;

class Permutation : public Genome {
  unique_ptr<vector<IR>> intergenic_regions_target;

 private:
  int target_ir(int i, bool use_abs) const;

 public:
  Permutation(){};
  /* Copy constructor */
  Permutation(const Permutation &pi);
  /* Construct a permutation replicas are mapped randomly */
  Permutation(const Genome &s, const Genome &h, bool duplicate);
  /* Separate iota from the permutation */
  InputData split_iota() const;
  /* Verify if the permutation is sorted and with the correct intergenic region
   * sizes */
  bool is_iota() const;
  /* Only read access to intergenic regions of target. */
  IR get_ir_target(int i) const { return (*intergenic_regions_target)[i - 1]; }
  bool breakpoint(Gene, bool use_abs) const;
  bool hard_breakpoint(Gene, bool use_abs) const;
  bool soft_breakpoint(Gene, bool use_abs) const;
  bool overcharged_breakpoint(Gene, bool use_abs) const;
  bool undercharged_breakpoint(Gene, bool use_abs) const;
  bool is_block(Gene, Gene, bool use_abs) const;
  virtual void serialize(ostream &) const override;
};
