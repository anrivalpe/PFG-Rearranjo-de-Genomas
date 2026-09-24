#pragma once
#include "../misc/dist.hpp"

/* Call to an external program to calculate the distances. The program must
 * receive three comma separated list as command line arguments (the
 * permutation, the target intergenic regions, and the origin intergenic
 * regions). The program must print the distance to its stdout.*/
class ExternalDistAlg : public DistAlg {
  string prog;

public:
  ExternalDistAlg(string prog) : prog(prog){};
  int estimate_distance(Permutation pi) override;
};
