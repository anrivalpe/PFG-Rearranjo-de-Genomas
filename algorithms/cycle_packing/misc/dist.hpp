#pragma once

#include "permutation.hpp"

class DistAlg {
public:
    virtual int estimate_distance(Permutation pi) = 0;
};
