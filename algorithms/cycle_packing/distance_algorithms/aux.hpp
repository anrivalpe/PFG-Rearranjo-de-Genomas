#pragma once

#include "../misc/permutation.hpp"

int is_connected(const Permutation &pi, int i, int j);

void transposition_move(Permutation &pi, Gene, Gene, Gene, IR, IR, IR);

// (i, j) / (i + 1, j + 1)
int case_i(Permutation &pi, int i, int j);
