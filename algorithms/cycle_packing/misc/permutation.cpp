#include "permutation.hpp"
#include "genome.hpp"

#include <algorithm>
#include <cassert>
#include <unordered_map>
#include <numeric>

#include "../misc/io.hpp"

Permutation::Permutation(const Permutation &pi) {
  genes = unique_ptr<vector<Genea>>(new vector<Genea>(*pi.genes));
  intergenic_regions =
      unique_ptr<vector<IR>>(new vector<IR>(*pi.intergenic_regions));
  positions =
      unique_ptr<vector<vector<int>>>(new vector<vector<int>>(*pi.positions));
  op_max = pi.op_max;
  intergenic_regions_target =
      unique_ptr<vector<IR>>(new vector<IR>(*pi.intergenic_regions_target));
}

Permutation::Permutation(const Genome &g, const Genome &h, bool duplicate) {

  if (duplicate) {
    genes.reset(new vector<Genea>(2 * g.size() - 2));
    intergenic_regions.reset(new vector<IR>(2 * g.size() - 3));
    intergenic_regions_target.reset(new vector<IR>(2 * g.size() - 3));
  } else {
    genes.reset(new vector<Genea>(g.size()));
    intergenic_regions.reset(new vector<IR>(g.size() - 1));
    intergenic_regions_target.reset(new vector<IR>(h.size() - 1));
  }

  /* Add intergenic regions */
  (*intergenic_regions)[0] = g.get_ir(1);
  for (size_t i = 2; i <= g.size() - 1; i++) {
    if (duplicate) {
      (*intergenic_regions)[2 * i - 3] = 0;
      (*intergenic_regions)[2 * i - 2] = g.get_ir(i);
    } else {
      (*intergenic_regions)[i - 1] = g.get_ir(i);
    }
  }

  /* Add intergenic regions of target genome */
  (*intergenic_regions_target)[0] = h.get_ir(1);
  for (size_t i = 1; i <= h.size() - 1; i++) {
    if (duplicate) {
      (*intergenic_regions_target)[2 * i - 3] = 0;
      (*intergenic_regions_target)[2 * i - 2] = h.get_ir(i);
    } else {
      (*intergenic_regions_target)[i - 1] = h.get_ir(i);
    }
  }

  /* Build vector mapping old to new labels */
  unordered_map<Gene, vector<Gene>> labels(max(g.get_op_max(), h.get_op_max()));
  for (size_t i = 0; i < h.size(); i++) {
    labels[abs(h[i + 1])].push_back((h[i + 1] >= 0) ? i : -i);
  }
  
  int val = int(h.size());
  /* Add genes that need to be deleted and the label for the last gene in g */
  // for (size_t i = 0; i < g.size(); i++) {
  //   int a = abs(g[i + 1]);
  //   int k;
  //   if (labels.find(a) == labels.end()) {
  //     k = g.occ(a);
  //   } else {
  //     k = g.occ(a) - labels[a].size();
  //   }
  //   for (int j = 0; j < k; j++) {
  //     labels[a].push_back(val);
  //     val++;
  //   }
  // }

  for (auto &l : labels) {
    random_shuffle(l.second.begin(), l.second.end());
  }

  /* Add genes mapping replicas */
  assert(g[1] >= 0 && g[g.size()] >= 0 && labels[g[1]].size() == 1 &&
         labels[g[g.size()]].size() == 1);
  (*genes)[0] = Genea(labels[abs(g[1])].back(), false);
  for (size_t i = 2; i <= g.size() - 1; i++) {
    int a;
    bool is_alpha = g.check_alpha(i);
    if (is_alpha) {
      a = val;
      val++;
    } else {
      a = labels[abs(g[i])].back();
      labels[abs(g[i])].pop_back();
    }
    if (duplicate) {
      if ((g[i] >= 0) == (a >= 0)) {
        (*genes)[2 * i - 3] = Genea(2 * abs(a) - 1, is_alpha);
        (*genes)[2 * i - 2] = Genea(2 * abs(a), is_alpha);
      } else {
        (*genes)[2 * i - 3] = Genea(2 * abs(a), is_alpha);
        (*genes)[2 * i - 2] = Genea(2 * abs(a) - 1, is_alpha);
      }
    } else {
      if (g[i] >= 0) {
        (*genes)[i - 1] = Genea(a, is_alpha);
      } else {
        (*genes)[i - 1] = Genea(-a, is_alpha);
      }
    }
  }

  op_max = max(int(g.size() + h.size()), val);

  if (duplicate) {
    (*genes)[2 * g.size() - 3] = Genea(2 * labels[g[g.size()]].back() - 1, g.check_alpha(g.size()));
  } else {
    (*genes)[g.size() - 1] = Genea(labels[g[g.size()]].back(), g.check_alpha(g.size()));
  }

  record_positions();
  assert(intergenic_regions->size() == genes->size() - 1);
}

InputData Permutation::split_iota() const {
  InputData data;
  
  data.g = unique_ptr<Genome>(new Genome(*this));
  vector<Genea> h_genes(this->intergenic_regions_target->size() + 1);
  for (int i = 0; i < int(h_genes.size()); i++) {
    h_genes[i] = Genea(i, false);
  }
  h_genes.back() = genes->back();
  data.h = unique_ptr<Genome>(new Genome(h_genes,*this->intergenic_regions_target));
  return data;
}

bool Permutation::is_iota() const {
  bool ok = true;

  for (size_t i = 0; i < size() - 1 && ok; i++) {
    if ((*genes)[i + 1].first - (*genes)[i].first != 1) ok = false;
    if ((*intergenic_regions)[i] != (*intergenic_regions_target)[i]) ok = false;
  }

  return ok;
}

int Permutation::target_ir(int i, bool use_abs) const {
  if (use_abs) {
    return (*intergenic_regions_target)[min((*genes)[i - 1].first, (*genes)[i].first)];
  } else {
    return (*intergenic_regions_target)[(*genes)[i - 1].first];
  }
}

bool Permutation::breakpoint(Gene ai, bool use_abs) const {
  int i = pos(ai)[0];
  return soft_breakpoint(ai, use_abs) ||
         (*intergenic_regions)[i - 1] != target_ir(i, use_abs);
}
bool Permutation::hard_breakpoint(Gene ai, bool use_abs) const {
  int i = pos(ai)[0];
  return !soft_breakpoint(ai, use_abs) &&
         (*intergenic_regions)[i - 1] != target_ir(i, use_abs);
}
bool Permutation::soft_breakpoint(Gene ai, bool use_abs) const {
  int i = pos(ai)[0];
  if (use_abs) {
    return abs((*genes)[i].first - (*genes)[i - 1].first) != 1;
  } else {
    return (*genes)[i].first - (*genes)[i - 1].first != 1;
  }
}
bool Permutation::overcharged_breakpoint(Gene ai, bool use_abs) const {
  int i = pos(ai)[0];
  return !soft_breakpoint(ai, use_abs) &&
         (*intergenic_regions)[i - 1] > target_ir(i, use_abs);
}

bool Permutation::undercharged_breakpoint(Gene ai, bool use_abs) const {
  int i = pos(ai)[0];
  return !soft_breakpoint(ai, use_abs) &&
         (*intergenic_regions)[i - 1] < target_ir(i, use_abs);
}

bool Permutation::is_block(Gene ai, Gene bi, bool use_abs) const {
  int i = pos(ai)[0];
  int j = pos(bi)[0];
  if (use_abs) {
    if (abs(ai - bi) != 1) {
      return false;
    }
    if (abs(i - j) != 1) {
      return false;
    }
    return (*intergenic_regions)[min(i, j) - 1] ==
           (*intergenic_regions_target)[min(ai, bi)];
  } else {
    if (bi - ai != 1) {
      return false;
    }
    if (j - i != 1) {
      return false;
    }
    return (*intergenic_regions)[i - 1] == (*intergenic_regions_target)[ai];
  }
}

void Permutation::serialize(ostream &os) const {
  Genome::serialize(os);
  os << " : [";
  for (size_t i = 1; i < size() - 1; ++i) {
    os << get_ir_target(i) << ", ";
  }
  os << get_ir_target(size() - 1) << "]";
  /* for (size_t i = 1; i <= size(); ++i) { */
  /*   os << (*this)[i] << " "; */
  /* } */
  /* os << endl; */
  /* for (size_t i = 1; i < size(); ++i) { */
  /*   os << get_ir(i) << " "; */
  /* } */
  /* os << endl; */
  /* for (size_t i = 1; i <= size(); ++i) { */
  /*   os << i-1 << " "; */
  /* } */
  /* os << endl; */
  /* for (size_t i = 1; i < size(); ++i) { */
  /*   os << get_ir_target(i) << " "; */
  /* } */
}
