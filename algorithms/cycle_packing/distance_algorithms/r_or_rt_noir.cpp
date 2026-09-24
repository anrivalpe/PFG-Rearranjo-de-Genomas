#include "r_or_rt_noir.hpp"

#include <cassert>
#include <cmath>

#include "../cycle/cycles.hpp"
#include "../misc/io.hpp"
#include "../misc/permutation.hpp"
#include "aux.hpp"

extern "C" {
#include "perm/perm_rearrange.h"
}

int lower_bound_gen(InputData &data, unique_ptr<CycleGraph> &cg, int div) {
  unordered_map<int,int> alp;
  data.g->alphabet(alp,false);
  data.h->alphabet(alp,true);
  int inter = 0;
  for (auto el : alp) {
    if (el.second == 0) inter++;
  }
  return ceil((inter - cg->dec_size() + cg->potation() - 1) / double(div));
}

int ReversalNOIR::lower_bound(InputData &data, unique_ptr<CycleGraph> &cg) {
    return lower_bound_gen(data, cg, 1);
}

int ReversalTranspositionNOIR::lower_bound(InputData &data, unique_ptr<CycleGraph> &cg) {
    return lower_bound_gen(data, cg, 2);
}

int R_OR_RT_NOIR::estimate_distance(Permutation pi) {
  unique_ptr<CycleGraph> cg;
  InputData data;
  int dist = 0;

  assert(pi.occ_max() == 1);
  data = pi.split_iota(); // create a permutation iota (except the last gene is bigger than any gene in pi)
  cg = unique_ptr<CycleGraph>(new CycleGraph(*data.g, *data.h));
  cg->decompose_with_bfs(true);
  int last_lb = lower_bound(data, cg);

  bool found_run;
  do {
    found_run = false;
    for (auto c : cg->cycle_list()) {
      Run run = cg->cycle_run(c.second);
      if (!run.genes_to_add.empty()) {
        if (run.genome == 'G') {
          int i = data.g->pos(run.fst_gene)[0];
          data.g->insertion(i, run.genes_to_add);
        } else {
          int i = data.h->pos(run.fst_gene)[0];
          data.h->insertion(i, run.genes_to_add);
        }
        dist++;
        found_run = true;
        break;
      }
    }
    if (found_run) {
      cg.reset(new CycleGraph(*data.g, *data.h));
      cg->decompose_with_bfs(true);
      int lb = lower_bound(data, cg);
      assert(lb < last_lb);
      last_lb = lb;
    }
  } while (found_run);

  assert(data.g->size() == data.h->size());
  assert(data.g->balanced(*data.h));
  assert(data.g->occ_max() == 1);


  pair<int *, int *> perms;
  perms.first = data.g->get_perm();
  perms.second = data.h->get_perm();
  dist += dist_aux(perms.first, perms.second, data.g->size());
  delete[] perms.first;
  delete[] perms.second;

  return dist;
}

InputData R_OR_RT_NOIR::make_genomes(Permutation pi) {
  unique_ptr<CycleGraph> cg;
  InputData data;
  int dist = 0;

  assert(pi.occ_max() == 1);
  data = pi.split_iota(); // create a permutation iota (except the last gene is bigger than any gene in pi)
  cg = unique_ptr<CycleGraph>(new CycleGraph(*data.g, *data.h));
  cg->decompose_with_bfs(true);
  int last_lb = lower_bound(data, cg);

  bool found_run;
  do {
    found_run = false;
    for (auto c : cg->cycle_list()) {
      Run run = cg->cycle_run(c.second);
      if (!run.genes_to_add.empty()) {
        if (run.genome == 'G') {
          int i = data.g->pos(run.fst_gene)[0];
          data.g->insertion(i, run.genes_to_add);
        } else {
          int i = data.h->pos(run.fst_gene)[0];
          data.h->insertion(i, run.genes_to_add);
        }
        dist++;
        found_run = true;
        break;
      }
    }
    if (found_run) {
      cg.reset(new CycleGraph(*data.g, *data.h));
      cg->decompose_with_bfs(true);
      int lb = lower_bound(data, cg);
      assert(lb < last_lb);
      last_lb = lb;
    }
  } while (found_run);

  /* assert(data.g->size() == data.h->size()); */
  /* assert(data.g->balanced(*data.h)); */
  assert(data.g->occ_max() == 1);

  return data;
}


int ReversalNOIR::dist_aux(int *g1, int *g2, int size) {
  return dist(g1, g2, size, 0);
}

int ReversalTranspositionNOIR::dist_aux(int *g1, int *g2, int size) {
  return dist(g1, g2, size, 2);
}
