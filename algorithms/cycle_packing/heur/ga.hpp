#pragma once

#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <vector>

#include "../cycle/cycles.hpp"
#include "../misc/timer.hpp"
#include "solution.hpp"
using namespace std;

class Chromossome : public CycleGraph {
 public:
  Chromossome(const Chromossome &chr) : CycleGraph(chr) {}
  Chromossome(const CycleGraph &cg) : CycleGraph(cg) {}
  vector<int> fitness() {
    vector<int> fit(1);
    /* fit[0] = this->dec_balanced_cycles(); */
    /* fit[1] = this->dec_size() - this->potation(); */
    fit[0] = this->dec_size() - this->potation();
    return fit;
  };
};
typedef vector<unique_ptr<Chromossome>> Population;

bool cmp_chr(const unique_ptr<Chromossome> &c1,
             const unique_ptr<Chromossome> &c2);

class GA {
 protected:
  unique_ptr<Chromossome> original;  // to create new cycle decompositions
  unique_ptr<Population> population;
  unique_ptr<Chromossome> best_chr = nullptr;
  double mutation_rate;
  double crossover_rate;
  int tournament_size;
  int population_size;
  vector<int> best_obj;
  int generations;
  ostream *os;

  int select_parent();  // Returns index of next selected parent
  void select_population(unique_ptr<Population> &mutants);
  bool eval_population(unique_ptr<Population> &, Timer);
  Chromossome *crossover(const Chromossome &, const Chromossome &) const;
  void mutation(unique_ptr<Chromossome> &) const;

 public:
  GA(Chromossome *chr, double mutation_rate, double crossover_rate, int tournament_size,
     int initial_size, int population_size, int generations, ostream *os,
     Timer timer) {
    this->original = unique_ptr<Chromossome>(chr);
    this->population_size = population_size;
    this->population = unique_ptr<Population>(new Population(initial_size));
    this->mutation_rate = mutation_rate;
    this->crossover_rate = crossover_rate;
    this->tournament_size = tournament_size;
    this->generations = generations;
    this->os = os;

#pragma omp parallel for // Create each chromosomes in parallel
    for (int i = 0; i < initial_size; ++i) {
      (*population)[i] = unique_ptr<Chromossome>(new Chromossome(*original));
      (*population)[i]->decompose_with_bfs(true);
    }

    best_obj = vector<int>(2, numeric_limits<int>::min());
    eval_population(population, timer);
    /* sort(population->begin(), population->end(), cmp_chr); */
    /* random_shuffle(population->begin() + population_size / 2,
     * population->end()); */
    /* population->resize(population_size); */
  }

  vector<int> get_best_obj() { return best_obj; }
  unique_ptr<Chromossome> get_best_chr() { return move(best_chr); }

  void solve(Timer timer) {
    unique_ptr<Population> offsprings = nullptr;
    /* int last_impr_gen = 0; */

    for (int g = 1; g <= generations; g++) {
      bool improved = false;

      offsprings.reset(new Population(population_size));

#pragma omp parallel for // Create each chromosomes in parallel
      for (int i = 0; i < population_size; i++) {
        unique_ptr<Chromossome> &chr1 = (*population)[select_parent()];
        unique_ptr<Chromossome> &chr2 = (*population)[select_parent()];
        (*offsprings)[i] = unique_ptr<Chromossome>(crossover(*chr1, *chr2));
        mutation((*offsprings)[i]);
      }
      improved = improved || eval_population(offsprings, timer);

      // Save new selected chromosomes in population and delete the old ones
      select_population(offsprings);

      // Stop after given number of generations without improvement
      /* if (improved) last_impr_gen = g; */
      /* if (g - last_impr_gen == 100) { */
      /* 	break; */
      /* } */

      // Stop after runing out of time
      /* if (timer.done()) { */
      /* 	break; */
      /* } */
    }
  }
};
