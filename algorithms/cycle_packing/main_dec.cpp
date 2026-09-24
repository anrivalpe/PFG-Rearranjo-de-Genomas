#include <getopt.h>

#include <experimental/filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

#include "cycle/cycles.hpp"
#include "heur/ga.hpp"
#include "misc/genome.hpp"
#include "misc/io.hpp"
#include "misc/reduction_rules.hpp"
#include "misc/timer.hpp"
namespace fs = experimental::filesystem;
using namespace std;

#define N_POS_ARGS 1

struct Args {
  string heuristic;
  string input_file;
  string output_folder;
  int iterations = 100;
  bool extend = false;
  int tournament_size = 2;
  int mutation_rate = 50;
  int crossover_rate = 50;
  bool fill_zero = false;
};

void help(char *name) {
  cout << "usage: Find a cycle decomposition for the breakpoint graph formed "
          "by the origin and target signed genomes."
       << endl
       << "\t" << name << " HEUR [OPTIONS]" << endl
       << endl
       << "positional arguments:" << endl
       << "\tHEUR                    the heuristic to use (ga|rand)" << endl
       << endl
       << "optional arguments:" << endl
       << "\t-h, --help              show this help message and exit" << endl
       << "\t-i, --input INPUT       input file (if not provided stdin is "
          "used). Each 2 lines of the input file correspond to a instance, "
          "each line has a list of space separated values, and represent in "
          "order the origin string and the target string."
       << endl
       << "\t-o, --output OUTPUT     output folder (if not provided stdout is "
          "used)"
       << endl
       << "\t-k, --iterations ITER   number of iterations (default 100)"
       << endl
       << "\t-m, --mutation MUT      mutation rate in percentage for GA (default 50%)"
       << endl
       << "\t-c, --crossover CROS    crossover rate in percentage for GA (default 50%)"
       << endl
       << "\t-t, --tournament TOUR   number of elements in each tournament for GA (default 2)"
       << endl
       << "\t-e, --extend            whether to extend the genomes before "
          "apply the algorithm"
       << endl;

  exit(EXIT_SUCCESS);
}

void get_args(Args &args, int argc, char *argv[]) {
  extern char *optarg;
  extern int optind;
  int n_pos_args = 0;

  struct option longopts[] = {{"input", 1, NULL, 'i'},
                              {"output", 1, NULL, 'o'},
                              {"iterations", 1, NULL, 'k'},
                              {"mutation", 1, NULL, 'm'},
                              {"crossover", 1, NULL, 'c'},
                              {"tournament", 1, NULL, 't'},
                              {"extend", 0, NULL, 'e'},
                              {"help", 0, NULL, 'h'},
  };

  char op;
  while ((op = getopt_long(argc, argv, "i:o:k:m:c:t:heaz", longopts, NULL)) != -1) {
    switch (op) {
      case 'i':
        args.input_file = optarg;
        break;
      case 'o':
        args.output_folder = optarg;
        break;
      case 'k':
        args.iterations = atoi(optarg);
        break;
      case 'm':
        args.mutation_rate = atoi(optarg);
        break;
      case 'c':
        args.crossover_rate = atoi(optarg);
        break;
      case 't':
        args.tournament_size = atoi(optarg);
        break;
      case 'e':
        args.extend = true;
        break;
      default:
        help(argv[0]);
    }
  }
  for (int i = optind; i < argc; i++) {
    args.heuristic = argv[i];
    n_pos_args++;
  }

  if (n_pos_args != N_POS_ARGS) {
    help(argv[0]);
  }
}

CycleGraph *get_best_cg(CycleGraph *cg1, CycleGraph *cg2) {
        if (cg1->dec_size() - cg1->potation() >
            cg2->dec_size() - cg2->potation()) {
            delete cg2;
            return cg1;
        } else {
            delete cg1;
            return cg2;
        }
}

int main(int argc, char *argv[]) {
  Args args;
  ifstream is;
  unique_ptr<vector<string>> input_lines;

  get_args(args, argc, argv);

  // set seed
  /* srand(1); */
  srand(time(0));

  if (args.input_file != "") {
    is.open(args.input_file);
    input_lines.reset(read_lines(is));
    is.close();
  } else {
    input_lines.reset(read_lines(cin));
  }

  int div = 2;
  try {
    if (input_lines->size() % div == 1) {
      throw invalid_argument("Number of lines is not multiple of 2.");
    }

    for (size_t i = 0; i < input_lines->size(); i += div) {
      Timer timer;
      ofstream os;
      unique_ptr<CycleGraph> cg, cg_aux, cg_best;
      int name_idx = i / div;

      InputData data;
      data = input((*input_lines)[i], (*input_lines)[i + 1], args.extend);
      suboptimal_rule_interval(*data.g, *data.h);
      suboptimal_rule_pairs(*data.g, *data.h);
      cg = unique_ptr<CycleGraph>(new CycleGraph(*data.g, *data.h));

      if (args.heuristic == "rand") {
        CycleGraph *cg_rand = new CycleGraph(*cg);
        cg_rand->decompose_with_bfs(false);

#pragma omp declare reduction(select_cg : CycleGraph* : omp_out = get_best_cg(omp_in, omp_out))
#pragma omp parallel for reduction(                                            \
        select_cg                                                                  \
        : cg_rand) // Process each decomposition in parallel
        for (int i = 1; i < args.iterations; ++i) {
          cg_rand = new CycleGraph(*cg);
          cg_rand->decompose_with_bfs(true);
        }
	cg_best.reset(cg_rand);
      } else if (args.heuristic == "ga") {
        ostream *ga_os = nullptr;
        int start = args.iterations / 10;
        if (start % 2 == 1) {
          start += 1;
        }
        GA ga = GA(new Chromossome(*cg), args.mutation_rate / 100.0,
                args.crossover_rate / 100.0, args.tournament_size, start, start,
                (args.iterations - start) / start, ga_os, timer);
        ga.solve(timer);
        cg_best = ga.get_best_chr();
      } else {
        help(argv[0]);
      }

      if (args.output_folder != "") {
        os.close();
        os.open((args.output_folder / fs::path(args.input_file).filename())
                    .string() +
                string(5 - to_string(name_idx).size(), '0') +
                to_string(name_idx) + "-best");
      }

      output((args.output_folder != "") ? os : cout, *cg_best,
             timer.elapsed_time());

      if (args.output_folder != "") {
        os.close();
        os.open((args.output_folder / fs::path(args.input_file).filename())
                    .string() +
                string(5 - to_string(name_idx).size(), '0') +
                to_string(name_idx) + "-perm");
      }

      output((args.output_folder != "") ? os : cout, cg_best->get_perms());
      /* cout << - cg_best->dec_size() + cg_best->potation() << endl; */
    }

  } catch (const invalid_argument &e) {
    cerr << "Something went wrong!!!" << endl;
    cerr << e.what() << endl;
  }

  return 0;
}
