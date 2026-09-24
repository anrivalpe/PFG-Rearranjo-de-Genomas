#pragma once

#include "../misc/genome.hpp"
#include <bitset>
#include <map>
#include <queue>
#include <set>

typedef int Vtx_id;
#define NO_EDGE -1

struct Vertex {
  Vtx_id black;
  int weigth;
  Vtx_id fix_gray;
  Vtx_id indel;
  vector<Vtx_id> grays;
  bool in_cycle;
  bool sign_positive;
  bool is_indel;
  int indel_update;
  Gene gene_val;
  Vertex() : grays() {
    gene_val = -1;
    indel_update = 0;
    is_indel = false;
    black = NO_EDGE;
    weigth = 0;
    fix_gray = NO_EDGE;
    indel = NO_EDGE;
    in_cycle = false;
  }
};

struct PermsIrs {
  size_t s_n, p_n;
  int *s;
  int *s_ir;
  int *p;
  int *p_ir;
};

struct QEntry {
  Vtx_id vtx;
  map<Vtx_id, Vtx_id> fixed;
  set<Vtx_id> vizited;
  map<Gene, int> indel_count;

  QEntry(Vtx_id start, map<Gene, int> indel_count)
      : fixed(), vizited(), indel_count(indel_count) {
    vtx = start;
  }

  QEntry(Vtx_id v, Vtx_id u, Vtx_id alter_v, Vtx_id alter_u,
         map<Vtx_id, Vtx_id> fixed_old, set<Vtx_id> vizited_old,
         map<Gene, int> indel_count_old)
      : fixed(fixed_old), vizited(vizited_old), indel_count(indel_count_old) {
    vtx = u;
    fixed[v] = u;
    fixed[u] = v;
    fixed[alter_v] = alter_u;
    fixed[alter_u] = alter_v;
    vizited.insert(u);
    vizited.insert(v);
  }

  QEntry(Vtx_id v, Vtx_id u, map<Vtx_id, Vtx_id> fixed_old,
         set<Vtx_id> vizited_old, map<Gene, int> indel_count_old)
      : fixed(fixed_old), vizited(vizited_old), indel_count(indel_count_old) {
    vtx = u;
    fixed[v] = u;
    fixed[u] = v;
    vizited.insert(u);
    vizited.insert(v);
  }
};

struct Run {
  Gene fst_gene; // first gene of run
  char genome; // G or H
  vector<Gene> genes_to_add;
};

class CycleGraph {

private:
  int balanced_cycles = 0;
  int indel_potation = 0;
  int fhs;
  /* Value bigger then all the labels */
  int op_max;
  vector<Vertex> vertices;
  vector<pair<size_t, Vtx_id>>
      cycles; // we indentify cycles by one of their vertices and their sizes
  map<Gene, int> indel_count;

  void bfs(Vtx_id start, unique_ptr<queue<QEntry>> &q,
           unique_ptr<set<pair<Vtx_id, int>>> &vizited_in_level,
           bool is_random);

public:
  /* Initial Constructor. */
  CycleGraph(const Genome &origin, const Genome &target);
  /* Copy Constructor. */
  CycleGraph(const CycleGraph &that)
      : vertices(that.vertices), cycles(that.cycles),
        indel_count(that.indel_count) {
    balanced_cycles = that.balanced_cycles;
    fhs = that.fhs;
    op_max = that.op_max;
  }
  size_t size() const { return vertices.size(); };
  void decompose_with_bfs(bool is_random);
  /* Select a cycle with a bfs
   * Arguments:
   *     start - initial vertex
   *     is_random - whether to use random approach
   */
  /* Get string with the cycles from the decomposition */
  void bfs(Vtx_id start, bool is_random);
  string show_cycles() const;
  /* Recover cycles from string */
  void read_cycles(string);
  /* Recover permutations from decomposition */
  PermsIrs get_perms();
  /* Get number of cycles from the decomposition */
  int dec_size() const { return cycles.size(); }
  int dec_balanced_cycles() const { return balanced_cycles; }
  void rem_cycle(pair<size_t, Vtx_id> c);
  /* Get indel potation */
  int potation() const { return indel_potation; }
  /* Verify if a cycle can be added */
  bool check_cycle(vector<Vtx_id> cycle);
  void add_cycle(vector<Vtx_id> cycle);
  void check_and_add_cycle(vector<Vtx_id> cycle);

  vector<pair<size_t, Vtx_id>> cycle_list() const { return cycles; }
  vector<Vtx_id> get_cycle(Vtx_id i) const;
  int cycle_weight(Vtx_id i) const;
  int cycle_potation(Vtx_id i) const;
  Run cycle_run(int i) const;
  void serialize(ostream &) const;
};

ostream &operator<<(std::ostream &os, const CycleGraph &cg);
ostream &operator<<(std::ostream &os, const Run &r);
