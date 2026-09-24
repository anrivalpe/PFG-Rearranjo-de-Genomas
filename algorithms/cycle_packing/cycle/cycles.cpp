#include "cycles.hpp"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <numeric>
#include <queue>
#include <sstream>

#include "../misc/genome.hpp"
#include "../misc/io.hpp"

CycleGraph::CycleGraph(const Genome &origin, const Genome &target)
    : vertices(2 * (origin.size() + target.size()) - 4),
      cycles(),
      indel_count() {
  int a = max(origin.size(), target.size());
  int b = max(origin.get_op_max(), target.get_op_max());
  op_max = max(a, b) + 1;
  fhs = 2 * origin.size() -
        2;  // number of vertices correspondent to genes of the origin genome

  /* Using gene values to label the vertices, also indicate how to update indels
   * of each vertice */
  vertices[0].gene_val = abs(origin[1]);
  vertices[0].indel_update = -1;
  for (size_t i = 2; i < origin.size(); ++i) {
    vertices[2 * i - 3].gene_val = abs(origin[i]);
    vertices[2 * i - 3].indel_update = -1;
    vertices[2 * i - 2].gene_val = abs(origin[i]);
    vertices[2 * i - 2].indel_update = -1;
  }
  vertices[fhs - 1].gene_val = abs(origin[origin.size()]);
  vertices[fhs - 1].indel_update = -1;
  vertices[fhs].gene_val = abs(target[1]);
  vertices[fhs].indel_update = 1;
  for (size_t i = 2; i < target.size(); ++i) {
    vertices[fhs + 2 * i - 3].gene_val = abs(target[i]);
    vertices[fhs + 2 * i - 3].indel_update = 1;
    vertices[fhs + 2 * i - 2].gene_val = abs(target[i]);
    vertices[fhs + 2 * i - 2].indel_update = 1;
  }
  vertices[vertices.size() - 1].gene_val = abs(target[target.size()]);
  vertices[vertices.size() - 1].indel_update = 1;

  /* Black Edges */
  for (size_t i = 0; i < vertices.size(); ++i) {
    if (i % 2 == 0) {
      vertices[i].black = i + 1;
    } else {
      vertices[i].black = i - 1;
    }
  }

  /* Indel Edges */
  for (size_t i = 2; i < origin.size(); ++i) {
    indel_count[abs(origin[i])]++;
    vertices[2 * i - 3].indel = 2 * i - 2;
    vertices[2 * i - 2].indel = 2 * i - 3;
  }

  for (size_t i = 2; i < target.size(); ++i) {
    indel_count[abs(target[i])]--;
    vertices[fhs + 2 * i - 3].indel = fhs + 2 * i - 2;
    vertices[fhs + 2 * i - 2].indel = fhs + 2 * i - 3;
  }

  /* Weigths */
  vertices[0].weigth = origin.get_ir(1);
  for (size_t i = 1; i < origin.size() - 1; ++i) {
    vertices[2 * i - 1].weigth = origin.get_ir(i);
    vertices[2 * i].weigth = origin.get_ir(i + 1);
  }
  vertices[fhs - 1].weigth = origin.get_ir(origin.size() - 1);
  vertices[fhs].weigth = target.get_ir(1);
  for (size_t i = 1; i < target.size() - 1; ++i) {
    vertices[fhs + 2 * i - 1].weigth = target.get_ir(i);
    vertices[fhs + 2 * i].weigth = target.get_ir(i + 1);
  }
  vertices[vertices.size() - 1].weigth = target.get_ir(target.size() - 1);

  /* Signed */
  vertices[0].sign_positive = true;
  for (size_t i = 1; i < origin.size() - 1; ++i) {
    vertices[2 * i - 1].sign_positive = origin[i + 1] >= 0;
    vertices[2 * i].sign_positive = origin[i + 1] >= 0;
  }
  vertices[fhs - 1].sign_positive = true;
  vertices[fhs].sign_positive = true;
  for (size_t i = 1; i < target.size() - 1; ++i) {
    vertices[fhs + 2 * i - 1].sign_positive = target[i + 1] >= 0;
    vertices[fhs + 2 * i].sign_positive = target[i + 1] >= 0;
  }
  vertices[vertices.size() - 1].sign_positive = true;

  /* Gray Edges */
  assert(origin[1] == target[1] &&
         origin[origin.size()] == target[target.size()] &&
         origin.pos(origin[1]).size() == 1 &&
         origin.pos(origin[origin.size()]).size() == 1 &&
         target.pos(target[1]).size() == 1 &&
         target.pos(target[target.size()]).size() == 1);

  vertices[0].grays.push_back(fhs);
  vertices[fhs].grays.push_back(0);
  vertices[fhs - 1].grays.push_back(vertices.size() - 1);
  vertices[vertices.size() - 1].grays.push_back(fhs - 1);
  for (size_t i = 2; i < origin.size(); ++i) {
    for (auto j : target.pos(abs(origin[i]))) {
      if (origin[i] == target[j]) {
        vertices[2 * i - 3].grays.push_back(fhs + 2 * j - 3);
        vertices[2 * i - 2].grays.push_back(fhs + 2 * j - 2);
        vertices[fhs + 2 * j - 3].grays.push_back(2 * i - 3);
        vertices[fhs + 2 * j - 2].grays.push_back(2 * i - 2);
      } else {
        vertices[2 * i - 2].grays.push_back(fhs + 2 * j - 3);
        vertices[2 * i - 3].grays.push_back(fhs + 2 * j - 2);
        vertices[fhs + 2 * j - 2].grays.push_back(2 * i - 3);
        vertices[fhs + 2 * j - 3].grays.push_back(2 * i - 2);
      }
    }
  }

  vertices[0].fix_gray = vertices[0].grays[0];
  vertices[fhs - 1].fix_gray = vertices[fhs - 1].grays[0];
  vertices[fhs].fix_gray = vertices[fhs].grays[0];
  vertices[vertices.size() - 1].fix_gray =
      vertices[vertices.size() - 1].grays[0];
}

/* Decompose the remaning graph using bfs */
void CycleGraph::decompose_with_bfs(bool is_random) {
  vector<Vtx_id> idxs(size());
  iota(idxs.begin(), idxs.end(), 0);
  if (is_random) {
    random_shuffle(idxs.begin(), idxs.end());
  }
  for (Vtx_id i : idxs) {
    bfs(i, is_random);
  }
}

void CycleGraph::bfs(Vtx_id start, bool is_random) {
  unique_ptr<QEntry> entry;
  vector<unique_ptr<QEntry>> q1;
  vector<unique_ptr<QEntry>> q2;
  set<Vtx_id> vizited_in_level;
  Vtx_id headtailcorresp_v, headtailcorresp_u;
  size_t pos = 0;

  if (vertices[start].in_cycle) return;

  q1.push_back(unique_ptr<QEntry>(new QEntry(start, this->indel_count)));
  vizited_in_level.insert(start);

  do {
    entry = move(q1[pos]);
    pos++;

    Vtx_id v = vertices[entry->vtx].black;

    /* Follow indel edge */
    if (vertices[v].fix_gray == NO_EDGE &&
        !vertices[vertices[v].indel].in_cycle) {
      Vtx_id u = vertices[v].indel;
      if (entry->indel_count[vertices[u].gene_val] * vertices[u].indel_update <
              0 &&
          vizited_in_level.find(u) == vizited_in_level.end() &&
          entry->vizited.find(u) == entry->vizited.end()) {
        q2.push_back(unique_ptr<QEntry>(new QEntry(
            v, u, entry->fixed, entry->vizited, entry->indel_count)));
        q2.back()->indel_count[vertices[u].gene_val] +=
            vertices[u].indel_update;
        vizited_in_level.insert(u);
      }
    }

    /* Follow each gray edge */
    vector<Vtx_id> neigs;
    if (vertices[v].fix_gray != NO_EDGE) {
      neigs.push_back(vertices[v].fix_gray);
    } else if (!vertices[v].is_indel) {
      auto vu = entry->fixed.find(v);
      if (vu != entry->fixed.end()) {
        neigs.push_back(vu->second);
      } else {
        for (auto u : vertices[v].grays) {
          if (entry->fixed.find(u) == entry->fixed.end() &&
              vertices[u].fix_gray == NO_EDGE) {
            neigs.push_back(u);
          }
        }
      }
    }

    vector<Vtx_id> notDone;
    for (Vtx_id u : neigs) {
      if (!vertices[u].in_cycle &&
          vizited_in_level.find(u) == vizited_in_level.end() &&
          entry->vizited.find(u) == entry->vizited.end()) {
        notDone.push_back(u);
      }
    }

    if (vertices[v].fix_gray == NO_EDGE) {
      for (Vtx_id u : notDone) {
        if ((v % fhs) % 2 == 1) {
          headtailcorresp_v = v + 1;
        } else {
          headtailcorresp_v = v - 1;
        }
        if ((u % fhs) % 2 == 1) {
          headtailcorresp_u = u + 1;
        } else {
          headtailcorresp_u = u - 1;
        }
        q2.push_back(unique_ptr<QEntry>(
            new QEntry(v, u, headtailcorresp_v, headtailcorresp_u, entry->fixed,
                       entry->vizited, entry->indel_count)));
        vizited_in_level.insert(u);
      }
    } else {
      for (Vtx_id u : notDone) {
        q2.push_back(unique_ptr<QEntry>(new QEntry(
            v, u, entry->fixed, entry->vizited, entry->indel_count)));
        vizited_in_level.insert(u);
      }
    }

    if (pos == q1.size()) {
      q1.clear();
      for (unique_ptr<QEntry> &e : q2) {
        q1.push_back(move(e));
      }
      if (is_random) {
        random_shuffle(q1.begin(), q1.end());
      }
      q2.clear();
      vizited_in_level.clear();
      pos = 0;
    }
  } while (q1[pos]->vtx != start);

  /* Find balanced cycle in level if it exists */
  vector<Vtx_id> cycle;
  bool ok = false;
  for (; pos < q1.size(); pos++) {
    if (q1[pos]->vtx == start) {
      entry = move(q1[pos]);
      cycle.clear();
      Vtx_id u, v;
      int weigth = 0;
      v = start;
      do {
        u = vertices[v].black;
        cycle.push_back(v);
        cycle.push_back(u);
        v = entry->fixed[u];
        if (v != vertices[u].indel) {
          weigth += vertices[v].weigth;
        }
      } while (v != start);
      if (weigth == 0) {
        ok = true;
        add_cycle(cycle);
        break;
      }
    }
  }
  if (!ok) {
    add_cycle(cycle);
  }
}

string CycleGraph::show_cycles() const {
  ostringstream ss;

  ss << '[';
  for (size_t i = 0; i < cycles.size(); ++i) {
    Vtx_id v = cycles[i].second;
    ss << '[' << v;
    v = vertices[v].black;
    ss << "," << v;
    v = vertices[v].is_indel ? vertices[v].indel : vertices[v].fix_gray;
    while (v != cycles[i].second) {
      ss << "," << v;
      v = vertices[v].black;
      ss << "," << v;
      v = vertices[v].is_indel ? vertices[v].indel : vertices[v].fix_gray;
    }
    ss << ']';
    if (i != cycles.size() - 1) ss << ',';
  }
  ss << ']';

  return ss.str();
}

void CycleGraph::read_cycles(string str) {
  string str_cycle, str_el;
  vector<Vtx_id> cycle;

  /* Remove '[[' and ']]'. */
  str = str.substr(2, str.size() - 4);

  /* Read each cycle. */
  string delimiter = "],[";
  while (not str.empty()) {
    size_t idx = str.find(delimiter);
    if (idx != str.npos) {
      str_cycle = str.substr(0, idx);
      str.erase(0, idx + delimiter.length());
    } else {
      str_cycle = str;
      str = "";
    }

    /* Read each element. */
    cycle.clear();
    stringstream ss(str_cycle);
    while (getline(ss, str_el, ',')) {
      cycle.push_back(stoi(str_el));
    }

    add_cycle(cycle);
  }
}

PermsIrs CycleGraph::get_perms() {
  PermsIrs perms_irs;
  perms_irs.s_n = (fhs + 2) / 2;
  perms_irs.p_n = (vertices.size() - fhs + 2) / 2;
  perms_irs.s = (int *)malloc(perms_irs.s_n * sizeof(int));
  perms_irs.s_ir = (int *)malloc((perms_irs.s_n - 1) * sizeof(int));
  perms_irs.p = (int *)malloc(perms_irs.p_n * sizeof(int));
  perms_irs.p_ir = (int *)malloc((perms_irs.p_n - 1) * sizeof(int));

  /* The second permutation is the identity. */
  for (size_t i = 0; i < perms_irs.p_n; ++i) {
    perms_irs.p[i] = i + 1;
  }

  for (size_t i = 0; i < cycles.size(); ++i) {
    Vtx_id v = cycles[i].second;
    Vtx_id u = vertices[v].black;

    while (vertices[u].is_indel) {
      if (u < fhs) {
        perms_irs.s[(u + 1) / 2] = 0;
      } else {
        perms_irs.p[(u - fhs + 1) / 2] = 0;
      }
      u = vertices[vertices[u].indel].black;
    }
    v = vertices[u].fix_gray;

    Vtx_id a = min(v, u);
    Vtx_id b = max(v, u);
    if (vertices[a].sign_positive == vertices[b].sign_positive) {
      perms_irs.s[(a + 1) / 2] = perms_irs.p[(b - fhs + 1) / 2];
    } else {
      perms_irs.s[(a + 1) / 2] = -perms_irs.p[(b - fhs + 1) / 2];
    }
    while (v != cycles[i].second) {
      u = vertices[v].black;
      if (vertices[u].is_indel) {
        if (u < fhs) {
          perms_irs.s[(u + 1) / 2] = 0;
        } else {
          perms_irs.p[(u - fhs + 1) / 2] = 0;
        }
        v = vertices[u].indel;
      } else {
        v = vertices[u].fix_gray;
        Vtx_id a = min(v, u);
        Vtx_id b = max(v, u);
        if (vertices[a].sign_positive == vertices[b].sign_positive) {
          perms_irs.s[(a + 1) / 2] = perms_irs.p[(b - fhs + 1) / 2];
        } else {
          perms_irs.s[(a + 1) / 2] = -perms_irs.p[(b - fhs + 1) / 2];
        }
      }
    }
  }

  /* Remove contiguous sequence of indels and mark weights */
  size_t i = 1, j = 1, k = 1, l = 1;
  for (; i < perms_irs.s_n || j < perms_irs.p_n;) {
    if (i < perms_irs.s_n) {
      perms_irs.s_ir[k - 1] = vertices[2 * i - 1].weigth;
      perms_irs.s[k - 1] = perms_irs.s[i - 1];
      k++;
      i++;
    }
    if (j < perms_irs.p_n) {
      perms_irs.p_ir[l - 1] = vertices[fhs + 2 * j - 1].weigth;
      perms_irs.p[l - 1] = perms_irs.p[j - 1];
      l++;
      j++;
    }
    if (perms_irs.s[i - 2] == 0) {
      while (i < perms_irs.s_n && perms_irs.s[i - 1] == 0) i++;
    }
    if (perms_irs.p[j - 2] == 0) {
      while (j < perms_irs.p_n && perms_irs.p[j - 1] == 0) j++;
    }
  }
  perms_irs.s[k - 1] = perms_irs.s[i - 1];
  perms_irs.p[l - 1] = perms_irs.p[j - 1];
  perms_irs.s_n = k;
  perms_irs.p_n = l;

  for (j = 0; j < perms_irs.p_n; ++j) {
    if (perms_irs.p[j] == 0) {
      perms_irs.p[j] = op_max;
      op_max++;
    }
  }

  return perms_irs;
}

void CycleGraph::serialize(ostream &os) const {
  for (size_t i = 0; i < vertices.size(); ++i) {
    os << i << "(" << vertices[i].gene_val << "," << vertices[i].black << ","
       << vertices[i].weigth << "," << vertices[i].in_cycle << ","
       << vertices[i].sign_positive << "|";
    os << "[";
    if (vertices[i].is_indel) {
      os << "!" << vertices[i].indel;
    } else if (vertices[i].fix_gray == NO_EDGE) {
      for (auto gray : vertices[i].grays) {
        os << gray << ",";
      }
      os << "!" << vertices[i].indel;
    } else {
      os << "*" << vertices[i].fix_gray;
    }
    os << "])"
       << " ";
    if (int(i) == fhs - 1) os << endl;
  }
}

ostream &operator<<(std::ostream &os, const CycleGraph &cg) {
  cg.serialize(os);
  return os;
}

ostream &operator<<(ostream &os, const Run &r) {
  os << r.genome << r.fst_gene << ":";
  for (Gene a : r.genes_to_add) {
    os << " " << a;
  }
  cout << endl;
  return os;
}

void CycleGraph::rem_cycle(pair<size_t, Vtx_id> c) {
  Vtx_id headtailcorresp_v, headtailcorresp_u;
  Vtx_id i = c.second;

  if (cycle_weight(i) == 0) {
    balanced_cycles--;
  }
  indel_potation -= cycle_potation(i);
  vector<Vtx_id> cycle = get_cycle(i);

  assert(cycle.size() % 2 == 0);
  for (size_t i = 0; i < cycle.size(); ++i) {
    Vtx_id v = cycle[i];
    assert(vertices[v].in_cycle);
    vertices[v].in_cycle = false;
    if (i % 2 == 1) {
      Vtx_id u = (i == cycle.size() - 1) ? cycle[0] : cycle[i + 1];
      if (vertices[v].is_indel) {
        vertices[v].is_indel = false;
        vertices[u].is_indel = false;
        this->indel_count[vertices[v].gene_val] -= vertices[v].indel_update;
      } else if (vertices[v].grays.size() > 1) {
        if ((v % fhs) % 2 == 1) {
          headtailcorresp_v = v + 1;
        } else {
          headtailcorresp_v = v - 1;
        }
        if ((u % fhs) % 2 == 1) {
          headtailcorresp_u = u + 1;
        } else {
          headtailcorresp_u = u - 1;
        }
        if (not vertices[headtailcorresp_v].in_cycle &&
            not vertices[headtailcorresp_u].in_cycle) {
          assert(vertices[v].fix_gray != NO_EDGE);
          assert(vertices[u].fix_gray != NO_EDGE);
          assert(vertices[headtailcorresp_v].fix_gray != NO_EDGE);
          assert(vertices[headtailcorresp_u].fix_gray != NO_EDGE);
          vertices[v].fix_gray = NO_EDGE;
          vertices[u].fix_gray = NO_EDGE;
          vertices[headtailcorresp_v].fix_gray = NO_EDGE;
          vertices[headtailcorresp_u].fix_gray = NO_EDGE;
        }
      }
    }
  }
  cycles.erase(find(cycles.begin(), cycles.end(), c));
}

bool CycleGraph::check_cycle(vector<Vtx_id> cycle) {
  for (size_t i = 0; i < cycle.size(); ++i) {
    Vtx_id v = cycle[i];
    if (vertices[v].in_cycle) return false;
    if (i % 2 == 1) {
      Vtx_id u = (i == cycle.size() - 1) ? cycle[0] : cycle[i + 1];
      if (vertices[v].is_indel) {
        if (vertices[v].indel != u) return false;
      } else if (vertices[v].fix_gray != NO_EDGE) {
        if (vertices[v].fix_gray != u) return false;
      } else if (vertices[u].fix_gray != NO_EDGE) {
        return false;
      }
    }
  }
  return true;
}

void CycleGraph::add_cycle(vector<Vtx_id> cycle) {
  Vtx_id headtailcorresp_v, headtailcorresp_u;

  assert(cycle.size() % 2 == 0);
  for (size_t i = 0; i < cycle.size(); ++i) {
    Vtx_id v = cycle[i];
    assert(not vertices[v].in_cycle);
    vertices[v].in_cycle = true;
    if (i % 2 == 1 && vertices[v].fix_gray == NO_EDGE) {
      Vtx_id u = (i == cycle.size() - 1) ? cycle[0] : cycle[i + 1];
      if (u == vertices[v].indel) {
        vertices[v].is_indel = true;
        vertices[u].is_indel = true;
        this->indel_count[vertices[v].gene_val] += vertices[v].indel_update;
      } else {
        assert(vertices[u].fix_gray == NO_EDGE);
        vertices[v].fix_gray = u;
        vertices[u].fix_gray = v;
        if ((v % fhs) % 2 == 1) {
          headtailcorresp_v = v + 1;
        } else {
          headtailcorresp_v = v - 1;
        }
        if ((u % fhs) % 2 == 1) {
          headtailcorresp_u = u + 1;
        } else {
          headtailcorresp_u = u - 1;
        }
        assert(vertices[headtailcorresp_u].fix_gray == NO_EDGE);
        assert(vertices[headtailcorresp_v].fix_gray == NO_EDGE);
        vertices[headtailcorresp_v].fix_gray = headtailcorresp_u;
        vertices[headtailcorresp_u].fix_gray = headtailcorresp_v;
      }
    }
  }
  int pot = cycle_potation(cycle[0]);
  cycles.push_back(pair<size_t, Vtx_id>(cycle.size() + pot, cycle[0]));
  /* cycles.push_back(pair<size_t, Vtx_id>(cycle.size(), cycle[0])); */

  indel_potation += pot;
  /* indel_potation += cycle_potation(cycle[0]); */
  if (cycle_weight(cycle[0]) == 0) {
    balanced_cycles++;
  }
}

void CycleGraph::check_and_add_cycle(vector<Vtx_id> cycle) {
  if (check_cycle(cycle)) {
    add_cycle(cycle);
  }
}

vector<Vtx_id> CycleGraph::get_cycle(Vtx_id i) const {
  vector<Vtx_id> cycle;

  Vtx_id v = i;
  cycle.push_back(v);
  v = vertices[v].black;
  cycle.push_back(v);
  v = vertices[v].is_indel ? vertices[v].indel : vertices[v].fix_gray;
  while (v != cycle[0]) {
    cycle.push_back(v);
    v = vertices[v].black;
    cycle.push_back(v);
    v = vertices[v].is_indel ? vertices[v].indel : vertices[v].fix_gray;
  }

  return cycle;
}

int CycleGraph::cycle_weight(Vtx_id i) const {
  int weigth = 0;

  Vtx_id v = i;
  do {
    v = vertices[v].black;
    if (vertices[v].is_indel) {
      v = vertices[v].indel;
    } else {
      v = vertices[v].fix_gray;
      weigth += vertices[v].weigth;
    }
  } while (v != i);

  return weigth;
}

int CycleGraph::cycle_potation(int i) const {
  int runs = 0;
  int first_state = 0, last_state = 0, state = 0;

  Vtx_id v = i;
  do {
    v = vertices[v].black;
    if (vertices[v].is_indel) {
      state = vertices[v].indel_update;
      if (last_state != state) {
        if (last_state != 0) {
          runs++;
        } else {
          first_state = state;
        }
        last_state = state;
      }
      v = vertices[v].indel;
    } else {
      v = vertices[v].fix_gray;
    }
  } while (v != i);
  if (last_state != 0 && (last_state != first_state || runs == 0)) {
    runs++;
  }

  if (runs == 0) {
    return 0;
  } else {
    return ceil((runs + 1) / 2.0);
  }
}

Run CycleGraph::cycle_run(int i) const {
  Run run;
  int run_state = 0, state = 0; // States indicate if a run is a insertion or a deletion run
  Vtx_id v = i;

  // Find one end of the run
  int first_end = -1;
  do {
    v = vertices[v].black;
    if (vertices[v].is_indel) {
      state = vertices[v].indel_update;
      if (run_state != state) {
        if (run_state == 0) {
          run_state = state;
        } else {
          first_end = v;
          break;
        }
      }
      v = vertices[v].indel;
    } else {
      v = vertices[v].fix_gray;
    }
  } while (v != i);
  
  if (run_state == 0) return run; // No run on the cycle
  
  bool revert = false;
  if (first_end == -1) { // single run
    while (vertices[v].indel_update == run_state) {
      v = vertices[v].black;
      if (vertices[v].is_indel) {
        v = vertices[v].indel;
      } else {
        v = vertices[v].fix_gray;
      }
    }
    first_end = v;
  }

  // Get insertion position and check if the list of insertions must be inverted
  assert(vertices[first_end].indel_update != run_state);
  if (first_end > vertices[first_end].black) {
    run.fst_gene = vertices[vertices[v].black].gene_val;
  } else {
    run.fst_gene = vertices[v].gene_val;
    revert = true;
  }

  // Get genome where the insertion will occur
  if (run_state == -1) {
    run.genome = 'H';
  } else {
    run.genome = 'G';
  }
  
  // Get genes to insert
  v = first_end;
  do {
    v = vertices[v].black;
    if (vertices[v].is_indel) {
      state = vertices[v].indel_update;
      if (run_state != state) {
        break;
      }

      if (vertices[v].sign_positive == (v < vertices[v].indel)) {
        run.genes_to_add.push_back(vertices[v].gene_val);
      } else {
        run.genes_to_add.push_back(-vertices[v].gene_val);
      }

      v = vertices[v].indel;
    } else {
      v = vertices[v].fix_gray;
    }
  } while (v != first_end);
  
  if (revert) {
    reverse(run.genes_to_add.begin(), run.genes_to_add.end());
    for (int i = 0; i < int(run.genes_to_add.size()); i++) {
      run.genes_to_add[i] = - run.genes_to_add[i];
    }
  }

  return run;
}
