#include "external.hpp"
#include <cstdio>
#include <sstream>

int ExternalDistAlg::estimate_distance(Permutation pi) {
  int dist;
  stringstream ss;
  ss << prog << " ";
  for (size_t i = 1; i < pi.size(); i++) {
    ss << pi[i] << ",";
  }
  ss << pi[pi.size()] << " ";
  for (size_t i = 1; i < pi.size() - 1; i++) {
    ss << pi.get_ir_target(i) << ",";
  }
  ss << pi.get_ir_target(pi.size() - 1) << " ";
  for (size_t i = 1; i < pi.size() - 1; i++) {
    ss << pi.get_ir(i) << ",";
  }
  ss << pi.get_ir(pi.size() - 1);
  FILE *prog_pipe;
  if ((prog_pipe = popen(ss.str().c_str(), "r")) == NULL) {
    throw runtime_error("Error calling child process.");
  }
  fscanf(prog_pipe, "%d", &dist);
  if (pclose(prog_pipe) != EXIT_SUCCESS) {
    throw runtime_error("Error on child process.");
  }
  return dist;
}
