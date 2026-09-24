#include "aux.hpp"

int is_connected(const Permutation &pi, int i, int j) {
  // return values:
  // 0 - not connected
  // 1 - connected (case i)
  // 2 - connected (case ii)
  // 3 - connected (case iii)
  // 4 - connected (case iv)
  int nucleotides = pi.get_ir(i) + pi.get_ir(j);

  if (abs(pi[i] - pi[j]) == 1 && !pi.is_block(pi[i], pi[j], true) &&
      pi.get_ir_target(min(pi[i], pi[j]) + 1) <= nucleotides) {
    return 1;
  }
  if (abs(pi[i + 1] - pi[j + 1]) == 1 &&
      !pi.is_block(pi[i + 1], pi[j + 1], true) &&
      pi.get_ir_target(min(pi[i + 1], pi[j + 1]) + 1) <= nucleotides) {
    return 1;
  }
  if (abs(pi[i] - pi[j + 1]) == 1 && !pi.is_block(pi[i], pi[j + 1], true) &&
      pi.get_ir_target(min(pi[i], pi[j + 1]) + 1) <= nucleotides) {
    return 2;
  }
  if (abs(pi[i + 1] - pi[j]) == 1 && !pi.is_block(pi[i + 1], pi[j], true) &&
      pi.get_ir_target(min(pi[i + 1], pi[j]) + 1) <= nucleotides) {
    return 3;
  }
  if (abs(pi[i] - pi[i + 1]) == 1 && !pi.is_block(pi[i], pi[i + 1], true) &&
      pi.get_ir_target(min(pi[i], pi[i + 1]) + 1) <= nucleotides) {
    return 4;
  }
  if (abs(pi[j] - pi[j + 1]) == 1 && !pi.is_block(pi[j], pi[j + 1], true) &&
      pi.get_ir_target(min(pi[j], pi[j + 1]) + 1) <= nucleotides) {
    return 4;
  }
  return 0;
}

void transposition_move(Permutation &pi, Gene i, Gene j, Gene k, IR x_, IR y_,
                        IR z_) {
  int x1, x2, x3, y1, y2, y3, z1, z2, z3;
  int x = pi.get_ir(i), y = pi.get_ir(j), z = pi.get_ir(k);

  x1 = min(x, x_);
  x -= x1;
  x_ -= x1;
  y1 = min(y, x_);
  y -= y1;
  x_ -= y1;
  z1 = min(z, x_);
  z -= z1;
  x_ -= z1;
  x2 = min(x, y_);
  x -= x2;
  y_ -= x2;
  y2 = min(y, y_);
  y -= y2;
  y_ -= y2;
  z2 = min(z, y_);
  z -= z2;
  y_ -= z2;
  x3 = min(x, z_);
  x -= x3;
  z_ -= x3;
  y3 = min(y, z_);
  y -= y3;
  z_ -= y3;
  z3 = min(z, z_);
  z -= z3;
  z_ -= z3;

  pi.transposition(i + 1, j + 1, k + 1, x1 + x2, y2 + y3, z1);
  pi.transposition(i + 1, i + k - j + 1, k + 1, x1 + y1, x3, y2 + z2);
}

int case_i(Permutation &pi, int i, int j) {
  int goal, x, y;
  if (abs(pi[i] - pi[j]) == 1) {
    goal = pi.get_ir_target(min(pi[i], pi[j]) + 1);
    if (pi.get_ir(i) + pi.get_ir(j) >= goal) {
      if (pi.get_ir(i) >= goal) {
        x = goal;
        y = 0;
        pi.reversal(i + 1, j, x, y);
      } else {
        x = pi.get_ir(i);
        y = goal - x;
        pi.reversal(i + 1, j, x, y);
      }
      return 1;
    }
  }
  if (abs(pi[i + 1] - pi[j + 1]) == 1) {
    goal = pi.get_ir_target(min(pi[i + 1], pi[j + 1]) + 1);
    if (pi.get_ir(i) + pi.get_ir(j) >= goal) {
      if (pi.get_ir(j) >= goal) {
        x = pi.get_ir(i);
        y = pi.get_ir(j) - goal;
        pi.reversal(i + 1, j, x, y);
      } else {
        x = pi.get_ir(i) - (goal - pi.get_ir(j));
        y = 0;
        pi.reversal(i + 1, j, x, y);
      }
      return 1;
    }
  }
  return 0;
}


