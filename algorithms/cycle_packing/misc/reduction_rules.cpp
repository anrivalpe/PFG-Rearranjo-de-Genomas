#include "reduction_rules.hpp"
#include "genome.hpp"

template <typename T> int sgn(T val) {
    return (T(0) < val) - (val < T(0));
}

void suboptimal_rule_interval(Genome &g, Genome &h) {
    vector<bool> original_singletons_pos(g.size() + 1, false);
    for (int i = 1; i <= int(g.size()); i++) {
        if (g.occ(g[i]) == 1 && h.occ(g[i]) == 1) original_singletons_pos[i] = true;
    }

    for (int i = 1; i < int(g.size()); i++) {
        int idx_al = -1, idx_ar = -1; // index from the left and the right of the interval a^l...a^r in g
        int idx_bl = -1, idx_br = -1; // index from the left and the right of the interval b^l...b^r in h

        // a^l is a singleton in both genomes
        if (original_singletons_pos[i]) {
            int j = h.pos(g[i]).back(); // get correspondent of g[i] in h
            bool dir = false, rev = false;

            // check if the interval is a direct match a^l...a^r = b^l...b^r
            idx_al = i;
            idx_bl = j;
            for (int k = 1; i+k <= int(g.size()) && j+k <= int(h.size()); k++) {
                if (g[i+k] != h[j+k]) {
                    break;
                }
                if (original_singletons_pos[i+k]) {
                    if (k > 1) {
                        idx_ar = i+k;
                        idx_br = j+k;
                        dir = true;
                    }
                    break;
                }
            }

            // check if the interval is a reverse match a^l...a^r = -b^r...-b^l
            if (!dir) {
                idx_al = i;
                idx_br = j;
                for (int k = 1; i+k <= int(g.size()) && j-k >= 1; k++) {
                    if (g[i+k] != -h[j-k]) {
                        break;
                    }
                    if (original_singletons_pos[i+k]) {
                        if (k > 1) {
                            idx_ar = i+k;
                            idx_bl = j-k;
                            rev = true;
                        }
                        break;
                    }
                }
            }
            
            // check if the middle is a reverse match a^la^{l+1}...a^{r-1}a^r = b^l-b^{r-1}...-b^{l+1}b^r
            if (!dir && !rev) {
                idx_al = i;
                idx_ar = -1;
                for (int k = i+1; k <= int(g.size()); k++) {
                    if (original_singletons_pos[k]) {
                        idx_ar = k;
                    }
                }
                
                if (idx_ar != -1) {
                    idx_bl = j;
                    for (int k = 1; idx_ar-k >= 1 && j+k <= int(h.size()); k++) {
                        if (idx_ar-k == idx_al) {
                            if (g[idx_ar] == h[j+k] && k > 1) {
                                idx_br = j+k;
                                rev = true;
                            }
                            break;
                        }
                        if (g[idx_ar-k] != -h[j+k]) {
                            break;
                        }
                    }
                }
            }
            
            if (dir || rev) {
                for (int k = 1; idx_al+k < idx_ar; k++) {
                    // Apply new assignment
                    Gene new_gene = max(g.get_op_max(), h.get_op_max());
                    g.replace_label(idx_al+k, new_gene);
                    if (dir) {
                        h.replace_label(idx_bl+k, new_gene);
                    } else {
                        h.replace_label(idx_br-k, new_gene);
                    }
                }
                i = idx_ar + 1;
            }
        }
    }
}

void suboptimal_rule_pairs(Genome &g, Genome &h) {
    vector<bool> original_singletons_pos(g.size() + 1, false);
    for (int i = 1; i <= int(g.size()); i++) {
        if (g.occ(g[i]) == 1 && h.occ(g[i]) == 1) original_singletons_pos[i] = true;
    }

    for (int i = 1; i < int(g.size()); i++) {
        int idx1 = -1, idx2 = -1;
        Gene ah = g[i], at = g[i+1]; // duo a^h a^t

        // a^h is a singleton in both genomes and a^t is a replica
        if (original_singletons_pos[i] && g.occ(at) > 1) {
            int b_idx = h.pos(ah).back();
            Gene b = h[b_idx];
            idx1 = i + 1;
            // a^t matches pred of b
            if (b_idx > 1 && sgn(ah) != sgn(b) && abs(at) == abs(h[b_idx - 1]) && sgn(at) != sgn(h[b_idx - 1])) {
                idx2 = b_idx - 1;
            // a^t matches succ of b
            } else if (b_idx < int(h.size()) && sgn(ah) == sgn(b) && abs(at) == abs(h[b_idx + 1]) && sgn(at) == sgn(h[b_idx + 1])) {
                idx2 = b_idx + 1;
            }
        // a^t is a singleton in both genomes and a^h is a replica
        } else if (original_singletons_pos[i+1] && g.occ(ah) > 1) {
            int b_idx = h.pos(at).back();
            Gene b = h[b_idx];
            idx1 = i;
            // a^h matches pred of b
            if (b_idx > 1 && sgn(at) == sgn(b) && abs(ah) == abs(h[b_idx - 1]) && sgn(ah) == sgn(h[b_idx - 1])) {
                idx2 = b_idx - 1;
            // a^h matches succ of b
            } else if (b_idx < int(h.size()) && sgn(at) != sgn(b) && abs(ah) == abs(h[b_idx + 1]) && sgn(ah) != sgn(h[b_idx + 1])) {
                idx2 = b_idx + 1;
            }
        }

        if (idx1 != -1 && idx2 != -1) {
            // Skip the next pair so that the new assignment is not taken into account
            if (original_singletons_pos[i]) {
                i++;
            }
            // Apply new assignment
            Gene new_gene = max(g.get_op_max(), h.get_op_max());
            g.replace_label(idx1, new_gene);
            h.replace_label(idx2, new_gene);
        }
    }
}
