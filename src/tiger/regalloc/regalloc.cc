#include "tiger/regalloc/regalloc.h"

#include "tiger/output/logger.h"

namespace ra {
void RegAllocator::RegAlloc() {
  do {
    liveness();
    prepare();
    while (!active_nodes_.empty()) {
      uint32_t active_node_size;
      uint32_t active_move_size;
      do {
        active_node_size = active_nodes_.size();
        active_move_size = active_move_.size();
        simplify();
        coalesce();
      } while (active_node_size != active_nodes_.size() ||
               active_move_size != active_move_.size());
      spill();
    }
    select();
    rebuild();
  } while (!spilled_.empty());
  auto color = temp::Map::Empty();

  for (auto a : alias_) {
    fprintf(stdout, "%d <- ", a.first->NodeInfo()->Int());
    for (auto aa : a.second) {
      fprintf(stdout, "%d ", aa->NodeInfo()->Int());
    }
    fprintf(stdout, "\n");
  }

  for (auto c : color_) {
    color->Enter(c.first->NodeInfo(), reg_manager->temp_map_->Look(c.second));
  }

  result_ = std::make_unique<Result>(color, assem_instr_->GetInstrList());
}
void RegAllocator::liveness() {
  fg::FlowGraphFactory factory(assem_instr_->GetInstrList());
  factory.AssemFlowGraph();
  auto control_flow = factory.GetFlowGraph();

  live::LiveGraphFactory l(control_flow);
  l.Liveness();
  live_graph_ = l.GetLiveGraph();
  temp_node_map_ = l.GetTempNodeMap();
}
void RegAllocator::simplify() {
  auto move_node = active_move_nodes();

  decltype(active_nodes_) to_simplify;
  for (auto *node : active_nodes_) {
    auto actual = actuals_[node];
    if (actual != node) {
      abort();
    }
    // Degree less than K && not move-related && not pre-colored can be
    // simplified
    if (move_node.count(actual) == 0 &&
        degree_[actual] < reg_manager->Registers()->GetList().size() &&
        active_nodes_.count(actual)) {
      to_simplify.insert(actual);
    }
  }

  for (auto s : to_simplify) {
    remove_node(s);
  }
}
void RegAllocator::coalesce() {
  decltype(active_move_) to_combine;
  decltype(active_move_) to_freeze;

  for (auto move : active_move_) {
    auto from = actuals_[move.first];
    auto to = actuals_[move.second];

    // Shouldn't occur

    // Already coalesced
    if (from == to) {
      abort();
    }
    // Already removed
    if (!active_nodes_.count(from) && !pre_colored_.count(from) ||
        !active_nodes_.count(to) && !pre_colored_.count(to)) {
      abort();
    }

    // Frozen
    auto adj_f = active_adj(from);
    if (adj_f.count(to)) {
      continue;
    }

    // Briggs
    if (bi_adj(from, to).size() < reg_manager->Registers()->GetList().size()) {
      to_combine.insert(std::make_pair(from, to));
      to_combine.insert(std::make_pair(to, from));
      break;
    }

    // George
    bool merge = true;
    auto adj_t = active_adj(to);
    for (auto af : active_adj(from)) {
      if (!adj_t.count(af) ||
          degree_[af] < reg_manager->Registers()->GetList().size()) {
        merge = false;
      }
    }
    if (merge) {
      to_combine.insert(std::make_pair(from, to));
      to_combine.insert(std::make_pair(to, from));
      break;
    }
  }

  for (auto c : to_combine) {
    link_node(c.first, c.second);
  }

  for (auto move : active_move_) {
    auto from = actuals_[move.first];
    auto to = actuals_[move.second];

    // Shouldn't occur

    // Already coalesced
    if (from == to) {
      abort();
    }
    // Already removed
    if (!active_nodes_.count(from) && !pre_colored_.count(from) ||
        !active_nodes_.count(to) && !pre_colored_.count(to)) {
      abort();
    }

    // Frozen
    auto adj_f = active_adj(from);
    if (adj_f.count(to)) {
      to_freeze.insert(std::make_pair(from, to));
      to_freeze.insert(std::make_pair(to, from));
      continue;
    }
  }
  for (auto c : to_freeze) {
    active_move_.erase(c);
  }
}
void RegAllocator::remove_node(GraphNodePtr ptr) {
  // Update active move
  decltype(active_move_) new_move;
  for (auto &edge : active_move_) {
    if (edge.first == ptr || edge.second == ptr) {
      continue;
    }
    new_move.insert(edge);
  }
  active_move_ = new_move;

  auto adj = active_adj(ptr);
  for (auto *node : adj) {
    degree_[node]--;
  }
  stack_.push(actuals_[ptr]);
  active_nodes_.erase(actuals_[ptr]);
}
std::set<RegAllocator::GraphNodePtr>
RegAllocator::active_adj(RegAllocator::GraphNodePtr ptr) {
  auto actual = actuals_[ptr];
  auto alias = alias_[actual];
  auto adj = std::set<GraphNodePtr>();

  for (auto *node : alias) {
    for (auto *a : node->Adj()->GetList()) {
      if (active_nodes_.count(actuals_[a]) || pre_colored_.count(actuals_[a])) {
        adj.insert(actuals_[a]);
      }
    }
  }
  adj.erase(actual);

  return adj;
}
std::set<RegAllocator::GraphNodePtr> RegAllocator::active_move_nodes() {
  auto move_nodes = std::set<GraphNodePtr>();
  for (auto move : active_move_) {
    auto from = actuals_[move.first];
    auto to = actuals_[move.second];

    // Active move  = Move - Coalesced - Frozen

    // They've been coalesced
    // Move - Coalesced
    if (from == to) {
      continue;
    }

    // Shouldn't occur
    if (!active_nodes_.count(from) && !pre_colored_.count(from) ||
        !active_nodes_.count(to) && !pre_colored_.count(to)) {
      abort();
    }

    move_nodes.insert(from);
    move_nodes.insert(to);
  }
  return move_nodes;
}
void RegAllocator::link_node(RegAllocator::GraphNodePtr left,
                             RegAllocator::GraphNodePtr right) {
  auto al = actuals_[left];
  auto ar = actuals_[right];

  // Already combined
  if (al == ar) {
    return;
  }

  // If there is a pre-colored node, ar should hold the pre-colored node
  if (pre_colored_.count(al)) {
    std::swap(al, ar);
  }

  // Should never combine two register since they are interfered by nature
  if (pre_colored_.count(al)) {
    abort();
  }

  // Update active move
  decltype(active_move_) new_move;
  for (auto &edge : active_move_) {
    new_move.insert(std::make_pair(edge.first == al ? ar : edge.first,
                                   edge.second == al ? ar : edge.second));
  }
  // The combined move (al -> ar) -> (ar -> ar)
  new_move.erase(std::make_pair(ar, ar));
  active_move_ = new_move;

  // Update alias
  auto aars = alias_[ar];
  for (auto aal : alias_[al]) {
    aars.insert(aal);
    // Update actual
    actuals_[aal] = ar;
  }
  // Handle left
  alias_[ar] = aars;
  alias_.erase(al);

  // Update degree of new big node
  auto adj_r = active_adj(ar);
  degree_[ar] = adj_r.size();
  for (auto node : adj_r) {
    degree_[node] = active_adj(node).size();
  }

  // Remove from active
  active_nodes_.erase(al);
}
std::set<RegAllocator::GraphNodePtr>
RegAllocator::bi_adj(RegAllocator::GraphNodePtr left,
                     RegAllocator::GraphNodePtr right) {
  auto al = actuals_[left];
  auto ar = actuals_[right];

  auto adj = active_adj(al);

  for (auto aar : active_adj(ar)) {
    if (aar != al) {
      adj.insert(aar);
    }
  }
  return adj;
}
void RegAllocator::spill() {
  if (active_nodes_.empty()) {
    return;
  }
  uint32_t max_degree = 0;
  GraphNodePtr node = nullptr;
  for (auto an : active_nodes_) {
    if (degree_[an] > max_degree) {
      max_degree = degree_[an];
      node = an;
    }
  }
  if (node == nullptr) {
    abort();
  }

  remove_node(node);
  stack_.push(node);
}
void RegAllocator::select() {
  auto all_color = std::set<temp::Temp *>();
  for (auto color : reg_manager->Registers()->GetList()) {
    all_color.insert(color);
  }
  while (!stack_.empty()) {
    auto node = stack_.top();
    active_nodes_.insert(node);
    stack_.pop();

    auto adj = active_adj(node);
    auto color = all_color;
    for (auto a : adj) {
      color.erase(color_[a]);
    }

    if (color.empty()) {
      color_[node] = nullptr;
      spilled_.insert(node);
    } else {
      color_[node] = *(color.begin());
    }
  }
}
void RegAllocator::prepare() {
  degree_.clear();
  actuals_.clear();
  alias_.clear();
  active_nodes_.clear();
  pre_colored_.clear();

  active_move_.clear();

  color_.clear();
  spilled_.clear();

  for (auto *reg_temp : reg_manager->Registers()->GetList()) {
    pre_colored_.insert(temp_node_map_->Look(reg_temp));
    color_.insert(std::make_pair(temp_node_map_->Look(reg_temp), reg_temp));
  }
  for (auto node : live_graph_.interf_graph->Nodes()->GetList()) {
    actuals_.insert({node, node});
    alias_.insert({node, {node}});
    degree_.insert({node, node->Adj()->GetList().size() / 2});
    if (!pre_colored_.count(node)) {
      active_nodes_.insert(node);
    }
  }
  for (auto move : live_graph_.moves->GetList()) {
    active_move_.insert(std::make_pair(move.first, move.second));
    active_move_.insert(std::make_pair(move.second, move.first));
  }
}
void RegAllocator::rebuild() {
  // Combine
  auto combined_instr = new assem::InstrList;
  for (auto *instr : assem_instr_->GetInstrList()->GetList()) {
    if (typeid(*instr) == typeid(assem::OperInstr)) {
      auto op_instr = static_cast<assem::OperInstr *>(instr);
      {
        auto new_src = new temp::TempList;
        for (auto src : op_instr->src_->GetList()) {
          new_src->Append(actuals_[temp_node_map_->Look(src)]->NodeInfo());
        }
        assert(op_instr->src_->GetList().size() == new_src->GetList().size());
        op_instr->src_ = new_src;
      }
      {
        auto new_dst = new temp::TempList;
        for (auto dst : op_instr->dst_->GetList()) {
          new_dst->Append(actuals_[temp_node_map_->Look(dst)]->NodeInfo());
        }
        assert(op_instr->dst_->GetList().size() == new_dst->GetList().size());
        op_instr->dst_ = new_dst;
      }
      combined_instr->Append(op_instr);
    } else if (typeid(*instr) == typeid(assem::MoveInstr)) {
      auto mv_instr = static_cast<assem::MoveInstr *>(instr);
      if (mv_instr->src_->GetList().size() != 1) {
        abort();
      }
      if (mv_instr->dst_->GetList().size() != 1) {
        abort();
      }
      if (actuals_[temp_node_map_->Look(mv_instr->src_->NthTemp(0))] ==
          actuals_[temp_node_map_->Look(mv_instr->dst_->NthTemp(0))]) {
        continue;
      } else {
        {
          auto new_src = new temp::TempList;
          for (auto src : mv_instr->src_->GetList()) {
            new_src->Append(actuals_[temp_node_map_->Look(src)]->NodeInfo());
          }
          assert(mv_instr->src_->GetList().size() == new_src->GetList().size());
          mv_instr->src_ = new_src;
        }
        {
          auto new_dst = new temp::TempList;
          for (auto dst : mv_instr->dst_->GetList()) {
            new_dst->Append(actuals_[temp_node_map_->Look(dst)]->NodeInfo());
          }
          assert(mv_instr->dst_->GetList().size() == new_dst->GetList().size());
          mv_instr->dst_ = new_dst;
        }
      }
      combined_instr->Append(mv_instr);
    } else {
      combined_instr->Append(instr);
    }
  }

#if 1
  // Spill
  auto old_spilled_instr = combined_instr;
  auto spilled_instr = new assem::InstrList;
  int pos = frame_->local_on_stack_;
  frame_->local_on_stack_ += spilled_.size();
  ++pos;
  for (auto s : spilled_) {
    for (auto *instr : old_spilled_instr->GetList()) {
      if (typeid(*instr) == typeid(assem::OperInstr)) {
        auto op_instr = static_cast<assem::OperInstr *>(instr);

        bool src_used = false;
        for (auto src : op_instr->src_->GetList()) {
          if (src == s->NodeInfo()) {
            src_used = true;
          }
        }

        bool dst_used = false;
        for (auto dst : op_instr->dst_->GetList()) {
          if (dst == s->NodeInfo()) {
            dst_used = true;
          }
        }
        if (src_used) {

          auto new_src = new temp::TempList;
          auto new_temp = temp::TempFactory::NewTemp();

          spilled_instr->Append(new assem::OperInstr(
              "movq (" + frame_->label_->Name() + "_framesize-" +
                  std::to_string(pos * 8) + ")(%rsp), `d0",
              new temp::TempList{new_temp}, new temp::TempList, nullptr));

          for (auto src : op_instr->src_->GetList()) {
            if (src == s->NodeInfo()) {
              new_src->Append(new_temp);
            } else {
              new_src->Append(src);
            }
          }
          op_instr->src_ = new_src;
        }

        spilled_instr->Append(op_instr);

        if (dst_used) {
          auto new_dst = new temp::TempList;
          auto new_temp = temp::TempFactory::NewTemp();

          for (auto dst : op_instr->dst_->GetList()) {
            if (dst == s->NodeInfo()) {
              new_dst->Append(new_temp);
            } else {
              new_dst->Append(dst);
            }
          }
          op_instr->dst_ = new_dst;
          spilled_instr->Append(new assem::OperInstr(
              "movq `s0, (" + frame_->label_->Name() + "_framesize-" +
                  std::to_string(pos * 8) + ")(%rsp)",
              new temp::TempList, new temp::TempList{new_temp}, nullptr));
        }

      } else if (typeid(*instr) == typeid(assem::MoveInstr)) {

        auto mv_instr = static_cast<assem::MoveInstr *>(instr);
        bool src_used = false;
        for (auto src : mv_instr->src_->GetList()) {
          if (src == s->NodeInfo()) {
            src_used = true;
          }
        }

        bool dst_used = false;
        for (auto dst : mv_instr->dst_->GetList()) {
          if (dst == s->NodeInfo()) {
            dst_used = true;
          }
        }
        if (src_used) {

          auto new_src = new temp::TempList;
          auto new_temp = temp::TempFactory::NewTemp();

          spilled_instr->Append(new assem::OperInstr(
              "movq (" + frame_->label_->Name() + "_framesize-" +
                  std::to_string(pos * 8) + ")(%rsp), `d0",
              new temp::TempList{new_temp}, new temp::TempList, nullptr));

          for (auto src : mv_instr->src_->GetList()) {
            if (src == s->NodeInfo()) {
              new_src->Append(new_temp);
            } else {
              new_src->Append(src);
            }
          }
          mv_instr->src_ = new_src;
        }

        spilled_instr->Append(mv_instr);

        if (dst_used) {
          auto new_dst = new temp::TempList;
          auto new_temp = temp::TempFactory::NewTemp();

          for (auto dst : mv_instr->dst_->GetList()) {
            if (dst == s->NodeInfo()) {
              new_dst->Append(new_temp);
            } else {
              new_dst->Append(dst);
            }
          }
          mv_instr->dst_ = new_dst;
          spilled_instr->Append(new assem::OperInstr(
              "movq `s0, (" + frame_->label_->Name() + "_framesize-" +
                  std::to_string(pos * 8) + ")(%rsp)",
              new temp::TempList, new temp::TempList{new_temp}, nullptr));
        }
      } else {
        spilled_instr->Append(instr);
      }
    }
    ++pos;
    old_spilled_instr = spilled_instr;
    spilled_instr = new assem::InstrList;
  }
  assem_instr_ = std::make_unique<cg::AssemInstr>(old_spilled_instr);
#else
  assem_instr_ = std::make_unique<cg::AssemInstr>(combined_instr);
#endif
}

} // namespace ra