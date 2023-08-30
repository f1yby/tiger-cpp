#include "tiger/liveness/flowgraph.h"
#include "tiger/frame/x64frame.h"

namespace fg {

void FlowGraphFactory::AssemFlowGraph() {
  // TODO check if it's right
  for (auto instr : instr_list_->GetList()) {
    if (typeid(*instr) == typeid(assem::LabelInstr)) {
      auto *node = flowgraph_->NewNode(instr);
      label_map_->Enter(static_cast<assem::LabelInstr *>(instr)->label_, node);
    }
  }

  auto curr = instr_list_->GetList().begin();
  auto next = instr_list_->GetList().begin();
  next++;

  auto curr_node = alloc_node_if_not_exit(*curr);
  while (next != instr_list_->GetList().end()) {
    auto next_node = alloc_node_if_not_exit(*next);

    auto *curr_instr = *curr;
    if (typeid(*curr_instr) == typeid(assem::OperInstr)) {
      auto op_instr = static_cast<assem::OperInstr *>(curr_instr);
      if (op_instr->jumps_ != nullptr) {
        for (auto *jump : *op_instr->jumps_->labels_) {
          auto *target = label_map_->Look(jump);
          flowgraph_->AddEdge(curr_node, target);
        }
      } else {
        flowgraph_->AddEdge(curr_node, next_node);
      }
    } else {
      flowgraph_->AddEdge(curr_node, next_node);
    }

    ++curr;
    ++next;
    curr_node = next_node;
  }
  fprintf(stdout, "---Control Flow Graph---\n");
  graph::Graph<assem::Instr>::Show(
      stdout, flowgraph_->Nodes(), [](assem::Instr *instr) {
        instr->Print(stdout, temp::Map::LayerMap(reg_manager->temp_map_,
                                                 temp::Map::Name()));
      });
}
FNodePtr FlowGraphFactory::alloc_node_if_not_exit(assem::Instr *instr) {
  if (typeid(*instr) == typeid(assem::LabelInstr)) {
    return label_map_->Look(static_cast<assem::LabelInstr *>(instr)->label_);
  } else {
    return flowgraph_->NewNode(instr);
  }
}

} // namespace fg

namespace assem {

temp::TempList const *LabelInstr::Def() const { return new temp::TempList; }

temp::TempList const *MoveInstr::Def() const { return dst_; }

temp::TempList const *OperInstr::Def() const { return dst_; }

temp::TempList const *LabelInstr::Use() const { return new temp::TempList; }

temp::TempList const *MoveInstr::Use() const { return src_; }

temp::TempList const *OperInstr::Use() const { return src_; }
} // namespace assem
