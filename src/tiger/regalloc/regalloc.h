#ifndef TIGER_REGALLOC_REGALLOC_H_
#define TIGER_REGALLOC_REGALLOC_H_

#include "tiger/codegen/assem.h"
#include "tiger/codegen/codegen.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/liveness/liveness.h"
#include "tiger/regalloc/color.h"
#include "tiger/util/graph.h"
#include <map>
#include <set>
#include <stack>

namespace ra {

class Result {
public:
  temp::Map *coloring_;
  assem::InstrList *il_;

  Result() : coloring_(nullptr), il_(nullptr) {}
  Result(temp::Map *coloring, assem::InstrList *il)
      : coloring_(coloring), il_(il) {}
  Result(const Result &result) = delete;
  Result(Result &&result) = delete;
  Result &operator=(const Result &result) = delete;
  Result &operator=(Result &&result) = delete;
  ~Result() = default;
};

class RegAllocator {
public:
  using GraphNodePtr = graph::Node<temp::Temp> *;

  void RegAlloc();

  std::unique_ptr<ra::Result> TransferResult() { return std::move(result_); }

  RegAllocator(frame::Frame *frame, std::unique_ptr<cg::AssemInstr> assem_instr)
      : frame_(frame), assem_instr_(std::move(assem_instr)) {}

private:
  frame::Frame *frame_;
  std::unique_ptr<cg::AssemInstr> assem_instr_;
  std::unique_ptr<ra::Result> result_;

  live::LiveGraph live_graph_{nullptr, nullptr};
  tab::Table<temp::Temp, graph::Node<temp::Temp>> *temp_node_map_;
  std::set<GraphNodePtr> pre_colored_;

  std::stack<GraphNodePtr> stack_;

  std::map<GraphNodePtr, uint32_t> degree_;

  std::set<std::pair<GraphNodePtr, GraphNodePtr>> active_move_;

  std::map<GraphNodePtr, GraphNodePtr> actuals_;
  std::map<GraphNodePtr, std::set<GraphNodePtr>> alias_;

  std::set<GraphNodePtr> active_nodes_;

  std::map<GraphNodePtr, temp::Temp*> color_;
  std::set<GraphNodePtr > spilled_;

  void liveness();
  void prepare();
  void simplify();
  void coalesce();
  void spill();
  void select();
  void rebuild();

  std::set<GraphNodePtr> active_adj(GraphNodePtr ptr);
  void remove_node(GraphNodePtr ptr);

  std::set<GraphNodePtr> active_move_nodes();
  std::set<GraphNodePtr> bi_adj(GraphNodePtr left, GraphNodePtr right);
  void link_node(GraphNodePtr left, GraphNodePtr right);

};

} // namespace ra

#endif