#include "tiger/liveness/liveness.h"
#include <map>
#include <set>

extern frame::RegManager *reg_manager;

namespace live {

bool MoveList::Contain(INodePtr src, INodePtr dst) {
  return std::any_of(move_list_.cbegin(), move_list_.cend(),
                     [src, dst](std::pair<INodePtr, INodePtr> move) {
                       return move.first == src && move.second == dst;
                     });
}

void MoveList::Delete(INodePtr src, INodePtr dst) {
  assert(src && dst);
  auto move_it = move_list_.begin();
  for (; move_it != move_list_.end(); move_it++) {
    if (move_it->first == src && move_it->second == dst) {
      break;
    }
  }
  move_list_.erase(move_it);
}

MoveList *MoveList::Union(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : move_list_) {
    res->move_list_.push_back(move);
  }
  for (auto move : list->GetList()) {
    if (!res->Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

MoveList *MoveList::Intersect(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : list->GetList()) {
    if (Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

void LiveGraphFactory::LiveMap() {
  for (auto *node : flowgraph_->Nodes()->GetList()) {
    auto in = new temp::TempList;
    for (auto use : node->NodeInfo()->Use()->GetList()) {
      in->Append(use);
    }
    in_->Enter(node, in);
    out_->Enter(node, new temp::TempList);
  }
  bool changed = true;
  while (changed) {
    changed = false;
    for (auto *node : flowgraph_->Nodes()->GetList()) {
      std::set<temp::Temp *> o;
      // Calc new out
      for (auto *out : out_->Look(node)->GetList()) {
        o.insert(out);
      }
      for (auto *suc : node->Succ()->GetList()) {
        for (auto *suc_in : in_->Look(suc)->GetList()) {
          if (o.count(suc_in) == 0) {
            out_->Look(node)->Append(suc_in);
            o.insert(suc_in);
            changed = true;
          }
        }
      }

      // Out - Def
      for (auto *def : node->NodeInfo()->Def()->GetList()) {
        if (o.count(def)) {
          o.erase(def);
        }
      }

      // In = Use + (Out - Def)
      std::set<temp::Temp *> i;
      for (auto *in : in_->Look(node)->GetList()) {
        i.insert(in);
      }
      for (auto *use : node->NodeInfo()->Use()->GetList()) {
        if (i.count(use) == 0) {
          in_->Look(node)->Append(use);
          i.insert(use);
        }
      }
      for (auto *o_d : o) {
        if (i.count(o_d) == 0) {
          in_->Look(node)->Append(o_d);
          i.insert(o_d);
        }
      }
    }
  }
}

void LiveGraphFactory::InterfGraph() {
  auto interfere_graph = live_graph_.interf_graph;

  temp_node_map_ = new tab::Table<temp::Temp, INode>;
  for (auto *reg : reg_manager->Registers()->GetList()) {
    if (temp_node_map_->Look(reg) == nullptr) {
      temp_node_map_->Enter(reg, interfere_graph->NewNode(reg));
    }
  }
  for (auto *node : flowgraph_->Nodes()->GetList()) {
    for (auto *def : node->NodeInfo()->Def()->GetList()) {
      if (temp_node_map_->Look(def) == nullptr) {
        temp_node_map_->Enter(def, interfere_graph->NewNode(def));
      }
    }
    for (auto *out : out_->Look(node)->GetList()) {
      if (temp_node_map_->Look(out) == nullptr) {
        temp_node_map_->Enter(out, interfere_graph->NewNode(out));
      }
    }
  }

  auto moves = live_graph_.moves;
  for (auto *node : flowgraph_->Nodes()->GetList()) {
    if (typeid(*(node->NodeInfo())) != typeid(assem::MoveInstr)) {
      for (auto *out : out_->Look(node)->GetList()) {
        for (auto *def : node->NodeInfo()->Def()->GetList()) {
          interfere_graph->AddEdge(temp_node_map_->Look(def),
                                   temp_node_map_->Look(out));
          interfere_graph->AddEdge(temp_node_map_->Look(out),
                                   temp_node_map_->Look(def));
        }
      }
    } else {
      std::set<temp::Temp *> o;
      for (auto *out : out_->Look(node)->GetList()) {
        o.insert(out);
      }

      for (auto *def : node->NodeInfo()->Def()->GetList()) {
        for (auto *use : node->NodeInfo()->Use()->GetList()) {
          moves->Append(temp_node_map_->Look(def), temp_node_map_->Look(use));
          moves->Append(temp_node_map_->Look(use), temp_node_map_->Look(def));
        }
      }

      for (auto *use : node->NodeInfo()->Use()->GetList()) {
        if (o.count(use)) {
          o.erase(use);
        }
      }

      for (auto *out : o) {
        for (auto *def : node->NodeInfo()->Def()->GetList()) {
          interfere_graph->AddEdge(temp_node_map_->Look(def),
                                   temp_node_map_->Look(out));
          interfere_graph->AddEdge(temp_node_map_->Look(out),
                                   temp_node_map_->Look(def));
        }
      }
    }
  }
  for (auto from : reg_manager->Registers()->GetList()) {
    for (auto to : reg_manager->Registers()->GetList()) {
      interfere_graph->AddEdge(temp_node_map_->Look(from),
                               temp_node_map_->Look(to));
      interfere_graph->AddEdge(temp_node_map_->Look(to),
                               temp_node_map_->Look(from));
    }
  }
  fprintf(stdout, "---Liveness---\n");
  for (auto *node : flowgraph_->Nodes()->GetList()) {
    node->NodeInfo()->Print(
        stdout, temp::Map::LayerMap(reg_manager->temp_map_, temp::Map::Name()));
    for (auto *in : in_->Look(node)->GetList()) {
      fprintf(stdout, "t%d ", in->Int());
    }
    fprintf(stdout, "| ");
    for (auto *out : out_->Look(node)->GetList()) {
      fprintf(stdout, "t%d ", out->Int());
    }
    fprintf(stdout, "\n");
  }

  fprintf(stdout, "---Interfere Graph---\n");
  IGraph ::Show(stdout, interfere_graph->Nodes(),
                [](temp::Temp *t) { fprintf(stdout, "t%d", t->Int()); });

  fprintf(stdout, "---Move Graph---\n");

  for (auto move : moves->GetList()) {
    fprintf(stdout, "t%d - t%d\n", move.first->NodeInfo()->Int(),
            move.second->NodeInfo()->Int());
  }
}

void LiveGraphFactory::Liveness() {
  LiveMap();
  InterfGraph();
}

} // namespace live
