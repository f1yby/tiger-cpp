#include "tiger/codegen/codegen.h"

#include <sstream>

namespace {

constexpr int maxlen = 1024;

std::string to_string(int i) {
  auto ss = std::stringstream();
  ss << i;
  auto s = std::string();
  ss >> s;
  return s;
}

} // namespace

namespace cg {

void CodeGen::Codegen() {

  auto *stmts = traces_->GetStmList();
  auto *i = new assem::InstrList();

  for (auto *stmt : stmts->GetList()) {
    stmt->Munch(*i, frame_->label_->Name() + "_framesize");
  }
  if (frame_->label_->Name() != "tigermain") {
    i->Append(new assem::OperInstr("", new temp::TempList,
                                   reg_manager->ReturnSink(), nullptr));
  }
  assem_instr_ = std::make_unique<AssemInstr>(i);
}

void AssemInstr::Print(FILE *out, temp::Map *map) const {
  for (auto instr : instr_list_->GetList())
    instr->Print(out, map);
  fprintf(out, "\n");
}
} // namespace cg

namespace tree {

void SeqStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  left_->Munch(instr_list, fs);
  right_->Munch(instr_list, fs);
}

void LabelStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  instr_list.Append(new assem::LabelInstr(label_->Name(), label_));
}

void JumpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  instr_list.Append(new assem::OperInstr("jmp `j0", new temp::TempList,
                                         new temp::TempList,
                                         new assem::Targets{jumps_}));
}

void CjumpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  auto *l = left_->Munch(instr_list, fs);
  auto *r = right_->Munch(instr_list, fs);

  auto *t = new assem::Targets{new std::vector{true_label_, false_label_}};
  auto j = std::string();
  switch (op_) {
  case EQ_OP:
    j = "je";
    break;
  case NE_OP:
    j = "jne";
    break;
  case LT_OP:
    j = "jl";
    break;
  case GT_OP:
    j = "jg";
    break;
  case LE_OP:
    j = "jle";
    break;
  case GE_OP:
    j = "jge";
    break;
  default:
    abort();
  }

  instr_list.Append(new assem::OperInstr("cmpq `s1, `s0", new temp::TempList,
                                         new temp::TempList{l, r}, nullptr));
  instr_list.Append(new assem::OperInstr(j + " `j0", new temp::TempList,
                                         new temp::TempList, t));
  instr_list.Append(new assem::OperInstr("jmp " + false_label_->Name(),
                                         new temp::TempList, new temp::TempList,
                                         nullptr));
}

void MoveStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  auto *s = src_->Munch(instr_list, fs);

  if (typeid(*dst_) == typeid(MemExp)) {
    auto *d = static_cast<MemExp *>(dst_)->exp_->Munch(instr_list, fs);
    instr_list.Append(new assem::OperInstr("movq `s0, (`s1)",
                                           new temp::TempList,
                                           new temp::TempList{s,d}, nullptr));
  } else {
    auto *d = dst_->Munch(instr_list, fs);
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{d}, new temp::TempList{s}));
  }
}

void ExpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  exp_->Munch(instr_list, fs);
}

temp::Temp *BinopExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  auto o = std::string();
  switch (op_) {
  case PLUS_OP:
    o = "addq";
    break;
  case MINUS_OP:
    o = "subq";
    break;
  case MUL_OP:
    o = "imulq";
    break;
  case DIV_OP:
    o = "idivq";
    break;
  case AND_OP:
  case OR_OP:
  case LSHIFT_OP:
  case RSHIFT_OP:
  case ARSHIFT_OP:
  case XOR_OP:
  default:
    abort();
  }

  if (op_ == PLUS_OP || op_ == MINUS_OP) {
    auto *t = temp::TempFactory::NewTemp();
    auto *l = left_->Munch(instr_list, fs);
    auto *r = right_->Munch(instr_list, fs);

    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{t}, new temp::TempList{l}));
    instr_list.Append(new assem::OperInstr(o + " `s0, `d0",
                                           new temp::TempList{t},
                                           new temp::TempList{r, t}, nullptr));
    return t;
  } else {
    auto *t = temp::TempFactory::NewTemp();
    auto *l = left_->Munch(instr_list, fs);
    auto *r = right_->Munch(instr_list, fs);
    auto *td = temp::TempFactory::NewTemp();
    auto *ta = temp::TempFactory::NewTemp();

    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList(ta),
        new temp::TempList{
            static_cast<frame::X64RegManager *>(reg_manager)->rax}));
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList(td),
        new temp::TempList{
            static_cast<frame::X64RegManager *>(reg_manager)->rdx}));

    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0",
        new temp::TempList{
            static_cast<frame::X64RegManager *>(reg_manager)->rax},
        new temp::TempList(l)));
    instr_list.Append(new assem::OperInstr(
        "cqto",
        new temp::TempList{
            static_cast<frame::X64RegManager *>(reg_manager)->rax,
            static_cast<frame::X64RegManager *>(reg_manager)->rdx,
        },
        new temp::TempList{
            static_cast<frame::X64RegManager *>(reg_manager)->rax,
        },
        nullptr));

    instr_list.Append(new assem::OperInstr(
        o + " `s0",
        new temp::TempList{
            static_cast<frame::X64RegManager *>(reg_manager)->rax,
            static_cast<frame::X64RegManager *>(reg_manager)->rdx,
        },
        new temp::TempList{
            r,
            static_cast<frame::X64RegManager *>(reg_manager)->rax,
        },
        nullptr));

    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList(t),
        new temp::TempList{
            static_cast<frame::X64RegManager *>(reg_manager)->rax}));

    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0",
        new temp::TempList{
            static_cast<frame::X64RegManager *>(reg_manager)->rax},
        new temp::TempList(ta)));
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0",
        new temp::TempList{
            static_cast<frame::X64RegManager *>(reg_manager)->rdx},
        new temp::TempList(td)));

    return t;
  }
}

temp::Temp *MemExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  auto *m = temp::TempFactory::NewTemp();
  auto *e = exp_->Munch(instr_list, fs);
  instr_list.Append(new assem::OperInstr("movq (`s0), `d0",
                                         new temp::TempList{m},
                                         new temp::TempList{e}, nullptr));
  return m;
}

temp::Temp *TempExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  if (temp_ == reg_manager->FramePointer()) {
    auto *t = temp::TempFactory::NewTemp();
    instr_list.Append(new assem::OperInstr(
        "leaq " + std::string(fs) + "(%rsp), `d0", new temp::TempList{t},
        new temp::TempList, nullptr));
    return t;
  }
  return temp_;
}

temp::Temp *EseqExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  stm_->Munch(instr_list, fs);
  return exp_->Munch(instr_list, fs);
}

temp::Temp *NameExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  auto *t = temp::TempFactory::NewTemp();
  instr_list.Append(new assem::OperInstr("leaq " + name_->Name() + "(%rip), `d0",
                                         new temp::TempList{t},
                                         new temp::TempList, nullptr));
  return t;
}

temp::Temp *ConstExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  auto s = to_string(consti_);
  auto *t = temp::TempFactory::NewTemp();
  instr_list.Append(new assem::OperInstr("movq $" + s + ", `d0",
                                         new temp::TempList(t),
                                         new temp::TempList, nullptr));
  return t;
}

temp::Temp *CallExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  auto *l = args_->MunchArgs(instr_list, fs);
  auto *f = static_cast<tree::NameExp *>(fun_);

  auto reg = reg_manager->ArgRegs()->GetList().size();
  auto used_arg = new temp::TempList;

  for (auto i = 0; i < reg && i < l->GetList().size(); ++i) {
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{reg_manager->ArgRegs()->NthTemp(i)},
        new temp::TempList{l->NthTemp(i)}));
    used_arg->Append(reg_manager->ArgRegs()->NthTemp(i));
  }

  if (reg < l->GetList().size()) {
    auto spill = l->GetList().size() - reg;
    auto sp = to_string(spill * reg_manager->WordSize());

    instr_list.Append(new assem::OperInstr("subq $" + sp + ", %rsp",
                                           new temp::TempList,
                                           new temp::TempList, nullptr));

    for (auto i = 0; i < spill; ++i) {
      auto s = to_string(i * reg_manager->WordSize());
      instr_list.Append(new assem::OperInstr(
          "movq `s0, " + s + "(%rsp)", new temp::TempList,
          new temp::TempList{l->NthTemp(i + reg)}, nullptr));
    }
  }

  instr_list.Append(new assem::OperInstr("callq " + f->name_->Name(),
                                         reg_manager->CallerSaves(), used_arg,
                                         nullptr));
  if (reg < l->GetList().size()) {
    auto spill = l->GetList().size() - reg;
    auto sp = to_string(spill * reg_manager->WordSize());

    instr_list.Append(new assem::OperInstr("addq $" + sp + ", %rsp",
                                           new temp::TempList,
                                           new temp::TempList, nullptr));
  }

  auto *t = temp::TempFactory::NewTemp();
  instr_list.Append(
      new assem::MoveInstr("movq `s0, `d0", new temp::TempList{t},
                           new temp::TempList{reg_manager->ReturnValue()}));

  return t;
}

temp::TempList *ExpList::MunchArgs(assem::InstrList &instr_list,
                                   std::string_view fs) {
  auto e = new temp::TempList();

  for (auto *exp : exp_list_) {
    e->Append(exp->Munch(instr_list, fs));
  }
  return e;
}

} // namespace tree
