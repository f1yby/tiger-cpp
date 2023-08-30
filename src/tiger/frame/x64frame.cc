#include "tiger/frame/x64frame.h"
#include <ios>
#include <sstream>

namespace frame {
tree::Stm *ProcEntryExit1(Frame *frame, tree::Stm *stmts) {
  auto *cs = reg_manager->CalleeSaves();
  tree::Stm *ret = new tree::ExpStm(new tree::ConstExp(0));
  temp::TempList tl;

  for (int i = 0; i < cs->GetList().size(); ++i) {
    auto *t = temp::TempFactory::NewTemp();
    tl.Append(t);
    ret = new tree::SeqStm(
        ret, new tree::MoveStm(new tree::TempExp(tl.NthTemp(i)),
                               new tree::TempExp(cs->NthTemp(i))));
  }
  ret = new tree::SeqStm(ret, stmts);
  for (int i = 0; i < cs->GetList().size(); ++i) {
    ret = new tree::SeqStm(ret,
                           new tree::MoveStm(new tree::TempExp(cs->NthTemp(i)),
                                             new tree::TempExp(tl.NthTemp(i))));
  }
  return ret;
}

assem::Proc *ProcEntryExit3(Frame *frame, assem::InstrList *pList) {

  auto s = std::to_string(frame->local_on_stack_ * reg_manager->WordSize());

  pList->Insert(pList->GetList().begin(),
                new assem::OperInstr("subq $" + s + ", %rsp",
                                     new temp::TempList, new temp::TempList,
                                     nullptr));

  pList->Append(new assem::OperInstr("addq $" + s + ", %rsp",
                                     new temp::TempList, new temp::TempList,
                                     nullptr));

  if (frame->name_->Name() == "main") {
    pList->Insert(pList->GetList().begin(),
                  new assem::OperInstr("movq %rbp, (%rsp)", new temp::TempList,
                                       new temp::TempList, nullptr));
    pList->Insert(pList->GetList().begin(),
                  new assem::OperInstr("subq $8, %rsp", new temp::TempList,
                                       new temp::TempList, nullptr));

    pList->Append(new assem::OperInstr("movq (%rsp), %rbp", new temp::TempList,
                                       new temp::TempList, nullptr));
    pList->Append(new assem::OperInstr("addq $8, %rsp", new temp::TempList,
                                       new temp::TempList, nullptr));
  }
  pList->Append(new assem::OperInstr("retq", nullptr, nullptr, nullptr));
  return new assem::Proc(".set " + frame->label_->Name() + "_framesize, " + s +
                             "\n" + frame->label_->Name() + ":\n",
                         pList, "");
}

class X64Frame : public Frame {
  /* TODO: Put your lab5 code here */
};

temp::TempList *X64RegManager::Registers() {
  return new temp::TempList({
      rax,
      rcx,
      rbx,
      rdx,
      rbp,
      rdi,
      rsi,
      r8,
      r9,
      r10,
      r11,
      r12,
      r13,
      r14,
      r15,
  });
}
temp::TempList *X64RegManager::ArgRegs() {
  return new temp::TempList({
      rdi,
      rsi,
      rdx,
      rcx,
      r8,
      r9,
  });
}
temp::TempList *X64RegManager::CallerSaves() {
  return new temp::TempList({
      rax,
      rcx,
      rdx,
      rdi,
      rsi,
      rbp,
      r8,
      r9,
      r10,
      r11,
  });
}
temp::TempList *X64RegManager::CalleeSaves() {
  return new temp::TempList({
      rbx,
      rbp,
      r12,
      r13,
      r14,
      r15,
  });
}

temp::TempList *X64RegManager::ReturnSink() {
  return new temp::TempList{
      rbx, rbp, r12, r13, r14, r15, rax,
  };
}

int X64RegManager::WordSize() { return 8; }

temp::Temp *X64RegManager::FramePointer() { return shadow_fp; }

temp::Temp *X64RegManager::StackPointer() { return rsp; }

temp::Temp *X64RegManager::ReturnValue() { return rax; }
X64RegManager::X64RegManager()
    : rax(temp::TempFactory::NewTemp()), rcx(temp::TempFactory::NewTemp()),
      rbx(temp::TempFactory::NewTemp()), rdx(temp::TempFactory::NewTemp()),
      rdi(temp::TempFactory::NewTemp()), rsi(temp::TempFactory::NewTemp()),
      rsp(temp::TempFactory::NewTemp()), rbp(temp::TempFactory::NewTemp()),
      r8(temp::TempFactory::NewTemp()), r9(temp::TempFactory::NewTemp()),
      r10(temp::TempFactory::NewTemp()), r11(temp::TempFactory::NewTemp()),
      r12(temp::TempFactory::NewTemp()), r13(temp::TempFactory::NewTemp()),
      r14(temp::TempFactory::NewTemp()), r15(temp::TempFactory::NewTemp()),
      shadow_fp(temp::TempFactory::NewTemp()) {
  temp_map_->Enter(rax, new std::string{"%rax"});
  temp_map_->Enter(rcx, new std::string{"%rcx"});
  temp_map_->Enter(rbx, new std::string{"%rbx"});
  temp_map_->Enter(rdx, new std::string{"%rdx"});
  temp_map_->Enter(rsi, new std::string{"%rsi"});
  temp_map_->Enter(rdi, new std::string{"%rdi"});
  temp_map_->Enter(rbp, new std::string{"%rbp"});
  temp_map_->Enter(rsp, new std::string{"%rsp"});
  temp_map_->Enter(r8, new std::string{"%r8"});
  temp_map_->Enter(r9, new std::string{"%r9"});
  temp_map_->Enter(r10, new std::string{"%r10"});
  temp_map_->Enter(r11, new std::string{"%r11"});
  temp_map_->Enter(r12, new std::string{"%r12"});
  temp_map_->Enter(r13, new std::string{"%r13"});
  temp_map_->Enter(r14, new std::string{"%r14"});
  temp_map_->Enter(r15, new std::string{"%r15"});
}
} // namespace frame