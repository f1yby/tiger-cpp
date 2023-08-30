//
// Created by wzl on 2021/10/12.
//

#ifndef TIGER_COMPILER_X64FRAME_H
#define TIGER_COMPILER_X64FRAME_H

#include "tiger/frame/frame.h"

extern frame::RegManager *reg_manager;

namespace frame {

class InFrameAccess : public Access {
public:
  int offset;

  explicit InFrameAccess(int offset) : offset(offset) {}

  tree::Exp *expr() override {
    return new tree::MemExp(new tree::BinopExp(
        tree::BinOp::PLUS_OP, new tree::TempExp(reg_manager->FramePointer()),
        new tree::ConstExp(offset)));
  }
};

class InRegAccess : public Access {
public:
  temp::Temp *reg;

  explicit InRegAccess(temp::Temp *reg) : reg(reg) {}

  tree::Exp *expr() override { return new tree::TempExp(reg); }
};
class X64RegManager : public RegManager {
public:
  X64RegManager();
  temp::Temp *rax;
  temp::Temp *rcx;
  temp::Temp *rbx;
  temp::Temp *rdx;
  temp::Temp *rbp;
  temp::Temp *rsp;
  temp::Temp *rdi;
  temp::Temp *rsi;
  temp::Temp *r8;
  temp::Temp *r9;
  temp::Temp *r10;
  temp::Temp *r11;
  temp::Temp *r12;
  temp::Temp *r13;
  temp::Temp *r14;
  temp::Temp *r15;

  temp::Temp *shadow_fp;

  temp::TempList *Registers() override;
  temp::TempList *ArgRegs() override;
  temp::TempList *CallerSaves() override;
  temp::TempList *CalleeSaves() override;
  temp::TempList *ReturnSink() override;
  int WordSize() override;
  temp::Temp *FramePointer() override;
  temp::Temp *StackPointer() override;
  temp::Temp *ReturnValue() override;
};

} // namespace frame
#endif // TIGER_COMPILER_X64FRAME_H
