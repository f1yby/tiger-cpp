#include "tiger/translate/translate.h"

#include <tiger/absyn/absyn.h>

#include <set>

#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/frame/x64frame.h"

extern frame::Frags *frags;

namespace tr {

Access *Access::AllocLocal(Level *level, bool escape) {
  if (!escape) {
    return new Access(level,
                      new frame::InRegAccess(temp::TempFactory::NewTemp()));
  } else {
    level->frame_->local_on_stack_++;
    auto *ret = new Access(
        level, new frame::InFrameAccess(-level->frame_->local_on_stack_ *
                                        reg_manager->WordSize()));
    return ret;
  }
}

class Cx {
public:
  PatchList trues_;
  PatchList falses_;
  tree::Stm *stm_;

  Cx(PatchList trues, PatchList falses, tree::Stm *stm)
      : trues_(trues), falses_(falses), stm_(stm) {}
};

class Exp {
public:
  [[nodiscard]] virtual tree::Exp *UnEx() = 0;
  [[nodiscard]] virtual tree::Stm *UnNx() = 0;
  [[nodiscard]] virtual Cx UnCx(err::ErrorMsg *errormsg) = 0;
};

class ExpAndTy {
public:
  tr::Exp *exp_;
  type::Ty *ty_;

  ExpAndTy(tr::Exp *exp, type::Ty *ty) : exp_(exp), ty_(ty) {}
};

class ExExp : public Exp {
public:
  tree::Exp *exp_;

  explicit ExExp(tree::Exp *exp) : exp_(exp) {}

  [[nodiscard]] tree::Exp *UnEx() override { return exp_; }
  [[nodiscard]] tree::Stm *UnNx() override { return new tree::ExpStm(exp_); }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    auto *c = new tree::CjumpStm(tree::RelOp::NE_OP, exp_,
                                 new tree::ConstExp(0), nullptr, nullptr);
    return {PatchList{{&(c->true_label_)}}, PatchList{{&(c->false_label_)}}, c};
  }
};

class NxExp : public Exp {
public:
  tree::Stm *stm_;

  explicit NxExp(tree::Stm *stm) : stm_(stm) {}

  [[nodiscard]] tree::Exp *UnEx() override {
    return new tree::EseqExp(stm_, new tree::ConstExp(0));
  }
  [[nodiscard]] tree::Stm *UnNx() override { return stm_; }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    return {{}, {}, stm_};
  }
};

class CxExp : public Exp {
public:
  Cx cx_;

  CxExp(PatchList trues, PatchList falses, tree::Stm *stm)
      : cx_(trues, falses, stm) {}

  [[nodiscard]] tree::Exp *UnEx() override {
    auto *t = temp::TempFactory::NewTemp();
    auto *lt = temp::LabelFactory::NewLabel();
    auto *lf = temp::LabelFactory::NewLabel();
    auto *le = temp::LabelFactory::NewLabel();
    cx_.trues_.DoPatch(lt);
    cx_.falses_.DoPatch(lf);
    return new tree::EseqExp(
        new tree::SeqStm(
            cx_.stm_,
            new tree::SeqStm(
                new tree::LabelStm(lt),
                new tree::SeqStm(
                    new tree::MoveStm(new tree::TempExp(t),
                                      new tree::ConstExp(1)),
                    new tree::SeqStm(
                        new tree::JumpStm(new tree::NameExp(le),
                                          new std::vector{le}),
                        new tree::SeqStm(
                            new tree::LabelStm(lf),
                            new tree::SeqStm(
                                new tree::MoveStm(new tree::TempExp(t),
                                                  new tree::ConstExp(0)),
                                new tree::LabelStm(le)))))

                    )),
        new tree::TempExp(t));
  }
  [[nodiscard]] tree::Stm *UnNx() override { return cx_.stm_; }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override { return cx_; }
};

void ProgTr::Translate() {
  FillBaseVEnv();
  FillBaseTEnv();
  frags->PushBack(new frame::ProcFrag(absyn_tree_
                                          ->Translate(venv_.get(), tenv_.get(),
                                                      main_level_.get(),
                                                      nullptr, errormsg_.get())
                                          ->exp_->UnNx(),
                                      main_level_->frame_));
}

ProgTr::ProgTr(std::unique_ptr<absyn::AbsynTree> absyn_tree,
               std::unique_ptr<err::ErrorMsg> errormsg)
    : absyn_tree_(std::move(absyn_tree)), errormsg_(std::move(errormsg)),
      tenv_(std::make_unique<env::TEnv>()),
      venv_(std::make_unique<env::VEnv>()),
      main_level_(std::make_unique<Level>(
          new frame::Frame(sym::Symbol::UniqueSymbol("tigermain"),
                           sym::Symbol::UniqueSymbol("main")),
          nullptr)) {}

} // namespace tr

namespace absyn {

tr::ExpAndTy *AbsynTree::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  return root_->Translate(venv, tenv, level, label, errormsg);
}

tr::ExpAndTy *SimpleVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  auto *e = venv->Look(sym_);
  if (e == nullptr || typeid(*e) != typeid(env::VarEntry)) {
    errormsg->Error(pos_, "undefined variable " + sym_->Name());
    abort();
  }

  auto *ve = static_cast<env::VarEntry *>(e);
  auto *ty = ve->ty_;
  if (typeid(*(ve->access_->access_)) == typeid(frame::InRegAccess)) {
    return new tr::ExpAndTy(new tr::ExExp(ve->access_->access_->expr()), ty);
  } else if (typeid(*(ve->access_->access_)) == typeid(frame::InFrameAccess)) {
    auto fe = dynamic_cast<frame::InFrameAccess *>(ve->access_->access_);
    auto *l = ve->access_->level_;
    auto *li = level;

    // got frame pointer
    tree::Exp *exp = new tree::TempExp(reg_manager->FramePointer());
    while (li != l) {
      li = li->parent_;
      // static link points to its parent's frame pointer
      exp = new tree::MemExp(
          new tree::BinopExp(tree::BinOp::MINUS_OP, exp,
                             new tree::ConstExp(reg_manager->WordSize())));
    }

    exp = new tree::MemExp(new tree::BinopExp(tree::BinOp::PLUS_OP, exp,
                                              new tree::ConstExp(fe->offset)));
    return new tr::ExpAndTy(new tr::ExExp(exp), ty);
  }
  assert(false); // Unimplemented
}

tr::ExpAndTy *FieldVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  auto *e = var_->Translate(venv, tenv, level, label, errormsg);
  if (e == nullptr) {
    abort();
  }
  if (typeid(*e->ty_) != typeid(type::RecordTy)) {
    errormsg->Error(pos_, "not a record type");
    abort();
  }

  type::Ty *ty = nullptr;
  int32_t offset = 0;

  auto vt = static_cast<type::RecordTy *>(e->ty_);
  bool find = false;
  for (auto *field : vt->fields_->GetList()) {
    if (field->name_ == sym_) {
      ty = field->ty_;
      break;
    }
    offset += reg_manager->WordSize();
  }

  if (ty == nullptr) {
    errormsg->Error(pos_, "field " + sym_->Name() + " doesn't exist");
    abort();
  }
  return new tr::ExpAndTy(
      new tr::ExExp(new tree::MemExp(new tree::BinopExp(
          tree::BinOp::PLUS_OP, e->exp_->UnEx(), new tree::ConstExp(offset)))),
      ty);
}

tr::ExpAndTy *SubscriptVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                      tr::Level *level, temp::Label *label,
                                      err::ErrorMsg *errormsg) const {
  auto *et = var_->Translate(venv, tenv, level, label, errormsg);
  if (et == nullptr) {
    abort();
  }

  auto set = subscript_->Translate(venv, tenv, level, label, errormsg);
  if (set == nullptr) {
    abort();
  }

  if (!set->ty_->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(pos_, "int type required");
    abort();
  }
  type::Ty *ty = nullptr;
  if (typeid(*et->ty_) == typeid(type::ArrayTy)) {
    ty = static_cast<type::ArrayTy *>(et->ty_)->ty_;
  } else {
    errormsg->Error(pos_, "array type required");
    abort();
  }

  auto t = temp::TempFactory::NewTemp();

  return new tr::ExpAndTy(
      new tr::ExExp(new tree::MemExp(new tree::BinopExp(
          tree::BinOp::PLUS_OP, et->exp_->UnEx(),
          new tree::BinopExp(tree::BinOp::MUL_OP, set->exp_->UnEx(),
                             new tree::ConstExp(reg_manager->WordSize()))))),
      ty);
}

tr::ExpAndTy *VarExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  return var_->Translate(venv, tenv, level, label, errormsg);
}

tr::ExpAndTy *NilExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(0)),
                          type::NilTy::Instance());
}

tr::ExpAndTy *IntExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(val_)),
                          type::IntTy::Instance());
}

tr::ExpAndTy *StringExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  auto *l = temp::LabelFactory::NewLabel();

  frags->PushBack(new frame::StringFrag(l, str_));

  return new tr::ExpAndTy(new tr::ExExp(new tree::NameExp(l)),
                          type::StringTy::Instance());
}

tr::ExpAndTy *CallExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *label,
                                 err::ErrorMsg *errormsg) const {
  type::Ty *ty = nullptr;

  auto *f = venv->Look(func_);

  if (f == nullptr || typeid(*f) != typeid(env::FunEntry)) {
    errormsg->Error(pos_, "undefined function " + func_->Name());
    abort();
  }

  auto *fe = static_cast<env::FunEntry *>(f);
  auto *args = new tree::ExpList();

  auto ai = args_->GetList().begin();
  auto fi = fe->formals_->GetList().begin();

  while (ai != args_->GetList().end() && fi != fe->formals_->GetList().end()) {
    auto et = (*ai)->Translate(venv, tenv, level, label, errormsg);
    if (et == nullptr) {
      abort();
    }
    if (!(*fi)->IsSameType(et->ty_)) {
      errormsg->Error(pos_, "para type mismatch");
      abort();
    }
    if ((*fi))
      args->Append(et->exp_->UnEx());
    ++ai;
    ++fi;
  }

  if (ai != args_->GetList().end()) {
    errormsg->Error(pos_, "too many params in function " + func_->Name());
  }
  if (fi != fe->formals_->GetList().end()) {
    errormsg->Error(pos_, "too  params in function " + func_->Name());
  }

  ty = fe->result_;

  // External Function has no parent label
  if (fe->label_ == nullptr) {
    return new tr::ExpAndTy(new tr::ExExp(frame::externalCall(func_, args)),
                            ty);
  } else {
    tree::Exp *sl = new tree::TempExp(reg_manager->FramePointer());
    auto il = level;
    auto *t = temp::TempFactory::NewTemp();
    while (il != fe->level_->parent_) {
      sl = new tree::MemExp(
          new tree::BinopExp(tree::BinOp::MINUS_OP, sl,
                             new tree::ConstExp(reg_manager->WordSize())));
      il = il->parent_;
    }

    args->Insert(sl);
    return new tr::ExpAndTy(
        new tr::ExExp(new tree::CallExp(new tree::NameExp(fe->label_), args)),
        ty);
  }
}

tr::ExpAndTy *OpExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *label,
                               err::ErrorMsg *errormsg) const {
  // Unary
  if (left_ == nullptr && oper_ == MINUS_OP) {
    auto ret = right_->Translate(venv, tenv, level, label, errormsg);
    if (ret == nullptr) {
      abort();
    }
    if (!ret->ty_->IsSameType(type::IntTy::Instance())) {
      errormsg->Error(pos_, "integer required");
      abort();
    }
    return new tr::ExpAndTy(
        new tr::ExExp(new tree::BinopExp(
            tree::BinOp::MINUS_OP, new tree::ConstExp(0), ret->exp_->UnEx())),
        type::IntTy::Instance());
  }

  auto *left = left_->Translate(venv, tenv, level, label, errormsg);
  auto *right = right_->Translate(venv, tenv, level, label, errormsg);
  if (left == nullptr || right == nullptr) {
    abort();
  }

  switch (oper_) {
  case AND_OP:
  case OR_OP:
  case PLUS_OP:
  case MINUS_OP:
  case TIMES_OP:
  case DIVIDE_OP:
    if (!left->ty_->IsSameType(type::IntTy::Instance())) {
      errormsg->Error(pos_, "integer required");
      abort();
    }
    if (!right->ty_->IsSameType(type::IntTy::Instance())) {
      errormsg->Error(pos_, "integer required");
      abort();
    }
    break;
  // Compare
  case EQ_OP:
  case NEQ_OP:
  case LT_OP:
  case LE_OP:
  case GT_OP:
  case GE_OP:
    if (!left->ty_->IsSameType(right->ty_)) {
      errormsg->Error(pos_, "same type required");
      abort();
    }
    if (!left->ty_->IsSameType(type::IntTy::Instance()) &&
        !left->ty_->IsSameType(type::StringTy::Instance()) &&
        !left->ty_->IsSameType(type::NilTy::Instance())) {
      errormsg->Error(pos_, "should be int or string");
      abort();
    }
    break;
  default:
    errormsg->Error(pos_, "Invalid Oper");
    abort();
  }

  if (oper_ == Oper::PLUS_OP) {
    return new tr::ExpAndTy(
        new tr::ExExp(new tree::BinopExp(
            tree::BinOp::PLUS_OP, left->exp_->UnEx(), right->exp_->UnEx())),
        type::IntTy::Instance());
  }

  if (oper_ == Oper::MINUS_OP) {
    return new tr::ExpAndTy(
        new tr::ExExp(new tree::BinopExp(
            tree::BinOp::MINUS_OP, left->exp_->UnEx(), right->exp_->UnEx())),
        type::IntTy::Instance());
  }

  if (oper_ == Oper::TIMES_OP) {
    return new tr::ExpAndTy(
        new tr::ExExp(new tree::BinopExp(
            tree::BinOp::MUL_OP, left->exp_->UnEx(), right->exp_->UnEx())),
        type::IntTy::Instance());
  }

  if (oper_ == Oper::DIVIDE_OP) {
    return new tr::ExpAndTy(
        new tr::ExExp(new tree::BinopExp(
            tree::BinOp::DIV_OP, left->exp_->UnEx(), right->exp_->UnEx())),
        type::IntTy::Instance());
  }

  if (oper_ == Oper::AND_OP) {
    auto *llt = temp::LabelFactory::NewLabel();
    auto cl = left->exp_->UnCx(errormsg);
    cl.trues_.DoPatch(llt);
    auto cr = right->exp_->UnCx(errormsg);
    return new tr::ExpAndTy(
        new tr::CxExp(cr.trues_,
                      tr::PatchList::JoinPatch(cl.falses_, cr.falses_),
                      new tree::SeqStm(left->exp_->UnNx(),
                                       new tree::SeqStm(new tree::LabelStm(llt),
                                                        right->exp_->UnNx()))),
        type::IntTy::Instance());
  }

  if (oper_ == Oper::OR_OP) {
    auto *llt = temp::LabelFactory::NewLabel();
    auto cl = left->exp_->UnCx(errormsg);
    cl.falses_.DoPatch(llt);
    auto cr = right->exp_->UnCx(errormsg);

    return new tr::ExpAndTy(
        new tr::CxExp(tr::PatchList::JoinPatch(cl.trues_, cr.trues_),
                      cr.falses_,
                      new tree::SeqStm(left->exp_->UnNx(),
                                       new tree::SeqStm(new tree::LabelStm(llt),
                                                        right->exp_->UnNx()))),
        type::IntTy::Instance());
  }

  if (left->ty_->IsSameType(type::IntTy::Instance()) ||
      left->ty_->IsSameType(type::NilTy::Instance())) {
    if (oper_ == Oper::EQ_OP) {
      auto *c = new tree::CjumpStm(tree::RelOp::EQ_OP, left->exp_->UnEx(),
                                   right->exp_->UnEx(), nullptr, nullptr);
      return new tr::ExpAndTy(new tr::CxExp(tr::PatchList{{&(c->true_label_)}},
                                            tr::PatchList{{&(c->false_label_)}},
                                            c),
                              type::IntTy::Instance());
    }

    if (oper_ == Oper::NEQ_OP) {
      auto *c = new tree::CjumpStm(tree::RelOp::NE_OP, left->exp_->UnEx(),
                                   right->exp_->UnEx(), nullptr, nullptr);
      return new tr::ExpAndTy(new tr::CxExp(tr::PatchList{{&(c->true_label_)}},
                                            tr::PatchList{{&(c->false_label_)}},
                                            c),
                              type::IntTy::Instance());
    }

    if (oper_ == Oper::LT_OP) {
      auto *c = new tree::CjumpStm(tree::RelOp::LT_OP, left->exp_->UnEx(),
                                   right->exp_->UnEx(), nullptr, nullptr);
      return new tr::ExpAndTy(new tr::CxExp(tr::PatchList{{&(c->true_label_)}},
                                            tr::PatchList{{&(c->false_label_)}},
                                            c),
                              type::IntTy::Instance());
    }

    if (oper_ == Oper::LE_OP) {
      auto *lt = temp::LabelFactory::NewLabel();
      auto *lf = temp::LabelFactory::NewLabel();
      auto *c = new tree::CjumpStm(tree::RelOp::LE_OP, left->exp_->UnEx(),
                                   right->exp_->UnEx(), lt, lf);
      return new tr::ExpAndTy(new tr::CxExp(tr::PatchList{{&(c->true_label_)}},
                                            tr::PatchList{{&(c->false_label_)}},
                                            c),
                              type::IntTy::Instance());
    }

    if (oper_ == Oper::GT_OP) {
      auto *c = new tree::CjumpStm(tree::RelOp::GT_OP, left->exp_->UnEx(),
                                   right->exp_->UnEx(), nullptr, nullptr);
      return new tr::ExpAndTy(new tr::CxExp(tr::PatchList{{&(c->true_label_)}},
                                            tr::PatchList{{&(c->false_label_)}},
                                            c),
                              type::IntTy::Instance());
    }

    if (oper_ == Oper::GE_OP) {
      auto *c = new tree::CjumpStm(tree::RelOp::GE_OP, left->exp_->UnEx(),
                                   right->exp_->UnEx(), nullptr, nullptr);
      return new tr::ExpAndTy(new tr::CxExp(tr::PatchList{{&(c->true_label_)}},
                                            tr::PatchList{{&(c->false_label_)}},
                                            c),
                              type::IntTy::Instance());
    }
  }

  if (left->ty_->IsSameType(type::StringTy::Instance())) {
    if (oper_ == Oper::EQ_OP) {
      auto *c = new tree::CjumpStm(
          tree::RelOp::EQ_OP,
          frame::externalCall(
              temp::Label::UniqueSymbol("string_equal"),
              new tree::ExpList{left->exp_->UnEx(), right->exp_->UnEx()}),
          new tree::ConstExp(1), nullptr, nullptr);
      return new tr::ExpAndTy(new tr::CxExp(tr::PatchList{{&(c->true_label_)}},
                                            tr::PatchList{{&(c->false_label_)}},
                                            c),
                              type::IntTy::Instance());
    }

    if (oper_ == Oper::NEQ_OP) {
      auto *c = new tree::CjumpStm(
          tree::RelOp::NE_OP,
          frame::externalCall(
              temp::Label::UniqueSymbol("string_equal"),
              new tree::ExpList{left->exp_->UnEx(), right->exp_->UnEx()}),
          new tree::ConstExp(1), nullptr, nullptr);
      return new tr::ExpAndTy(new tr::CxExp(tr::PatchList{{&(c->true_label_)}},
                                            tr::PatchList{{&(c->false_label_)}},
                                            c),
                              type::IntTy::Instance());
    }
  }
  abort();
}

tr::ExpAndTy *RecordExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  type::Ty *ty = tenv->Look(typ_);
  if (ty == nullptr) {
    errormsg->Error(pos_, "undefined type " + typ_->Name());
    abort();
  }

  if (typeid(*ty) != typeid(type::RecordTy)) {
    errormsg->Error(pos_, "should be record type");
    abort();
  }
  auto *sp = frame::externalCall(
      temp::Label::UniqueSymbol("alloc_record"),
      new tree::ExpList{new tree::ConstExp(reg_manager->WordSize() *
                                           fields_->GetList().size())});

  auto *rt = static_cast<type::RecordTy *>(ty);
  auto fl = rt->fields_->GetList();
  auto efl = fields_->GetList();
  if (fl.size() != efl.size()) {
    errormsg->Error(pos_, "should be same field size");
    abort();
  }
  auto ifl = fl.begin();
  auto iefl = efl.begin();
  int offset = 0;

  auto *t = temp::TempFactory::NewTemp();
  tree::Stm *stmt = new tree::MoveStm(new tree::TempExp(t), sp);
  while (ifl != fl.end()) {
    auto ieflt = (*iefl)->exp_->Translate(venv, tenv, level, label, errormsg);
    if (ieflt == nullptr) {
      abort();
    }
    if (!ieflt->ty_->IsSameType((*ifl)->ty_) ||
        (*ifl)->name_ != (*iefl)->name_) {
      errormsg->Error(pos_, "fields should match");
      abort();
    }

    stmt = new tree::SeqStm(
        stmt, new tree::MoveStm(new tree::MemExp(new tree::BinopExp(
                                    tree::BinOp::PLUS_OP, new tree::TempExp(t),
                                    new tree::ConstExp(offset))),
                                ieflt->exp_->UnEx()));

    ++ifl;
    ++iefl;
    offset += reg_manager->WordSize();
  }

  return new tr::ExpAndTy(
      new tr::ExExp(new tree::EseqExp(stmt, new tree::TempExp(t))), ty);
}

tr::ExpAndTy *SeqExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  type::Ty *ty = type::VoidTy::Instance();

  tree::Exp *expr = nullptr;
  tree::Stm *stmt = new tree::ExpStm(new tree::ConstExp(0));
  for (auto *exp : seq_->GetList()) {
    auto tet = exp->Translate(venv, tenv, level, label, errormsg);
    if (!tet) {
      abort();
    }
    if (expr) {
      stmt = new tree::SeqStm(stmt, new tree::ExpStm(expr));
    }
    ty = tet->ty_;
    expr = tet->exp_->UnEx();
  }

  if (ty == type::VoidTy::Instance()) {
    return new tr::ExpAndTy(
        new tr::NxExp(new tree::SeqStm(stmt, new tree::ExpStm(expr))), ty);
  } else {
    return new tr::ExpAndTy(new tr::ExExp(new tree::EseqExp(stmt, expr)), ty);
  }
}

tr::ExpAndTy *AssignExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  auto *vet = var_->Translate(venv, tenv, level, label, errormsg);
  auto *eet = exp_->Translate(venv, tenv, level, label, errormsg);

  if (vet == nullptr || eet == nullptr) {
    abort();
  }

  if (typeid(*var_) == typeid(SimpleVar)) {
    auto sv = static_cast<SimpleVar *>(var_);
    auto sve = venv->Look(sv->sym_); // This shouldn't abort()
    if (sve->readonly_) {
      errormsg->Error(pos_, "loop variable can't be assigned");
      abort();
    }
  }

  if (!vet->ty_->IsSameType(eet->ty_)) {
    errormsg->Error(pos_, "unmatched assign exp");
    abort();
  }

  return new tr::ExpAndTy(
      new tr::NxExp(new tree::MoveStm(vet->exp_->UnEx(), eet->exp_->UnEx())),
      type::VoidTy::Instance());
}

tr::ExpAndTy *IfExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *label,
                               err::ErrorMsg *errormsg) const {
  auto *true_label = temp::LabelFactory::NewLabel();
  auto *false_label = temp::LabelFactory::NewLabel();

  auto *test = test_->Translate(venv, tenv, level, label, errormsg);
  auto *then = then_->Translate(venv, tenv, level, label, errormsg);
  if (test == nullptr || then == nullptr) {
    abort();
  }
  auto c = test->exp_->UnCx(errormsg);
  c.trues_.DoPatch(true_label);
  c.falses_.DoPatch(false_label);

  if (elsee_) {
    auto *elsee = elsee_->Translate(venv, tenv, level, label, errormsg);
    if (elsee == nullptr) {
      abort();
    }
    if (!then->ty_->IsSameType(elsee->ty_)) {
      errormsg->Error(pos_, "then exp and else exp type mismatch");
      abort();
    }
    auto *ld = temp::LabelFactory::NewLabel();
    if (then->ty_ == type::VoidTy::Instance()) {
      return new tr::ExpAndTy(
          new tr::NxExp(new tree::SeqStm(
              c.stm_,
              new tree::SeqStm(
                  new tree::LabelStm(true_label),
                  new tree::SeqStm(
                      then->exp_->UnNx(),
                      new tree::SeqStm(
                          new tree::JumpStm(new tree::NameExp(ld),
                                            new std::vector{ld}),
                          new tree::SeqStm(
                              new tree::LabelStm(false_label),
                              new tree::SeqStm(elsee->exp_->UnNx(),
                                               new tree::LabelStm(ld)))))))),
          type::VoidTy::Instance());
    } else {
      auto *t = temp::TempFactory::NewTemp();
      return new tr::ExpAndTy(
          new tr::ExExp(new tree::EseqExp(
              new tree::SeqStm(
                  c.stm_,
                  new tree::SeqStm(
                      new tree::LabelStm(true_label),
                      new tree::SeqStm(
                          new tree::MoveStm(new tree::TempExp(t),
                                            then->exp_->UnEx()),
                          new tree::SeqStm(
                              new tree::JumpStm(new tree::NameExp(ld),
                                                new std::vector{ld}),
                              new tree::SeqStm(
                                  new tree::LabelStm(false_label),
                                  new tree::SeqStm(
                                      new tree::MoveStm(new tree::TempExp(t),
                                                        elsee->exp_->UnEx()),
                                      new tree::LabelStm(ld))))))),
              new tree::TempExp(t))),
          then->ty_);
    }
  } else {
    return new tr::ExpAndTy(
        new tr::NxExp(new tree::SeqStm(
            c.stm_, new tree::SeqStm(
                        new tree::LabelStm(true_label),
                        new tree::SeqStm(then->exp_->UnNx(),
                                         new tree::LabelStm(false_label))))),
        type::VoidTy::Instance());
  }
}

tr::ExpAndTy *WhileExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  auto *lb = temp::LabelFactory::NewLabel();
  auto *le = temp::LabelFactory::NewLabel();
  auto *lt = temp::LabelFactory::NewLabel();

  auto bet = body_->Translate(venv, tenv, level, le, errormsg);
  auto tet = test_->Translate(venv, tenv, level, le, errormsg);
  if (bet == nullptr || tet == nullptr) {
    abort();
  }
  if (!tet->ty_->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(pos_, "test result should be int");
    abort();
  }
  if (!bet->ty_->IsSameType(type::VoidTy::Instance())) {
    errormsg->Error(pos_, "while body must produce no value");
    abort();
  }

  auto c = tet->exp_->UnCx(errormsg);
  c.trues_.DoPatch(lb);
  c.falses_.DoPatch(le);

  return new tr::ExpAndTy(
      new tr::NxExp(new tree::SeqStm(
          new tree::JumpStm(new tree::NameExp(lt), new std::vector{lt}),
          new tree::SeqStm(
              new tree::LabelStm(lb),
              new tree::SeqStm(
                  bet->exp_->UnNx(),
                  new tree::SeqStm(
                      new tree::LabelStm(lt),
                      new tree::SeqStm(c.stm_, new tree::LabelStm(le))))))),
      type::VoidTy::Instance());
}

tr::ExpAndTy *ForExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  auto *lb = temp::LabelFactory::NewLabel();
  auto *le = temp::LabelFactory::NewLabel();
  auto *la = temp::LabelFactory::NewLabel();
  auto *th = temp::TempFactory::NewTemp();
  auto *lo = lo_->Translate(venv, tenv, level, label, errormsg);
  auto *hi = hi_->Translate(venv, tenv, level, label, errormsg);

  if (lo == nullptr || hi == nullptr) {
    abort();
  }

  if (!lo->ty_->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(pos_, "for exp's range type is not integer");
    abort();
  }

  if (!hi->ty_->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(pos_, "for exp's range type is not integer");
    abort();
  }

  auto *access = tr::Access::AllocLocal(level, escape_);
  venv->BeginScope();
  venv->Enter(var_, new env::VarEntry(access, type::IntTy::Instance(), true));
  auto *body = body_->Translate(venv, tenv, level, le, errormsg);
  if (body == nullptr) {
    abort();
  }
  if (!body->ty_->IsSameType(type::VoidTy::Instance())) {
    errormsg->Error(pos_, "for body must produce no value");
    abort();
  }
  venv->EndScope();
  return new tr::ExpAndTy(
      new tr::NxExp(new tree::SeqStm(
          new tree::MoveStm(access->access_->expr(), lo->exp_->UnEx()),
          new tree::SeqStm(
              new tree::MoveStm(new tree::TempExp(th), hi->exp_->UnEx()),
              new tree::SeqStm(
                  new tree::CjumpStm(tree::RelOp::GT_OP,
                                     access->access_->expr(),
                                     new tree::TempExp(th), le, lb),
                  new tree::SeqStm(
                      new tree::LabelStm(lb),
                      new tree::SeqStm(
                          body->exp_->UnNx(),
                          new tree::SeqStm(
                              new tree::CjumpStm(tree::RelOp::GE_OP,
                                                 access->access_->expr(),
                                                 new tree::TempExp(th), le, la),
                              new tree::SeqStm(
                                  new tree::LabelStm(la),
                                  new tree::SeqStm(
                                      new tree::MoveStm(
                                          access->access_->expr(),
                                          new tree::BinopExp(
                                              tree::BinOp::PLUS_OP,
                                              access->access_->expr(),
                                              new tree::ConstExp(1))),
                                      new tree::SeqStm(
                                          new tree::JumpStm(
                                              new tree::NameExp(lb),
                                              new std::vector{lb}),
                                          new tree::LabelStm(le))))))))))),
      type::VoidTy::Instance());
}

tr::ExpAndTy *BreakExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  if (label == nullptr) {
    errormsg->Error(pos_, "break is not inside any loop");
    abort();
  }

  return new tr::ExpAndTy(
      new tr::NxExp(
          new tree::JumpStm(new tree::NameExp(label), new std::vector{label})),
      type::VoidTy::Instance());
}

tr::ExpAndTy *LetExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  tenv->BeginScope();

  tree::Stm *let = new tree::ExpStm(new tree::ConstExp(0));

  for (auto *dec : decs_->GetList()) {
    auto *det = dec->Translate(venv, tenv, level, label, errormsg);
    if (det == nullptr) {
      abort();
    }

    let = new tree::SeqStm(let, det->UnNx());
  }

  if (body_) {
    auto *bet = body_->Translate(venv, tenv, level, label, errormsg);

    venv->EndScope();
    tenv->EndScope();

    if (bet == nullptr) {
      abort();
    }

    return new tr::ExpAndTy(
        new ::tr::ExExp(new tree::EseqExp(let, bet->exp_->UnEx())), bet->ty_);

  } else {
    return new tr::ExpAndTy(new tr::NxExp(let), type::VoidTy::Instance());
    venv->EndScope();
    tenv->EndScope();
  }
}

tr::ExpAndTy *ArrayExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  type::Ty *ty = tenv->Look(typ_);

  if (ty == nullptr) {
    errormsg->Error(pos_, "undeclared array type");
    abort();
  }
  if (typeid(*ty) != typeid(type::ArrayTy)) {
    errormsg->Error(pos_, "should be array type");
    abort();
  }

  auto *set = size_->Translate(venv, tenv, level, label, errormsg);
  if (set == nullptr) {
    abort();
  }
  if (!set->ty_->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(pos_, "size should be int");
    abort();
  }

  auto *iet = init_->Translate(venv, tenv, level, label, errormsg);
  if (iet == nullptr) {
    abort();
  }

  auto at = static_cast<type::ArrayTy *>(ty);
  if (!at->ty_->IsSameType(iet->ty_)) {
    errormsg->Error(pos_, "type mismatch");
    abort();
  }

  return new tr::ExpAndTy(
      new tr::ExExp(frame::externalCall(
          temp::Label::UniqueSymbol("init_array"),
          new tree::ExpList{set->exp_->UnEx(), iet->exp_->UnEx()})),
      ty);
}
tr::ExpAndTy *VoidExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *label,
                                 err::ErrorMsg *errormsg) const {
  return new tr::ExpAndTy(
      new tr::NxExp(new tree::ExpStm(new tree::ConstExp(0))),
      type::VoidTy::Instance());
}

tr::Exp *FunctionDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  auto fset = std::set<std::string>();

  for (auto *fun_dec : functions_->GetList()) {
    auto tl = fun_dec->params_->MakeFormalTyList(tenv, errormsg);
    auto rt = fun_dec->result_ == nullptr ? type::VoidTy::Instance()
                                          : tenv->Look(fun_dec->result_);
    if (rt == nullptr) {
      errormsg->Error(pos_, "undeclared type " + fun_dec->result_->Name());
      abort();
    }
    if (fset.count(fun_dec->name_->Name()) != 0) {
      errormsg->Error(pos_, "two functions have the same name");
      abort();
    }
    fset.insert(fun_dec->name_->Name());
    auto *lf = temp::LabelFactory::NewLabel();
    auto *l = new tr::Level(new frame::Frame(lf, fun_dec->name_), level);
    venv->Enter(fun_dec->name_, new env::FunEntry(l, lf, tl, rt));
  }

  for (auto *fun_dec : functions_->GetList()) {
    tree::Stm *stmt = new tree::ExpStm(new tree::ConstExp(0));
    venv->BeginScope();

    auto *fe = static_cast<env::FunEntry *>(venv->Look(fun_dec->name_));

    int i = 0;
    auto *sl = tr::Access::AllocLocal(fe->level_, true);
    stmt = new tree::SeqStm(
        stmt, new tree::MoveStm(
                  sl->access_->expr(),
                  new tree::TempExp(reg_manager->ArgRegs()->NthTemp(i))));
    ++i;
    for (auto *field : fun_dec->params_->GetList()) {
      auto *a = tr::Access::AllocLocal(fe->level_, field->escape_);
      if (i < reg_manager->ArgRegs()->GetList().size()) {
        stmt = new tree::SeqStm(
            stmt, new tree::MoveStm(
                      a->access_->expr(),
                      new tree::TempExp(reg_manager->ArgRegs()->NthTemp(i))));
      } else {
        stmt = new tree::SeqStm(
            stmt,
            new tree::MoveStm(
                a->access_->expr(),
                new tree::MemExp(new tree::BinopExp(
                    tree::BinOp::PLUS_OP,
                    new tree::TempExp(reg_manager->FramePointer()),
                    new tree::ConstExp(
                        reg_manager->WordSize() *
                        (i + 1 - reg_manager->ArgRegs()->GetList().size()))))));
      }
      venv->Enter(field->name_, new env::VarEntry(a, tenv->Look(field->typ_)));
      ++i;
    }

    auto *et =
        fun_dec->body_->Translate(venv, tenv, fe->level_, nullptr, errormsg);

    venv->EndScope();

    auto rt = fun_dec->result_ == nullptr ? type::VoidTy::Instance()
                                          : tenv->Look(fun_dec->result_);
    if (rt == nullptr) {
      errormsg->Error(pos_, "undeclared return type");
      abort();
    }

    if (et == nullptr) {
      abort();
    }

    if (rt->IsSameType(type::VoidTy::Instance()) && !et->ty_->IsSameType(rt)) {
      errormsg->Error(pos_, "procedure returns value");
      abort();
    }

    if (!et->ty_->IsSameType(rt)) {
      errormsg->Error(pos_, "unmatched return type");
      abort();
    }

    stmt = new tree::SeqStm(
        stmt, new tree::MoveStm(new tree::TempExp(reg_manager->ReturnValue()),
                                et->exp_->UnEx()));

    frags->PushBack(new frame::ProcFrag(
        frame::ProcEntryExit1(fe->level_->frame_, stmt), fe->level_->frame_));
  }

  return new tr::NxExp(new tree::ExpStm(new tree::ConstExp(0)));
}

tr::Exp *VarDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                           tr::Level *level, temp::Label *label,
                           err::ErrorMsg *errormsg) const {
  type::Ty *ty = nullptr;
  auto *et = init_->Translate(venv, tenv, level, label, errormsg);
  if (!et) {
    abort();
  }

  if (typ_) {
    ty = tenv->Look(typ_);
    if (ty == nullptr) {
      errormsg->Error(pos_, "type undefined");
      abort();
    }
    if (!ty->IsSameType(et->ty_)) {
      errormsg->Error(pos_, "type mismatch");
      abort();
    }
  } else {
    if (et->ty_ == type::NilTy::Instance()) {
      errormsg->Error(pos_, "init should not be nil without type specified");
      abort();
    }
    ty = et->ty_;
  }

  auto *access = tr::Access::AllocLocal(level, escape_);
  venv->Enter(var_, new env::VarEntry(access, ty));

  return new tr::NxExp(
      new tree::MoveStm(access->access_->expr(), et->exp_->UnEx()));
}

tr::Exp *TypeDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                            tr::Level *level, temp::Label *label,
                            err::ErrorMsg *errormsg) const {
  for (auto *nat : types_->GetList()) {
    tenv->Enter(nat->name_, type::VoidTy::Instance());
  }

  for (auto *nat : types_->GetList()) {
    auto *ty = nat->ty_->SemAnalyze(tenv, errormsg);
    auto old_ty = tenv->Look(nat->name_);
    if (old_ty != type::VoidTy::Instance()) {
      errormsg->Error(pos_, "two types have the same name");
      abort();
    }
    tenv->Set(nat->name_, ty);
  }

  for (auto *nat : types_->GetList()) {
    auto *ty = tenv->Look(nat->name_);
    if (ty && ty->IsSameType(type::VoidTy::Instance())) {
      errormsg->Error(pos_, "illegal type cycle");
      abort();
    }
  }

  for (auto *nat : types_->GetList()) {
    auto *ty = nat->ty_->SemAnalyze(tenv, errormsg);
    auto *old_ty = tenv->Look(nat->name_);
    if (typeid(*old_ty) == typeid(type::RecordTy)) {
      static_cast<type::RecordTy *>(old_ty)->fields_ =
          static_cast<type::RecordTy *>(ty)->fields_;
    }
  }

  return new tr::NxExp(new tree::ExpStm(new tree::ConstExp(0)));
}

type::Ty *NameTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  auto *ty = tenv->Look(name_);

  if (ty == nullptr) {
    errormsg->Error(pos_, "unknown type name");
    abort();
  }

  return ty;
}

type::Ty *RecordTy::Translate(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  auto fields = new type::FieldList();

  for (auto *field : record_->GetList()) {
    auto *ty = tenv->Look(field->typ_);
    if (ty == nullptr) {
      errormsg->Error(field->pos_, "undefined type " + field->typ_->Name());
      abort();
    }
    fields->Append(new type::Field(field->name_, ty));
  }

  return new type::RecordTy(fields);
}

type::Ty *ArrayTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  auto ty = tenv->Look(array_);

  if (ty == nullptr) {
    errormsg->Error(pos_, "undeclared array type");
    abort();
  }

  return new type::ArrayTy(ty);
}

} // namespace absyn
