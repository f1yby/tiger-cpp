#include "tiger/semant/semant.h"
#include "tiger/absyn/absyn.h"
#include <set>

namespace absyn {

void AbsynTree::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                           err::ErrorMsg *errormsg) const {
  root_->SemAnalyze(venv, tenv, 0, errormsg);
}

type::Ty *SimpleVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *ty = nullptr;

  auto *t = venv->Look(sym_);
  if (t && typeid(*t) == typeid(env::VarEntry)) {
    ty = static_cast<env::VarEntry *>(t)->ty_;
  } else {
    errormsg->Error(pos_, "undefined variable " + sym_->Name());
    return nullptr;
  }

  return ty;
}

type::Ty *FieldVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *ty = nullptr;

  auto *t = var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (t == nullptr) {
    return nullptr;
  }
  if (typeid(*t) != typeid(type::RecordTy)) {
    errormsg->Error(pos_, "not a record type");
    return nullptr;
  }

  auto vt = static_cast<type::RecordTy *>(t);

  bool valid = false;
  std::for_each(vt->fields_->GetList().begin(), vt->fields_->GetList().end(),
                [&](type::Field *field) {
                  if (!valid) {
                    if (field->name_ == sym_) {
                      ty = field->ty_;
                      valid = true;
                    }
                  }
                });

  if (!valid) {
    errormsg->Error(pos_, "field " + sym_->Name() + " doesn't exist");
    return nullptr;
  }

  return ty;
}

type::Ty *SubscriptVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   int labelcount,
                                   err::ErrorMsg *errormsg) const {
  type::Ty *ty = nullptr;

  auto t = var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (t == nullptr) {
    return nullptr;
  }
  if (typeid(*t) == typeid(type::ArrayTy)) {
    ty = static_cast<type::ArrayTy *>(t)->ty_;
  } else {
    errormsg->Error(pos_, "array type required");
    return nullptr;
  }

  return ty;
}

type::Ty *VarExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {

  return var_->SemAnalyze(venv, tenv, labelcount, errormsg);
}

type::Ty *NilExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  return type::NilTy::Instance();
}

type::Ty *IntExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  return type::IntTy::Instance();
}

type::Ty *StringExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  return type::StringTy::Instance();
}

type::Ty *CallExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *ty = nullptr;

  auto *f = venv->Look(func_);

  if (f == nullptr || typeid(*f) != typeid(env::FunEntry)) {
    errormsg->Error(pos_, "undefined function " + func_->Name());
    return nullptr;
  }

  auto *fe = static_cast<env::FunEntry *>(f);

  auto ai = args_->GetList().begin();
  auto fi = fe->formals_->GetList().begin();

  while (ai != args_->GetList().end() && fi != fe->formals_->GetList().end()) {
    auto et = (*ai)->SemAnalyze(venv, tenv, labelcount, errormsg);
    if (et && !(*fi)->IsSameType(et)) {
      errormsg->Error(pos_, "para type mismatch");
    }
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

  return ty;
}

type::Ty *OpExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  // Unary
  if (left_ == nullptr && oper_ == MINUS_OP) {
    auto right = right_->SemAnalyze(venv, tenv, labelcount, errormsg);
    if (right && !right->IsSameType(type::IntTy::Instance())) {
      errormsg->Error(pos_, "integer required");
    }
    return type::IntTy::Instance();
  }

  auto *left = left_->SemAnalyze(venv, tenv, labelcount, errormsg);
  auto *right = right_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (left == nullptr || right == nullptr) {
    return type::IntTy::Instance();
  }

  switch (oper_) {
  case AND_OP:
  case OR_OP:
  case PLUS_OP:
  case MINUS_OP:
  case TIMES_OP:
  case DIVIDE_OP:
    if (!left->IsSameType(type::IntTy::Instance())) {
      errormsg->Error(pos_, "integer required");
    }
    if (!right->IsSameType(type::IntTy::Instance())) {
      errormsg->Error(pos_, "integer required");
    }
    return type::IntTy::Instance();
  // Compare
  case EQ_OP:
  case NEQ_OP:
  case LT_OP:
  case LE_OP:
  case GT_OP:
  case GE_OP:
    if (!left->IsSameType(right)) {
      errormsg->Error(pos_, "same type required");
      return type::IntTy::Instance();
    }
    if (!left->IsSameType(type::IntTy::Instance()) &&
        !left->IsSameType(type::StringTy::Instance()) &&
        !left->IsSameType(type::NilTy::Instance())) {
      errormsg->Error(pos_, "should be int or string");
    }
    return type::IntTy::Instance();
  default:
    errormsg->Error(pos_, "Invalid Oper");
  }

  return type::IntTy::Instance();
}

type::Ty *RecordExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *ty = tenv->Look(typ_);
  if (ty == nullptr) {
    errormsg->Error(pos_, "undefined type " + typ_->Name());
    return nullptr;
  }

  if (typeid(*ty) != typeid(type::RecordTy)) {
    errormsg->Error(pos_, "should be record type");
    return nullptr;
  }

  auto *rt = static_cast<type::RecordTy *>(ty);
  auto fl = rt->fields_->GetList();
  auto efl = fields_->GetList();
  if (fl.size() != efl.size()) {
    errormsg->Error(pos_, "should be same field size");
    return ty;
  }
  auto ifl = fl.begin();
  auto iefl = efl.begin();
  while (ifl != fl.end()) {
    auto ieflt = (*iefl)->exp_->SemAnalyze(venv, tenv, labelcount, errormsg);
    if (!ieflt->IsSameType((*ifl)->ty_) || (*ifl)->name_ != (*iefl)->name_) {
      errormsg->Error(pos_, "fields should match");
      return ty;
    }
    ++ifl;
    ++iefl;
  }

  return ty;
}

type::Ty *SeqExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *ty = type::VoidTy::Instance();

  std::for_each(seq_->GetList().begin(), seq_->GetList().end(), [&](Exp *exp) {
    ty = exp->SemAnalyze(venv, tenv, labelcount, errormsg);
  });

  return ty;
}

type::Ty *AssignExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  auto *vt = var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  auto *et = exp_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (vt == nullptr || et == nullptr) {
    return type::VoidTy::Instance();
  }

  if (typeid(*var_) == typeid(SimpleVar)) {
    auto sv = static_cast<SimpleVar *>(var_);
    auto sve = venv->Look(sv->sym_); // This shouldn't return nullptr
    if (sve->readonly_) {
      errormsg->Error(pos_, "loop variable can't be assigned");
      return type::VoidTy::Instance();
    }
  }

  if (!vt->IsSameType(et)) {
    errormsg->Error(pos_, "unmatched assign exp");
    return type::VoidTy::Instance();
  }

  return type::VoidTy::Instance();
}

type::Ty *IfExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  auto *test = test_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (test && !test->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(pos_, "test should be int");
  }

  auto *then = then_->SemAnalyze(venv, tenv, labelcount, errormsg);

  if (elsee_) {
    type::Ty *elsee = elsee_->SemAnalyze(venv, tenv, labelcount, errormsg);

    if (elsee == nullptr) {
      return then;
    }

    if (then == nullptr) {
      return elsee;
    }

    if (!then->IsSameType(elsee)) {
      errormsg->Error(pos_, "then exp and else exp type mismatch");
    }

    return then;
  } else {

    if (then && !then->IsSameType(type::VoidTy::Instance())) {
      errormsg->Error(pos_, "if-then exp's body must produce no value");
    }

    return type::VoidTy::Instance();
  }
}

type::Ty *WhileExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  auto *test = test_->SemAnalyze(venv, tenv, labelcount, errormsg);
  auto *body = body_->SemAnalyze(venv, tenv, labelcount + 1, errormsg);

  if (test && !test->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(pos_, "test result should be int");
  }
  if (body && !body->IsSameType(type::VoidTy::Instance())) {
    errormsg->Error(pos_, "while body must produce no value");
  }

  return type::VoidTy::Instance();
}

type::Ty *ForExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  auto *lo = lo_->SemAnalyze(venv, tenv, labelcount, errormsg);
  auto *hi = hi_->SemAnalyze(venv, tenv, labelcount, errormsg);

  if (lo && !lo->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(pos_, "for exp's range type is not integer");
  }

  if (hi && !hi->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(pos_, "for exp's range type is not integer");
  }

  venv->BeginScope();
  venv->Enter(var_, new env::VarEntry(type::IntTy::Instance(), true));
  auto *body = body_->SemAnalyze(venv, tenv, labelcount + 1, errormsg);
  venv->EndScope();

  if (body && !body->IsSameType(type::VoidTy::Instance())) {
    errormsg->Error(pos_, "for body must produce no value");
  }

  return type::VoidTy::Instance();
}

type::Ty *BreakExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  if (labelcount <= 0) {
    errormsg->Error(pos_, "break is not inside any loop");
  }

  return type::VoidTy::Instance();
}

type::Ty *LetExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  tenv->BeginScope();

  std::for_each(
      decs_->GetList().begin(), decs_->GetList().end(),
      [&](Dec *dec) { dec->SemAnalyze(venv, tenv, labelcount, errormsg); });
  type::Ty *ty = type::VoidTy::Instance();
  if (body_) {
    ty = body_->SemAnalyze(venv, tenv, labelcount, errormsg);
  }

  venv->EndScope();
  tenv->EndScope();

  return ty;
}

type::Ty *ArrayExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *ty = nullptr;

  ty = tenv->Look(typ_);
  if (ty == nullptr) {
    errormsg->Error(pos_, "undeclared array type");
    return nullptr;
  }
  if (typeid(*ty) != typeid(type::ArrayTy)) {
    errormsg->Error(pos_, "should be array type");
    return nullptr;
  }

  auto *et = init_->SemAnalyze(venv, tenv, labelcount, errormsg);
  auto at = static_cast<type::ArrayTy *>(ty);
  if (!at->ty_->IsSameType(et)) {
    errormsg->Error(pos_, "type mismatch");
  }

  return ty;
}

type::Ty *VoidExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  return type::VoidTy::Instance();
}

void FunctionDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  auto fset = std::set<std::string>();
  std::for_each(
      functions_->GetList().begin(), functions_->GetList().end(),
      [&](FunDec *fun_dec) {
        auto tl = fun_dec->params_->MakeFormalTyList(tenv, errormsg);
        auto rt = fun_dec->result_ == nullptr ? type::VoidTy::Instance()
                                              : tenv->Look(fun_dec->result_);
        if (rt == nullptr) {
          errormsg->Error(pos_, "undeclared type " + fun_dec->result_->Name());
        }
        if (fset.count(fun_dec->name_->Name()) != 0) {
          errormsg->Error(pos_, "two functions have the same name");
          return;
        }
        fset.insert(fun_dec->name_->Name());
        venv->Enter(fun_dec->name_, new env::FunEntry(tl, rt));
      });
  std::for_each(
      functions_->GetList().begin(), functions_->GetList().end(),
      [&](FunDec *fun_dec) {
        venv->BeginScope();
        std::for_each(fun_dec->params_->GetList().begin(),
                      fun_dec->params_->GetList().end(), [&](Field *field) {
                        venv->Enter(field->name_,
                                    new env::VarEntry(tenv->Look(field->typ_)));
                      });
        auto et = fun_dec->body_->SemAnalyze(venv, tenv, labelcount, errormsg);
        venv->EndScope();
        auto rt = fun_dec->result_ == nullptr ? type::VoidTy::Instance()
                                              : tenv->Look(fun_dec->result_);
        if (rt == nullptr) {
          errormsg->Error(pos_, "undeclared return type");
        } else {
          if (et != nullptr) {
            if (rt->IsSameType(type::VoidTy::Instance()) &&
                !et->IsSameType(rt)) {
              errormsg->Error(pos_, "procedure returns value");
            } else if (!et->IsSameType(rt)) {
              errormsg->Error(pos_, "unmatched return type");
            }
          }
        }
      });
}

void VarDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                        err::ErrorMsg *errormsg) const {
  type::Ty *ty = nullptr;
  auto *et = init_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!et) {
    return;
  }

  if (typ_) {
    ty = tenv->Look(typ_);
    if (ty == nullptr) {
      errormsg->Error(pos_, "type undefined");
      return;
    }
    if (!ty->IsSameType(et)) {
      errormsg->Error(pos_, "type mismatch");
      return;
    }
  } else {
    if (et == type::NilTy::Instance()) {
      errormsg->Error(pos_, "init should not be nil without type specified");
      return;
    }
    ty = et;
  }

  venv->Enter(var_, new env::VarEntry(ty));
}

void TypeDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                         err::ErrorMsg *errormsg) const {
  std::for_each(types_->GetList().begin(), types_->GetList().end(),
                [&](NameAndTy *nameAndTy) {
                  tenv->Enter(nameAndTy->name_, type::VoidTy::Instance());
                });

  bool valid = true;
  std::for_each(types_->GetList().begin(), types_->GetList().end(),
                [&](NameAndTy *nameAndTy) {
                  auto *ty = nameAndTy->ty_->SemAnalyze(tenv, errormsg);
                  auto old_ty = tenv->Look(nameAndTy->name_);
                  if (old_ty != type::VoidTy::Instance()) {
                    errormsg->Error(pos_, "two types have the same name");
                    valid = false;
                    return;
                  }
                  if (ty == nullptr) {
                    valid = false;
                    return;
                  }
                  tenv->Set(nameAndTy->name_, ty);
                });

  std::for_each(types_->GetList().begin(), types_->GetList().end(),
                [&](NameAndTy *nameAndTy) {
                  if (valid) {
                    auto *ty = tenv->Look(nameAndTy->name_);
                    if (ty == nullptr) {
                      valid = false;
                      return;
                    }
                    if (ty->IsSameType(type::VoidTy::Instance())) {
                      errormsg->Error(pos_, "illegal type cycle");
                      valid = false;
                    }
                  }
                });

  if (!valid) {
    return;
  }
  std::for_each(types_->GetList().begin(), types_->GetList().end(),
                [&](NameAndTy *nameAndTy) {
                  auto *ty = nameAndTy->ty_->SemAnalyze(tenv, errormsg);
                  auto old_ty = tenv->Look(nameAndTy->name_);
                  if (typeid(*old_ty) == typeid(type::RecordTy)) {
                    static_cast<type::RecordTy *>(old_ty)->fields_ =
                        static_cast<type::RecordTy *>(ty)->fields_;
                  }
                });
}

type::Ty *NameTy::SemAnalyze(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  auto *ty = tenv->Look(name_);

  if (ty == nullptr) {
    errormsg->Error(pos_, "unknown type name");
    return nullptr;
  }

  return ty;
}

type::Ty *RecordTy::SemAnalyze(env::TEnvPtr tenv,
                               err::ErrorMsg *errormsg) const {
  auto fields = new type::FieldList();

  bool valid = true;
  std::for_each(record_->GetList().begin(), record_->GetList().end(),
                [&](Field *field) {
                  if (valid) {
                    auto *ty = tenv->Look(field->typ_);
                    if (ty == nullptr) {
                      errormsg->Error(field->pos_,
                                      "undefined type " + field->typ_->Name());
                      valid = false;
                    } else {
                      fields->Append(new type::Field(field->name_, ty));
                    }
                  }
                });

  if (!valid) {
    return nullptr;
  }

  return new type::RecordTy(fields);
}

type::Ty *ArrayTy::SemAnalyze(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  auto ty = tenv->Look(array_);

  if (ty == nullptr) {
    errormsg->Error(pos_, "undeclared array type");
    return nullptr;
  }

  return new type::ArrayTy(ty);
}

} // namespace absyn

namespace sem {

void ProgSem::SemAnalyze() {
  FillBaseVEnv();
  FillBaseTEnv();
  absyn_tree_->SemAnalyze(venv_.get(), tenv_.get(), errormsg_.get());
}

} // namespace sem
