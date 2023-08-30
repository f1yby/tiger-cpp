#include "tiger/escape/escape.h"
#include "tiger/absyn/absyn.h"

namespace esc {
void EscFinder::FindEscape() { absyn_tree_->Traverse(env_.get()); }
} // namespace esc

namespace absyn {

void AbsynTree::Traverse(esc::EscEnvPtr env) { root_->Traverse(env, 0); }

void SimpleVar::Traverse(esc::EscEnvPtr env, int depth) {
  auto *v = env->Look(sym_);
  if (v->depth_ < depth) {
    *(v->escape_) = true;
  }
}

void FieldVar::Traverse(esc::EscEnvPtr env, int depth) {
  var_->Traverse(env, depth);
}

void SubscriptVar::Traverse(esc::EscEnvPtr env, int depth) {
  subscript_->Traverse(env, depth);
  var_->Traverse(env, depth);
}

void VarExp::Traverse(esc::EscEnvPtr env, int depth) {
  var_->Traverse(env, depth);
}

void NilExp::Traverse(esc::EscEnvPtr env, int depth) {}

void IntExp::Traverse(esc::EscEnvPtr env, int depth) {}

void StringExp::Traverse(esc::EscEnvPtr env, int depth) {}

void CallExp::Traverse(esc::EscEnvPtr env, int depth) {
  for (auto *expr : args_->GetList()) {
    expr->Traverse(env, depth);
  }
}

void OpExp::Traverse(esc::EscEnvPtr env, int depth) {
  left_->Traverse(env, depth);
  right_->Traverse(env, depth);
}

void RecordExp::Traverse(esc::EscEnvPtr env, int depth) {
  for (auto e : fields_->GetList()) {
    e->exp_->Traverse(env, depth);
  }
}

void SeqExp::Traverse(esc::EscEnvPtr env, int depth) {
  for (auto *expr : seq_->GetList()) {
    expr->Traverse(env, depth);
  }
}

void AssignExp::Traverse(esc::EscEnvPtr env, int depth) {
  var_->Traverse(env, depth);
  exp_->Traverse(env, depth);
}

void IfExp::Traverse(esc::EscEnvPtr env, int depth) {
  test_->Traverse(env, depth);
  then_->Traverse(env, depth);
  if (elsee_) {
    elsee_->Traverse(env, depth);
  }
}

void WhileExp::Traverse(esc::EscEnvPtr env, int depth) {
  test_->Traverse(env, depth);
  body_->Traverse(env, depth);
}

void ForExp::Traverse(esc::EscEnvPtr env, int depth) {
  lo_->Traverse(env, depth);
  hi_->Traverse(env, depth);
  env->BeginScope();
  escape_ = false;
  env->Enter(var_, new esc::EscapeEntry(depth, &escape_));
  body_->Traverse(env, depth);
  env->EndScope();
}

void BreakExp::Traverse(esc::EscEnvPtr env, int depth) {}

void LetExp::Traverse(esc::EscEnvPtr env, int depth) {
  env->BeginScope();
  for (auto *dec : decs_->GetList()) {
    dec->Traverse(env, depth);
  };
  body_->Traverse(env, depth);
  env->EndScope();
}

void ArrayExp::Traverse(esc::EscEnvPtr env, int depth) {
  size_->Traverse(env, depth);
  init_->Traverse(env, depth);
}

void VoidExp::Traverse(esc::EscEnvPtr env, int depth) {}

void FunctionDec::Traverse(esc::EscEnvPtr env, int depth) {
  depth++;
  for (auto *fun_dec : functions_->GetList()) {
    env->BeginScope();
    for (auto *field : fun_dec->params_->GetList()) {
      field->escape_ = false;
      env->Enter(field->name_, new esc::EscapeEntry(depth, &field->escape_));
    }
    fun_dec->body_->Traverse(env, depth);
    env->EndScope();
  }
}

void VarDec::Traverse(esc::EscEnvPtr env, int depth) {
  init_->Traverse(env, depth);
  escape_ = false;
  env->Enter(var_, new esc::EscapeEntry(depth, &escape_));
}

void TypeDec::Traverse(esc::EscEnvPtr env, int depth) {
  for (auto *nat : types_->GetList()) {
    if (typeid(*nat->ty_) == typeid(RecordTy)) {
      auto *rt = dynamic_cast<RecordTy *>(nat->ty_);
      for (auto *f : rt->record_->GetList()) {
        f->escape_ = true;
      };
    }
  }
}

} // namespace absyn
