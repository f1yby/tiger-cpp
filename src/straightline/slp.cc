#include "straightline/slp.h"

#include <iostream>

using namespace std;
namespace A {
int A::CompoundStm::MaxArgs() const {
  return max(stm1->MaxArgs(), stm2->MaxArgs());
}

Table *A::CompoundStm::Interp(Table *t) const {
  return stm2->Interp(stm1->Interp(t));
}

int A::AssignStm::MaxArgs() const { return exp->MaxArgs(); }

Table *A::AssignStm::Interp(Table *t) const {
  auto it = this->exp->Interp(t);
  return it->t->Update(id, it->i);
}

Table *A::PrintStm::Interp(Table *t) const { return exps->Interp(t)->t; }
int A::PrintStm::MaxArgs() const {
  return max(exps->Length(), exps->MaxArgs());
}

int Table::Lookup(const std::string &key) const {
  if (id == key) {
    return value;
  } else if (tail != nullptr) {
    return tail->Lookup(key);
  } else {
    abort();
  }
}
Table *Table::Update(const std::string &key, int val) const {
  return new Table(key, val, this);
}

IntAndTable *PairExpList::Interp(Table *t) const {
  auto it = exp->Interp(t);
  std::cout << it->i << ' ';
  return tail->Interp(it->t);
}
int PairExpList::MaxArgs() const {
  return max(exp->MaxArgs(), tail->MaxArgs());
}
int PairExpList::Length() const { return 1 + tail->Length(); }

IntAndTable *LastExpList::Interp(Table *t) const {
  auto it = exp->Interp(t);
  cout << it->i << endl;
  return it;
}
int LastExpList::MaxArgs() const { return exp->MaxArgs(); }
int LastExpList::Length() const { return 1; }

int IdExp::MaxArgs() const { return 0; }
IntAndTable *IdExp::Interp(Table *t) const {
  return new IntAndTable{t->Lookup(id), t};
}

int NumExp::MaxArgs() const { return 0; }
IntAndTable *NumExp::Interp(Table *t) const { return new IntAndTable{num, t}; }

int OpExp::MaxArgs() const { return 0; }
IntAndTable *OpExp::Interp(Table *t) const {
  int ans;
  auto l = left->Interp(t);
  auto r = right->Interp(l->t);
  switch (oper) {
  case PLUS:
    ans = l->i + r->i;
    break;
  case MINUS:
    ans = l->i - r->i;
    break;
  case TIMES:
    ans = l->i * r->i;
    break;
  case DIV:
    ans = l->i / r->i;
    break;
  }
  return new IntAndTable{ans, r->t};
};

int EseqExp::MaxArgs() const { return max(stm->MaxArgs(), exp->MaxArgs()); }
IntAndTable *EseqExp::Interp(Table *t) const {
  return exp->Interp(stm->Interp(t));
}
} // namespace A
