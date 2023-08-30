#ifndef STRAIGHTLINE_SLP_H_
#define STRAIGHTLINE_SLP_H_

#include <algorithm>
#include <cassert>
#include <list>
#include <string>

namespace A {

class Stm;
class Exp;
class ExpList;

enum BinOp { PLUS = 0, MINUS, TIMES, DIV };

// some data structures used by interp
class Table;
class IntAndTable;

class Stm {
public:
  [[nodiscard]] virtual int MaxArgs() const = 0;
  [[nodiscard]] virtual Table *Interp(Table *) const = 0;
};

class CompoundStm : public Stm {
public:
  CompoundStm(Stm *stm1, Stm *stm2) : stm1(stm1), stm2(stm2) {}

  [[nodiscard]] int MaxArgs() const override;
  [[nodiscard]] Table *Interp(Table *) const override;

private:
  Stm *stm1, *stm2;
};

class AssignStm : public Stm {
public:
  AssignStm(std::string id, Exp *exp) : id(std::move(id)), exp(exp) {}

  [[nodiscard]] int MaxArgs() const override;
  [[nodiscard]] Table *Interp(Table *) const override;

private:
  std::string id;
  Exp *exp;
};

class PrintStm : public Stm {
public:
  explicit PrintStm(ExpList *exps) : exps(exps) {}

  [[nodiscard]] int MaxArgs() const override;
  [[nodiscard]] Table *Interp(Table *) const override;

private:
  ExpList *exps;
};

class Exp {
public:
  [[nodiscard]] virtual int MaxArgs() const = 0;
  [[nodiscard]] virtual IntAndTable *Interp(Table *) const = 0;
};

class IdExp : public Exp {
public:
  explicit IdExp(std::string id) : id(std::move(id)) {}

  [[nodiscard]] int MaxArgs() const override;
  [[nodiscard]] IntAndTable *Interp(Table *) const override;

private:
  std::string id;
};

class NumExp : public Exp {
public:
  explicit NumExp(int num) : num(num) {}

  [[nodiscard]] int MaxArgs() const override;
  [[nodiscard]] IntAndTable *Interp(Table *) const override;

private:
  int num;
};

class OpExp : public Exp {
public:
  OpExp(Exp *left, BinOp oper, Exp *right)
      : left(left), oper(oper), right(right) {}

  [[nodiscard]] int MaxArgs() const override;
  [[nodiscard]] IntAndTable *Interp(Table *) const override;

private:
  Exp *left;
  BinOp oper;
  Exp *right;
};

class EseqExp : public Exp {
public:
  EseqExp(Stm *stm, Exp *exp) : stm(stm), exp(exp) {}

  [[nodiscard]] int MaxArgs() const override;
  [[nodiscard]] IntAndTable *Interp(Table *) const override;

private:
  Stm *stm;
  Exp *exp;
};

class ExpList {
public:
  [[nodiscard]] virtual int MaxArgs() const = 0;
  [[nodiscard]] virtual IntAndTable *Interp(Table *) const = 0;
  [[nodiscard]] virtual int Length() const = 0;
};

class PairExpList : public ExpList {
public:
  PairExpList(Exp *exp, ExpList *tail) : exp(exp), tail(tail) {}

  [[nodiscard]] IntAndTable *Interp(Table *) const override;
  [[nodiscard]] int MaxArgs() const override;
  [[nodiscard]] int Length() const override;

private:
  Exp *exp;
  ExpList *tail;
};

class LastExpList : public ExpList {
public:
  explicit LastExpList(Exp *exp) : exp(exp) {}
  [[nodiscard]] IntAndTable *Interp(Table *) const override;
  [[nodiscard]] int MaxArgs() const override;
  [[nodiscard]] int Length() const override;

private:
  Exp *exp;
};

class Table {
public:
  Table(std::string id, int value, const Table *tail)
      : id(std::move(id)), value(value), tail(tail) {}

  [[nodiscard]] int Lookup(const std::string &key) const;
  [[nodiscard]] Table *Update(const std::string &key, int val) const;

private:
  std::string id;
  int value;
  const Table *tail;
};

struct IntAndTable {
  int i;
  Table *t;

  IntAndTable(int i, Table *t) : i(i), t(t) {}
};

} // namespace A

#endif // STRAIGHTLINE_SLP_H_
