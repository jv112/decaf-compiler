
#include "default-defs.h"
#include <list>
#include <ostream>
#include <iostream>
#include <sstream>
#include <map>

#ifndef YYTOKENTYPE
#include "decafcomp.tab.h"
#endif

using namespace std;

static llvm::Module *TheModule;
static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);

typedef enum { METHOD, VAR_LOCAL, VAR_GLOBAL, VAR_GLOBAL_ARRAY } descType;

struct descriptor {
  descType type;
  llvm::Value *value;
  descriptor(descType t, llvm::Value *v) : type(t), value(v) {}
};

typedef map<string, descriptor *> symbol_table;
typedef list<symbol_table *> symbol_table_list;
symbol_table_list symtbl;

descriptor *access_symtbl(string ident) {
  for (auto &it : symtbl) {
    auto find_ident = it->find(ident);
    if (find_ident != it->end()) {
      return find_ident->second;
    }
  }
  return nullptr;
}

void add_symbol(string ident, descriptor *d) {
  if (symtbl.empty())
    throw runtime_error("Symbol table is empty");

  symbol_table *table = symtbl.front();
  auto find_ident = table->find(ident);
  if (find_ident != table->end()) {
    throw runtime_error("Symbol exists in current scope: " + ident);
  }
  (*table)[ident] = d;
}

void scope_enter() {
  symbol_table *table = new symbol_table();
  symtbl.push_front(table);
}

void scope_exit() {
  symbol_table *table = symtbl.front();
  for (auto &it : *table) {
    delete it.second;
  }
  delete table;
  symtbl.pop_front();
}

typedef map<string, llvm::BasicBlock *> block_table;
typedef list<block_table *> block_table_list;
block_table_list blocktbl;

llvm::BasicBlock *getBlock(string name) {
  for (auto &it : blocktbl) {
    auto find_block = it->find(name);
    if (find_block != it->end()) {
      return find_block->second;
    }
  }
  return nullptr;
}

void addBlock(string name, llvm::BasicBlock *bb) {
  if (blocktbl.empty())
    throw runtime_error("Block table is empty");

  block_table *table = blocktbl.front();
  auto find_block = table->find(name);
  if (find_block != table->end()) {
    throw runtime_error("Block already exists: " + name);
  }
  (*table)[name] = bb;
}

void scope_enter_block() {
  block_table *table = new block_table();
  blocktbl.push_front(table);
}

void scope_exit_block() {
  block_table *table = blocktbl.front();
  delete table;
  blocktbl.pop_front();
}

typedef enum {VOIDTYPE, INTTYPE, BOOLTYPE, STRINGTYPE } decafType;
llvm::Type *getLLVMType(decafType ty) {
  switch (ty) {
    case VOIDTYPE:return Builder.getVoidTy();
    case INTTYPE:return Builder.getInt32Ty();
    case BOOLTYPE:return Builder.getInt1Ty();
    case STRINGTYPE:return llvm::PointerType::getUnqual(Builder.getInt8Ty());
    default: throw runtime_error("getLLVMType invalid decafType");
  }
}

llvm::Constant *getZeroInit(decafType ty) {
  switch (ty) {
    case INTTYPE: return Builder.getInt32(0);
    case BOOLTYPE: return Builder.getInt1(0);
    default: throw runtime_error("getZeroInit invalid decafType");
  }
}

llvm::Constant *getDefaultValue(llvm::Type *ty) {
  if (ty->isIntegerTy(32))
    return Builder.getInt32(0);
  else if (ty->isIntegerTy(1))
    return Builder.getInt1(1);
  else
    throw runtime_error("getDefaultValue invalid llvm type");
}

string replace_escape_chars(const string &s) {
  string result;
  for (int i = 0; i < s.length(); i++) {
    if (s[i] == '\\') {
      i++;
      switch (s[i]) {
        case 'n': result += '\n'; break;
        case 'r': result += '\r'; break;
        case 't': result += '\t'; break;
        case 'v': result += '\v'; break;
        case 'f': result += '\f'; break;
        case 'a': result += '\a'; break;
        case 'b': result += '\b'; break;
        case '\\': result += '\\'; break;
        case '\'': result += '\''; break;
        case '\"': result += '\"'; break;
        default: result += s[i]; break;
      }
    }
    else {
      result += s[i];
    }
  }
  return result;
}

/// decafAST - Base class for all abstract syntax tree nodes.
class decafAST {
public:
  int lineno;

  virtual ~decafAST() {}
  virtual string str() { return string(""); }
  virtual llvm::Value *Codegen() = 0;
  void throw_error(const string &msg) {
    throw runtime_error("line " + to_string(lineno) + ", " + msg);
  }
};

string getString(decafAST *d) {
  if (d != NULL) {
    return d->str();
  } else {
    return string("None");
  }
}

template <class T>
string commaList(list<T> vec) {
  string s("");
  for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) {
    s = s + (s.empty() ? string("") : string(",")) + (*i)->str();
  }
  if (s.empty()) {
    s = string("None");
  }
  return s;
}

template <class T>
llvm::Value *listCodegen(list<T> vec) {
  llvm::Value *val = NULL;
  for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) {
    llvm::Value *j = (*i)->Codegen();
    if (j != NULL) {
      val = j;
    }
  }
  return val;
}

/// decafStmtList - List of Decaf statements
class decafStmtList : public decafAST {
  list<decafAST *> stmts;

public:
  decafStmtList() {}
  ~decafStmtList() {
    for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++) {
      delete *i;
    }
  }
  int size() { return stmts.size(); }
  void push_front(decafAST *e) { stmts.push_front(e); }
  void push_back(decafAST *e) { stmts.push_back(e); }
  void merge(decafStmtList *l) {
    if (l != NULL) {
      for (auto it = l->stmts.begin(); it != l->stmts.end(); ++it) {
        stmts.push_back(*it);
      }
    }
    l->stmts.clear();
  }
  list<decafAST *> &get_stmts() { return stmts; }
  string str() { return commaList<class decafAST *>(stmts); }
  llvm::Value *Codegen(){
    return listCodegen<decafAST *>(stmts);
  }
};

class TypeAST : public decafAST {
  decafType type;

public:
  TypeAST(decafType name) : type(name) {}
  TypeAST(const TypeAST &other) : type(other.type) {}
  ~TypeAST() {}
  decafType getType() { return type; }
  string str() {
    switch (type){
      case VOIDTYPE: return "VoidType";
      case INTTYPE: return "IntType";
      case BOOLTYPE: return "BoolType";
      case STRINGTYPE: return "StringType";
      default: throw runtime_error("TypeAST unknown decafType");
    }
  }
  llvm::Value *Codegen() {
    return nullptr;
  }
};

class NumberExprAST : public decafAST {
  string value;

public:
  NumberExprAST(string v) : value(v) {}
  ~NumberExprAST() {}
  string str() { return string("NumberExpr(") + value + ")"; }
  llvm::Value *Codegen() {
    if (value.find("0x") == 0 || value.find("0X") == 0) {
      return Builder.getInt32(static_cast<int32_t>(strtol(value.c_str(), nullptr, 16)));
    } 
    else {
      return Builder.getInt32(static_cast<int32_t>(strtol(value.c_str(), nullptr, 10)));
    }
  }
};

class BoolExprAST : public decafAST {
  bool value;

public:
  BoolExprAST(bool v) : value(v) {}
  ~BoolExprAST() {}
  string str() { return string("BoolExpr(") + (value ? "True" : "False") + ")"; }
  llvm::Value *Codegen() {
    return Builder.getInt1(value);
  }
};

class StringConstantAST : public decafAST {
  string value;

public:
  StringConstantAST(string v) : value(v) {}
  ~StringConstantAST() {}
  string str() { return string("StringConstant(") + value + ")"; }
  llvm::Value *Codegen() {
    string s = replace_escape_chars(value);
    s = s.substr(1, s.length() - 2);
    return Builder.CreateGlobalString(s.c_str(), "globalstring");
  }
};

class VariableExprAST : public decafAST {
  string name;

public:
  VariableExprAST(string n) : name(n) {}
  ~VariableExprAST() {}
  string str() { return string("VariableExpr(") + name + ")"; }
  llvm::Value *Codegen() {
    descriptor *desc = access_symtbl(name);
    if (!desc) 
      throw_error("variable does not exist: " + name);
    if (desc->type == VAR_GLOBAL) {
      llvm::GlobalVariable *var = (llvm::GlobalVariable *)desc->value;
      llvm::Type *type = var->getValueType();
      return Builder.CreateLoad(type, var, name.c_str());
    }
    else if (desc->type == VAR_LOCAL) {
      llvm::AllocaInst *alloca = (llvm::AllocaInst *)desc->value;
      llvm::Type *type = alloca->getAllocatedType();
      return Builder.CreateLoad(type, alloca, name.c_str());
    }
    throw runtime_error("VariableExprAST invalid type");
  }
};

class ArrayLocExprAST : public decafAST {
  string name;
  decafAST *index;

public:
  ArrayLocExprAST(string n, decafAST *i) : name(n), index(i) {}
  ~ArrayLocExprAST() {
    if (index != nullptr) delete index; index = nullptr;
  }
  string str() { return string("ArrayLocExpr(") + name + "," + getString(index) + ")"; }
  llvm::Value *Codegen() {
    descriptor *desc = access_symtbl(name);
    if (!desc) 
      throw_error("array does not exist: " + name);
    if (desc->type != VAR_GLOBAL_ARRAY) 
      throw_error("variable is not a global array: " + name);

    llvm::GlobalVariable *arr = (llvm::GlobalVariable *)desc->value;
    llvm::Value *idx = index->Codegen();
    if (!idx->getType()->isIntegerTy(32)) 
      throw_error("array index must be an integer: " + name);
    if (llvm::ConstantInt *constIdx = llvm::dyn_cast<llvm::ConstantInt>(idx)) {
      uint64_t arraySize = arr->getValueType()->getArrayNumElements();
      uint64_t idxValue = constIdx->getZExtValue();
      if (idxValue < 0 || idxValue >= arraySize)
        throw_error("array index out of bounds: " + name);
    }

    llvm::ArrayType *arrayType = llvm::cast<llvm::ArrayType>(arr->getValueType());
    llvm::Value *arrayLoc = Builder.CreateStructGEP(arrayType, arr, 0, "arrayloc");
    llvm::Value *arrayIndex = Builder.CreateGEP(arrayType->getArrayElementType(), arrayLoc, idx, "arrayindex");
    return Builder.CreateLoad(arrayType->getArrayElementType(), arrayIndex, "arrayval");
  }
};

class MethodCallAST : public decafAST {
  string method_name;
  decafStmtList *arg_list;

public:
  MethodCallAST(string name, decafStmtList *a) : method_name(name), arg_list(a) {}
  ~MethodCallAST() { delete arg_list; arg_list = nullptr; }
  string str() { return string("MethodCall(") + method_name + "," + getString(arg_list) + ")"; }
  llvm::Value *Codegen() {
    descriptor *desc = access_symtbl(method_name);
    if (!desc) 
      throw_error("method does not exist: " + method_name);
    if (desc->type != METHOD) 
      throw_error("variable is not a method: " + method_name);

    llvm::Function *func = (llvm::Function *)desc->value;
    vector<llvm::Value *> args;
    auto method_args = arg_list->get_stmts();
    auto it = method_args.begin();
    if (method_args.size() != func->arg_size())
      throw_error("argument count mismatch in method: " + method_name);

    for (auto &arg : func->args()) {
      llvm::Value *argVal = (*it++)->Codegen();
      if (argVal->getType() != arg.getType()) {
        if (argVal->getType()->isIntegerTy(1) && arg.getType()->isIntegerTy(32)){
          argVal = Builder.CreateZExt(argVal, Builder.getInt32Ty(), "zexttmp");
        }
        else throw_error("argument type mismatch: " + method_name);
      }
      args.push_back(argVal);
    }
    bool isVoid = func->getReturnType()->isVoidTy();
    llvm::Value *val = Builder.CreateCall(func, args, isVoid ? "" : "calltmp");
    return val;
  }
};

class AssignVarAST : public decafAST {
  string name;
  decafAST *value;

public:
  AssignVarAST(string n, decafAST *v) : name(n), value(v) {}
  ~AssignVarAST() { delete value; value = nullptr; }
  string str() { return string("AssignVar(") + name + "," + getString(value) + ")"; }
  llvm::Value *Codegen() {
    descriptor *desc = access_symtbl(name);
    if (!desc) 
      throw_error("variable does not exist: " + name);

    if (desc->type == VAR_GLOBAL) {
      llvm::GlobalVariable *var = (llvm::GlobalVariable *)desc->value;
      llvm::Type *type = var->getValueType();
      llvm::Value *val = value->Codegen();
      if (val->getType() != type) 
        throw_error("assignment type mismatch: " + name);
      Builder.CreateStore(val, var);
    }
    else if (desc->type == VAR_LOCAL) {
      llvm::AllocaInst *alloca = (llvm::AllocaInst *)desc->value;
      llvm::Type *type = alloca->getAllocatedType();
      llvm::Value *val = value->Codegen();
      if (val->getType() != type)
        throw_error("assignment type mismatch: " + name);
      Builder.CreateStore(val, alloca);
    }
    else throw_error("variable is not a valid type: " + name);
    return nullptr;
  }
};

class AssignArrayLocAST : public decafAST {
  string name;
  decafAST *index;
  decafAST *value;

public:
  AssignArrayLocAST(string n, decafAST *i, decafAST *v) : name(n), index(i), value(v) {}
  ~AssignArrayLocAST() { 
    delete index; index = nullptr; 
    delete value; value = nullptr; 
  }
  string str() {
    return string("AssignArrayLoc(") + name + "," + getString(index) + "," + getString(value) + ")";
  }
  llvm::Value *Codegen() {
    descriptor *desc = access_symtbl(name);
    if (!desc)
      throw_error("array does not exist: " + name);
    if (desc->type != VAR_GLOBAL_ARRAY)
      throw_error("variable is not a global array: " + name);

    llvm::GlobalVariable *arr = (llvm::GlobalVariable *)desc->value;
    llvm::Value *idx = index->Codegen();
    if (!idx->getType()->isIntegerTy(32))
      throw_error("array index must be an integer: " + name);
    if (llvm::ConstantInt *constIdx = llvm::dyn_cast<llvm::ConstantInt>(idx)) {
      uint64_t arraySize = arr->getValueType()->getArrayNumElements();
      uint64_t idxValue = constIdx->getZExtValue();
      if (idxValue < 0 || idxValue >= arraySize)
        throw_error("array index out of bounds: " + name);
    }

    llvm::ArrayType *arrayType = llvm::cast<llvm::ArrayType>(arr->getValueType());
    llvm::Value *arrayLoc = Builder.CreateStructGEP(arrayType, arr, 0, "arrayloc");
    llvm::Value *arrayIndex = Builder.CreateGEP(arrayType->getArrayElementType(), arrayLoc, idx, "arrayindex");
    llvm::Value *val = value->Codegen();
    if (val->getType() != arr->getValueType()->getArrayElementType())
      throw_error("cannot assign value of type " + name);
    Builder.CreateStore(val, arrayIndex);
    return nullptr;
  }
};

class VarDefAST : public decafAST {
  string name;
  TypeAST *type;

public:
  VarDefAST(string n, TypeAST *t) : name(n), type(t) {}
  ~VarDefAST() { delete type; type = nullptr; }
  decafType getType() { return type->getType(); }
  string getName() { return name; }
  string str() {
    if (name == "") {
      return string("VarDef(" + getString(type) + ")");
    } else {
      return string("VarDef(" + name + "," + getString(type) + ")");
    }
  }
  llvm::Value *Codegen() {
    llvm::Type *llvmTy = getLLVMType(type->getType());
    llvm::AllocaInst *alloca = Builder.CreateAlloca(llvmTy, 0, name.c_str());
    Builder.CreateStore(getZeroInit(type->getType()), alloca);
    add_symbol(name, new descriptor(VAR_LOCAL, alloca));
    return alloca;
  }
};

class BlockAST : public decafAST {
  decafStmtList *var_decl_list;
  decafStmtList *statement_list;

public:
  BlockAST(decafStmtList *v, decafStmtList *s) : var_decl_list(v), statement_list(s) {}
  ~BlockAST() {
    delete var_decl_list; var_decl_list = nullptr;
    delete statement_list; statement_list = nullptr;
  }
  string str() {
    return string("Block(") + getString(var_decl_list) + "," + getString(statement_list) + ")";
  }
  llvm::Value *Codegen() {
    scope_enter();
    var_decl_list->Codegen();
    statement_list->Codegen();
    scope_exit();
    return nullptr;
  }
};

class MethodBlockAST : public decafAST {
  decafStmtList *var_decl_list;
  decafStmtList *statement_list;

public:
  MethodBlockAST(decafStmtList *v, decafStmtList *s) : var_decl_list(v), statement_list(s) {}
  ~MethodBlockAST() {
    delete var_decl_list; var_decl_list = nullptr;
    delete statement_list; statement_list = nullptr;
  }
  string str() {
    return string("MethodBlock(") + getString(var_decl_list) + "," + getString(statement_list) + ")";
  }
  llvm::Value *Codegen() {
    var_decl_list->Codegen();
    statement_list->Codegen();
    return nullptr;
  }
};

class IfStmtAST : public decafAST {
  decafAST *condition;
  BlockAST *if_block;
  BlockAST *else_block;

public:
  IfStmtAST(decafAST *c, BlockAST *i, BlockAST *e) : condition(c), if_block(i), else_block(e) {}
  ~IfStmtAST() {
    delete condition; condition = nullptr;
    delete if_block; if_block = nullptr;
    delete else_block; else_block = nullptr;
  }
  string str() {
    return string("IfStmt(") +
           getString(condition) + "," + getString(if_block) + "," + getString(else_block) + ")";
  }
  llvm::Value *Codegen() {
    llvm::Function *func = Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *IfBB = llvm::BasicBlock::Create(TheContext, "if", func);
    llvm::BasicBlock *TrueBB = llvm::BasicBlock::Create(TheContext, "true", func);
    llvm::BasicBlock *FalseBB = llvm::BasicBlock::Create(TheContext, "false", func);
    llvm::BasicBlock *ElseBB;

    Builder.CreateBr(IfBB);
    Builder.SetInsertPoint(IfBB);
    llvm::Value *cond = condition->Codegen();
    if (!cond->getType()->isIntegerTy(1))
      throw_error("if condition must be a boolean expression");

    if (else_block != nullptr) {
      ElseBB = llvm::BasicBlock::Create(TheContext, "else", func);
      Builder.CreateCondBr(cond, TrueBB, ElseBB);
    } else {
      Builder.CreateCondBr(cond, TrueBB, FalseBB);
    }

    Builder.SetInsertPoint(TrueBB);
    if_block->Codegen();
    if (!Builder.GetInsertBlock()->getTerminator()) {
      Builder.CreateBr(FalseBB);
    }

    if (else_block != nullptr) {
      Builder.SetInsertPoint(ElseBB);
      else_block->Codegen();
      if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateBr(FalseBB);
      }
    }

    Builder.SetInsertPoint(FalseBB);
    return nullptr;
  }
};

class WhileStmtAST : public decafAST {
  decafAST *condition;
  BlockAST *while_block;

public:
  WhileStmtAST(decafAST *c, BlockAST *b) : condition(c), while_block(b) {}
  ~WhileStmtAST() {
    delete condition; condition = nullptr;
    delete while_block; while_block = nullptr;
  }
  string str() {
    return string("WhileStmt(") + getString(condition) + "," + getString(while_block) + ")";
  }
  llvm::Value *Codegen() {
    llvm::Function *func = Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *GateBB = llvm::BasicBlock::Create(TheContext, "gate", func);
    llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(TheContext, "loop", func);
    llvm::BasicBlock *DeclBB = llvm::BasicBlock::Create(TheContext, "decl", func);
    llvm::BasicBlock *BodyBB = llvm::BasicBlock::Create(TheContext, "body", func);
    llvm::BasicBlock *EndBB = llvm::BasicBlock::Create(TheContext, "end", func);

    scope_enter_block();
    addBlock("loopstart", LoopBB);
    addBlock("loopend", EndBB);

    Builder.CreateBr(GateBB);
    Builder.SetInsertPoint(GateBB);
    llvm::Value *gateCond = condition->Codegen();
    if (!gateCond->getType()->isIntegerTy(1))
      throw_error("while condition must be a boolean expression");
    Builder.CreateCondBr(gateCond, DeclBB, EndBB);

    Builder.SetInsertPoint(LoopBB);
    llvm::Value *loopCond = condition->Codegen();
    Builder.CreateCondBr(loopCond, BodyBB, EndBB);

    Builder.SetInsertPoint(DeclBB);
    Builder.CreateBr(BodyBB);

    Builder.SetInsertPoint(BodyBB);
    while_block->Codegen();
    if (!Builder.GetInsertBlock()->getTerminator()) {
      Builder.CreateBr(LoopBB);
    }

    Builder.SetInsertPoint(EndBB);
    scope_exit_block();
    return nullptr;
  }
};

class ForStmtAST : public decafAST {
  decafStmtList *pre_assign_list;
  decafAST *condition;
  decafStmtList *loop_assign_list;
  BlockAST *for_block;

public:
  ForStmtAST(decafStmtList *p, decafAST *c, decafStmtList *l, BlockAST *b)
      : pre_assign_list(p), condition(c), loop_assign_list(l), for_block(b) {}
  ~ForStmtAST() {
    delete pre_assign_list; pre_assign_list = nullptr;
    delete condition; condition = nullptr;
    delete loop_assign_list; loop_assign_list = nullptr;
    delete for_block; for_block = nullptr;
  }
  string str() {
    return string("ForStmt(") + getString(pre_assign_list) + "," + getString(condition) +
           "," + getString(loop_assign_list) + "," + getString(for_block) + ")";
  }
  llvm::Value *Codegen() {
    llvm::Function *func = Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *GateBB = llvm::BasicBlock::Create(TheContext, "gate", func);
    llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(TheContext, "loop", func);
    llvm::BasicBlock *DeclBB = llvm::BasicBlock::Create(TheContext, "decl", func);
    llvm::BasicBlock *BodyBB = llvm::BasicBlock::Create(TheContext, "body", func);
    llvm::BasicBlock *NextBB = llvm::BasicBlock::Create(TheContext, "next", func);
    llvm::BasicBlock *EndBB = llvm::BasicBlock::Create(TheContext, "end", func);

    scope_enter_block();
    addBlock("loopstart", NextBB);
    addBlock("loopend", EndBB);

    pre_assign_list->Codegen();
    Builder.CreateBr(GateBB);

    Builder.SetInsertPoint(GateBB);
    llvm::Value *gateCond = condition->Codegen();
    if (!gateCond->getType()->isIntegerTy(1))
      throw_error("for loop condition must be a boolean expression");
    Builder.CreateCondBr(gateCond, DeclBB, EndBB);

    Builder.SetInsertPoint(LoopBB);
    llvm::Value *loopCond = condition->Codegen();
    Builder.CreateCondBr(loopCond, BodyBB, EndBB);

    Builder.SetInsertPoint(NextBB);
    loop_assign_list->Codegen();
    Builder.CreateBr(LoopBB);

    Builder.SetInsertPoint(BodyBB);
    for_block->Codegen();
    if (!Builder.GetInsertBlock()->getTerminator()) {
      Builder.CreateBr(NextBB);
    }

    Builder.SetInsertPoint(DeclBB);
    Builder.CreateBr(BodyBB);

    Builder.SetInsertPoint(EndBB);
    scope_exit_block();
    return nullptr;
  }
};

class ReturnStmtAST : public decafAST {
  decafAST *return_value;

public:
  ReturnStmtAST(decafAST *v) : return_value(v) {}
  ~ReturnStmtAST() { delete return_value; return_value = nullptr; }
  string str() { return string("ReturnStmt(") + getString(return_value) + ")"; }
  llvm::Value *Codegen() {
    llvm::Function *func = Builder.GetInsertBlock()->getParent();
    if (!return_value) {
      if (func->getReturnType()->isVoidTy()) {
        Builder.CreateRetVoid();
      }
      else {
        Builder.CreateRet(getDefaultValue(func->getReturnType()));
      }
    }
    else {
      llvm::Value *retVal = return_value->Codegen();
      if (retVal->getType() != func->getReturnType())
        throw_error("return value type mismatch");
      Builder.CreateRet(retVal);
    }

    llvm::BasicBlock *unreachableBB = llvm::BasicBlock::Create(TheContext, "unreachable", func);
    Builder.SetInsertPoint(unreachableBB);
    return nullptr;
  }
};

class BreakStmtAST : public decafAST {
public:
  BreakStmtAST() {}
  ~BreakStmtAST() {}
  string str() { return string("BreakStmt"); }
  llvm::Value *Codegen() {
    llvm::Function *func = Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *EndBB = getBlock("loopend");
    if (!EndBB)
      throw_error("break statement used outside of loop");
    Builder.CreateBr(EndBB);
    llvm::BasicBlock *unreachableBB = llvm::BasicBlock::Create(TheContext, "unreachable", func);
    Builder.SetInsertPoint(unreachableBB);
    return nullptr;
  }
};

class ContinueStmtAST : public decafAST {
public:
  ContinueStmtAST() {}
  ~ContinueStmtAST() {}
  string str() { return string("ContinueStmt"); }
  llvm::Value *Codegen() {
    llvm::Function *func = Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *StartBB = getBlock("loopstart");
    if (!StartBB)
      throw_error("continue statement used outside of loop");
    Builder.CreateBr(StartBB);
    llvm::BasicBlock *unreachableBB = llvm::BasicBlock::Create(TheContext, "unreachable", func);
    Builder.SetInsertPoint(unreachableBB);
    return nullptr;
  }
};

typedef enum { 
  PLUS, MINUS, MULT, DIV, LEFTSHIFT, RIGHTSHIFT, MOD, LT, GT, LEQ, GEQ, EQ, NEQ, AND, OR
} BinaryOp;

class BinaryExprAST : public decafAST {
  BinaryOp op;
  decafAST *left;
  decafAST *right;

public:
  BinaryExprAST(BinaryOp o, decafAST *l, decafAST *r) : op(o), left(l), right(r) {}
  ~BinaryExprAST() {
    delete left; left = nullptr;
    delete right; right = nullptr;
  }
  string str() {
    string op_str;
    switch (op) {
      case PLUS: op_str = "Plus"; break;
      case MINUS: op_str = "Minus"; break;
      case MULT: op_str = "Mult"; break;
      case DIV: op_str = "Div"; break;
      case LEFTSHIFT: op_str = "Leftshift"; break;
      case RIGHTSHIFT: op_str = "Rightshift"; break;
      case MOD: op_str = "Mod"; break;
      case LT: op_str = "Lt"; break;
      case GT: op_str = "Gt"; break;
      case LEQ: op_str = "Leq"; break;
      case GEQ: op_str = "Geq"; break;
      case EQ: op_str = "Eq"; break;
      case NEQ: op_str = "Neq"; break;
      case AND: op_str = "And"; break;
      case OR: op_str = "Or"; break;
      default: throw runtime_error("unknown binary operator");
    }
    return string("BinaryExpr(") + op_str + "," + getString(left) + "," + getString(right) + ")";
  }
  llvm::Value *Codegen() {
    llvm::Value *L = left->Codegen();
    if (op == AND || op == OR) {
      if (!L->getType()->isIntegerTy(1))
        throw_error("left operand of logical operator must be a boolean expression");
    }

    switch (op) {
      case AND:
      {
        llvm::BasicBlock *LBB = Builder.GetInsertBlock();
        llvm::Function *func = Builder.GetInsertBlock()->getParent();
        llvm::BasicBlock *NoskctBB = llvm::BasicBlock::Create(TheContext, "noskct", func);
        llvm::BasicBlock *SkctendBB = llvm::BasicBlock::Create(TheContext, "skctend", func);
        Builder.CreateCondBr(L, NoskctBB, SkctendBB);

        Builder.SetInsertPoint(NoskctBB);
        llvm::Value *R = right->Codegen();
        if (!R->getType()->isIntegerTy(1))
          throw_error("right operand of logical operator must be a boolean expression");
        llvm::BasicBlock *RBB = Builder.GetInsertBlock();
        Builder.CreateBr(SkctendBB);

        Builder.SetInsertPoint(SkctendBB);
        llvm::PHINode *val = Builder.CreatePHI(L->getType(), 2, "phival");
        val->addIncoming(L, LBB);
        val->addIncoming(R, RBB);
        return val;
      }
      case OR:
      {
        llvm::BasicBlock *LBB = Builder.GetInsertBlock();
        llvm::Function *func = Builder.GetInsertBlock()->getParent();
        llvm::BasicBlock *NoskctBB = llvm::BasicBlock::Create(TheContext, "noskct", func);
        llvm::BasicBlock *SkctendBB = llvm::BasicBlock::Create(TheContext, "skctend", func);
        Builder.CreateCondBr(L, SkctendBB, NoskctBB);

        Builder.SetInsertPoint(NoskctBB);
        llvm::Value *R = right->Codegen();
        if (!R->getType()->isIntegerTy(1))
          throw_error("right operand of logical operator must be a boolean expression");
        llvm::BasicBlock *RBB = Builder.GetInsertBlock();
        Builder.CreateBr(SkctendBB);

        Builder.SetInsertPoint(SkctendBB);
        llvm::PHINode *val = Builder.CreatePHI(L->getType(), 2, "phival");
        val->addIncoming(L, LBB);
        val->addIncoming(R, RBB);
        return val;
      }
    }

    llvm::Value *R = right->Codegen();
    if (op == EQ || op == NEQ) {
      if (L->getType() != R->getType())
        throw_error("equality expression operands must have the same type");
    }
    else {
      if (!L->getType()->isIntegerTy(32) || !R->getType()->isIntegerTy(32))
        throw_error("binary operator requires integer operands");
    }

    switch (op) {
      case DIV: return Builder.CreateSDiv(L, R, "divtmp");
      case PLUS: return Builder.CreateAdd(L, R, "addtmp");
      case MINUS: return Builder.CreateSub(L, R, "subtmp");
      case MULT: return Builder.CreateMul(L, R, "multmp");
      case LEFTSHIFT: return Builder.CreateShl(L, R, "shltmp");
      case RIGHTSHIFT: return Builder.CreateLShr(L, R, "lshrtmp");
      case MOD: return Builder.CreateSRem(L, R, "modtmp");
      case LT: return Builder.CreateICmpSLT(L, R, "lttmp");
      case GT: return Builder.CreateICmpSGT(L, R, "gttmp");
      case LEQ: return Builder.CreateICmpSLE(L, R, "leqtmp");
      case GEQ: return Builder.CreateICmpSGE(L, R, "geqtmp");
      case EQ: return Builder.CreateICmpEQ(L, R, "eqtmp");
      case NEQ: return Builder.CreateICmpNE(L, R, "neqtmp");
      case AND: return Builder.CreateAnd(L, R, "andtmp");
      case OR: return Builder.CreateOr(L, R, "ortmp");
      default: throw runtime_error("unknown binary operator");
    }
  }
};

typedef enum { NOT, UNARYMINUS } UnaryOp;

class UnaryExprAST : public decafAST {
  UnaryOp op;
  decafAST *expr;

public:
  UnaryExprAST(UnaryOp o, decafAST *e) : op(o), expr(e) {}
  ~UnaryExprAST() { delete expr; expr = nullptr; }
  string str() {
    switch (op) {
      case NOT: return string("UnaryExpr(Not,") + getString(expr) + ")";
      case UNARYMINUS: return string("UnaryExpr(UnaryMinus,") + getString(expr) + ")";
      default: throw runtime_error("unknown unary operator");
    }
    return "";
  }
  llvm::Value *Codegen() {
    llvm::Value *value = expr->Codegen();
    switch (op) {
      case NOT:
      {
        if (!value->getType()->isIntegerTy(1)) {
          throw_error("not operator requires a boolean type");
        }
        return Builder.CreateNot(value, "nottmp");
      }
      case UNARYMINUS:
      {
        if (!value->getType()->isIntegerTy(32)) {
          throw_error("unary minus operator requires an integer type");
        }
        return Builder.CreateNeg(value, "negtmp");
      }
      default: throw runtime_error("unknown unary operator");
    }
  }
};

class FieldSizeAST : public decafAST {
  int size;
  bool is_array;

public:
  FieldSizeAST(int s, bool a) : size(s), is_array(a) {}
  ~FieldSizeAST() {}
  string str() {
    if (is_array) {
      return string("Array(") + to_string(size) + ")";
    } else {
      return string("Scalar");
    }
  }
  int getSize() { return size; }
  bool isArray() { return is_array; }
  llvm::Value *Codegen() {
    return nullptr;
  }
};

class FieldDeclAST : public decafAST {
  string name;
  TypeAST *type;
  FieldSizeAST *size;

public:
  FieldDeclAST(string n, TypeAST *t, FieldSizeAST *s) : name(n), type(t), size(s) {}
  ~FieldDeclAST() {
    delete type; type = nullptr;
    delete size; size = nullptr;
  }
  string str() {
    return string("FieldDecl(" + name + "," + getString(type) + "," + getString(size) + ")");
  }
  llvm::Value *Codegen() {
    if (size->isArray() && size->getSize() <= 0)
      throw_error("array size must be greater than 0");

    if (size->isArray()) {
      llvm::ArrayType *array = llvm::ArrayType::get(getLLVMType(type->getType()), size->getSize());
      llvm::Constant *zeroInit = llvm::Constant::getNullValue(array);
      llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
          *TheModule,
          array,
          false,
          llvm::GlobalValue::ExternalLinkage,
          zeroInit,
          name);
      add_symbol(name, new descriptor(VAR_GLOBAL_ARRAY, globalVar));
    } else {
      llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
          *TheModule,
          getLLVMType(type->getType()),
          false,
          llvm::GlobalValue::ExternalLinkage,
          getZeroInit(type->getType()),
          name);
      add_symbol(name, new descriptor(VAR_GLOBAL, globalVar));
    }

    return nullptr;
  }
};

class AssignGlobalVarAST : public decafAST {
  string name;
  TypeAST *type;
  decafAST *value;

public:
  AssignGlobalVarAST(string n, TypeAST *t, decafAST *v)
      : name(n), type(t), value(v) {}
  ~AssignGlobalVarAST() { 
    delete type; type = nullptr;
    delete value; value = nullptr;
  }
  string str() {
    return string("AssignGlobalVar(") + name + "," + getString(type) + "," + getString(value) + ")";
  }
  llvm::Value *Codegen() {
    llvm::Constant *right = llvm::dyn_cast<llvm::Constant>(value->Codegen());
    if (right->getType() != getLLVMType(type->getType()))
      throw_error("value type does not match variable type");

    llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
        *TheModule,
        getLLVMType(type->getType()),
        false,
        llvm::GlobalValue::ExternalLinkage,
        right,
        name);
    add_symbol(name, new descriptor(VAR_GLOBAL, globalVar));
    return nullptr;
  }
};

class MethodAST : public decafAST {
  string name;
  TypeAST *return_type;
  decafStmtList *param_list;
  MethodBlockAST *block;
  llvm::Function *func;

public:
  MethodAST(string n, TypeAST *r, decafStmtList *p, MethodBlockAST *b)
      : name(n), return_type(r), param_list(p), block(b) {}
  ~MethodAST() {
    delete return_type; return_type = nullptr;
    delete param_list; param_list = nullptr;
    delete block; block = nullptr;
  }
  string str() {
    return string("Method(") + name + "," + getString(return_type) + "," +
           getString(param_list) + "," + getString(block) + ")";
  }
  llvm::Value *Codegen_sym() {
    if (name == "main" && param_list->get_stmts().size() > 0)
      throw_error("main method cannot have parameters");

    llvm::Type *returnTy = getLLVMType(this->return_type->getType());
    std::vector<llvm::Type *> args;

    for (auto &arg : param_list->get_stmts()) {
      VarDefAST *var_def = (VarDefAST *)arg;
      args.push_back(getLLVMType(var_def->getType()));
    }

    llvm::Function *func = llvm::Function::Create(
        llvm::FunctionType::get(returnTy, args, false),
        llvm::Function::ExternalLinkage,
        name,
        TheModule);

    add_symbol(name, new descriptor(METHOD, func));
    this->func = func;
    return nullptr;
  }

  llvm::Value *Codegen() {
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "func", func);
    Builder.SetInsertPoint(BB);

    scope_enter();
    auto params = param_list->get_stmts();
    auto it = params.begin();
    for (auto &arg : func->args()) {
      VarDefAST *var_def = (VarDefAST *)*it++;
      arg.setName(var_def->getName().c_str());
      llvm::AllocaInst *alloca = Builder.CreateAlloca(arg.getType(), 0, var_def->getName().c_str());
      Builder.CreateStore(&arg, alloca);
      add_symbol(var_def->getName(), new descriptor(VAR_LOCAL, alloca));
    }

    block->Codegen();

    if (!Builder.GetInsertBlock()->getTerminator()) {
      if (getLLVMType(this->return_type->getType())->isVoidTy()) {
        Builder.CreateRetVoid();
      } else if (getLLVMType(this->return_type->getType())->isIntegerTy(32)) {
        Builder.CreateRet(Builder.getInt32(0));
      } else if (getLLVMType(this->return_type->getType())->isIntegerTy(1)) {
        Builder.CreateRet(Builder.getInt1(1));
      }
    }

    scope_exit();
    llvm::verifyFunction(*func);
    return nullptr;
  }
};

class ExternFunctionAST : public decafAST {
  string name;
  TypeAST *return_type;
  decafStmtList *type_list;

public:
  ExternFunctionAST(string n, TypeAST *r, decafStmtList *t)
      : name(n), return_type(r), type_list(t) {}
  ~ExternFunctionAST() {
    delete return_type; return_type = nullptr;
    delete type_list; type_list = nullptr;
  }
  string str() {
    return string("ExternFunction(") +
           name + "," + getString(return_type) + "," + getString(type_list) + ")";
  }
  llvm::Value *Codegen() {
    llvm::Type *returnTy = getLLVMType(return_type->getType());
    vector<llvm::Type *> args;

    for (auto &type : type_list->get_stmts()) {
      VarDefAST *var_def = (VarDefAST *)type;
      args.push_back(getLLVMType(var_def->getType()));
    }

    llvm::Function *func = llvm::Function::Create(
        llvm::FunctionType::get(returnTy, args, false),
        llvm::Function::ExternalLinkage,
        name,
        TheModule);

    add_symbol(name, new descriptor(METHOD, func));
    llvm::verifyFunction(*func);
    return nullptr;
  }
};

class PackageAST : public decafAST {
  string Name;
  decafStmtList *FieldDeclList;
  decafStmtList *MethodDeclList;

public:
  PackageAST(string name, decafStmtList *fieldlist, decafStmtList *methodlist)
      : Name(name), FieldDeclList(fieldlist), MethodDeclList(methodlist) {}
  ~PackageAST() {
      delete FieldDeclList; FieldDeclList = nullptr;
      delete MethodDeclList; MethodDeclList = nullptr;
  }
  string str() {
    return string("Package") + "(" +
           Name + "," + getString(FieldDeclList) + "," + getString(MethodDeclList) + ")";
  }
  llvm::Value *Codegen() {
    scope_enter();
    llvm::Value *val = NULL;
    TheModule->setModuleIdentifier(llvm::StringRef(Name));
    if (NULL != FieldDeclList) {
      val = FieldDeclList->Codegen();
    }
    if (NULL != MethodDeclList) {
      for (auto &stmt : MethodDeclList->get_stmts()) {
        MethodAST *method = (MethodAST *)stmt;
        method->Codegen_sym();
      }
      if (!access_symtbl("main"))
        throw_error("main method is missing");
      val = MethodDeclList->Codegen();
    }
    scope_exit();
    // Q: should we enter the class name into the symbol table?
    return val;
  }
};

/// ProgramAST - the decaf program
class ProgramAST : public decafAST {
  decafStmtList *ExternList;
  PackageAST *PackageDef;

public:
  ProgramAST(decafStmtList *externs, PackageAST *c) : ExternList(externs), PackageDef(c) {}
  ~ProgramAST() {
      delete ExternList; ExternList = nullptr;
      delete PackageDef; PackageDef = nullptr;
  }
  string str() { 
    return string("Program") + "(" +
           getString(ExternList) + "," + getString(PackageDef) + ")";
  }
  llvm::Value *Codegen() {
    scope_enter();
    llvm::Value *val = NULL;
    if (NULL != ExternList) {
      val = ExternList->Codegen();
    }
    if (NULL != PackageDef) {
      val = PackageDef->Codegen();
    }
    else {
      throw runtime_error("no package definition in decaf program");
    }
    scope_exit();
    return val;
  }
};