%{
#include <iostream>
#include <ostream>
#include <string>
#include <vector>
#include <cstdlib>

#include "default-defs.h"
#include "decafcomp.cc"

using namespace std;

int yylex(void);
int yyerror(char *); 

// print AST?
bool printAST = false;

string escapeCharToString(const string &s) {
    if (s[1] == '\\') {
        char c = s[2];
        switch (c) {
            case 'n': return to_string('\n');
            case 'r': return to_string('\r');
            case 't': return to_string('\t');
            case 'v': return to_string('\v');
            case 'f': return to_string('\f');
            case 'a': return to_string('\a');
            case 'b': return to_string('\b');
            case '\\': return to_string('\\');
            case '\'': return to_string('\'');
            case '\"': return to_string('\"');
        }
    }
    return to_string(s[1]);
}

%}

%define parse.error verbose

%union{
    class decafAST *ast;
    std::string *sval;
    std::vector<std::string> *vec;
 }

%token T_AND T_ASSIGN T_BOOLTYPE T_BREAK T_COMMA T_CONTINUE T_DIV T_DOT T_ELSE T_EQ T_EXTERN 
%token T_FALSE T_FOR T_FUNC T_GEQ T_GT T_IF T_INTTYPE T_LCB T_LEFTSHIFT T_LEQ T_LPAREN T_LSB 
%token T_LT T_MINUS T_MOD T_MULT T_NEQ T_NOT T_NULL T_OR T_PACKAGE T_PLUS T_RCB T_RETURN 
%token T_RIGHTSHIFT T_RPAREN T_RSB T_SEMICOLON T_STRINGTYPE T_TRUE T_VAR T_VOID T_WHILE
%token <sval> T_CHARCONSTANT T_STRINGCONSTANT T_INTCONSTANT T_ID

%type <ast> extern_list extern_defn extern_type_list decafpackage field_list field_decl
%type <ast> method_list method_decl id_type_list method_block block var_list var_decl 
%type <ast> statement_list statement assign method_call method_arg_list method_arg else 
%type <ast> assign_list return expr extern_type type method_type constant
%type <vec> id_list

%left T_OR
%left T_AND
%left T_EQ T_NEQ T_LT T_LEQ T_GT T_GEQ
%left T_PLUS T_MINUS
%left T_MULT T_DIV T_MOD T_LEFTSHIFT T_RIGHTSHIFT
%right UnaryNot
%right UnaryMinus

%%

start: program

program: extern_list decafpackage
    { 
        ProgramAST *prog = new ProgramAST((decafStmtList *)$1, (PackageAST *)$2);
        prog->lineno = lineno;
        if (printAST) {
         cout << getString(prog) << endl;
        }
        try {
            prog->Codegen();
        } 
        catch (std::runtime_error &e) {
            cerr << "semantic error: " << e.what() << endl;
            //cout << prog->str() << endl; 
            exit(EXIT_FAILURE);
        }
        delete prog;
    }

extern_list: /* extern_list can be empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    | extern_list extern_defn 
    { 
        decafStmtList *slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; 
    }

extern_defn: T_EXTERN T_FUNC T_ID T_LPAREN extern_type_list T_RPAREN method_type T_SEMICOLON
    { 
        ExternFunctionAST *node = new ExternFunctionAST(
                                            *$3, (TypeAST *)$7, (decafStmtList *)$5);
        node->lineno = lineno;
        delete $3;
        $$ = node; 
    }

extern_type_list: { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    | extern_type 
    { 
        decafStmtList* slist = new decafStmtList(); slist->push_back($1); $$ = slist;  
    }
    | extern_type_list T_COMMA extern_type 
    { 
        decafStmtList* slist = (decafStmtList *)$1; slist->push_back($3); $$ = slist; 
    }

decafpackage: T_PACKAGE T_ID T_LCB field_list method_list T_RCB
    { 
        PackageAST *node = new PackageAST(*$2, (decafStmtList *)$4, (decafStmtList *)$5); 
        node->lineno = lineno;
        delete $2;
        $$ = node; 
    }

field_list: { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    | field_list field_decl
    {
        decafStmtList *a = (decafStmtList *)$1;
        decafStmtList *b = (decafStmtList *)$2;
        a->merge(b);
        delete b;
        $$ = a;
    }

field_decl: T_VAR id_list type T_SEMICOLON 
    {
        decafStmtList *slist = new decafStmtList();
        for (auto it = $2->begin(); it != $2->end(); ++it) {
            TypeAST *type_node = new TypeAST(*(TypeAST *)$3);
            FieldSizeAST *size_node = new FieldSizeAST(1, false);
            FieldDeclAST *node = new FieldDeclAST(*it, type_node, size_node);
            type_node->lineno = lineno; size_node->lineno = lineno; node->lineno = lineno;
            slist->push_back(node);
        }
        delete $2; delete $3;
        $$ = slist;
    }
    | T_VAR id_list T_LSB T_INTCONSTANT T_RSB type T_SEMICOLON 
    { 
        decafStmtList *slist = new decafStmtList();
        int size;
        string *s = $4;
        if (s->find("0x") == 0 || s->find("0X") == 0) {
            size = stoi(*s, nullptr, 16);
        } else {
            size = stoi(*s);
        }
        for (auto it = $2->begin(); it != $2->end(); ++it) {
            TypeAST *type_node = new TypeAST(*(TypeAST *)$6);
            FieldSizeAST *size_node = new FieldSizeAST(size, true);
            FieldDeclAST *node = new FieldDeclAST(*it, type_node, size_node);
            type_node->lineno = lineno; size_node->lineno = lineno; node->lineno = lineno;
            slist->push_back(node);
        }
        delete $2; delete $6;
        $$ = slist;
    }
    | T_VAR id_list type T_ASSIGN constant T_SEMICOLON
    { 
        if ($2->size() != 1) {
            yyerror("Invalid number of identifiers");
            YYABORT;
        }
        decafStmtList *slist = new decafStmtList();
        AssignGlobalVarAST *node = new AssignGlobalVarAST((*$2)[0], (TypeAST *)$3, (decafAST *)$5);
        node->lineno = lineno;
        slist->push_back(node);
        delete $2;
        $$ = slist;
    }

id_list: T_ID 
    {
        vector<string> *list = new vector<string>();
        list->push_back(*$1);
        delete $1;
        $$ = list;
    }
    | id_list T_COMMA T_ID
    {
        vector<string> *list = $1; 
        list->push_back(*$3); 
        delete $3;
        $$ = list; 
    }

method_list: { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    | method_list method_decl 
    { 
        decafStmtList *slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; 
    }

method_decl: T_FUNC T_ID T_LPAREN id_type_list T_RPAREN method_type method_block 
    {
        MethodAST *node = new MethodAST(
            *$2, (TypeAST *)$6, (decafStmtList *)$4, (MethodBlockAST *)$7);
        node->lineno = lineno;
        delete $2;
        $$ = node;
    }

id_type_list: { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    | T_ID type 
    {
        decafStmtList *slist = new decafStmtList();
        VarDefAST *node = new VarDefAST(*($1), (TypeAST *)$2);
        node->lineno = lineno;
        delete $1;
        slist->push_back(node);
        $$ = slist;
    }
    | id_type_list T_COMMA T_ID type 
    {  
        VarDefAST *node = new VarDefAST(*($3), (TypeAST *)$4);
        node->lineno = lineno;
        delete $3;
        decafStmtList *slist = (decafStmtList *)$1;
        slist->push_back(node);
        $$ = slist;
    }

method_block: T_LCB var_list statement_list T_RCB 
    {
        MethodBlockAST *node = new MethodBlockAST((decafStmtList *)$2, (decafStmtList *)$3);
        node->lineno = lineno;
        $$ = node;
    }

block: T_LCB var_list statement_list T_RCB 
    {
        BlockAST *node = new BlockAST((decafStmtList *)$2, (decafStmtList *)$3);
        node->lineno = lineno;
        $$ = node;
    }

var_list: { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    | var_list var_decl 
    {
        decafStmtList *a = (decafStmtList *)$1;
        decafStmtList *b = (decafStmtList *)$2;
        a->merge(b);
        delete b;
        $$ = a;
    }

var_decl: T_VAR id_list type T_SEMICOLON 
    { 
        decafStmtList *slist = new decafStmtList();
        for (auto it = $2->begin(); it != $2->end(); ++it) {
            TypeAST *type_node = new TypeAST(*(TypeAST *)$3);
            VarDefAST *node = new VarDefAST(*it, type_node);
            type_node->lineno = lineno; node->lineno = lineno;
            slist->push_back(node);
        }
        delete $2; delete $3;
        $$ = slist;
    }

statement_list: { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    | statement_list statement 
    {
        decafStmtList *slist = (decafStmtList *)$1;
        slist->push_back($2);
        $$ = slist;
    }

statement: block { $$ = $1; }
    | assign T_SEMICOLON { $$ = $1; }
    | method_call T_SEMICOLON { $$ = $1; }
    | T_IF T_LPAREN expr T_RPAREN block else
    {
        IfStmtAST *node = new IfStmtAST((decafAST *)$3, (BlockAST *)$5, (BlockAST *)$6);
        node->lineno = lineno;
        $$ = node;
    }
    | T_WHILE T_LPAREN expr T_RPAREN block
    {
        WhileStmtAST *node = new WhileStmtAST((decafAST *)$3, (BlockAST *)$5);
        node->lineno = lineno;
        $$ = node;
    }
    | T_FOR T_LPAREN assign_list T_SEMICOLON expr T_SEMICOLON assign_list T_RPAREN block
    {
        ForStmtAST *node = new ForStmtAST(
            (decafStmtList *)$3, (decafAST *)$5, (decafStmtList *)$7, (BlockAST *)$9);
        node->lineno = lineno;
        $$ = node;
    }
    | return { $$ = $1; }
    | T_BREAK T_SEMICOLON 
    { 
        BreakStmtAST *node = new BreakStmtAST(); 
        node->lineno = lineno; 
        $$ = node; 
    }
    | T_CONTINUE T_SEMICOLON 
    { 
        ContinueStmtAST *node = new ContinueStmtAST(); 
        node->lineno = lineno; 
        $$ = node;
    }

assign: T_ID T_ASSIGN expr
    {
        AssignVarAST *node = new AssignVarAST(*$1, (decafAST *)$3);
        node->lineno = lineno; 
        delete $1;
        $$ = node;
    }
    | T_ID T_LSB expr T_RSB T_ASSIGN expr
    {
        AssignArrayLocAST *node = new AssignArrayLocAST(*$1, (decafAST *)$3, (decafAST *)$6);
        node->lineno = lineno; 
        delete $1;
        $$ = node;
    }

method_call: T_ID T_LPAREN method_arg_list T_RPAREN 
    {
        MethodCallAST *node = new MethodCallAST(*$1, (decafStmtList *)$3);
        node->lineno = lineno; 
        delete $1;
        $$ = node;
    }

method_arg_list: { decafStmtList *slist = new decafStmtList(); $$ = slist;}
    | method_arg 
    { 
        decafStmtList* slist = new decafStmtList(); 
        slist->push_back($1);
        $$ = slist; 
    }
    | method_arg_list T_COMMA method_arg 
    { 
        decafStmtList *slist = (decafStmtList *)$1;
        slist->push_back($3);
        $$ = slist; 
    }

method_arg: expr { $$ = $1; }
    | T_STRINGCONSTANT 
    {
        StringConstantAST *node = new StringConstantAST(*$1);
        node->lineno = lineno; 
        delete $1;
        $$ = node;
    }

else: {$$ = nullptr; }
    | T_ELSE block 
    {
        $$ = $2;
    }

assign_list: assign
    {
        decafStmtList *slist = new decafStmtList();
        slist->push_back($1);
        $$ = slist;
    }
    | assign_list T_SEMICOLON assign
    {
        decafStmtList *slist = (decafStmtList *)$1;
        slist->push_back($3);
        $$ = slist;
    }

return: T_RETURN T_SEMICOLON
    { 
        ReturnStmtAST *node = new ReturnStmtAST(nullptr);
        node->lineno = lineno; 
        $$ = node;
    }
    | T_RETURN T_LPAREN T_RPAREN T_SEMICOLON
    {
        ReturnStmtAST *node = new ReturnStmtAST(nullptr);
        node->lineno = lineno; 
        $$ = node;
    }
    | T_RETURN T_LPAREN expr T_RPAREN T_SEMICOLON
    {
        ReturnStmtAST *node = new ReturnStmtAST((decafAST *)$3);
        node->lineno = lineno; 
        $$ = node;
    }

expr: T_ID        { $$ = new VariableExprAST(*$1); $$->lineno = lineno; delete $1; }
    | method_call { $$ = $1; }
    | constant    { $$ = $1; }
    | expr T_PLUS expr { $$ = new BinaryExprAST(PLUS, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_MINUS expr { $$ = new BinaryExprAST(MINUS, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_MULT expr { $$ = new BinaryExprAST(MULT, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_DIV expr { $$ = new BinaryExprAST(DIV, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_LEFTSHIFT expr { $$ = new BinaryExprAST(LEFTSHIFT, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_RIGHTSHIFT expr { $$ = new BinaryExprAST(RIGHTSHIFT, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_MOD expr { $$ = new BinaryExprAST(MOD, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_EQ expr { $$ = new BinaryExprAST(EQ, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_NEQ expr { $$ = new BinaryExprAST(NEQ, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_LT expr { $$ = new BinaryExprAST(LT, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_LEQ expr { $$ = new BinaryExprAST(LEQ, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_GT expr { $$ = new BinaryExprAST(GT, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_GEQ expr { $$ = new BinaryExprAST(GEQ, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_AND expr { $$ = new BinaryExprAST(AND, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | expr T_OR expr { $$ = new BinaryExprAST(OR, (decafAST *)$1, (decafAST *)$3); $$->lineno = lineno; }
    | T_MINUS expr %prec UnaryMinus { $$ = new UnaryExprAST(UNARYMINUS, (decafAST *)$2); $$->lineno = lineno; }
    | T_NOT expr %prec UnaryNot { $$ = new UnaryExprAST(NOT, (decafAST *)$2); $$->lineno = lineno; }
    | T_LPAREN expr T_RPAREN { $$ = $2; }
    | T_ID T_LSB expr T_RSB { $$ = new ArrayLocExprAST(*$1, (decafAST *)$3); delete $1; $$->lineno = lineno; }

extern_type: T_STRINGTYPE { TypeAST *node = new TypeAST(STRINGTYPE); 
                            $$ = new VarDefAST("", node); 
                            node->lineno = lineno; $$->lineno = lineno; } 
    | type                { $$ = new VarDefAST("", (TypeAST *)$1); $$->lineno = lineno; }

type: T_INTTYPE  { $$ = new TypeAST(INTTYPE); $$->lineno = lineno; }
    | T_BOOLTYPE { $$ = new TypeAST(BOOLTYPE); $$->lineno = lineno; }

method_type: T_VOID     { $$ = new TypeAST(VOIDTYPE); $$->lineno = lineno; }
    | type              { $$ = (TypeAST *)$1; }

constant: T_INTCONSTANT { $$ = new NumberExprAST(*$1); delete $1; $$->lineno = lineno; }
    | T_CHARCONSTANT    { $$ = new NumberExprAST(escapeCharToString(*$1)); delete $1; $$->lineno = lineno; }
    | T_TRUE            { $$ = new BoolExprAST(1); $$->lineno = lineno; }
    | T_FALSE           { $$ = new BoolExprAST(0); $$->lineno = lineno; }

%%

int main() {
  llvm::LLVMContext &Context = TheContext;
  TheModule = new llvm::Module("DecafComp", Context);
  int retval = yyparse();
  TheModule->print(llvm::errs(), nullptr);
  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
}
