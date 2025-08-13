%{
#include "default-defs.h"
#include "decafcomp.tab.h"
#include <cstring>
#include <string>
#include <sstream>
#include <iostream>

using namespace std;

int lineno = 1;
int tokenpos = 1;

void addLength(char *s) {
  tokenpos += strlen(s);
}

%}

letter                     [A-Za-z_]
decimal_digit              [0-9]
hex_digit                  [0-9A-Fa-f]
digit                      [0-9]
all_char                   [\x07-\x0D\x20-\7E]
char                       [\x07-\x09\x0B-\x0D\x20-\x21\x23-\x5B\x5D-\x7E]
char_lit_chars             [\x07-\x0D\x20-\x26\x28-\x5B\x5D-\x7E]
char_no_n1                 [\x07-\x09\x0B-\x0D\x20-\x7E]

comment                    \/\/{char_no_n1}*\n
whitespace                 [\n\r\t\v\f ]+
identifier                 {letter}({letter}|{digit})*
decimal_lit                {decimal_digit}+
hex_lit                    0[xX]{hex_digit}+
int_lit                    {decimal_lit}|{hex_lit}
escaped_char               \\[nrtvfab\\'"]
char_lit                   '({char_lit_chars}|{escaped_char})'
string_lit                 \"({char}|{escaped_char})*\"

unknown_escape             \"({char}|{escaped_char}|\\[^nrtvfab\\'"])*\"
newline_string_lit         \"({char}|{escaped_char}|\n)*\"
unterminated_string_lit    \"({char}|{escaped_char})*
long_char_lit              '({char_lit_chars}|{escaped_char})+'
unterminated_char_lit      '({char_lit_chars}|{escaped_char})
empty_char_lit             ''

%%
  /*
    Pattern definitions for all tokens
  */
  
&&            { addLength(yytext); return T_AND; }
=             { addLength(yytext); return T_ASSIGN; }
bool          { addLength(yytext); return T_BOOLTYPE; }
break         { addLength(yytext); return T_BREAK; }
,             { addLength(yytext); return T_COMMA; }
continue      { addLength(yytext); return T_CONTINUE; }
\/            { addLength(yytext); return T_DIV; }
\.            { addLength(yytext); return T_DOT; }
else          { addLength(yytext); return T_ELSE; }
==            { addLength(yytext); return T_EQ; }
extern        { addLength(yytext); return T_EXTERN; }
false         { addLength(yytext); return T_FALSE; }
for           { addLength(yytext); return T_FOR; }
func          { addLength(yytext); return T_FUNC; }
>=            { addLength(yytext); return T_GEQ; }
>             { addLength(yytext); return T_GT; }
if            { addLength(yytext); return T_IF; }
int           { addLength(yytext); return T_INTTYPE; }
\{            { addLength(yytext); return T_LCB; }
\<\<          { addLength(yytext); return T_LEFTSHIFT; }
\<=           { addLength(yytext); return T_LEQ; }
\(            { addLength(yytext); return T_LPAREN; }
\[            { addLength(yytext); return T_LSB; }
\<            { addLength(yytext); return T_LT; }
-             { addLength(yytext); return T_MINUS; }
%             { addLength(yytext); return T_MOD; }
\*            { addLength(yytext); return T_MULT; }
!=            { addLength(yytext); return T_NEQ; }
!             { addLength(yytext); return T_NOT; }
null          { addLength(yytext); return T_NULL; }
\|\|          { addLength(yytext); return T_OR; }
package       { addLength(yytext); return T_PACKAGE; }
\+            { addLength(yytext); return T_PLUS; }
\}            { addLength(yytext); return T_RCB; }
return        { addLength(yytext); return T_RETURN; }
>>            { addLength(yytext); return T_RIGHTSHIFT; }
\)            { addLength(yytext); return T_RPAREN; }
\]            { addLength(yytext); return T_RSB; }
;             { addLength(yytext); return T_SEMICOLON; }
string        { addLength(yytext); return T_STRINGTYPE; }
true          { addLength(yytext); return T_TRUE; }
var           { addLength(yytext); return T_VAR; }
void          { addLength(yytext); return T_VOID; }
while         { addLength(yytext); return T_WHILE; }

{int_lit}       { addLength(yytext); yylval.sval = new string(yytext); return T_INTCONSTANT; }
{char_lit}      { addLength(yytext); yylval.sval = new string(yytext); return T_CHARCONSTANT; }
{string_lit}    { addLength(yytext); yylval.sval = new string(yytext); return T_STRINGCONSTANT; }
{identifier}    { addLength(yytext); yylval.sval = new string(yytext); return T_ID; }
{comment}       { lineno += 1; tokenpos = 1; }
{whitespace}    { for (char c : string(yytext)) {
                    if (c == '\n') {
                      lineno += 1; tokenpos = 1;
                    } else {
                      tokenpos += 1;  
                    } 
                  }
                } 

{unknown_escape}              { cerr << "Error: unknown escape sequence in string constant" << endl; return -1; }
{newline_string_lit}          { cerr << "Error: newline in string constant" << endl; return -1; }
{unterminated_string_lit}     { cerr << "Error: string constant is missing closing delimiter" << endl; return -1; }
{long_char_lit}               { cerr << "Error: char constant length is greater than one" << endl; return -1; }
{unterminated_char_lit}       { cerr << "Error: unterminated char constant" << endl; return -1; }
{empty_char_lit}              { cerr << "Error: char constant has zero width" << endl; return -1; }
.                             { cerr << "Error: unexpected character in input" << endl; return -1; }

%%

int yyerror(const char *s) {
  cerr << s << " on line " << lineno << ", position " << tokenpos << endl;
  return 1;
}
