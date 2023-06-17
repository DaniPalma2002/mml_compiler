%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <iostream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

// don't change this
#define yyerror LexerError

int x_stoi(const std::string &str, int base, bool ascii) {
  try {
    int n = std::stoi(str, NULL, base);
    if (ascii && n > 255) {
      std::cerr << "base 8 out of range: " << str << std::endl;
      exit(1);
    }
    return n;
  } catch (const std::invalid_argument &err) {
    std::cerr << "invalid integer: " << str << std::endl;
    exit(1);
  } catch (const std::out_of_range &err) {
    std::cerr << "integer out of range: " << str << std::endl;
    exit(1);
  }
}

double x_stod(const std::string &str) {
  try {
    return std::stod(str, NULL);
  } catch (const std::invalid_argument &err) {
    std::cerr << "invalid double: " << str << std::endl;
    exit(1);
  } catch (const std::out_of_range &err) {
    std::cerr << "double out of range: " << str << std::endl;
    exit(1);
  }
}

%}
%x X_COMMENT
%x X_STRING X_BACKSLASH X_STRING_IGNORE
%%
  yydebug=1; set_debug(1);

"//".*                 ; /* ignore comments */

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>"/*"        yy_push_state(X_COMMENT);
<X_COMMENT>.|\n        ; /* ignore comments */

">="                   return tGE; 
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE; 
"&&"                   return tAND;
"||"                   return tOR;

"int"                  return tTYPE_INT;
"double"               return tTYPE_REAL;
"string"               return tTYPE_STRING;
"void"                 return tTYPE_VOID;
"auto"                 return tTYPE_AUTO;

"foreign"              return tFOREIGN;
"forward"              return tFORWARD;
"public"               return tPUBLIC;

"while"                return tWHILE; 
"if"                   return tIF; 
"elif"                 return tELIF;
"else"                 return tELSE; 
"!"                    return tPRINT; 
"!!"                   return tPRINTLN; 
"stop"                 return tSTOP;
"next"                 return tNEXT;
"return"               return tRETURN;
"begin"                return tBEGIN; 
"end"                  return tEND; 
"input"                return tINPUT;
"null"                 return tNULL;
"sizeof"               return tSIZEOF;
"->"                   return tARROW;

[A-Za-z][A-Za-z0-9]*             yylval.s = new std::string(yytext); return tIDENTIFIER;

\"                                yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\\                      yy_push_state(X_BACKSLASH);  
<X_STRING>\"                      yy_pop_state(); return tSTRING;
<X_STRING>\0                      yyerror("nullbyte in string");
<X_STRING>.                       *yylval.s += yytext;
<X_STRING>\n                      yyerror("newline in string");

<X_BACKSLASH>n                    *yylval.s += '\n'; yy_pop_state();
<X_BACKSLASH>t                    *yylval.s += '\t'; yy_pop_state();
<X_BACKSLASH>r                    *yylval.s += '\r'; yy_pop_state();
<X_BACKSLASH>0                    yy_pop_state(); yy_push_state(X_STRING_IGNORE);
<X_BACKSLASH>\"                   *yylval.s += yytext + 1; yy_pop_state();
<X_BACKSLASH>\\                   *yylval.s += yytext + 1; yy_pop_state();
<X_BACKSLASH>[0-7]{1,3}           *yylval.s += (char)(unsigned char) x_stoi(yytext, 8, true); yy_pop_state();

<X_STRING_IGNORE>\0               yyerror("nullbyte in string");
<X_STRING_IGNORE>\"               yy_pop_state(); yy_pop_state(); return tSTRING;
<X_STRING_IGNORE>.                ; 
<X_STRING_IGNORE>\n               yyerror("newline in string");

[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?  yylval.d = x_stod(yytext); return tREAL;
[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?  yylval.d = x_stod(yytext); return tREAL;
[0-9]+([eE][+-]?[0-9]+)           yylval.d = x_stod(yytext); return tREAL;

0[0-7]+                           yylval.i = x_stoi(yytext, 8, false); return tINTEGER;
0[0-9]+                           yyerror("invalid octal number");
0|[1-9][0-9]*                     yylval.i = x_stoi(yytext, 10, false); return tINTEGER;

[-()<>=+*/%;{},?\[\]@~]         return *yytext;

[ \t\n]+                          ; /* ignore whitespace */

.                                 yyerror("Unknown character");

%%
