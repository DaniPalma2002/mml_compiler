%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                  i;	/* integer value */
  double               d;  /* double value */
  std::string          *s;	/* symbol name or string literal */
  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;

  mml::block_node *block;
  std::vector<std::shared_ptr<cdk::basic_type>> *vectypes;
};

%token <i> tINTEGER
%token <d> tREAL
%token <s> tIDENTIFIER tSTRING

%token tWHILE tPRINT tPRINTLN tINPUT tBEGIN tEND
%token tTYPE_REAL tTYPE_INT tTYPE_STRING tTYPE_VOID tTYPE_AUTO
%token tFOREIGN tFORWARD tPUBLIC tPRIVATE
%token tNEXT tSTOP tRETURN tSIZEOF tNULL tARROW

%nonassoc tIFX
%nonassoc tIF
%nonassoc tELIF
%nonassoc tELSE

%right '='
%left tOR
%left tAND
%nonassoc '~'
%left tEQ tNE
%left tGE tLE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY
%nonassoc '[' '('

%type <node> cond_if elif
%type <sequence> program exprs
%type <expression> expr main_program fundef funcall
%type <lvalue> lval
%type <block> block block_main

%type<node> vardec instruction arg declaration
%type<sequence> vardecs opt_vardecs instructions opt_instructions args declarations opt_declarations

%type<s> string
%type<type> type type_auto function_type
%type<i> qualifier
%type<vectypes> types
%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

program : opt_declarations main_program { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
        ;

declaration  : qualifier type      tIDENTIFIER          ';'  { $$ = new mml::variable_declaration_node(LINE, $1, $2, *$3, nullptr); delete $3; }
             |           type      tIDENTIFIER          ';'  { $$ = new mml::variable_declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); delete $2; }
             | qualifier type      tIDENTIFIER '=' expr ';'  { $$ = new mml::variable_declaration_node(LINE, $1, $2, *$3, $5); delete $3; }
             |           type      tIDENTIFIER '=' expr ';'  { $$ = new mml::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $4); delete $2; }
             | qualifier type_auto tIDENTIFIER '=' expr ';'  { $$ = new mml::variable_declaration_node(LINE, $1, $2, *$3, $5); delete $3; }
             |           type_auto tIDENTIFIER '=' expr ';'  { $$ = new mml::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $4); delete $2; }
             | qualifier           tIDENTIFIER '=' expr ';'  { $$ = new mml::variable_declaration_node(LINE, $1, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$2, $4); delete $2; }
             ;
declarations : declaration              { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;
opt_declarations : /* empty */          { $$ = new cdk::sequence_node(LINE); }
                 | declarations         { $$ = $1; }
                 ;

main_program : tBEGIN block_main tEND   { $$ = new mml::function_definition_node(LINE, new cdk::sequence_node(LINE), $2, true); }
             | /* empty */              { $$ = nullptr; }
             ;

instruction      : expr ';'                                 { $$ = new mml::evaluation_node(LINE, $1); }
                 | exprs tPRINTLN                           { $$ = new mml::print_node(LINE, $1, true); }
                 | exprs tPRINT                             { $$ = new mml::print_node(LINE, $1, false); }
                 | tWHILE '(' expr ')' instruction          { $$ = new mml::while_node(LINE, $3, $5); }
                 | cond_if                                  { $$ = $1; }
                 | tSTOP ';'                                { $$ = new mml::stop_node(LINE, 1); }
                 | tSTOP tINTEGER ';'                       { $$ = new mml::stop_node(LINE, $2); }
                 | tNEXT ';'                                { $$ = new mml::next_node(LINE, 1); }
                 | tNEXT tINTEGER ';'                       { $$ = new mml::next_node(LINE, $2); }
                 | tRETURN expr ';'                         { $$ = new mml::return_node(LINE, $2); }
                 | block                                    { $$ = $1; }
                 ;
instructions     : instruction                              { $$ = new cdk::sequence_node(LINE, $1);     }
                 | instructions instruction                 { $$ = new cdk::sequence_node(LINE, $2, $1); }
                 ;
opt_instructions : /* empty */                              { $$ = new cdk::sequence_node(LINE); }
                 | instructions                             { $$ = $1; }
                 ;

expr      : tINTEGER                 { $$ = new cdk::integer_node(LINE, $1); }
          | tREAL                    { $$ = new cdk::double_node(LINE, $1); }
	  | string                   { $$ = new cdk::string_node(LINE, *$1); delete $1; }
          | tNULL                    { $$ = new mml::null_ptr_node(LINE); }
          | '+' expr %prec tUNARY    { $$ = new mml::identity_node(LINE, $2); }
          | '-' expr %prec tUNARY    { $$ = new cdk::neg_node(LINE, $2); }
          | '~' expr                 { $$ = new cdk::not_node(LINE, $2); }
          | expr '+' expr	           { $$ = new cdk::add_node(LINE, $1, $3); }
          | expr '-' expr	           { $$ = new cdk::sub_node(LINE, $1, $3); }
          | expr '*' expr	           { $$ = new cdk::mul_node(LINE, $1, $3); }
          | expr '/' expr	           { $$ = new cdk::div_node(LINE, $1, $3); }
          | expr '%' expr	           { $$ = new cdk::mod_node(LINE, $1, $3); }
          | expr '<' expr	           { $$ = new cdk::lt_node(LINE, $1, $3); }
          | expr '>' expr	           { $$ = new cdk::gt_node(LINE, $1, $3); }
          | expr tGE expr	           { $$ = new cdk::ge_node(LINE, $1, $3); }
          | expr tLE expr            { $$ = new cdk::le_node(LINE, $1, $3); }
          | expr tNE expr	           { $$ = new cdk::ne_node(LINE, $1, $3); }
          | expr tEQ expr	           { $$ = new cdk::eq_node(LINE, $1, $3); }
          | expr tAND expr	         { $$ = new cdk::and_node(LINE, $1, $3); }
          | expr tOR expr	           { $$ = new cdk::or_node(LINE, $1, $3); }
          | lval                     { $$ = new cdk::rvalue_node(LINE, $1); }
          | lval '=' expr            { $$ = new cdk::assignment_node(LINE, $1, $3); }
          | lval '?'                 { $$ = new mml::address_of_node(LINE, $1); }
          | '(' expr ')'             { $$ = $2; }
          | '[' expr ']'             { $$ = new mml::stack_alloc_node(LINE, $2); }
          | tINPUT                   { $$ = new mml::input_node(LINE); }
          | tSIZEOF '(' expr ')'     { $$ = new mml::sizeof_node(LINE, $3); }
          | fundef                   { $$ = $1; }
          | funcall                  { $$ = $1; }
          ;
exprs     : expr                     { $$ = new cdk::sequence_node(LINE, $1); }
          | exprs ',' expr           { $$ = new cdk::sequence_node(LINE, $3, $1); }
          ;

vardec      : type tIDENTIFIER                 { $$ = new mml::variable_declaration_node(LINE, tPUBLIC,  $1, *$2, nullptr); delete $2; }
            | type tIDENTIFIER '=' expr        { $$ = new mml::variable_declaration_node(LINE, tPUBLIC,  $1, *$2, $4); delete $2; }
            | type_auto tIDENTIFIER '=' expr   { $$ = new mml::variable_declaration_node(LINE, tPUBLIC,  $1, *$2, $4); delete $2; }
            ;
vardecs     : vardec ';'                       { $$ = new cdk::sequence_node(LINE, $1); }
            | vardecs vardec ';'               { $$ = new cdk::sequence_node(LINE, $2, $1); }
            ;
opt_vardecs : /* empty */                      { $$ = new cdk::sequence_node(LINE); }
            | vardecs                          { $$ = $1; }
            ;

qualifier : tPUBLIC   { $$ = tPUBLIC; }
          | tFORWARD  { $$ = tFORWARD; }
          | tFOREIGN  { $$ = tFOREIGN; }
          ;

fundef : '(' args ')' tARROW type  block    { $$ = new mml::function_definition_node(LINE, $2, $5, $6, false); }
       ;

funcall : expr '(' exprs ')'             { $$ = new mml::function_call_node(LINE, $1, $3); } 
        | expr '('       ')'             { $$ = new mml::function_call_node(LINE, $1); }     
        | '@' '(' ')'                    { $$ = new mml::function_call_node(LINE, nullptr); }     
        | '@' '(' exprs ')'              { $$ = new mml::function_call_node(LINE, nullptr, $3); }     
        | '(' fundef ')' '(' exprs ')'   { $$ = new mml::function_call_node(LINE, $2, $5); }     
        | '(' fundef ')' '('       ')'   { $$ = new mml::function_call_node(LINE, $2); }     
        ;

arg  : type tIDENTIFIER             { $$ = new mml::variable_declaration_node(LINE, tPUBLIC, $1, *$2, nullptr); delete $2; }
     | expr                         { $$ = $1; }
     ;
args : /* empty */                  { $$ = new cdk::sequence_node(LINE); }
     | arg                          { $$ = new cdk::sequence_node(LINE, $1); }
     | args ',' arg                 { $$ = new cdk::sequence_node(LINE, $3, $1); }
     ;

type      : tTYPE_INT                 { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
          | tTYPE_REAL                { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
          | tTYPE_STRING              { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
          | tTYPE_VOID                { $$ = cdk::primitive_type::create(4, cdk::TYPE_VOID); }
          | '[' type ']'              { $$ = cdk::reference_type::create(4, std::shared_ptr<cdk::basic_type>($2)); }
          | function_type             { $$ = $1; }
          ;
types     : type                      { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
          | types ',' type            { $$ = $1; $$->push_back($3); }
          ;

type_auto : tTYPE_AUTO                { $$ = cdk::primitive_type::create(0, cdk::TYPE_UNSPEC); }
          ;

function_type : type '<' types '>'     { $$ = cdk::functional_type::create(*$3, $1); delete $3; }
              | type '<' '>'           { $$ = cdk::functional_type::create($1); }
              ;

cond_if : tIF '(' expr ')' instruction %prec tIFX         { $$ = new mml::if_node(LINE, $3, $5); }
        | tIF '(' expr ')' instruction elif               { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
        ;
elif    : tELSE instruction                                  { $$ = $2; }
        | tELIF '(' expr ')' instruction %prec tIFX          { $$ = new mml::if_node(LINE, $3, $5); }
        | tELIF '(' expr ')' instruction elif                { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
        ;

block    : '{'opt_vardecs opt_instructions'}'     { $$ = new mml::block_node(LINE, $2, $3); }
         ;

block_main : opt_vardecs opt_instructions    { $$ = new mml::block_node(LINE, $1, $2); }
           ;

lval : tIDENTIFIER               { $$ = new cdk::variable_node(LINE, *$1); delete $1; }
     | expr '[' expr ']'         { $$ = new mml::pointer_index_node(LINE, $1, $3); }
     ;

string : tSTRING                       { $$ = $1; }
       | string tSTRING                { $$->append(*$2); delete $2; }
       ;
%%
