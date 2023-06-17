#ifndef __MML_TARGETS_POSTFIX_WRITER_H__
#define __MML_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <set>
#include <sstream>
#include <stack>
#include <cdk/emitters/basic_postfix_emitter.h>

namespace mml {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<mml::symbol> &_symtab;

    std::set<std::string> _external_functions;
    std::set<std::string> _symbols_to_declare; // TDDO change

    std::vector<std::shared_ptr<mml::symbol>> _func_symbols_stack; // for keeping track of nested functions
    std::vector<std::string> _return_labels;

    bool _insideFunctionBody, _insideFunctionArgs;
    bool _returnSeen;
    int _offset;
    std::string _extern_label; 
    std::string _function_label; 
    std::vector<int> _whileCondStack, _whileEndStack; // for nested whiles

    // code generation
    cdk::basic_postfix_emitter &_pf;
    int _lbl;

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<mml::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _insideFunctionBody(false), _insideFunctionArgs(false),
        _returnSeen(false), _offset(0), _pf(pf), _lbl(0) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  protected:
    void initialize_variable(cdk::expression_node * const node, int lvl, std::shared_ptr<mml::symbol> const symbol);

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

} // mml

#endif
