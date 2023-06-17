#ifndef __MML_AST_VARIABLE_DECLARATION_NODE_H__
#define __MML_AST_VARIABLE_DECLARATION_NODE_H__

#include <cdk/ast/typed_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/types/basic_type.h>

namespace mml {

  /**
   * Class for describing variable declaration nodes.
   */
  class variable_declaration_node: public cdk::typed_node {
    int _qualifier;
    std::string _name;
    cdk::expression_node *_value;

  public:
    variable_declaration_node(int lineno, int qualifier, std::shared_ptr<cdk::basic_type> varType, const std::string &name, cdk::expression_node *value) :
        cdk::typed_node(lineno), _qualifier(qualifier), _name(name), _value(value) 
    {
      type(varType);
    }

  public:
    int qualifier() {
      return _qualifier;
    }

    const std::string &name() const {
      return _name;
    }

    cdk::expression_node *value() {
      return _value;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_variable_declaration_node(this, level);
    }

  };

}//mml

#endif