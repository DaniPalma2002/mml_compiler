#ifndef __MML_AST_RETURN_NODE_H__
#define __MML_AST_RETURN_NODE_H__

#include <cdk/ast/expression_node.h>

namespace mml {

  /**
   * Class for describing return nodes.
   */
  class return_node: public cdk::basic_node {
    cdk::expression_node *_expr;

  public:
    inline return_node(int lineno, cdk::expression_node *expr) :
        cdk::basic_node(lineno), _expr(expr) {
    }

  public:
    inline cdk::expression_node *expr() {
      return _expr;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_return_node(this, level);
    }

  };

} // mml

#endif
