#ifndef __MML_AST_NEXT_NODE_H__
#define __MML_AST_NEXT_NODE_H__

#include <cdk/ast/integer_node.h>

namespace mml {

  /**
   * Class for describing next nodes.
   */
  class next_node: public cdk::basic_node {
    int _val;

  public:
    inline next_node(int lineno, int val) :
        cdk::basic_node(lineno), _val(val) {
    }

  public:
    inline int val() {
      return _val;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_next_node(this, level);
    }

  };

} // mml

#endif
