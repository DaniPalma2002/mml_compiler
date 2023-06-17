#ifndef __MML_AST_STOP_NODE_H__
#define __MML_AST_STOP_NODE_H__

#include <cdk/ast/integer_node.h>

namespace mml {

  /**
   * Class for describing stop nodes.
   */
  class stop_node: public cdk::basic_node {
    int _val;

  public:
    inline stop_node(int lineno, int val) :
        cdk::basic_node(lineno), _val(val) {
    }

  public:
    inline int val() {
      return _val;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_stop_node(this, level);
    }

  };

} // mml

#endif
