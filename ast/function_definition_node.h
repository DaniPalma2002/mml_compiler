#ifndef __MML_AST_FUNCTION_DEFINITION_H__
#define __MML_AST_FUNCTION_DEFINITION_H__

#include <memory>
#include <cdk/ast/typed_node.h>
#include <cdk/ast/sequence_node.h>
#include "ast/block_node.h"

namespace mml {

  /**
   * Class for describing function definitions.
   */
  class function_definition_node: public cdk::expression_node {
    cdk::sequence_node *_arguments;
    block_node *_block;
    bool _is_main = false;
    std::shared_ptr<cdk::basic_type> _outputType;

  public:

    // void type
    function_definition_node(int lineno, cdk::sequence_node *arguments, mml::block_node *block, bool is_main) :
        cdk::expression_node(lineno), 
        _arguments(arguments), 
        _block(block),
        _is_main(is_main),
        _outputType(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
    
    function_definition_node(int lineno, cdk::sequence_node *arguments, 
                              std::shared_ptr<cdk::basic_type> funType, mml::block_node *block, bool is_main) :
        cdk::expression_node(lineno), 
        _arguments(arguments), 
        _block(block),
        _is_main(is_main),
        _outputType(funType) {} 

  public:
    cdk::sequence_node* arguments() {
      return _arguments;
    }
    cdk::typed_node* argument(size_t ax) {
      return dynamic_cast<cdk::typed_node*>(_arguments->node(ax));
    }
    block_node* block() {
      return _block;
    }
    bool is_main() {
      return _is_main;
    }
    std::shared_ptr<cdk::basic_type> outputType() {
      return _outputType;
    }
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_definition_node(this, level);
    }

  };

}//mml

#endif