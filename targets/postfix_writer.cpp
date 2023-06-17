#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include <cdk/types/primitive_type.h>
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated

#include <mml_parser.tab.h>

#define ERROR(msg) { std::cerr << (msg) << std::endl; exit(1); }
//---------------------------------------------------------------------------

void mml::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}

void mml::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}

void mml::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  std::string lbl = mklbl(++_lbl);
  if (_insideFunctionBody) {
    _pf.CALL(lbl);
    _pf.TEXT();
    _pf.ALIGN();
    _pf.LABEL(lbl);
    _pf.START();

    _pf.DOUBLE(node->value());

    _pf.STFVAL64();
    _pf.LEAVE();
    _pf.RET();
    _pf.TEXT(_return_labels.back()); 
    _pf.LDFVAL64();
  } else {
    _pf.SDOUBLE(node->value());
  }
}

void mml::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.INT(0);
  _pf.EQ();
}

void mml::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::cout << "POSTFIX WRITER: AND NODE" << std::endl;
  std::string lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(lbl);
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(lbl);
}

void mml::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::string lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(lbl);
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(lbl);
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (_insideFunctionBody)
    _pf.INT(node->value()); // push an integer
  else
    _pf.SINT(node->value()); // push an integer
}

void mml::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  std::string lbl = mklbl(++_lbl);
  _pf.RODATA();
  _pf.ALIGN();
  _pf.LABEL(lbl);
  _pf.SSTRING(node->value());
  if (_insideFunctionBody) {
    _pf.TEXT(_return_labels.back());
    _pf.ADDR(lbl);
  } else {
    _pf.DATA();
    _pf.SADDR(lbl);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    auto referenced = cdk::reference_type::cast(node->right()->type())->referenced();
    _pf.INT(referenced->size());
    _pf.MUL();
  }

  node->right()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto referenced = cdk::reference_type::cast(node->left()->type())->referenced();
    _pf.INT(referenced->size());
    _pf.MUL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DADD();
  } else {
    _pf.ADD();
  }
}

void mml::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    auto referenced = cdk::reference_type::cast(node->right()->type())->referenced();
    _pf.INT(referenced->size());
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto referenced = cdk::reference_type::cast(node->right()->type())->referenced();
    _pf.INT(referenced->size());
    _pf.MUL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DSUB();
  } else {
    _pf.SUB();
  }
}

void mml::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}

void mml::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}

void mml::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}

void mml::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.LT();
}

void mml::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.LE();
}

void mml::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.GE();
}

void mml::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.GT();
}

void mml::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.NE();
}

void mml::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::cout << "POSTFIX WRITER: do_eq_node" << std::endl;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.EQ();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::cout << "POSTFIX WRITER: do_variable_node" << std::endl;
  const std::string &id = node->name();
  auto symbol = _symtab.find(id);
 
  if (symbol->is_foreign()) {
    _extern_label = symbol->name();
  } else if (symbol->offset() == 0) { // is global
    _pf.ADDR(symbol->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void mml::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  } else {
    if (_extern_label.empty()) {
      _pf.LDINT();
    }
  }
}

void mml::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::cout << "POSTFIX WRITER: do_assignment_node" << std::endl;
  node->rvalue()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      _pf.I2D();
    }
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }
  node->lvalue()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_evaluation_node(mml::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.TRASH(node->argument()->type()->size());
}

void mml::postfix_writer::do_print_node(mml::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::cout << "POSTFIX WRITER: do_print_node" << std::endl;
  for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ix));
    arg->accept(this, lvl); // expression to print
    if (arg->type() == 0) {
      ERROR("something went wrong");
    }
    if (arg->is_typed(cdk::TYPE_INT)) {
      _external_functions.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4); // trash int
    } else if (arg->is_typed(cdk::TYPE_DOUBLE)) {
      _external_functions.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8); // trash double
    } else if (arg->is_typed(cdk::TYPE_STRING)) {
      _external_functions.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4); // trash char pointer
    } else {
      ERROR("cannot print expression of unknown type");
    }
  }

  if (node->newline()) {
    _external_functions.insert("println");
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_while_node(mml::while_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _whileCondStack.push_back(++_lbl);
  _whileEndStack.push_back(++_lbl);

  _symtab.push(); // new context

  _pf.ALIGN();
  _pf.LABEL(mklbl(_whileCondStack.back()));
  node->condition()->accept(this, lvl + 2);
  _pf.JZ(mklbl(_whileEndStack.back()));

  node->block()->accept(this, lvl + 2);
  _pf.JMP(mklbl(_whileCondStack.back()));
  _pf.ALIGN();
  _pf.LABEL(mklbl(_whileEndStack.back()));

  _symtab.pop();
  
  _whileEndStack.pop_back();
  _whileCondStack.pop_back();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_node(mml::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_else_node(mml::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}

void mml::postfix_writer::do_input_node(mml::input_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->is_typed(cdk::TYPE_INT)) {
    _external_functions.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
  }
  else if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _external_functions.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
  } else {
     ERROR("CANNOT READ INPUT TYPE");
  }
}

void mml::postfix_writer::do_next_node(mml::next_node * const node, int lvl) {
  std::cout << "POSTFIX WRITER: do_next_node" << std::endl;

  if (_whileCondStack.size() != 0) {
    if (node->val() > 0 && _whileCondStack.size() >= (size_t)node->val()) {
        _pf.JMP(mklbl(_whileCondStack.at(_whileCondStack.size() - node->val())));
    } else {
      ERROR("next value bigger than while stack size or negative");
    }
  } else {
    ERROR("next statement not inside a while loop");
  }
}

void mml::postfix_writer::do_stop_node(mml::stop_node * const node, int lvl) {
  std::cout << "POSTFIX WRITER: do_stop_node" << std::endl;

  if (_whileEndStack.size() != 0) {
    if (node->val() > 0 && _whileEndStack.size() >= (size_t)node->val()) {
        _pf.JMP(mklbl(_whileEndStack.at(_whileEndStack.size() - node->val())));
    } else {
      ERROR("stop value bigger than while stack size or negative");
    }
  } else {
    ERROR("stop statement not inside a while loop");
  }
}

void mml::postfix_writer::do_return_node(mml::return_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::cout << "POSTFIX WRITER: do_return_node" << std::endl;
  if (_func_symbols_stack.size() == 0) {
    ERROR("return statement not inside a function");
    return;
  }
  
  std::cout << "func symbols stack: ";
  for (auto element : _func_symbols_stack) {
        std::cout << element->name() << "-" << element->is_main() << " ";
  } std::cout << std::endl;

  _returnSeen = true; // to check if its the last statement in the block
  auto current_function = _func_symbols_stack.back();
  auto outputType = cdk::functional_type::cast(current_function->type())->output(0);

  if (outputType->name() != cdk::TYPE_VOID) {
    node->expr()->accept(this, lvl + 2);
    if (outputType->name() == cdk::TYPE_INT) {
      if (!current_function->is_main()) {
        _pf.I2D();
        _pf.STFVAL64();
      } else {
        _pf.STFVAL32();
      }
    } else if (outputType->name() == cdk::TYPE_DOUBLE) {
      if (node->expr()->is_typed(cdk::TYPE_INT)) _pf.I2D();
      _pf.STFVAL64();
    } else if (outputType->name() == cdk::TYPE_STRING || outputType->name() == cdk::TYPE_POINTER || 
              outputType->name() == cdk::TYPE_FUNCTIONAL) {
      _pf.STFVAL32();
    } else {
      ERROR("unknown return type");
    }
  }
  _pf.LEAVE();
  _pf.RET();
}

void mml::postfix_writer::do_sizeof_node(mml::sizeof_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_insideFunctionBody)
    _pf.INT(node->argument()->type()->size());
  else 
    _pf.SINT(node->argument()->type()->size());
}

void mml::postfix_writer::do_block_node(mml::block_node * const node, int lvl) {
  _symtab.push(); // for block-local vars
  if (node->declarations()) node->declarations()->accept(this, lvl + 2);
  if (node->instructions()) node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

void mml::postfix_writer::do_function_definition_node(mml::function_definition_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->is_main()) {
    // generate the main function (RTS mandates that its name be "_main")
    std::cout << "POSTFIX WRITER: do_function_definition_node" << std::endl;

    std::shared_ptr<mml::symbol> symbol;

    for (std::string name : _symbols_to_declare) {
      auto symbol = _symtab.find(name, 0);
      if (symbol->is_foreign()) {
        _external_functions.insert(name);
      } else {
        _pf.BSS();
        _pf.ALIGN();
        _pf.LABEL(name);
        _pf.SALLOC(symbol->type()->size());    
      }
    }
    
    symbol = new_symbol();
    _func_symbols_stack.push_back(symbol);
    reset_new_symbol();

    _return_labels.push_back("_main");
    
    _symtab.push();
    _pf.TEXT(_return_labels.back());
    _pf.ALIGN();
    _pf.GLOBAL("_main", _pf.FUNC());
    _pf.LABEL("_main");

    frame_size_calculator lsc(_compiler, _symtab, symbol);

    _symtab.push();
    node->accept(&lsc, lvl);
    _symtab.pop();
  
    _pf.ENTER(lsc.localsize());

    bool oldReturnSeen = _returnSeen;
    _returnSeen = false;
    _insideFunctionBody = true;
    if (node->block()) {
      node->block()->accept(this, lvl + 2);
    }
    _insideFunctionBody = false;

    _symtab.pop();
    
    _return_labels.pop_back();
    if (!_returnSeen) {
      _pf.INT(0);
      _pf.STFVAL32();
    }
    _pf.LEAVE();
    _pf.RET();

    _func_symbols_stack.pop_back();
    for (std::string ext : _external_functions) {
      _pf.EXTERN(ext);
    }
    _external_functions.clear();
    _returnSeen = oldReturnSeen;

  } else {

    bool publicFun = false; 
    std::string funName; 
    auto symbol = new_symbol();

    if (symbol) {
      _func_symbols_stack.push_back(symbol);
      reset_new_symbol();
    } 
    
    _offset = 8;
    _symtab.push();

    if(node->arguments()) {
      _insideFunctionArgs = true;
      for (size_t ix = 0; ix < node-> arguments()->size(); ix++){
        cdk::basic_node *argument = node->arguments()->node(ix);
        if (!argument) break;
        argument->accept(this, 0);
      }
      _insideFunctionArgs = false;
    }

    std::string lbl = mklbl(++_lbl);
    _return_labels.push_back(lbl);
    _pf.TEXT(_return_labels.back());
    _pf.ALIGN();
    if (publicFun) {
      _pf.GLOBAL(funName, _pf.FUNC());
    }
    _pf.LABEL(lbl);
    frame_size_calculator lsc(_compiler, _symtab, symbol);

    _symtab.push();
    node->accept(&lsc, lvl);
    _symtab.pop();
    
    _pf.ENTER(lsc.localsize());

    _offset = 0; // Prepare for local variables

    bool _wasInFunctionBody = _insideFunctionBody;
    _insideFunctionBody = true;
    if (node->block()) {
      node->block()->accept(this, lvl + 4);
    }
  
    _insideFunctionBody = _wasInFunctionBody;

    _symtab.pop();
  
    if (!_returnSeen) {
      _pf.LEAVE();
      _pf.RET();
    }

    _return_labels.pop_back();
    if (symbol) {
      _func_symbols_stack.pop_back();
    }

    if (_insideFunctionBody) {
      _pf.TEXT(_return_labels.back());
      _pf.ADDR(lbl);
    } 
    _function_label = lbl;
  }
}

void mml::postfix_writer::do_function_call_node(mml::function_call_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::cout << "POSTFIX WRITER: do_function_call_node" << std::endl;
  std::vector<std::shared_ptr<cdk::basic_type>> inputTypes;

  if (node->identifier()) {   // non recursive case: formal types are encolsed in identifier type!
    inputTypes = cdk::functional_type::cast(node->identifier()->type())->input()->components();
  } else {                     // recursive case: must fetch formal types from current function symbol
    auto currFun = _func_symbols_stack.back();
    inputTypes = cdk::functional_type::cast(currFun->type())->input()->components();
  }

  // we have made sure that actuals and formals are compatible
  size_t argsSize = 0;
  if (node->arguments()) {
    for (int ix = node->arguments()->size() - 1; ix >= 0; --ix) {
      auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ix));
      arg->accept(this, lvl + 2);
      if (inputTypes.empty())
        ERROR("ERROR: no input types for function call");
      if (arg->is_typed(cdk::TYPE_INT) && inputTypes.at(ix)->name() == cdk::TYPE_DOUBLE) {
        _pf.I2D();
        argsSize += 4;
      }
      argsSize += arg->type()->size();
    }
  }

  if (node->identifier()) {  // non recursive case: need to get address value of the pointer and jump to it
    _extern_label.clear();
    node->identifier()->accept(this, lvl + 2);
    if (!_extern_label.empty()) {
      _pf.CALL(_extern_label);
    } else {
      _pf.BRANCH();
    }
  } else {  // recursive case: just call last pushed function label
    _pf.CALL(_return_labels.back());
  }

  if (argsSize != 0) {
    _pf.TRASH(argsSize);
  }
  
  // By convention, all int return types are doubles, it is the call who must properly cast them
  if (node->is_typed(cdk::TYPE_INT)) {
    if (!_extern_label.empty()) {
      _pf.LDFVAL32();
    } else {
      _pf.LDFVAL64();
      _pf.D2I();
    }
  } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDFVAL64();
  } else if (node->is_typed(cdk::TYPE_POINTER) || node->is_typed(cdk::TYPE_STRING) || node->is_typed(cdk::TYPE_FUNCTIONAL)) {
    _pf.LDFVAL32();
  }

  _extern_label.clear();
}

void mml::postfix_writer::do_variable_declaration_node(mml::variable_declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::cout << "POSTFIX WRITER: do_variable_declaration_node" << std::endl;
  auto id = node->name();
  int offset = 0, typesize = node->type()->size();
  if (_insideFunctionArgs) {
    offset = _offset;
    _offset += typesize;
  } else if (_insideFunctionBody) {
    _offset -= typesize;
    offset = _offset;
  }
  std::shared_ptr<mml::symbol> symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  // Insert new symbol name into last set of possible uninitialized identifiers
  if (!_insideFunctionArgs && !_insideFunctionBody) {
    _symbols_to_declare.insert(symbol->name());
  }
 
  if (node->value()) {
     initialize_variable(node->value(), lvl, symbol);
  } 
}

void mml::postfix_writer::do_null_ptr_node(mml::null_ptr_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_insideFunctionBody)
    _pf.INT(0);
  else
    _pf.SINT(0);
}

void mml::postfix_writer::do_stack_alloc_node(mml::stack_alloc_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto referenced = cdk::reference_type::cast(node->type())->referenced();
  node->argument()->accept(this, lvl);
  _pf.INT(referenced->size());
  _pf.MUL();
  _pf.ALLOC();
  _pf.SP();
}

void mml::postfix_writer::do_address_of_node(mml::address_of_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 2);
}

void mml::postfix_writer::do_identity_node(mml::identity_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2);
}

void mml::postfix_writer::do_pointer_index_node(mml::pointer_index_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->base()->accept(this, lvl);
  node->index()->accept(this, lvl);
  _pf.INT(node->type()->size());
  _pf.MUL();
  _pf.ADD();
}

// auxiliary functions ---------------------------------------------------------
void mml::postfix_writer::initialize_variable(cdk::expression_node * const node, int lvl, std::shared_ptr<mml::symbol> const symbol) {
  std::cout << "POSTFIX WRITER: initialize_variable" << std::endl;
  if (_insideFunctionBody) {
      node->accept(this, lvl);
      if (symbol->is_typed(cdk::TYPE_INT) || symbol->is_typed(cdk::TYPE_STRING) || 
          symbol->is_typed(cdk::TYPE_POINTER) || symbol->is_typed(cdk::TYPE_FUNCTIONAL)) {
        _pf.LOCAL(symbol->offset());
        _pf.STINT();
      } else if(symbol->is_typed(cdk::TYPE_DOUBLE)) {
        if (node->is_typed(cdk::TYPE_INT)) {
          _pf.I2D();
        }
        _pf.LOCAL(symbol->offset());
        _pf.STDOUBLE();
      } else {
        ERROR("unknown declaration type");
        return;
      }
  } else {
    if (symbol->is_typed(cdk::TYPE_STRING)) {
      _pf.DATA();
      _pf.ALIGN();
      _pf.LABEL(symbol->name());
      node->accept(this, lvl);
    } else if (symbol->is_typed(cdk::TYPE_INT) || symbol->is_typed(cdk::TYPE_DOUBLE) || symbol->is_typed(cdk::TYPE_POINTER)) {
      _pf.DATA();
      _pf.ALIGN();
      _pf.LABEL(symbol->name());
      if (symbol->is_typed(cdk::TYPE_INT)) {
        node->accept(this, lvl);
      } else if (symbol->is_typed(cdk::TYPE_DOUBLE)) {
        if (node->is_typed(cdk::TYPE_DOUBLE)) {
          node->accept(this, lvl);
        }
        else if (node->is_typed(cdk::TYPE_INT)) {
          cdk::integer_node *dclini = dynamic_cast<cdk::integer_node*>(node);
          cdk::double_node ddi(dclini->lineno(), dclini->value());
          ddi.accept(this, lvl);
        } else {
          ERROR("bad declarations for real value");
        }
      } else if (symbol->is_typed(cdk::TYPE_POINTER)) {
        node->accept(this, lvl);
      }
    } else if (symbol->is_typed(cdk::TYPE_FUNCTIONAL)) {
      _func_symbols_stack.push_back(symbol);
      reset_new_symbol();
      node->accept(this, lvl);
      _pf.DATA();
      if (_func_symbols_stack.back()->qualifier() == tPUBLIC) {
        _pf.GLOBAL(_func_symbols_stack.back()->name(), _pf.OBJ());
      }
      _pf.ALIGN();
      _pf.LABEL(symbol->name());
      std::string label = _function_label;
      _function_label.clear();
      _pf.SADDR(label);
    } else {
      ERROR("unexpected initializer in declaration");
    }
  }
  _symbols_to_declare.erase(symbol->name());
}