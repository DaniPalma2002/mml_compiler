#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#include <mml_parser.tab.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

#define THROW_ERROR(M) throw std::string(M)

// auxiliary functions ---------------------------------------------------------

bool compatible_pointer_types(std::shared_ptr<cdk::basic_type> ltype, std::shared_ptr<cdk::basic_type> rtype) {
  return ltype->name() == rtype->name() || rtype == nullptr;
}

bool compatible_functional_types(std::shared_ptr<cdk::functional_type> ltype, std::shared_ptr<cdk::functional_type> rtype) {
  if (ltype->output(0)->name() == cdk::TYPE_POINTER) {
    if (!(rtype->output(0)->name() == cdk::TYPE_POINTER && compatible_pointer_types(ltype->output(0), rtype->output(0)))) {
      return false;
    }
  } else if (ltype->output(0)->name() == cdk::TYPE_FUNCTIONAL) {
     if (!(rtype->output(0)->name() == cdk::TYPE_FUNCTIONAL && 
          compatible_functional_types(cdk::functional_type::cast(ltype->output(0)), 
                                    cdk::functional_type::cast(rtype->output(0))))) {
      return false;
    }
  } else if (ltype->output(0)->name() == cdk::TYPE_DOUBLE) {
    if (!((rtype->output(0)->name() == cdk::TYPE_INT) || (rtype->output(0)->name() == cdk::TYPE_DOUBLE))) {
      return false;
    }
  } else if ((ltype->output(0)->name() != rtype->output(0)->name())) {
    return false;
  }

  if (ltype->input_length() != rtype->input_length()) {
    return false;
  }
  for (size_t tx = 0; tx < ltype->input_length(); tx++) {
    if (ltype->input(tx)->name() == cdk::TYPE_POINTER) {
      if (!(rtype->input(tx)->name() == cdk::TYPE_POINTER && compatible_pointer_types(ltype->input(tx), rtype->input(tx)))) {
        return false;
      }
    } else if (ltype->input(tx)->name() == cdk::TYPE_FUNCTIONAL) {
      if (!(rtype->input(tx)->name() == cdk::TYPE_FUNCTIONAL && 
          compatible_functional_types(cdk::functional_type::cast(ltype->input(tx)), cdk::functional_type::cast(rtype->input(tx))))) {
        return false;
      }
    } else if (rtype->input(tx)->name() == cdk::TYPE_DOUBLE) {
      if ((!((ltype->input(tx)->name() == cdk::TYPE_INT) || (ltype->input(tx)->name() == cdk::TYPE_DOUBLE)))) {
        return false;
      }
    } else if ((ltype->input(tx)->name() != rtype->input(tx)->name())) {
      return false;
    }
  }

  return true;
}


//---------------------------------------------------------------------------

void mml::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (auto n : node->nodes()) {
    n->accept(this, lvl + 2);
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

void mml::type_checker::do_null_ptr_node(mml::null_ptr_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(1, cdk::TYPE_VOID)));
}

//---------------------------------------------------------------------------

void mml::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!(node->argument()->is_typed(cdk::TYPE_INT) || node->argument()->is_typed(cdk::TYPE_DOUBLE))) 
    THROW_ERROR("wrong type in argument of unary expression");

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

void mml::type_checker::do_identity_node(mml::identity_node * const node, int lvl) {
  processUnaryExpression(node, lvl);
}

void mml::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_arithmeticExpression(cdk::binary_operation_node *const node, int lvl, bool multiplicative) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  auto lval = node->left();
  auto rval = node->right();

  if (lval->is_typed(cdk::TYPE_DOUBLE)) {
    if (rval->is_typed(cdk::TYPE_DOUBLE))
      node->type(lval->type());
    else if (rval->is_typed(cdk::TYPE_INT))
      node->type(lval->type());
    else
      THROW_ERROR("wrong type in right argument of binary operation when left is double");
  
  } else if (lval->is_typed(cdk::TYPE_INT)) {
    if (rval->is_typed(cdk::TYPE_INT))
      node->type(lval->type());
    else if (rval->is_typed(cdk::TYPE_DOUBLE))
      node->type(rval->type());
    else if (rval->is_typed(cdk::TYPE_POINTER))
      node->type(rval->type());
    else
      THROW_ERROR("wrong type in right argument of binary operation when left is int");
  
  } else if (!multiplicative && lval->is_typed(cdk::TYPE_POINTER)) {
      if (rval->is_typed(cdk::TYPE_INT))
        node->type(lval->type());
      else
        THROW_ERROR("wrong type in right argument of binary operation when left is pointer");
  
  } else if (lval->is_typed(cdk::TYPE_UNSPEC) && rval->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    lval->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    rval->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  
  } else {
    THROW_ERROR("wrong types in binary operation");
  }
}

void mml::type_checker::do_intOnlyExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) {
    THROW_ERROR("integer expression expected in binary operator (left)");
  }

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) {
    THROW_ERROR("integer expression expected in binary operator (right)");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  do_arithmeticExpression(node, lvl, false);
}
void mml::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  do_arithmeticExpression(node, lvl, false);
}
void mml::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  do_arithmeticExpression(node, lvl, true);
}
void mml::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  do_arithmeticExpression(node, lvl, true);
}
void mml::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  do_intOnlyExpression(node, lvl);
}

// ---------------------------------------------------------------------------

void mml::type_checker::do_ScalarLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) {
    THROW_ERROR("integer expression expected in binary logical expression (left)");
  }

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) {
    THROW_ERROR("integer expression expected in binary logical expression (right)");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_BooleanLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) THROW_ERROR("wrong type in left argument in binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) THROW_ERROR("wrong type in right argument in binary expression");

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_GeneralLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  if (node->left()->type() != node->right()->type()) {
    THROW_ERROR("same type expected on both sides of equality operator");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  do_GeneralLogicalExpression(node, lvl);
}
void mml::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  do_GeneralLogicalExpression(node, lvl);
}
void mml::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  do_BooleanLogicalExpression(node, lvl);
}
void mml::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  do_BooleanLogicalExpression(node, lvl);
}
//---------------------------------------------------------------------------

void mml::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  auto symbol = _symtab.find(id);
  if (symbol) {
    node->type(symbol->type());
  } else {
    THROW_ERROR("undeclared variable '" + id + "'");
  }
}

void mml::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    THROW_ERROR("undeclared variable '" + id + "'");
  }
}

void mml::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 4);
  node->rvalue()->accept(this, lvl + 4);

  auto lval = node->lvalue()->type()->name();
  auto rval = node->rvalue()->type()->name();

  switch (lval) {
    case cdk::TYPE_INT:
      if (rval == cdk::TYPE_INT || rval == cdk::TYPE_UNSPEC) {
        node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        if (rval == cdk::TYPE_UNSPEC)
          node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      } else {
        THROW_ERROR("wrong assignment to integer");
      }
      break;

    case cdk::TYPE_POINTER:
      if (rval == cdk::TYPE_POINTER) {
        if (!compatible_pointer_types(node->lvalue()->type(), node->rvalue()->type()))
          THROW_ERROR("wrong assignment to pointer.");
        node->type(node->rvalue()->type());
      } else if (rval == cdk::TYPE_UNSPEC) {
        node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
        node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      } else {
        THROW_ERROR("wrong assignment to pointer");
      }
      break;

    case cdk::TYPE_DOUBLE:
      if (rval == cdk::TYPE_DOUBLE || rval == cdk::TYPE_INT) {
        node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
        if (rval == cdk::TYPE_UNSPEC)
          node->rvalue()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      } else {
        THROW_ERROR("wrong assignment to real");
      }
      break;

    case cdk::TYPE_STRING:
      if (rval == cdk::TYPE_STRING || rval == cdk::TYPE_UNSPEC) {
        node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
        if (rval == cdk::TYPE_UNSPEC)
          node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
      } else {
        THROW_ERROR("wrong assignment to string");
      }
      break;

    case cdk::TYPE_FUNCTIONAL:
      if (rval == cdk::TYPE_FUNCTIONAL) {
        if (!compatible_functional_types(cdk::functional_type::cast(node->lvalue()->type()),
                                         cdk::functional_type::cast(node->rvalue()->type()))
            && !(node->rvalue()->is_typed(cdk::TYPE_POINTER)
                && cdk::reference_type::cast(node->rvalue()->type())->referenced() == nullptr)) {
          THROW_ERROR("wrong type for initializer (function expected).");
        }
        node->type(node->rvalue()->type());
      } else if (rval == cdk::TYPE_UNSPEC) {
        node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
        node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      } else {
        THROW_ERROR("wrong assignment to function");
      }
      break;

    default:
      THROW_ERROR("wrong types in assignment");
      break;
  }

  /* if (node->lvalue()->is_typed(cdk::TYPE_INT)) {
    if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {       
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else {
      THROW_ERROR("wrong assignment to integer");
    }

  } else if (node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
    if (node->rvalue()->is_typed(cdk::TYPE_POINTER)) {
      if (!(compatible_pointer_types(node->lvalue()->type(), node->rvalue()->type()))) {
        THROW_ERROR("wrong assignment to pointer.");
      }
      node->type(node->rvalue()->type());
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {             
      node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
    } else {
      THROW_ERROR("wrong assignment to pointer");
    }

  } else if (node->lvalue()->is_typed(cdk::TYPE_DOUBLE)) {
    if (node->rvalue()->is_typed(cdk::TYPE_DOUBLE) || node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {            
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->rvalue()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else {
      THROW_ERROR("wrong assignment to real");
    }

  } else if (node->lvalue()->is_typed(cdk::TYPE_STRING)) {
    if (node->rvalue()->is_typed(cdk::TYPE_STRING)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {           
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } else {
      THROW_ERROR("wrong assignment to string");
    }

  } else if (node->lvalue()->is_typed(cdk::TYPE_FUNCTIONAL)) {  
    if (node->rvalue()->is_typed(cdk::TYPE_FUNCTIONAL)) {
      if (!(compatible_functional_types(cdk::functional_type::cast(node->lvalue()->type()), 
                                        cdk::functional_type::cast(node->rvalue()->type()))
          || (node->rvalue()->is_typed(cdk::TYPE_POINTER) && 
          cdk::reference_type::cast(node->rvalue()->type())->referenced() == nullptr))) {
        THROW_ERROR("wrong type for initializer (function expected).");
      }
      node->type(node->rvalue()->type());
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {              
      node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
    } else {
      THROW_ERROR("wrong assignment to function");
    }
  } else {
    THROW_ERROR("wrong types in assignment");
  } */
}

//---------------------------------------------------------------------------

void mml::type_checker::do_evaluation_node(mml::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void mml::type_checker::do_print_node(mml::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_while_node(mml::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) THROW_ERROR("expected integer condition");

}

//---------------------------------------------------------------------------

void mml::type_checker::do_if_node(mml::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) THROW_ERROR("expected integer condition");

}

void mml::type_checker::do_if_else_node(mml::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) THROW_ERROR("expected integer condition");

}

void mml::type_checker::do_input_node(mml::input_node * const node, int lvl) {
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

void mml::type_checker::do_next_node(mml::next_node * const node, int lvl) {
}

void mml::type_checker::do_stop_node(mml::stop_node * const node, int lvl) {
}

void mml::type_checker::do_return_node(mml::return_node * const node, int lvl) {
  auto symbol = _symtab.find("@", 1);
  if (symbol) {
    if (node->expr()) {
      node->expr()->accept(this, lvl + 2);

      auto retType = cdk::functional_type::cast(symbol->type());

      if (retType->output()) {
        auto returnType = retType->output(0)->name();
        auto exprType = node->expr();
        
        switch (returnType) {
          case(cdk::TYPE_VOID):
            THROW_ERROR("return value specified for void function.");
            break;
          case(cdk::TYPE_INT):
            if (!exprType->is_typed(cdk::TYPE_INT))
              THROW_ERROR("wrong type for return expression (integer expected).");
            break;
          case(cdk::TYPE_DOUBLE):
            if (!exprType->is_typed(cdk::TYPE_INT) && !exprType->is_typed(cdk::TYPE_DOUBLE))
              THROW_ERROR("wrong type for return expression (integer or double expected).");
            break;
          case(cdk::TYPE_STRING):
            if (!exprType->is_typed(cdk::TYPE_STRING))
              THROW_ERROR("wrong type for return expression (string expected).");
            break;
          case(cdk::TYPE_POINTER):
            if (exprType->is_typed(cdk::TYPE_POINTER)) {
              if (!(compatible_pointer_types(retType->output(0), exprType->type())))
                THROW_ERROR("wrong type for return expression (pointer expected).");
            }
            break;
          case(cdk::TYPE_FUNCTIONAL):
            if (exprType->is_typed(cdk::TYPE_FUNCTIONAL)) {
              if (!(compatible_functional_types(cdk::functional_type::cast(retType->output(0)), 
                                                cdk::functional_type::cast(exprType->type()))
                  || (exprType->is_typed(cdk::TYPE_POINTER) && cdk::reference_type::cast(exprType->type())->referenced() == nullptr))) {
                THROW_ERROR("wrong type for return expression (function expected).");
              }
            }
            break;
          default:
            THROW_ERROR("unknown type for return expression.");
            break;
        }
      } else {
        THROW_ERROR("empty return value specified for non void function.");
      }
    }
  } else {
    symbol = _symtab.find("_main", 0);
    if (symbol) {
      if (!node->expr())
         THROW_ERROR("wrong type for program return expression (integer expected).");
      node->expr()->accept(this, lvl + 2);
      if (!node->expr()->is_typed(cdk::TYPE_INT))
        THROW_ERROR("wrong type for program return expression (integer expected).");
    } else {
      THROW_ERROR("return statement outside main function block");
    }
  }
}

void mml::type_checker::do_sizeof_node(mml::sizeof_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_block_node(mml::block_node * const node, int lvl) {
}

void mml::type_checker::do_function_definition_node(mml::function_definition_node * const node, int lvl) {
  std::shared_ptr<mml::symbol> function;
  if (node->is_main()) {
    auto mainfun = mml::make_symbol(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)), "_main", tPRIVATE);
    mainfun->set_main();
    function = mml::make_symbol(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)), "@", tPRIVATE);
    function->set_main();
   
    _parent->set_new_symbol(mainfun); 
  
  } else {
    std::vector<std::shared_ptr<cdk::basic_type>> input_types;
    for (size_t ax = 0; ax < node->arguments()->size(); ax++) {
      input_types.push_back(node->argument(ax)->type());
    }
    node->type(cdk::functional_type::create(input_types, node->outputType())); 
    function = mml::make_symbol(node->type(), "@", tPRIVATE);

    _parent->set_new_symbol(function);
  }

  if (_symtab.find_local(function->name())) {
    _symtab.replace(function->name(), function);
  } else {
    if (!_symtab.insert(function->name(), function)) {
      std::cerr << "error inserting function @" << std::endl;
      return;
    }
  }

}

void mml::type_checker::do_function_call_node(mml::function_call_node * const node, int lvl) {
  ASSERT_UNSPEC;
  std::vector<std::shared_ptr<cdk::basic_type>> input_types;
  std::shared_ptr<cdk::basic_type> output_type;
  std::cout << "function call checker" << std::endl;
  if (node->identifier()) {            
    node->identifier()->accept(this, lvl + 2);
    if (!(node->identifier()->is_typed(cdk::TYPE_FUNCTIONAL))) {
      THROW_ERROR("expected function pointer on function call");
    }
    input_types = cdk::functional_type::cast(node->identifier()->type())->input()->components();
    output_type = cdk::functional_type::cast(node->identifier()->type())->output(0);

  } else {

    auto symbol = _symtab.find("@", 1);
    std::cout << "symbol: " << symbol->name() << "->" << symbol->is_main() << std::endl;
    if (symbol == nullptr) {
      THROW_ERROR("recursive call outside function");
    }
    if (symbol->is_main()) {
      THROW_ERROR("recursive call in main function");
    }
    input_types = cdk::functional_type::cast(symbol->type())->input()->components();
    output_type = cdk::functional_type::cast(symbol->type())->output(0);
  }
  
  node->type(output_type);   

  if (node->arguments()->size() == input_types.size()) {
    node->arguments()->accept(this, lvl + 4);
    for (size_t ax = 0; ax < node->arguments()->size(); ax++) {
      if (node->argument(ax)->type()->name() == input_types[ax]->name()) continue;
      if (input_types[ax]->name() == cdk::TYPE_DOUBLE && node->argument(ax)->is_typed(cdk::TYPE_INT)) continue;
      THROW_ERROR("type mismatch for argument " + std::to_string(ax + 1) + ".");
    }
  } else {
    THROW_ERROR("number of arguments in call (" + std::to_string(node->arguments()->size()) + 
                ") must match declaration (" + std::to_string(input_types.size()) + ").");
  }
}

void mml::type_checker::do_variable_declaration_node(mml::variable_declaration_node * const node, int lvl) {
  if (node->value()) {
    node->value()->accept(this, lvl + 2);

    if (node->qualifier() == tFOREIGN)
      THROW_ERROR("cannot initialize foreign variable.");
    if (node->qualifier() == tFORWARD)
      THROW_ERROR("cannot initialize forward variable.");

    if (node->type()) {
      const auto nodeType = node->type();
      const auto value = node->value();

      switch (nodeType->name()) {
        case cdk::TYPE_INT:
          if (!value->is_typed(cdk::TYPE_INT))
            THROW_ERROR("wrong type for initializer (integer expected).");
          break;

        case cdk::TYPE_DOUBLE:
          if (!(value->is_typed(cdk::TYPE_INT) || value->is_typed(cdk::TYPE_DOUBLE)))
            THROW_ERROR("wrong type for initializer (integer or double expected).");
          break;

        case cdk::TYPE_STRING:
          if (!value->is_typed(cdk::TYPE_STRING))
            THROW_ERROR("wrong type for initializer (string expected).");
          break;

        case cdk::TYPE_POINTER:
            if (!(value->is_typed(cdk::TYPE_POINTER) &&
                  compatible_pointer_types(nodeType, value->type())))
              THROW_ERROR("wrong type for initializer (pointer expected).");
          break;

        case cdk::TYPE_FUNCTIONAL:
          if (value->is_typed(cdk::TYPE_FUNCTIONAL)) {
            if (!((value->is_typed(cdk::TYPE_FUNCTIONAL) && 
                  compatible_functional_types(cdk::functional_type::cast(nodeType), 
                                            cdk::functional_type::cast(value->type())))
              || ((value->is_typed(cdk::TYPE_POINTER) && 
                cdk::reference_type::cast(value->type())->referenced() == nullptr))))
              THROW_ERROR("wrong type for initializer (function expected).");
          }
          break;

        case cdk::TYPE_UNSPEC:
          node->type(value->type());
          break;

        default:
          THROW_ERROR("unknown type for initializer.");
          break;
      }
    } else {
      node->type(node->value()->type());
    }
  }
  std::string id = node->name();

  auto symbol = mml::make_symbol(node->type(), id, node->qualifier());
  auto previous = _symtab.find_local(symbol->name());

  if (previous) { 
    // Redeclaration of variable
    if (previous->is_typed(cdk::TYPE_FUNCTIONAL) && symbol->is_typed(cdk::TYPE_FUNCTIONAL) && 
        compatible_functional_types(cdk::functional_type::cast(previous->type()), cdk::functional_type::cast(symbol->type()))) {
          _symtab.replace(symbol->name(), symbol);

    } else if (previous->is_typed(cdk::TYPE_POINTER) && symbol->is_typed(cdk::TYPE_POINTER) && 
        compatible_pointer_types(previous->type(), symbol->type())) {
        _symtab.replace(symbol->name(), symbol);

    } else if (previous->type()->name() == symbol->type()->name()) {
        _symtab.replace(symbol->name(), symbol);
      
    } else {
      THROW_ERROR("variable '" + id + "' redeclared");
    }
  } else {
    _symtab.insert(id, symbol);
  } 

  _parent->set_new_symbol(symbol); // advise parent that a symbol has been inserted

  if (node->qualifier() == tFOREIGN) {
    symbol->set_foreign(true);
  }

}

void mml::type_checker::do_stack_alloc_node(mml::stack_alloc_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    THROW_ERROR("integer expression expected in allocation expression");
  }
  auto mytype = cdk::reference_type::create(4, cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  node->type(mytype);
}

void mml::type_checker::do_address_of_node(mml::address_of_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

void mml::type_checker::do_pointer_index_node(mml::pointer_index_node * const node, int lvl) {
  ASSERT_UNSPEC;

  node->base()->accept(this, lvl + 2);
  node->index()->accept(this, lvl + 2);
  
  if (!node->base()->is_typed(cdk::TYPE_POINTER)) 
    THROW_ERROR("pointer expression expected in index left-value");

  if (!node->index()->is_typed(cdk::TYPE_INT)) 
    THROW_ERROR("integer expression expected in left-value index");

  auto basetype = cdk::reference_type::cast(node->base()->type());
  node->type(basetype->referenced());
}

