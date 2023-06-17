#ifndef __MML_TARGETS_SYMBOL_H__
#define __MML_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace mml {

  class symbol {
    std::shared_ptr<cdk::basic_type> _type;
    std::string _name; // identifier

    int _qualifier; // qualifiers: public, forward, foreign (i.e., none)
    int _offset = 0; // 0 (zero) means global variable/function
    bool _initialized; 
    bool _main = false; 
    bool _extern;
    bool _foreign;

  public:
    symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name, int qualifier) :
        _type(type), _name(name), _qualifier(qualifier), _main(false), _extern(false), _foreign(false) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }
    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    const std::string &name() const {
      return _name;
    }
    int offset() const {
      return _offset;
    }
    void set_offset(int offset) {
      _offset = offset;
    }
    int qualifier() {
      return _qualifier;
    }
    bool is_main() {
      return _main;
    }
    void set_main() {
      _main = true;
    }
    void set_extern(bool val) {
      _extern = true;
    }
    bool is_extern() {
      return _extern;
    }
    void set_foreign(bool val) {
      _foreign = true;
    }
    bool is_foreign() {
      return _foreign;
    }
  };

  inline auto make_symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name, int qualifier) {
    return std::make_shared<symbol>(type, name, qualifier);
  }

} // mml

#endif
