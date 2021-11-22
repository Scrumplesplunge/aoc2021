#ifndef CHECKER_H_
#define CHECKER_H_

#include "ast.h"
#include "ir.h"
#include "source.h"

#include <filesystem>
#include <functional>
#include <map>
#include <optional>
#include <span>

namespace aoc2021 {

struct CheckError : public SourceError {
  using SourceError::SourceError;
};

enum class Category {
  kLvalue,
  kRvalue,
};

enum class Representation {
  kDirect,
  kAddress,
};

struct TypedExpression {
  Category category;
  ir::Type type;
  Representation representation;
  ir::Expression value;
};

struct ExpressionInfo {
  ir::Code code = ir::Sequence{};
  TypedExpression value;
};

class Environment;
using Value = std::variant<TypedExpression, ir::Type, const Environment*>;

class Environment {
 public:
  enum class ShadowMode {
    kAllow,
    kDeny,
  };

  struct Definition {
    Location location;
    Value value;
  };

  Environment() noexcept : parent_(nullptr), shadow_mode_(ShadowMode::kDeny) {}
  Environment(Environment& parent, ShadowMode shadow_mode) noexcept
      : parent_(&parent), shadow_mode_(shadow_mode) {}

  void Define(std::string_view name, Definition definition);
  void SetBreak(ir::Label label) { break_ = label; }
  void SetContinue(ir::Label label) { continue_ = label; }

  const ir::Label* Break() const noexcept;
  const ir::Label* Continue() const noexcept;
  const Definition* Lookup(std::string_view name) const;

  // Like Lookup(), except that it only searches within the broadest lexical
  // scope that forbids shadowing: variables outside this scope are ignored.
  const Definition* LookupWithinShadowDomain(std::string_view name) const;

  void SetFunctionType(ir::FunctionPointer type) {
    function_type_ = std::move(type);
  }

  const ir::FunctionPointer* FunctionType() const;

 private:
  Environment* parent_;
  ShadowMode shadow_mode_;
  std::map<std::string, Definition, std::less<>> names_;
  std::optional<ir::Label> break_, continue_;
  std::optional<ir::FunctionPointer> function_type_;
};

class Checker {
 public:
  struct Entry {
    std::filesystem::path path;
    std::optional<Source> source;
    std::optional<std::vector<ast::Statement>> ast;
    std::optional<ir::Unit> ir;
    Environment exports;
  };

  Checker(std::function<Source(const std::filesystem::path&)> loader) noexcept
      : loader_(std::move(loader)) {}

  Checker() noexcept
      : Checker([](auto& path) { return Source(path.native()); }) {}

  const Environment& EnvironmentFor(const std::filesystem::path& path);
  const ir::Unit& Check(const std::filesystem::path& path);

  ir::Unit Finish();

  ir::Struct::Id NextStruct() {
    return ir::Struct::Id{next_index_++};
  }

  ir::Global NextGlobal(std::string_view prefix) {
    return ir::Global(prefix, next_index_++);
  }

  ir::Label NextLabel(std::string_view prefix) {
    return ir::Label(prefix, next_index_++);
  }

 private:
  const Entry& EntryFor(const std::filesystem::path&);

  std::function<Source(const std::filesystem::path&)> loader_;
  std::map<std::filesystem::path, Entry> entries_;
  std::int64_t next_index_ = 0;
};

}  // namespace aoc2021

#endif  // CHECKER_H_
