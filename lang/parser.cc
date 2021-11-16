#include "parser.h"

#include <cassert>

namespace aoc2021 {
namespace {

using namespace ast;

constexpr bool IsWhitespace(char c) {
  switch (c) {
    case ' ': return true;
    case '\t': return true;
    case '\r': return true;
    case '\n': return true;
    case '\v': return true;
    default: return false;
  }
}

constexpr bool IsOperator(char c) {
  switch (c) {
    case '!': return true;
    case '%': return true;
    case '&': return true;
    case '*': return true;
    case '+': return true;
    case ',': return true;
    case '-': return true;
    case '.': return true;
    case '/': return true;
    case ':': return true;
    case '<': return true;
    case '=': return true;
    case '>': return true;
    case '?': return true;
    case '^': return true;
    case '|': return true;
    case '~': return true;
    default: return false;
  }
}

constexpr bool IsDigit(char c) { return '0' <= c && c <= '9'; }
constexpr bool IsLower(char c) { return 'a' <= c && c <= 'z'; }
constexpr bool IsUpper(char c) { return 'A' <= c && c <= 'Z'; }
constexpr bool IsAlpha(char c) { return IsLower(c) || IsUpper(c); }
constexpr bool IsAlphaNumeric(char c) { return IsDigit(c) || IsAlpha(c); }

}  // namespace

Expression Parser::ParseExpression() { return ParseTernary(); }

Statement Parser::ParseStatement() {
  const std::string_view keyword = PeekWord();
  if (keyword == "break") return ParseBreak();
  if (keyword == "continue") return ParseContinue();
  if (keyword == "function") return ParseFunctionDefinition();
  if (keyword == "if") return ParseIf();
  if (keyword == "return") return ParseReturn();
  if (keyword == "var") return ParseDeclaration();
  if (keyword == "while") return ParseWhile();
  Expression expression = ParseExpression();
  SkipWhitespaceAndComments();
  if (reader_.ConsumePrefix(";")) {
    return DiscardedExpression(std::move(expression));
  }
  const Location location = reader_.location();
  if (ConsumeOperator("=")) {
    SkipWhitespaceAndComments();
    Expression value = ParseExpression();
    SkipWhitespaceAndComments();
    if (!reader_.ConsumePrefix(";")) throw Error("expected ';'");
    return Assign(location, std::move(expression), std::move(value));
  }
  throw Error("expected statement");
}

std::vector<Statement> Parser::ParseProgram() {
  std::vector<Statement> program;
  while (true) {
    SkipWhitespaceAndComments();
    if (reader_.empty()) return program;
    program.push_back(ParseStatement());
  }
}

void Parser::ExpectEnd() {
  SkipWhitespaceAndComments();
  if (!reader_.empty()) throw Error("trailing characters after expected end");
}

Name Parser::ParseName() {
  const std::string_view word = PeekWord();
  // We should always have at least one character here, since we dispatch to
  // ParseName based on the lookahead, which would have been an alpha character.
  assert(!word.empty());
  assert(IsAlpha(word.front()));
  const Location location = reader_.location();
  reader_.Advance(word.size());
  return Name(location, std::string(word));
}

CharacterLiteral Parser::ParseCharacterLiteral() {
  assert(!reader_.empty() && reader_.front() == '\'');
  const Location location = reader_.location();
  reader_.Advance(1);
  const std::string_view remaining = reader_.remaining();
  if (remaining.empty()) throw Error("unterminated character literal");
  if (remaining[0] == '\'') throw Error("empty character literal");
  char value;
  if (remaining[0] == '\\') {
    // Complex case with an escape sequence.
    switch (remaining[1]) {
      case '\\': value = '\\'; break;
      case '\'': value = '\''; break;
      case '\"': value = '\"'; break;
      case 'n': value = '\n'; break;
      case 'r': value = '\r'; break;
      case 't': value = '\t'; break;
      case '\0': value = '\0'; break;
      default: throw Error("unrecognised escape sequence");
    }
    reader_.Advance(2);
  } else {
    value = remaining[0];
    reader_.Advance(1);
  }
  if (!reader_.ConsumePrefix("'")) throw Error("expected '\\''");
  return CharacterLiteral(location, value);
}

IntegerLiteral Parser::ParseIntegerLiteral() {
  // For now, this only supports decimal literals.
  // TODO: Extend this to support hex.
  const std::string_view word = PeekWord();
  std::int64_t value = 0;
  // We should always have at least one character here, since we dispatch to
  // ParseIntegerLiteral based on the lookahead, which would have been a digit.
  assert(!word.empty());
  if (word != "0" && word.starts_with("0")) {
    throw Error("integer literals must not be 0-padded");
  }
  for (char c : word) {
    if (!IsDigit(c)) throw Error("invalid integer literal");
    value = 10 * value + (c - '0');
  }
  const Location location = reader_.location();
  reader_.Advance(word.size());
  return IntegerLiteral(location, value);
}

Expression Parser::ParseTerm() {
  if (reader_.empty()) throw Error("expected expression");
  const char lookahead = reader_.front();
  if (lookahead == '\'') return ParseCharacterLiteral();
  if (lookahead == '(') {
    const Location start = reader_.location();
    reader_.Advance(1);
    SkipWhitespaceAndComments();
    // This term is a bracketed expression.
    Expression term = ParseExpression();
    SkipWhitespaceAndComments();
    if (!reader_.ConsumePrefix(")")) {
      // The inner expression is not followed by a ')' to match the '(' that
      // preceded it.
      std::vector<Message> messages;
      messages.emplace_back(Message{
          .location = reader_.location(),
          .type = Message::Type::kError,
          .text = "expected ')'",
      });
      messages.emplace_back(Message{
          .location = start,
          .type = Message::Type::kNote,
          .text = "to match this '('",
      });
      throw ParseError(std::move(messages));
    }
    return term;
  }
  if (IsDigit(lookahead)) return ParseIntegerLiteral();
  if (IsAlpha(lookahead)) return ParseName();
  throw Error("expected term");
}

Expression Parser::ParseArrayType() {
  const Location location = reader_.location();
  if (reader_.ConsumePrefix("[")) {
    SkipWhitespaceAndComments();
    if (reader_.ConsumePrefix("]")) {
      return SpanType(location, ParseArrayType());
    } else {
      Expression size = ParseExpression();
      if (!reader_.ConsumePrefix("]")) {
        std::vector<Message> messages;
        messages.emplace_back(Message{
            .location = reader_.location(),
            .type = Message::Type::kError,
            .text = "expected ']'",
        });
        messages.emplace_back(Message{
            .location = location,
            .type = Message::Type::kNote,
            .text = "to match this '['",
        });
        throw ParseError(std::move(messages));
      }
      SkipWhitespaceAndComments();
      return ArrayType(location, std::move(size), ParseArrayType());
    }
  } else {
    return ParseTerm();
  }
}

Expression Parser::ParseSuffix() {
  Expression term = ParseArrayType();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (reader_.ConsumePrefix("(")) {
      // Function call.
      SkipWhitespaceAndComments();
      std::vector<Expression> arguments;
      if (!reader_.ConsumePrefix(")")) {
        while (true) {
          arguments.push_back(ParseExpression());
          SkipWhitespaceAndComments();
          if (!ConsumeOperator(",")) break;
          SkipWhitespaceAndComments();
        }
        if (!reader_.ConsumePrefix(")")) {
          std::vector<Message> messages;
          messages.emplace_back(Message{
              .location = reader_.location(),
              .type = Message::Type::kError,
              .text = "expected ')'",
          });
          messages.emplace_back(Message{
              .location = location,
              .type = Message::Type::kNote,
              .text = "to match this '('",
          });
          throw ParseError(std::move(messages));
        }
      }
      term = Call(location, std::move(term), std::move(arguments));
    } else if (reader_.ConsumePrefix("[")) {
      // Index expression.
      SkipWhitespaceAndComments();
      Expression index = ParseExpression();
      SkipWhitespaceAndComments();
      if (!reader_.ConsumePrefix("]")) {
        std::vector<Message> messages;
        messages.emplace_back(Message{
            .location = reader_.location(),
            .type = Message::Type::kError,
            .text = "expected ']'",
        });
        messages.emplace_back(Message{
            .location = location,
            .type = Message::Type::kNote,
            .text = "to match this '['",
        });
        throw ParseError(std::move(messages));
      }
      term = Index(location, std::move(term), std::move(index));
    } else {
      return term;
    }
  }
}

Expression Parser::ParsePrefix() {
  const Location location = reader_.location();
  // This function does not use ConsumeOperator: unary prefix operators are
  // frequently used together (e.g. -*x or !!y) and it would be inconvenient to
  // force brackets or spaces in all of these cases.
  if (reader_.ConsumePrefix("-")) {
    return Negate(location, ParsePrefix());
  } else if (reader_.ConsumePrefix("!")) {
    return LogicalNot(location, ParsePrefix());
  } else if (reader_.ConsumePrefix("~")) {
    return BitwiseNot(location, ParsePrefix());
  } else if (reader_.ConsumePrefix("*")) {
    return Dereference(location, ParsePrefix());
  } else if (reader_.ConsumePrefix("&")) {
    return AddressOf(location, ParsePrefix());
  } else {
    return ParseSuffix();
  }
}

Expression Parser::ParseCast() {
  Expression expression = ParsePrefix();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeWord("as")) {
      SkipWhitespaceAndComments();
      expression = As(location, std::move(expression), ParsePrefix());
    } else {
      return expression;
    }
  }
}

Expression Parser::ParseProduct() {
  Expression expression = ParseCast();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeOperator("*")) {
      SkipWhitespaceAndComments();
      expression = Multiply(location, std::move(expression), ParseCast());
    } else if (ConsumeOperator("/")) {
      SkipWhitespaceAndComments();
      expression = Divide(location, std::move(expression), ParseCast());
    } else if (ConsumeOperator("%")) {
      SkipWhitespaceAndComments();
      expression = Modulo(location, std::move(expression), ParseCast());
    } else {
      return expression;
    }
  }
}

Expression Parser::ParseSum() {
  Expression expression = ParseProduct();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeOperator("+")) {
      SkipWhitespaceAndComments();
      expression = Add(location, std::move(expression), ParseProduct());
    } else if (ConsumeOperator("-")) {
      SkipWhitespaceAndComments();
      expression = Subtract(location, std::move(expression), ParseProduct());
    } else {
      return expression;
    }
  }
}

Expression Parser::ParseShift() {
  Expression expression = ParseSum();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeOperator("<<")) {
      SkipWhitespaceAndComments();
      expression = ShiftLeft(location, std::move(expression), ParseSum());
    } else if (ConsumeOperator(">>")) {
      SkipWhitespaceAndComments();
      expression = ShiftRight(location, std::move(expression), ParseSum());
    } else {
      return expression;
    }
  }
}

Expression Parser::ParseOrder() {
  Expression expression = ParseShift();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeOperator("<")) {
      SkipWhitespaceAndComments();
      expression = LessThan(location, std::move(expression), ParseShift());
    } else if (ConsumeOperator("<=")) {
      SkipWhitespaceAndComments();
      expression = LessOrEqual(location, std::move(expression), ParseShift());
    } else if (ConsumeOperator(">")) {
      SkipWhitespaceAndComments();
      expression = GreaterThan(location, std::move(expression), ParseShift());
    } else if (ConsumeOperator(">=")) {
      SkipWhitespaceAndComments();
      expression =
          GreaterOrEqual(location, std::move(expression), ParseShift());
    } else {
      return expression;
    }
  }
}

Expression Parser::ParseEqual() {
  Expression expression = ParseOrder();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeOperator("==")) {
      SkipWhitespaceAndComments();
      expression = Equal(location, std::move(expression), ParseOrder());
    } else if (ConsumeOperator("!=")) {
      SkipWhitespaceAndComments();
      expression = NotEqual(location, std::move(expression), ParseOrder());
    } else {
      return expression;
    }
  }
}

Expression Parser::ParseBitwiseAnd() {
  Expression expression = ParseEqual();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeOperator("&")) {
      SkipWhitespaceAndComments();
      expression = BitwiseAnd(location, std::move(expression), ParseEqual());
    } else {
      return expression;
    }
  }
}

Expression Parser::ParseBitwiseXor() {
  Expression expression = ParseBitwiseAnd();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeOperator("^")) {
      SkipWhitespaceAndComments();
      expression =
          BitwiseXor(location, std::move(expression), ParseBitwiseAnd());
    } else {
      return expression;
    }
  }
}

Expression Parser::ParseBitwiseOr() {
  Expression expression = ParseBitwiseXor();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeOperator("|")) {
      SkipWhitespaceAndComments();
      expression =
          BitwiseOr(location, std::move(expression), ParseBitwiseXor());
    } else {
      return expression;
    }
  }
}

Expression Parser::ParseConjunction() {
  Expression expression = ParseBitwiseOr();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeOperator("&&")) {
      SkipWhitespaceAndComments();
      expression =
          LogicalAnd(location, std::move(expression), ParseBitwiseOr());
    } else {
      return expression;
    }
  }
}

Expression Parser::ParseDisjunction() {
  Expression expression = ParseConjunction();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeOperator("||")) {
      SkipWhitespaceAndComments();
      expression =
          LogicalOr(location, std::move(expression), ParseConjunction());
    } else {
      return expression;
    }
  }
}

Expression Parser::ParseTernary() {
  Expression expression = ParseDisjunction();
  SkipWhitespaceAndComments();
  const Location start = reader_.location();
  if (!ConsumeOperator("?")) return expression;
  SkipWhitespaceAndComments();
  Expression then_branch = ParseTernary();
  SkipWhitespaceAndComments();
  if (!ConsumeOperator(":")) {
    // The then expression is not followed by a ':'
    // preceded it.
    std::vector<Message> messages;
    messages.emplace_back(Message{
        .location = reader_.location(),
        .type = Message::Type::kError,
        .text = "expected ':'",
    });
    messages.emplace_back(Message{
        .location = start,
        .type = Message::Type::kNote,
        .text = "to continue this ternary expression",
    });
    throw ParseError(std::move(messages));
  }
  SkipWhitespaceAndComments();
  Expression else_branch = ParseTernary();
  return TernaryExpression(start, std::move(expression), std::move(then_branch),
                           std::move(else_branch));
}

std::vector<Statement> Parser::ParseBlock() {
  if (!reader_.ConsumePrefix("{")) throw Error("expected statement block");
  std::vector<Statement> body;
  while (true) {
    SkipWhitespaceAndComments();
    if (reader_.ConsumePrefix("}")) return body;
    body.push_back(ParseStatement());
  }
}

Statement Parser::ParseBreak() {
  const Location location = reader_.location();
  if (!ConsumeWord("break")) throw Error("expected break statement");
  SkipWhitespaceAndComments();
  if (!reader_.ConsumePrefix(";")) throw Error("expected ';'");
  return Break(location);
}

Statement Parser::ParseContinue() {
  const Location location = reader_.location();
  if (!ConsumeWord("continue")) throw Error("expected continue statement");
  SkipWhitespaceAndComments();
  if (!reader_.ConsumePrefix(";")) throw Error("expected ';'");
  return Continue(location);
}

Statement Parser::ParseFunctionDefinition() {
  const Location location = reader_.location();
  if (!ConsumeWord("function")) throw Error("expected function definition");
  SkipWhitespaceAndComments();
  const std::string_view name = PeekWord();
  if (name.empty()) throw Error("expected function name");
  reader_.Advance(name.size());
  SkipWhitespaceAndComments();
  if (!reader_.ConsumePrefix("(")) throw Error("expected '('");
  SkipWhitespaceAndComments();
  std::vector<FunctionDefinition::Parameter> parameters;
  if (!reader_.ConsumePrefix(")")) {
    while (true) {
      Name name = ParseName();
      SkipWhitespaceAndComments();
      if (!reader_.ConsumePrefix(":")) throw Error("expected ':'");
      SkipWhitespaceAndComments();
      Expression type = ParseExpression();
      SkipWhitespaceAndComments();
      parameters.push_back(
          FunctionDefinition::Parameter(std::move(name), std::move(type)));
      if (reader_.ConsumePrefix(")")) break;
      if (!reader_.ConsumePrefix(",")) throw Error("expected ','");
      SkipWhitespaceAndComments();
    }
  }
  SkipWhitespaceAndComments();
  if (!reader_.ConsumePrefix(":")) throw Error("expected ':'");
  SkipWhitespaceAndComments();
  Expression return_type = ParseExpression();
  SkipWhitespaceAndComments();
  return FunctionDefinition(location, std::string(name), std::move(parameters),
                            std::move(return_type), ParseBlock());
}

Statement Parser::ParseIf() {
  const Location location = reader_.location();
  if (!ConsumeWord("if")) throw Error("expected if statement");
  SkipWhitespaceAndComments();
  Expression condition = ParseExpression();
  SkipWhitespaceAndComments();
  std::vector<Statement> then_branch = ParseBlock();
  SkipWhitespaceAndComments();
  if (!ConsumeWord("else")) {
    // if .. {}
    return If(location, std::move(condition), std::move(then_branch), {});
  }
  SkipWhitespaceAndComments();
  if (PeekWord() == "if") {
    // if .. {} else if ..
    std::vector<Statement> else_branch;
    else_branch.push_back(ParseIf());
    return If(location, std::move(condition), std::move(then_branch),
              std::move(else_branch));
  } else {
    // if .. {} else {}
    return If(location, std::move(condition), std::move(then_branch),
              ParseBlock());
  }
}

Statement Parser::ParseReturn() {
  const Location location = reader_.location();
  if (!ConsumeWord("return")) throw Error("expected return statement");
  SkipWhitespaceAndComments();
  if (reader_.ConsumePrefix(";")) return Return(location);
  Expression value = ParseExpression();
  SkipWhitespaceAndComments();
  if (!reader_.ConsumePrefix(";")) throw Error("expected ';'");
  return Return(location, std::move(value));
}

Statement Parser::ParseDeclaration() {
  const Location location = reader_.location();
  if (!ConsumeWord("var")) throw Error("expected variable declaration");
  SkipWhitespaceAndComments();
  const std::string_view name = PeekWord();
  if (name.empty() || !IsAlpha(name.front())) {
    throw ParseError({Message{
        .location = location,
        .type = Message::Type::kError,
        .text = "variable name must begin with a letter",
    }});
  }
  reader_.Advance(name.size());
  SkipWhitespaceAndComments();
  if (!reader_.ConsumePrefix(":")) throw Error("expected ':'");
  SkipWhitespaceAndComments();
  Expression type = ParseExpression();
  if (!reader_.ConsumePrefix(";")) throw Error("expected ';'");
  return DeclareVariable(location, std::string(name), std::move(type));
}

Statement Parser::ParseWhile() {
  const Location location = reader_.location();
  if (!ConsumeWord("while")) throw Error("expected while statement");
  SkipWhitespaceAndComments();
  Expression condition = ParseExpression();
  SkipWhitespaceAndComments();
  return While(location, std::move(condition), ParseBlock());
}

std::string_view Parser::PeekWord() const noexcept {
  const std::string_view tail = reader_.remaining();
  const char* const first = tail.data();
  const char* const end = first + tail.size();
  const char* i = first;
  while (i != end && IsAlphaNumeric(*i)) i++;
  return std::string_view(first, i - first);
}

std::string_view Parser::PeekOperator() const noexcept {
  const std::string_view tail = reader_.remaining();
  const char* const first = tail.data();
  const char* const end = first + tail.size();
  const char* i = first;
  while (i != end && IsOperator(*i)) i++;
  return std::string_view(first, i - first);
}

bool Parser::ConsumeWord(std::string_view value) noexcept {
  const std::string_view o = PeekWord();
  if (o != value) return false;
  reader_.Advance(value.size());
  return true;
}

bool Parser::ConsumeOperator(std::string_view value) noexcept {
  const std::string_view o = PeekOperator();
  if (o != value) return false;
  reader_.Advance(value.size());
  return true;
}

Message Parser::ErrorMessage(std::string text) const noexcept {
  return Message{
      .location = reader_.location(),
      .type = Message::Type::kError,
      .text = std::move(text),
  };
}

void Parser::SkipWhitespace() {
  std::string_view tail = reader_.remaining();
  const char* const first = tail.data();
  const char* const last = first + tail.size();
  const char* i = first;
  while (i != last && IsWhitespace(*i)) i++;
  reader_.Advance(i - first);
}

void Parser::SkipWhitespaceAndComments() {
  while (true) {
    SkipWhitespace();
    if (!reader_.starts_with("//")) return;
    const char* const first = reader_.remaining().data();
    const char* i = first;
    // This is guaranteed to terminate safely: a Source() always has
    // a newline character at the end.
    while (*i != '\n') i++;
    reader_.Advance(i - first);
  }
}

}  // namespace aoc2021
