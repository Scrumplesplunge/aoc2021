#include "parser.h"

#include <cassert>

namespace aoc2021 {
namespace {

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

AnyExpression Parser::ParseExpression() { return ParseTernary(); }

AnyStatement Parser::ParseStatement() {
  const std::string_view keyword = PeekWord();
  if (keyword == "break") return ParseBreak();
  if (keyword == "continue") return ParseContinue();
  if (keyword == "function") return ParseFunctionDefinition();
  if (keyword == "if") return ParseIf();
  if (keyword == "return") return ParseReturn();
  if (keyword == "var") return ParseDeclaration();
  if (keyword == "while") return ParseWhile();
  AnyExpression expression = ParseExpression();
  SkipWhitespaceAndComments();
  if (reader_.ConsumePrefix(";")) {
    return DiscardedExpression(std::move(expression));
  }
  const Location location = reader_.location();
  if (ConsumeOperator("=")) {
    SkipWhitespaceAndComments();
    AnyExpression value = ParseExpression();
    SkipWhitespaceAndComments();
    if (!reader_.ConsumePrefix(";")) throw Error("expected ';'");
    return Assign(location, std::move(expression), std::move(value));
  }
  // TODO: Handle assignments and discarded expressions.
  throw Error("expected statement");
}

Name Parser::ParseName() {
  const std::string_view word = PeekWord();
  // We should always have at least one character here, since we dispatch to
  // ParseName based on the lookahead, which would have been an alpha character.
  assert(!word.empty());
  assert(IsAlpha(word.front()));
  const Location location = reader_.location();
  reader_.Advance(word.size());
  return Name(location, word);
}

IntegerLiteral Parser::ParseIntegerLiteral() {
  // For now, this only supports decimal literals, and ignores 0-padding.
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

AnyExpression Parser::ParseTerm() {
  if (reader_.empty()) throw Error("expected expression");
  const char lookahead = reader_.front();
  if (lookahead == '(') {
    const Location start = reader_.location();
    reader_.Advance(1);
    SkipWhitespaceAndComments();
    // This term is a bracketed expression.
    AnyExpression term = ParseExpression();
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

AnyExpression Parser::ParseSuffix() {
  AnyExpression term = ParseTerm();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (reader_.ConsumePrefix("(")) {
      // Function call.
      SkipWhitespaceAndComments();
      std::vector<AnyExpression> arguments;
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
      AnyExpression index = ParseExpression();
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

AnyExpression Parser::ParsePrefix() {
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
  } else {
    return ParseSuffix();
  }
}

AnyExpression Parser::ParseProduct() {
  AnyExpression expression = ParsePrefix();
  while (true) {
    SkipWhitespaceAndComments();
    const Location location = reader_.location();
    if (ConsumeOperator("*")) {
      SkipWhitespaceAndComments();
      expression = Multiply(location, std::move(expression), ParsePrefix());
    } else if (ConsumeOperator("/")) {
      SkipWhitespaceAndComments();
      expression = Divide(location, std::move(expression), ParsePrefix());
    } else if (ConsumeOperator("%")) {
      SkipWhitespaceAndComments();
      expression = Modulo(location, std::move(expression), ParsePrefix());
    } else {
      return expression;
    }
  }
}

AnyExpression Parser::ParseSum() {
  AnyExpression expression = ParseProduct();
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

AnyExpression Parser::ParseShift() {
  AnyExpression expression = ParseSum();
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

AnyExpression Parser::ParseOrder() {
  AnyExpression expression = ParseShift();
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

AnyExpression Parser::ParseEqual() {
  AnyExpression expression = ParseOrder();
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

AnyExpression Parser::ParseBitwiseAnd() {
  AnyExpression expression = ParseEqual();
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

AnyExpression Parser::ParseBitwiseXor() {
  AnyExpression expression = ParseBitwiseAnd();
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

AnyExpression Parser::ParseBitwiseOr() {
  AnyExpression expression = ParseBitwiseXor();
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

AnyExpression Parser::ParseConjunction() {
  AnyExpression expression = ParseBitwiseOr();
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

AnyExpression Parser::ParseDisjunction() {
  AnyExpression expression = ParseConjunction();
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

AnyExpression Parser::ParseTernary() {
  AnyExpression expression = ParseDisjunction();
  SkipWhitespaceAndComments();
  const Location start = reader_.location();
  if (!ConsumeOperator("?")) return expression;
  SkipWhitespaceAndComments();
  AnyExpression then_branch = ParseTernary();
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
  AnyExpression else_branch = ParseTernary();
  return TernaryExpression(start, std::move(expression), std::move(then_branch),
                           std::move(else_branch));
}

std::vector<AnyStatement> Parser::ParseBlock() {
  if (!reader_.ConsumePrefix("{")) throw Error("expected statement block");
  std::vector<AnyStatement> body;
  while (true) {
    SkipWhitespaceAndComments();
    if (reader_.ConsumePrefix("}")) return body;
    body.push_back(ParseStatement());
  }
}

AnyStatement Parser::ParseBreak() {
  const Location location = reader_.location();
  if (!ConsumeWord("break")) throw Error("expected break statement");
  SkipWhitespaceAndComments();
  if (!reader_.ConsumePrefix(";")) throw Error("expected ';'");
  return Break(location);
}

AnyStatement Parser::ParseContinue() {
  const Location location = reader_.location();
  if (!ConsumeWord("continue")) throw Error("expected continue statement");
  SkipWhitespaceAndComments();
  if (!reader_.ConsumePrefix(";")) throw Error("expected ';'");
  return Continue(location);
}

AnyStatement Parser::ParseFunctionDefinition() {
  const Location location = reader_.location();
  if (!ConsumeWord("function")) throw Error("expected function definition");
  SkipWhitespaceAndComments();
  const std::string_view name = PeekWord();
  if (name.empty()) throw Error("expected function name");
  reader_.Advance(name.size());
  SkipWhitespaceAndComments();
  if (!reader_.ConsumePrefix("(")) throw Error("expected '('");
  SkipWhitespaceAndComments();
  std::vector<Name> arguments;
  if (!reader_.ConsumePrefix(")")) {
    while (true) {
      arguments.push_back(ParseName());
      SkipWhitespaceAndComments();
      if (reader_.ConsumePrefix(")")) break;
      if (!reader_.ConsumePrefix(",")) throw Error("expected ','");
      SkipWhitespaceAndComments();
    }
  }
  SkipWhitespaceAndComments();
  return FunctionDefinition(location, name, std::move(arguments), ParseBlock());
}

AnyStatement Parser::ParseIf() {
  const Location location = reader_.location();
  if (!ConsumeWord("if")) throw Error("expected if statement");
  SkipWhitespaceAndComments();
  AnyExpression condition = ParseExpression();
  SkipWhitespaceAndComments();
  std::vector<AnyStatement> then_branch = ParseBlock();
  SkipWhitespaceAndComments();
  if (!ConsumeWord("else")) {
    // if .. {}
    return If(location, std::move(condition), std::move(then_branch), {});
  }
  SkipWhitespaceAndComments();
  if (PeekWord() == "if") {
    // if .. {} else if ..
    std::vector<AnyStatement> else_branch;
    else_branch.push_back(ParseIf());
    return If(location, std::move(condition), std::move(then_branch),
              std::move(else_branch));
  } else {
    // if .. {} else {}
    return If(location, std::move(condition), std::move(then_branch),
              ParseBlock());
  }
}

AnyStatement Parser::ParseReturn() {
  const Location location = reader_.location();
  if (!ConsumeWord("return")) throw Error("expected return statement");
  SkipWhitespaceAndComments();
  if (reader_.ConsumePrefix(";")) return Return(location);
  AnyExpression value = ParseExpression();
  SkipWhitespaceAndComments();
  if (!reader_.ConsumePrefix(";")) throw Error("expected ';'");
  return Return(location, std::move(value));
}

AnyStatement Parser::ParseDeclaration() {
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
  const Location bracket_position = reader_.location();
  if (reader_.ConsumePrefix("[")) {
    AnyExpression size = ParseExpression();
    SkipWhitespaceAndComments();
    if (!reader_.ConsumePrefix("]")) {
      std::vector<Message> messages;
      messages.emplace_back(Message{
          .location = reader_.location(),
          .type = Message::Type::kError,
          .text = "expected ']'",
      });
      messages.emplace_back(Message{
          .location = bracket_position,
          .type = Message::Type::kNote,
          .text = "to match this '['",
      });
      throw ParseError(std::move(messages));
    }
    SkipWhitespaceAndComments();
    // TODO: Add support for `var x[3] = {1, 2, 3};`.
    if (!reader_.ConsumePrefix(";")) throw Error("expected ';'");
    return DeclareArray(location, name, std::move(size));
  } else if (reader_.ConsumePrefix(";")) {
    return DeclareScalar(location, name);
  } else {
    // TODO: Add support for `var x = 1;`
    throw Error("expected ';'");
  }
}

AnyStatement Parser::ParseWhile() {
  const Location location = reader_.location();
  if (!ConsumeWord("while")) throw Error("expected while statement");
  SkipWhitespaceAndComments();
  AnyExpression condition = ParseExpression();
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
