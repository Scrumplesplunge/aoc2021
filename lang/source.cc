#include "source.h"

#include <cassert>
#include <fstream>
#include <iostream>

namespace aoc2021 {
namespace {

struct Spaces {
  int count;
};

std::ostream& operator<<(std::ostream& output, Spaces s) {
  for (int i = 0; i < s.count; i++) output.put(' ');
  return output;
}

}  // namespace

Source::Source(std::string_view filename) : filename_(filename) {
  std::ifstream file(filename_);
  contents_ = std::string(std::istreambuf_iterator<char>(file), {});
  if (!file.good()) {
    throw std::runtime_error("Failed to read file " + filename_);
  }
  // Unconditionally append a newline character so that we can rely on there
  // always being one after every valid location, including EOF.
  contents_.push_back('\n');
}

std::string_view Source::contents() const noexcept {
  return std::string_view(contents_).substr(0, contents_.size() - 1);
}

std::string_view Location::LineContents() const noexcept {
  assert(source().contents().data() <= offset_);
  assert(offset_ <= source().contents().data() + source().contents().size());
  const char* const line_start = offset_ - (column_ - 1);
  const char* i = offset_;
  // There will always be a newline character eventually: the constructor for
  // Source guarantees that the last line ends with one.
  while (*i != '\n') i++;
  const char* const line_end = i;
  return std::string_view(line_start, line_end - line_start);
}

std::ostream& operator<<(std::ostream& output,
                         const Location& location) noexcept {
  return output << location.source().filename() << ':' << location.line() << ':'
                << location.column();
}

std::ostream& operator<<(std::ostream& output, Message::Type type) noexcept {
  switch (type) {
    case Message::Type::kError:
      return output << "error";
    case Message::Type::kWarning:
      return output << "warning";
    case Message::Type::kNote:
      return output << "note";
  }
  return output << "(invalid type)";
}

std::ostream& operator<<(std::ostream& output,
                         const Message& message) noexcept {
  return output << message.location << ": " << message.type << ": "
                << message.text << "\n\n    " << message.location.LineContents()
                << "\n"
                << Spaces{message.location.column() - 1 + 4} << "^\n";
}

SourceError::SourceError(std::vector<Message> messages)
    : messages_(std::move(messages)) {
  std::ostringstream output;
  for (const auto& message : messages_) {
    output << message;
  }
  text_ = output.str();
}

void Reader::Advance(std::size_t amount) noexcept {
  assert(amount <= remaining_.size());
  for (char c : remaining_.substr(0, amount)) {
    if (c == '\n') {
      line_++;
      column_ = 1;
    } else {
      column_++;
    }
  }
  remaining_.remove_prefix(amount);
}

std::string_view Reader::remaining() const noexcept { return remaining_; }

bool Reader::empty() const noexcept { return remaining().empty(); }

char Reader::front() const noexcept {
  assert(!empty());
  return remaining_.front();
}

bool Reader::starts_with(std::string_view prefix) const noexcept {
  return remaining().substr(0, prefix.size()) == prefix;
}

bool Reader::ConsumePrefix(std::string_view prefix) noexcept {
  if (!starts_with(prefix)) return false;
  Advance(prefix.size());
  return true;
}

Location Reader::location() const noexcept {
  return Location(*source_, remaining_.data(), line_, column_);
}

}  // namespace aoc2021
