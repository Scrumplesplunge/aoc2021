#ifndef SOURCE_H_
#define SOURCE_H_

#include <memory>
#include <optional>
#include <span>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace aoc2021 {

// A single source file.
class Source {
 public:
  Source() = default;

  explicit Source(std::string_view filename);
  explicit Source(std::string_view name, std::string_view contents);

  std::string_view filename() const noexcept { return filename_; }
  std::string_view contents() const noexcept;

 private:
  std::string filename_;
  std::string contents_;
};

// A source location, consisting of a file, line, and column.
class Location {
 public:
  explicit Location(const Source& source, const char* offset, int line,
                    int column) noexcept
      : source_(&source), offset_(offset), line_(line), column_(column) {}

  const Source& source() const noexcept { return *source_; }
  int line() const noexcept { return line_; }
  int column() const noexcept { return column_; }
  std::string_view LineContents() const noexcept;

  auto operator<=>(const Location& other) const = default;

 private:
  const Source* source_;
  const char* offset_;
  int line_, column_;
};

std::ostream& operator<<(std::ostream&, const Location&) noexcept;

// A single diagnostic message, such as an error message.
struct Message {
  enum class Type {
    kError,
    kWarning,
    kNote,
  };
  Location location;
  Type type;
  std::string text;
};

std::ostream& operator<<(std::ostream&, Message::Type) noexcept;
std::ostream& operator<<(std::ostream&, const Message&) noexcept;

class SourceError : public std::exception {
 public:
  SourceError(std::vector<Message> messages);

  const char* what() const noexcept override { return text_.c_str(); }

  std::span<const Message> messages() const noexcept { return messages_; }

 private:
  std::vector<Message> messages_;
  std::string text_;
};

// A bare-bones wrapper around a Source in order to maintain a cursor position.
class Reader {
 public:
  explicit Reader(const Source& source) noexcept
      : source_(&source), remaining_(source.contents()) {}

  // Advances the cursor by the given amount, which must be no more than the
  // length of the remaining input.
  void Advance(std::size_t amount) noexcept;

  // Returns the remainder of the input, starting at the cursor position.
  std::string_view remaining() const noexcept;

  // Returns true if the reader has reached the end of the source file.
  bool empty() const noexcept;

  // Returns the first character of the remaining input. Must only be called if
  // !empty().
  char front() const noexcept;

  // Returns true if the remaining input starts with the given value.
  bool starts_with(std::string_view prefix) const noexcept;

  // Returns true and advances by prefix.size() iff starts_with(prefix).
  bool ConsumePrefix(std::string_view prefix) noexcept;

  // Returns the current location of the cursor.
  Location location() const noexcept;

 private:
  const Source* source_;
  std::string_view remaining_;
  int line_ = 1;
  int column_ = 1;
};

std::optional<Location> Seek(const Source& source, int line, int column);

}  // namespace aoc2021

#endif  // SOURCE_H_
