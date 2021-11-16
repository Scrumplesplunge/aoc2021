#ifndef STRING_UTILS_H_
#define STRING_UTILS_H_

#include <sstream>
#include <string>
#include <string_view>

namespace aoc2021 {

template <typename T>
concept Outputtable = requires (const T& value, std::ostream& output) {
  { output << value } -> std::same_as<std::ostream&>;
};

template <typename... Args>
std::string StrCat(const Args&... args) {
  std::ostringstream output;
  (output << ... << args);
  return output.str();
}

// A wrapper for wrapping a string in quotes and escaping special characters.
// This behaves similarly to std::quoted() except that it handles other escape
// sequences like '\n'.
struct Escaped {
  std::string_view value;
  char quote = '"';
};

std::ostream& operator<<(std::ostream&, Escaped) noexcept;

}  // namespace aoc2021

#endif  // STRING_UTILS_H_
