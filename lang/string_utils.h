#ifndef STRING_UTILS_H_
#define STRING_UTILS_H_

#include <sstream>
#include <string>

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

}  // namespace aoc2021

#endif  // STRING_UTILS_H_
