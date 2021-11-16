#include "string_utils.h"

namespace aoc2021 {

std::ostream& operator<<(std::ostream& output, Escaped escaped) noexcept {
  output << '"';
  for (char c : escaped.value) {
    switch (c) {
      case '\"':
        output << "\\\"";
        break;
      case '\'':
        output << "\\\'";
        break;
      case '\\':
        output << "\\\\";
        break;
      case '\n':
        output << "\\n";
        break;
      default:
        output << c;
        break;
    }
  }
  return output << '"';
}

}  // namespace aoc2021
