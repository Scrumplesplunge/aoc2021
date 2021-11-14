#include "test.h"

#include <iostream>
#include <map>
#include <source_location>
#include <sstream>

namespace aoc2021 {
namespace {

struct TestInfo {
  std::source_location location;
  void (*run)(Sink& sink);
};

auto& Registry() {
  static auto& instance = *new std::map<std::string, TestInfo>();
  return instance;
}

}  // namespace

void RegisterTestFunction(std::source_location location, std::string_view name,
                          void (*run)(Sink& sink)) noexcept {
  auto [i, is_new] =
      Registry().emplace(name, TestInfo{.location = location, .run = run});
  if (!is_new) std::abort();
}

int RunTests() noexcept {
  std::map<std::string, TestInfo> tests;
  std::swap(tests, Registry());
  int passed = 0;
  int failed = 0;
  for (const auto& [name, info] : tests) {
    std::cout << name << "..." << std::flush;
    std::ostringstream log;
    Sink sink(log);
    try {
      info.run(sink);
      if (sink.failed()) throw TestAbort();
      std::cout << " \x1b[32mPASSED\x1b[0m\n";
      passed++;
    } catch (const TestAbort&) {
      std::cout << " \x1b[31mFAILED\x1b[0m\n"
                << log.str() << '\n';
      failed++;
    } catch (const std::exception& e) {
      std::cout << " \x1b[31mFAILED\x1b[0m\n";
      if (auto x = log.str(); !x.empty()) std::cout << x << '\n';
      std::cout << "Aborting with an exception: " << e.what() << '\n';
    } catch (...) {
      std::cout << " \x1b[31mFAILED\x1b[0m\n";
      if (auto x = log.str(); !x.empty()) std::cout << x << '\n';
      std::cout << "Aborting with a non-standard exception.\n";
    }
  }
  std::cout << "Ran " << (passed + failed) << " tests, with " << failed
            << " failures.\n";
  return failed ? EXIT_FAILURE : EXIT_SUCCESS;
}

bool Expect(std::string_view text, bool result, Sink& sink,
            std::source_location location) {
  if (result) return true;
  sink.Fail();
  sink.Log() << location.file_name() << ':' << location.line()
             << ": expectation failed: " << text << "\n";
  return false;
}

}  // namespace aoc2021

int main() { aoc2021::RunTests(); }
