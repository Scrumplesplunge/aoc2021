#ifndef TEST_H_
#define TEST_H_

#include <iomanip>
#include <iostream>
#include <optional>
#include <string_view>
#include <source_location>

namespace aoc2021 {

class Sink {
 public:
  Sink(std::ostream& log) noexcept : log_(&log) {}

  Sink(const Sink&) = delete;
  Sink& operator=(const Sink&) = delete;

  void Fail() noexcept { failed_ = true; }
  std::ostream& Log() const noexcept { return *log_; }
  bool failed() const noexcept { return failed_; }

 private:
  bool failed_ = false;
  std::ostream* log_;
};

class Test {
 public:
  virtual void Run(Sink& test_sink) = 0;
};

void RegisterTestFunction(std::source_location location, std::string_view name,
                          void (*f)(Sink& sink)) noexcept;

int RunTests() noexcept;

template <std::derived_from<Test> T>
void RegisterTest(std::string_view name, std::source_location location =
                                             std::source_location::current()) {
  RegisterTestFunction(location, name, [](Sink& sink) { T().Run(sink); });
}

bool Expect(std::string_view text, bool result, Sink& sink,
            std::source_location location = std::source_location::current());

template <typename L, typename R, typename Compare>
bool ExpectCompare(
    std::string_view text, const L& left, const R& right, Compare&& compare,
    Sink& sink,
    std::source_location location = std::source_location::current()) {
  if (compare(left, right)) return true;
  sink.Fail();
  sink.Log() << location.file_name() << ':' << location.line()
             << ": expectation failed: " << text << "\nleft  = " << left
             << "\nright = " << right << "\n";
  return false;
}

template <typename F>
bool ExpectError(
    std::string_view text, F&& f, std::string_view expected_message, Sink& sink,
    std::source_location location = std::source_location::current()) {
  std::optional<std::string> error;
  try {
    f();
  } catch (const std::exception& e) {
    error.emplace(e.what());
  }
  if (error && std::string_view(*error).find(expected_message) !=
                   std::string_view::npos) {
    return true;
  }
  sink.Fail();
  sink.Log() << text << " throws an exception stating "
             << std::quoted(expected_message) << ", but instead got ";
  if (error) {
    sink.Log() << std::quoted(*error);
  } else {
    sink.Log() << "no exception";
  }
  sink.Log() << ".\n";
  return false;
}

}  // namespace aoc2021

#define TEST_F(Fixture, name)                     \
  class name##Test : public Fixture {             \
   public:                                        \
    void Run(::aoc2021::Sink& test_sink) override; \
  };                                              \
  const bool name##_registerer = [] {             \
    ::aoc2021::RegisterTest<name##Test>(#name);   \
    return false;                                 \
  }();                                            \
  void name##Test::Run(::aoc2021::Sink& test_sink)

#define TEST(name) TEST_F(::aoc2021::Test, name)

#define ASSERT_TRUE(expr)                                                \
  for (; !::aoc2021::Expect(#expr " should be true", (expr), test_sink); \
       throw 1)                                                          \
    test_sink.Log()

#define ASSERT_FALSE(expr)                                                 \
  for (; !::aoc2021::Expect(#expr " should be false", !(expr), test_sink); \
       throw 1)                                                            \
    test_sink.Log()

#define ASSERT_EQ(left, right)                                           \
  for (; !::aoc2021::ExpectCompare(#left " == " #right, (left), (right), \
                                   std::equal_to<>(), test_sink);        \
       throw 1)                                                          \
    test_sink.Log()

#define ASSERT_EQ(left, right)                                           \
  for (; !::aoc2021::ExpectCompare(#left " == " #right, (left), (right), \
                                   std::equal_to<>(), test_sink);        \
       throw 1)                                                          \
    test_sink.Log()

#define ASSERT_NE(left, right)                                           \
  for (; !::aoc2021::ExpectCompare(#left " != " #right, (left), (right), \
                                   std::not_equal_to<>(), test_sink);    \
       throw 1)                                                          \
    test_sink.Log()

#define ASSERT_LT(left, right)                                          \
  for (; !::aoc2021::ExpectCompare(#left " < " #right, (left), (right), \
                                   std::less<>(), test_sink);           \
       throw 1)                                                         \
    test_sink.Log()

#define ASSERT_LE(left, right)                                           \
  for (; !::aoc2021::ExpectCompare(#left " <= " #right, (left), (right), \
                                   std::less_equal<>(), test_sink);      \
       throw 1)                                                          \
    test_sink.Log()

#define ASSERT_GT(left, right)                                          \
  for (; !::aoc2021::ExpectCompare(#left " > " #right, (left), (right), \
                                   std::greater<>(), test_sink);        \
       throw 1)                                                         \
    test_sink.Log()

#define ASSERT_GE(left, right)                                           \
  for (; !::aoc2021::ExpectCompare(#left " >= " #right, (left), (right), \
                                   std::greater_equal<>(), test_sink);   \
       throw 1)                                                          \
    test_sink.Log()

#define ASSERT_ERROR(expr, text)                       \
  for (; !::aoc2021::ExpectError(                      \
           #expr, [&] { (expr); }, (text), test_sink); \
       throw 1)                                        \
  test_sink.Log()

#define EXPECT_TRUE(expr)                                             \
  if (!::aoc2021::Expect(#expr " should be true", (expr), test_sink)) \
    test_sink.Log()

#define EXPECT_FALSE(expr)                                              \
  if (!::aoc2021::Expect(#expr " should be false", !(expr), test_sink); \
      throw 1)                                                          \
    test_sink.Log()

#define EXPECT_EQ(left, right)                                        \
  if (!::aoc2021::ExpectCompare(#left " == " #right, (left), (right), \
                                std::equal_to<>(), test_sink))        \
    test_sink.Log()

#define EXPECT_NE(left, right)                                        \
  if (!::aoc2021::ExpectCompare(#left " != " #right, (left), (right), \
                                std::not_equal_to<>(), test_sink))    \
    test_sink.Log()

#define EXPECT_LT(left, right)                                       \
  if (!::aoc2021::ExpectCompare(#left " < " #right, (left), (right), \
                                std::less<>(), test_sink))           \
    test_sink.Log()

#define EXPECT_LE(left, right)                                        \
  if (!::aoc2021::ExpectCompare(#left " <= " #right, (left), (right), \
                                std::less_equal<>(), test_sink))      \
    test_sink.Log()

#define EXPECT_GT(left, right)                                       \
  if (!::aoc2021::ExpectCompare(#left " > " #right, (left), (right), \
                                std::greater<>(), test_sink))        \
    test_sink.Log()

#define EXPECT_GE(left, right)                                        \
  if (!::aoc2021::ExpectCompare(#left " == " #right, (left), (right), \
                                std::greater_equal<>(), test_sink))   \
    test_sink.Log()

#define EXPECT_ERROR(expr, text)                      \
  if (!::aoc2021::ExpectError(                        \
          #expr, [&] { (expr); }, (text), test_sink)) \
    test_sink.Log()

#endif  // TEST_H_
