#include "source.h"

#include "test.h"

namespace aoc2021 {
namespace {

TEST(NewlineAppendedToSource) {
  Source source("demo.txt", "Hello, World!");
  std::string_view text = source.contents();
  EXPECT_EQ(text[text.size()], '\n');
}

TEST(AdvanceUpdatesLocation) {
  Source source("demo.txt", "Hello\nWorld!");
  const char* i = source.contents().data();
  Reader reader(source);
  EXPECT_EQ(reader.location(), Location(source, i, 1, 1));
  reader.Advance(2);
  EXPECT_EQ(reader.location(), Location(source, i + 2, 1, 3));
  // Advancing past the last character of a line, but not past the newline
  // character, should leave the cursor at the end of the line.
  reader.Advance(3);
  EXPECT_EQ(reader.location(), Location(source, i + 5, 1, 6));
  // Advancing past the newline character should leave the cursor at the start
  // of the next line.
  reader.Advance(1);
  EXPECT_EQ(reader.location(), Location(source, i + 6, 2, 1));
}

TEST(LineContents) {
  Source source("demo.txt", "Foo\nbar\nbaz!");
  Reader reader(source);
  // Foo\nbar\nbaz!
  // ^
  EXPECT_EQ(reader.location().LineContents(), "Foo");
  reader.Advance(3);
  // Foo\nbar\nbaz!
  //    ^
  EXPECT_EQ(reader.location().LineContents(), "Foo");
  reader.Advance(1);
  // Foo\nbar\nbaz!
  //      ^
  EXPECT_EQ(reader.location().LineContents(), "bar");
  reader.Advance(5);
  // Foo\nbar\nbaz!
  //            ^
  EXPECT_EQ(reader.location().LineContents(), "baz!");
}

TEST(ErrorMessage) {
  Source source("demo.txt", "Foo\nbar\nbaz!");
  Reader reader(source);
  reader.Advance(5);
  // Foo\nbar\nbaz!
  //       ^
  const Message message{.location = reader.location(),
                        .type = Message::Type::kError,
                        .text = "something bad happened"};
  std::ostringstream output;
  output << message;
  EXPECT_EQ(output.str(), "demo.txt:2:2: error: something bad happened\n"
                          "\n"
                          "    bar\n"
                          "     ^\n");
}

TEST(Seek) {
  Source source("demo.txt", "Foo\nbar\nbaz!");
  Reader reader(source);
  const auto start = Seek(source, 1, 1);
  ASSERT_TRUE(start.has_value());
  EXPECT_EQ(*start, reader.location());
  const auto a = Seek(source, 2, 2);
  ASSERT_TRUE(a.has_value());
  reader.Advance(5);
  EXPECT_EQ(*a, reader.location());
  const auto beyond_end = Seek(source, 4, 1);
  ASSERT_FALSE(beyond_end.has_value());
}

}  // namespace
}  // namespace aoc2021
