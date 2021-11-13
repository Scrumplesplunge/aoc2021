#ifndef VARIANT_UTILS_H_
#define VARIANT_UTILS_H_

#include <type_traits>

namespace aoc2021 {

template <typename T>
struct Wrapper {};

template <typename T>
struct Holder {
  void CanHold(Wrapper<T>) {}
};

template <typename... Ts>
struct MultiHolder : Holder<Ts>... {
  using Holder<Ts>::CanHold...;
};

template <typename T, typename... Ts>
concept ElementOf = requires (MultiHolder<Ts...> h) {
  h.CanHold(Wrapper<T>());
};

static_assert(ElementOf<int, char, int, double>);

template <typename Variant, typename T>
struct CanHoldImpl;

template <typename... Ts, typename T>
struct CanHoldImpl<std::variant<Ts...>, T> {
  static constexpr bool value = ElementOf<T, Ts...>;
};

template <typename T, typename Variant>
concept HoldableBy =
    CanHoldImpl<std::decay_t<decltype(std::declval<Variant>().value)>,
                T>::value;

}  // namespace aoc2021

#endif  // VARIANT_UTILS_
