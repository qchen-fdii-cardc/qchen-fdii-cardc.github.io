#include <concepts>
#include <iostream>

template <typename T, typename... U>
concept either = (std::same_as<T, U> || ...);

template <typename T>
concept is_printable = std::integral<T> || std::floating_point<T> ||
                       either<std::remove_cvref_t<std::remove_pointer_t<std::decay_t<T>>>, char, wchar_t>;

void println(is_printable auto const... arguments)
{
    (std::wcout << ... << arguments) << "\n";
}

int main()
{
    println(1, 2, 3.0, ",", 0x34f, L"Hello, World!", L'_', L"z]");
    return 0;
}
