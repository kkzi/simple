#pragma once

#include <algorithm>
#include <charconv>
#include <locale>
#include <string>
#include <string_view>
#include <vector>

namespace str::detail
{
    constexpr const char* const space = "\x20\x09\x0A\x0B\x0C\x0D";

    template <typename T>
    T parse_number(std::string_view sv, int base = 10)
    {
        T value{};
        std::from_chars(sv.data(), sv.data() + sv.size(), value, base);
        return value;
    }

    template <typename I>
    bool compare(I a, I b, const I last)
    {
        if (*a != *b) return false;

        while (b != last && *(++a) == *(++b))
            ;

        if (b == last) return true;

        return false;
    }

    template <typename T>
    std::vector<T> split_keep_empty(std::string_view sv, std::string_view token)
    {
        std::size_t start = 0;
        auto i = sv.find(token);
        std::vector<T> parts;

        while (i != std::string_view::npos)
        {
            parts.emplace_back(sv.substr(start, i - start));
            start = i + token.size();
            i = sv.find(token, start);
        }
        parts.emplace_back(sv.substr(start));

        return parts;
    }

    template <typename T>
    std::vector<T> split_ignore_empty(std::string_view sv, std::string_view token)
    {
        std::size_t start = 0;
        auto i = sv.find(token);
        std::vector<T> parts;

        while (i != std::string_view::npos)
        {
            if (auto len = i - start; len > 0) parts.emplace_back(sv.substr(start, len));
            start = i + token.size();
            i = sv.find(token, start);
        }
        if (sv.size() - start > 0) parts.emplace_back(sv.substr(start));

        return parts;
    }

    template <typename T>
    std::vector<T> split_chars(std::string_view sv, std::size_t char_count, std::size_t skip = 0)
    {
        if (char_count == 0) return std::vector<T>{};

        std::vector<T> v;
        auto size = sv.size();
        std::size_t i = 0;
        while (i < size)
        {
            v.emplace_back(sv.substr(i, char_count));
            i += char_count + skip;
        }

        return v;
    }

    template <typename T>
    std::tuple<T, T> split_first(std::string_view sv, std::string_view token)
    {
        if (auto i = sv.find(token); i != std::string_view::npos)
        {
            return { T{ sv.substr(0, i) }, T{ sv.substr(i + token.size()) } };
        }
        return { T{ sv }, T{} };
    }

    template <typename T>
    std::tuple<T, T> split_last(std::string_view sv, std::string_view token)
    {
        if (auto i = sv.rfind(token); i != std::string_view::npos)
        {
            return { T{ sv.substr(0, i) }, T{ sv.substr(i + token.size()) } };
        }
        return { T{ sv }, T{} };
    }

    template <typename T>
    T before_first(std::string_view sv, std::string_view token)
    {
        if (auto i = sv.find(token); i != std::string_view::npos)
        {
            return T{ sv.substr(0, i) };
        }
        return T{};
    }

    template <typename T>
    T before_last(std::string_view sv, std::string_view token)
    {
        if (auto i = sv.rfind(token); i != std::string_view::npos)
        {
            return T{ sv.substr(0, i) };
        }
        return T{};
    }

    template <typename T>
    T after_first(std::string_view sv, std::string_view token)
    {
        if (auto i = sv.find(token); i != std::string_view::npos)
        {
            return T{ sv.substr(i + token.size()) };
        }
        return T{};
    }

    template <typename T>
    T after_last(std::string_view sv, std::string_view token)
    {
        if (auto i = sv.rfind(token); i != std::string_view::npos)
        {
            return T{ sv.substr(i + token.size()) };
        }
        return T{};
    }

    template <typename T>
    T between(std::string_view sv, std::string_view first_token, std::string_view second_token, bool greedy = false)
    {
        if (auto i = sv.find(first_token), j = greedy ? sv.rfind(second_token) : sv.find(second_token);
            i != std::string_view::npos && j != std::string_view::npos && j > i)
        {
            return T{ sv.substr(i + first_token.size(), j - i - first_token.size()) };
        }
        return T{};
    }

    template <typename T>
    T rbetween(std::string_view sv, std::string_view first_token, std::string_view second_token, bool greedy = false)
    {
        if (auto i = sv.rfind(first_token), j = greedy ? sv.find(second_token) : sv.rfind(second_token);
            i != std::string_view::npos && j != std::string_view::npos && j < i)
        {
            return T{ sv.substr(j + first_token.size(), i - j - first_token.size()) };
        }
        return T{};
    }

    inline std::string replace(std::string_view sv, std::string_view search_token, std::string_view replace_token)
    {
        std::vector<std::size_t> positions;
        for (auto p = sv.find(search_token); p != std::string_view::npos; p = sv.find(search_token, p + search_token.size()))
        {
            positions.push_back(p);
        }
        if (positions.empty()) return std::string{ sv };

        std::string result;
        result.resize(sv.size() - search_token.size() * positions.size() + replace_token.size() * positions.size());
        auto result_it = std::begin(result);
        auto source_it = std::begin(sv);
        for (auto p : positions)
        {
            result_it = std::copy(source_it, std::begin(sv) + p, result_it);
            result_it = std::copy(std::begin(replace_token), std::end(replace_token), result_it);
            source_it = std::begin(sv) + p + search_token.size();
        }
        if (result_it != std::end(result))
        {
            std::copy(source_it, std::end(sv), result_it);
        }

        return result;
    }

    inline std::string replace_inplace(std::string_view sv, std::string_view search_token, std::string_view replace_token)
    {
        std::string result{ sv };
        auto result_it = std::begin(result);
        auto pos = sv.find(search_token);
        while (pos != std::string_view::npos)
        {
            result_it = std::copy(std::begin(replace_token), std::end(replace_token), std::begin(result) + pos);
            pos = sv.find(search_token, pos + search_token.size());
        }
        return result;
    }

}

namespace str
{
    inline void to_upper(std::string& s)
    {
        std::transform(std::begin(s), std::end(s), std::begin(s), ::toupper);
    }

    inline void to_lower(std::string& s)
    {
        std::transform(std::begin(s), std::end(s), std::begin(s), ::tolower);
    }

    inline std::string as_upper(std::string_view sv)
    {
        std::string s{ sv };
        to_upper(s);
        return s;
    }

    inline std::string as_lower(std::string_view sv)
    {
        std::string s{ sv };
        to_lower(s);
        return s;
    }

    inline bool starts_with(std::string_view sv, std::string_view test)
    {
        return sv.starts_with(test.data());
    }

    inline bool ends_with(std::string_view sv, std::string_view test)
    {
        return sv.ends_with(test.data());
    }

    inline bool istarts_with(std::string_view sv, std::string_view test)
    {
        if (sv.empty() || test.empty() || test.size() > sv.size()) return false;
        auto left = as_upper(sv.substr(0, test.size()).data());
        auto right = as_upper(test);
        return starts_with(left, right);
    }

    inline bool iends_with(std::string_view sv, std::string_view test)
    {
        if (sv.empty() || test.empty() || test.size() > sv.size()) return false;
        auto left = as_upper(sv.substr(0, test.size()).data());
        auto right = as_upper(test);
        return starts_with(left, right);
    }

    inline std::vector<std::string_view> split(std::string_view sv, std::string_view token, bool keep_empty_parts = true)
    {
        if (keep_empty_parts) return detail::split_keep_empty<std::string_view>(sv, token);
        return detail::split_ignore_empty<std::string_view>(sv, token);
    }

    inline std::vector<std::string> split_copy(std::string_view sv, std::string_view token, bool keep_empty_parts = true)
    {
        if (keep_empty_parts) return detail::split_keep_empty<std::string>(sv, token);
        return detail::split_ignore_empty<std::string>(sv, token);
    }

    inline std::vector<std::string_view> split_chars(std::string_view sv, std::size_t char_count, std::size_t skip = 0)
    {
        return detail::split_chars<std::string_view>(sv, char_count, skip);
    }

    inline std::vector<std::string> split_chars_copy(std::string_view sv, std::size_t char_count, std::size_t skip = 0)
    {
        return detail::split_chars<std::string>(sv, char_count, skip);
    }

    inline std::tuple<std::string_view, std::string_view> split_first(std::string_view sv, std::string_view token)
    {
        return detail::split_first<std::string_view>(sv, token);
    }

    inline std::tuple<std::string, std::string> split_first_copy(std::string_view sv, std::string_view token)
    {
        return detail::split_first<std::string>(sv, token);
    }

    inline std::tuple<std::string_view, std::string_view> split_last(std::string_view sv, std::string_view token)
    {
        return detail::split_last<std::string_view>(sv, token);
    }

    inline std::tuple<std::string, std::string> split_last_copy(std::string_view sv, std::string_view token)
    {
        return detail::split_last<std::string>(sv, token);
    }

    inline std::string_view before_first(std::string_view sv, std::string_view token)
    {
        return detail::before_first<std::string_view>(sv, token);
    }

    inline std::string before_first_copy(std::string_view sv, std::string_view token)
    {
        return detail::before_first<std::string>(sv, token);
    }

    inline std::string_view before_last(std::string_view sv, std::string_view token)
    {
        return detail::before_last<std::string_view>(sv, token);
    }

    inline std::string before_last_copy(std::string_view sv, std::string_view token)
    {
        return detail::before_last<std::string>(sv, token);
    }

    inline std::string_view after_first(std::string_view sv, std::string_view token)
    {
        return detail::after_first<std::string_view>(sv, token);
    }

    inline std::string after_first_copy(std::string_view sv, std::string_view token)
    {
        return detail::after_first<std::string>(sv, token);
    }

    inline std::string_view after_last(std::string_view sv, std::string_view token)
    {
        return detail::after_last<std::string_view>(sv, token);
    }

    inline std::string after_last_copy(std::string_view sv, std::string_view token)
    {
        return detail::after_last<std::string>(sv, token);
    }

    inline std::string_view between(std::string_view sv, std::string_view first_token, std::string_view second_token, bool greedy = false)
    {
        return detail::between<std::string_view>(sv, first_token, second_token, greedy);
    }

    inline std::string between_copy(std::string_view sv, std::string_view first_token, std::string_view second_token, bool greedy = false)
    {
        return detail::between<std::string>(sv, first_token, second_token, greedy);
    }

    inline std::string_view rbetween(std::string_view sv, std::string_view first_token, std::string_view second_token, bool greedy = false)
    {
        return detail::rbetween<std::string_view>(sv, first_token, second_token, greedy);
    }

    inline std::string rbetween_copy(std::string_view sv, std::string_view first_token, std::string_view second_token, bool greedy = false)
    {
        return detail::rbetween<std::string>(sv, first_token, second_token, greedy);
    }

    inline std::string replace(std::string_view sv, std::string_view search_token, std::string_view replace_token)
    {
        if (search_token.size() == replace_token.size()) return detail::replace_inplace(sv, search_token, replace_token);
        return detail::replace(sv, search_token, replace_token);
    }

    inline std::string as_string(std::string_view sv)
    {
        return std::string{ sv };
    }

    template <class T>
    T as_number(std::string_view sv, int base = 10)
    {
        return detail::parse_number<T>(sv, base);
    }

    template <class T>
    T to_number(std::string_view sv)
    {
        if (istarts_with(sv, "0x"))
        {
            return detail::parse_number<T>(sv.substr(2), 16);
        }
        else if (istarts_with(sv, "0b"))
        {
            return detail::parse_number<T>(sv.substr(2), 2);
        }
        return detail::parse_number<T>(sv, 10);
    }

    inline int as_int(std::string_view sv, int base = 10)
    {
        return as_number<int>(sv, base);
    }

    inline std::uint8_t as_uint8(std::string_view sv, int base = 10)
    {
        return as_number<std::uint8_t>(sv, base);
    }

    inline std::uint16_t as_uint16(std::string_view sv, int base = 10)
    {
        return as_number<std::uint16_t>(sv, base);
    }

    inline std::uint32_t as_uint32(std::string_view sv, int base = 10)
    {
        return as_number<std::uint32_t>(sv, base);
    }

    inline std::uint64_t as_uint64(std::string_view sv, int base = 10)
    {
        return as_number<std::uint64_t>(sv, base);
    }

    inline std::int8_t as_int8(std::string_view sv, int base = 10)
    {
        return as_number<std::int8_t>(sv, base);
    }

    inline std::int16_t as_int16(std::string_view sv, int base = 10)
    {
        return as_number<std::int16_t>(sv, base);
    }

    inline std::int32_t as_int32(std::string_view sv, int base = 10)
    {
        return as_number<std::int32_t>(sv, base);
    }

    inline std::int64_t as_int64(std::string_view sv, int base = 10)
    {
        return as_number<std::int64_t>(sv, base);
    }

    inline float as_float(std::string_view sv)
    {
        return std::stof(std::string{ sv });
    }

    inline double as_double(std::string_view sv)
    {
        return std::stod(std::string{ sv });
    }

    inline long double as_longdouble(std::string_view sv)
    {
        return std::stold(std::string{ sv });
    }

    void trim_left(std::string& input)
    {
        input.erase(0, input.find_first_not_of(detail::space));
    }

    void trim_right(std::string& input)
    {
        std::size_t pos = input.find_last_not_of(detail::space);
        if (pos == std::string::npos)
            input.erase(0);
        else if (++pos != input.size())
            input.erase(pos);
    }

    void trim(std::string& input)
    {
        trim_left(input);
        trim_right(input);
    }

    std::string trim_left_copy(std::string_view input)
    {
        std::string copy(input);
        trim_left(copy);
        return copy;
    }

    std::string trim_right_copy(std::string_view input)
    {
        std::string copy(input);
        trim_right(copy);
        return copy;
    }

    std::string trim_copy(std::string_view input)
    {
        std::string copy(input);
        trim(copy);
        return copy;
    }

}
