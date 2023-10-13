#pragma once

#include <cstdint>
#include <functional>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <string_view>
#include <tuple>
#include <variant>
#include <vector>
#include <optional>

template <class T>
class flags : T
{
private:
    using property_t = std::variant<
        std::string T::*, bool T::*,
        uint8_t T::*, int8_t T::*, uint16_t T::*, int16_t T::*,
        int32_t T::*, uint32_t T::*, uint64_t T::*, int64_t T::*,
        float T::*, double T::*
    >;
    using callback_t = std::function<void(const std::string&)>;
    using option_prop_t = std::tuple<std::string, std::string, property_t>;
    using option_item_t = std::tuple<std::string, std::string, callback_t>;

public:
    flags(std::string app, const std::initializer_list<option_prop_t>& opts)
        : app_(std::move(app))
    {
        register_option_item("--help", "show this help message", [this](auto&&) {
            std::cout << usage();
            exit(0);
            });

        if (!app_.empty())
        {
            register_option_item("--version", "show app version", [this](auto&&) {
                std::cout << app_ << std::endl;
                exit(0);
                });
        }

        auto parse = [this](const property_t& p, const std::string& val) {
            std::visit(
                [this, val](auto&& arg) {
                    std::stringstream ss;
                    ss << val;
                    ss >> this->*arg;
                },
                p);
        };

        auto read = [this](const property_t& p) {
            std::stringstream ss;
            std::visit(
                [this, &ss](auto&& arg) {
                    ss << this->*arg;
                },
                p);
            return ss.str();
        };

        for (const auto& [name, desc, prop] : opts)
        {
            auto val = read(prop);
            register_option_item(
                name, desc + (val.empty() ? "" : " (default is " + val + ")"), [parse, prop](auto&& val) {
                    parse(prop, val);
                });
        }
    }

    ~flags() = default;

    T operator()(int argc, char** argv)
    {
        return parse(argc, argv);
    }

public:
    T parse(int argc, char** argv)
    {
        std::vector<std::string_view> vargv(argv, argv + argc);
        for (auto idx = 1; idx < argc; ++idx)
        {
            std::string key, val;
            std::string line = argv[idx];
            auto equal_pos = line.find('=');
            if (equal_pos == std::string::npos)
            {
                key = line;
                val = argc > (++idx) ? argv[idx] : "";
            }
            else
            {
                key = line.substr(0, equal_pos);
                val = line.substr(equal_pos + 1);
            }

            if (auto it = item(key); it)
            {
                std::get<2>(*it)(val);
            }
            else
            {
                std::cout << "invalid argument " << key << std::endl;
                exit(0);
            }
        }

        return *this;
    }

private:
    std::string usage()
    {
        std::stringstream ss;
        if (!app_.empty())
        {
            ss << app_ << std::endl;
        }
        for (auto&& [name, it] : key2opt_)
        {
            ss << "  " << std::setw(12) << std::setiosflags(std::ios::left) << name << std::get<1>(it) << std::endl;
        }
        return ss.str();
    }

    void register_option_item(const std::string& name, const std::string& desc, const callback_t& func)
    {
        if (func != nullptr)
            key2opt_[name] = { name, desc, func };
    }

    std::optional<option_item_t> item(const std::string& name)
    {
        return key2opt_.contains(name) ? std::make_optional(key2opt_.at(name)) : std::nullopt;
    }

private:
    flags(const flags&) = delete;
    flags(flags&&) = delete;
    flags& operator=(const flags&) = delete;
    flags& operator=(flags&&) = delete;

private:
    std::string app_;
    std::unordered_map<std::string, option_item_t> key2opt_;
};
