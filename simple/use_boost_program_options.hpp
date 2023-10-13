#pragma once

#include <boost/algorithm/string.hpp>
#include <boost/program_options.hpp>
#include <functional>
#include <iostream>
#include <tuple>
#include <variant>
#include <vector>

template <class T>
class flags : T
{
private:
    using property_t = std::variant<std::string T::*, bool T::*, uint8_t T::*, int8_t T::*, uint16_t T::*, int16_t T::*, int32_t T::*, uint32_t T::*,
        uint64_t T::*, int64_t T::*, float T::*, double T::*>;
    using callback_t = std::function<void(const boost::program_options::variable_value&)>;
    using option_prop_t = std::tuple<std::string, std::string, property_t>;
    using option_item_t = std::tuple<std::string, std::string, callback_t>;

public:
    flags(std::string app, const std::initializer_list<option_prop_t>& opts)
        : desc_(app)
    {
        register_def_option("help,h", "show this help message", [this](auto&&) {
            std::cout << desc_ << "\n";
            exit(0);
            });

        if (!app.empty())
        {
            register_def_option("version", "show app version", [ver = std::move(app)](auto&&) {
                std::cout << ver << "\n";
                exit(0);
            });
        }

        for (auto&& [name, desc, prop] : opts)
        {
            register_app_option(name, desc, prop, [this, prop](auto&& val) {
                std::visit(
                    [this, val = std::move(val)](auto&& item) {
                    using U = std::decay_t<decltype(this->*item)>;
                    this->*item = val.as<U>();
                }, prop);
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
        try
        {
            boost::program_options::variables_map args;
            boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc_), args);
            boost::program_options::notify(args);
            for (auto&& [k, v] : args)
            {
                if (key2opt_.contains(k))
                {
                    std::get<2>(key2opt_.at(k))(v);
                }
            }
        }
        catch (std::exception& e)
        {
            std::cout << e.what() << "\n";
            exit(EXIT_FAILURE);
        }
        return *this;
    }

    boost::program_options::options_description& raw() const
    {
        return desc_;
    }

    boost::program_options::options_description_easy_init add_options() const
    {
        return desc_.add_options();
    }

private:
    void register_def_option(const std::string& name, const std::string& desc, const callback_t& func)
    {
        desc_.add_options()(name.c_str(), desc.c_str());
        do_register_option_item(name, desc, func);
    }

    void register_app_option(const std::string& name, const std::string& desc, const property_t& prop, const callback_t& func)
    {
        std::visit(
            [this, name, desc](auto&& item) {
                using U = std::decay_t<decltype(this->*item)>;
                desc_.add_options()(name.c_str(), boost::program_options::value<U>()->default_value(this->*item), desc.c_str());
            },
            prop);

        do_register_option_item(name, desc, func);
    }

    void do_register_option_item(const std::string& name, const std::string& desc, const callback_t& func)
    {
        assert(func != nullptr);
        std::vector<std::string> parts;
        boost::split(parts, name, boost::is_any_of(","), boost::token_compress_on);
        for (auto&& k : parts)
        {
            key2opt_[k] = { k, desc, func };
        }
    }

private:
    flags(const flags&) = delete;
    flags(flags&&) = delete;
    flags& operator=(const flags&) = delete;
    flags& operator=(flags&&) = delete;

private:
    boost::program_options::options_description desc_;
    std::unordered_map<std::string, option_item_t> key2opt_;
};
