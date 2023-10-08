// This is an auto-generated header-only single-file distribution of libcluon.
// Date: Sun, 08 Oct 2023 14:05:26 +0800
// Version: 0.0.145
//
//
// Implementation of N4562 std::experimental::any (merged into C++17) for C++11 compilers.
//
// See also:
//   + http://en.cppreference.com/w/cpp/any
//   + http://en.cppreference.com/w/cpp/experimental/any
//   + http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/n4562.html#any
//   + https://cplusplus.github.io/LWG/lwg-active.html#2509
//
//
// Copyright (c) 2016 Denilson das Mercês Amorim
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#ifndef LINB_ANY_HPP
#define LINB_ANY_HPP
//#pragma once
#if defined(__APPLE__)
  #include <utility>
#endif
#include <typeinfo>
#include <type_traits>
#include <stdexcept>


#if defined(PARTICLE)
#if !defined(__cpp_exceptions) && !defined(ANY_IMPL_NO_EXCEPTIONS) && !defined(ANY_IMPL_EXCEPTIONS)
#   define ANY_IMPL_NO_EXCEPTIONS
# endif
#else
// you can opt-out of exceptions by definining ANY_IMPL_NO_EXCEPTIONS,
// but you must ensure not to cast badly when passing an `any' object to any_cast<T>(any)
#endif

#if defined(PARTICLE)
#if !defined(__cpp_rtti) && !defined(ANY_IMPL_NO_RTTI) && !defined(ANY_IMPL_RTTI)
#   define ANY_IMPL_NO_RTTI
# endif
#else
// you can opt-out of RTTI by defining ANY_IMPL_NO_RTTI,
// in order to disable functions working with the typeid of a type
#endif


namespace linb
{

class bad_any_cast : public std::bad_cast
{
public:
    const char* what() const noexcept override
    {
        return "bad any cast";
    }
};

class any final
{
public:
    /// Constructs an object of type any with an empty state.
    any() :
        vtable(nullptr)
    {
    }

    /// Constructs an object of type any with an equivalent state as other.
    any(const any& rhs) :
        vtable(rhs.vtable)
    {
        if(!rhs.empty())
        {
            rhs.vtable->copy(rhs.storage, this->storage);
        }
    }

    /// Constructs an object of type any with a state equivalent to the original state of other.
    /// rhs is left in a valid but otherwise unspecified state.
    any(any&& rhs) noexcept :
        vtable(rhs.vtable)
    {
        if(!rhs.empty())
        {
            rhs.vtable->move(rhs.storage, this->storage);
            rhs.vtable = nullptr;
        }
    }

    /// Same effect as this->clear().
    ~any()
    {
        this->clear();
    }

    /// Constructs an object of type any that contains an object of type T direct-initialized with std::forward<ValueType>(value).
    ///
    /// T shall satisfy the CopyConstructible requirements, otherwise the program is ill-formed.
    /// This is because an `any` may be copy constructed into another `any` at any time, so a copy should always be allowed.
    template<typename ValueType, typename = typename std::enable_if<!std::is_same<typename std::decay<ValueType>::type, any>::value>::type>
    any(ValueType&& value)
    {
        static_assert(std::is_copy_constructible<typename std::decay<ValueType>::type>::value,
            "T shall satisfy the CopyConstructible requirements.");
        this->construct(std::forward<ValueType>(value));
    }

    /// Has the same effect as any(rhs).swap(*this). No effects if an exception is thrown.
    any& operator=(const any& rhs)
    {
        any(rhs).swap(*this);
        return *this;
    }

    /// Has the same effect as any(std::move(rhs)).swap(*this).
    ///
    /// The state of *this is equivalent to the original state of rhs and rhs is left in a valid
    /// but otherwise unspecified state.
    any& operator=(any&& rhs) noexcept
    {
        any(std::move(rhs)).swap(*this);
        return *this;
    }

    /// Has the same effect as any(std::forward<ValueType>(value)).swap(*this). No effect if a exception is thrown.
    ///
    /// T shall satisfy the CopyConstructible requirements, otherwise the program is ill-formed.
    /// This is because an `any` may be copy constructed into another `any` at any time, so a copy should always be allowed.
    template<typename ValueType, typename = typename std::enable_if<!std::is_same<typename std::decay<ValueType>::type, any>::value>::type>
    any& operator=(ValueType&& value)
    {
        static_assert(std::is_copy_constructible<typename std::decay<ValueType>::type>::value,
            "T shall satisfy the CopyConstructible requirements.");
        any(std::forward<ValueType>(value)).swap(*this);
        return *this;
    }

    /// If not empty, destroys the contained object.
    void clear() noexcept
    {
        if(!empty())
        {
            this->vtable->destroy(storage);
            this->vtable = nullptr;
        }
    }

    /// Returns true if *this has no contained object, otherwise false.
    bool empty() const noexcept
    {
        return this->vtable == nullptr;
    }

#ifndef ANY_IMPL_NO_RTTI
    /// If *this has a contained object of type T, typeid(T); otherwise typeid(void).
    const std::type_info& type() const noexcept
    {
        return empty()? typeid(void) : this->vtable->type();
    }
#endif

    /// Exchange the states of *this and rhs.
    void swap(any& rhs) noexcept
    {
        if(this->vtable != rhs.vtable)
        {
            any tmp(std::move(rhs));

            // move from *this to rhs.
            rhs.vtable = this->vtable;
            if(this->vtable != nullptr)
            {
                this->vtable->move(this->storage, rhs.storage);
                //this->vtable = nullptr; -- unneeded, see below
            }

            // move from tmp (previously rhs) to *this.
            this->vtable = tmp.vtable;
            if(tmp.vtable != nullptr)
            {
                tmp.vtable->move(tmp.storage, this->storage);
                tmp.vtable = nullptr;
            }
        }
        else // same types
        {
            if(this->vtable != nullptr)
                this->vtable->swap(this->storage, rhs.storage);
        }
    }

private: // Storage and Virtual Method Table

    union storage_union
    {
        using stack_storage_t = typename std::aligned_storage<2 * sizeof(void*), std::alignment_of<void*>::value>::type;

        void*               dynamic;
        stack_storage_t     stack;      // 2 words for e.g. shared_ptr
    };

    /// Base VTable specification.
    struct vtable_type
    {
        // Note: The caller is responssible for doing .vtable = nullptr after destructful operations
        // such as destroy() and/or move().

#ifndef ANY_IMPL_NO_RTTI
        /// The type of the object this vtable is for.
        const std::type_info& (*type)() noexcept;
#endif

        /// Destroys the object in the union.
        /// The state of the union after this call is unspecified, caller must ensure not to use src anymore.
        void(*destroy)(storage_union&) noexcept;

        /// Copies the **inner** content of the src union into the yet unitialized dest union.
        /// As such, both inner objects will have the same state, but on separate memory locations.
        void(*copy)(const storage_union& src, storage_union& dest);

        /// Moves the storage from src to the yet unitialized dest union.
        /// The state of src after this call is unspecified, caller must ensure not to use src anymore.
        void(*move)(storage_union& src, storage_union& dest) noexcept;

        /// Exchanges the storage between lhs and rhs.
        void(*swap)(storage_union& lhs, storage_union& rhs) noexcept;
    };

    /// VTable for dynamically allocated storage.
    template<typename T>
    struct vtable_dynamic
    {
#ifndef ANY_IMPL_NO_RTTI
        static const std::type_info& type() noexcept
        {
            return typeid(T);
        }
#endif

        static void destroy(storage_union& storage) noexcept
        {
            //assert(reinterpret_cast<T*>(storage.dynamic));
            delete reinterpret_cast<T*>(storage.dynamic);
        }

        static void copy(const storage_union& src, storage_union& dest)
        {
            dest.dynamic = new T(*reinterpret_cast<const T*>(src.dynamic));
        }

        static void move(storage_union& src, storage_union& dest) noexcept
        {
            dest.dynamic = src.dynamic;
            src.dynamic = nullptr;
        }

        static void swap(storage_union& lhs, storage_union& rhs) noexcept
        {
            // just exchage the storage pointers.
            std::swap(lhs.dynamic, rhs.dynamic);
        }
    };

    /// VTable for stack allocated storage.
    template<typename T>
    struct vtable_stack
    {
#ifndef ANY_IMPL_NO_RTTI
        static const std::type_info& type() noexcept
        {
            return typeid(T);
        }
#endif

        static void destroy(storage_union& storage) noexcept
        {
            reinterpret_cast<T*>(&storage.stack)->~T();
        }

        static void copy(const storage_union& src, storage_union& dest)
        {
            new (&dest.stack) T(reinterpret_cast<const T&>(src.stack));
        }

        static void move(storage_union& src, storage_union& dest) noexcept
        {
            // one of the conditions for using vtable_stack is a nothrow move constructor,
            // so this move constructor will never throw a exception.
            new (&dest.stack) T(std::move(reinterpret_cast<T&>(src.stack)));
            destroy(src);
        }

        static void swap(storage_union& lhs, storage_union& rhs) noexcept
        {
            storage_union tmp_storage;
            move(rhs, tmp_storage);
            move(lhs, rhs);
            move(tmp_storage, lhs);
        }
    };

    /// Whether the type T must be dynamically allocated or can be stored on the stack.
    template<typename T>
    struct requires_allocation :
        std::integral_constant<bool,
                !(std::is_nothrow_move_constructible<T>::value      // N4562 §6.3/3 [any.class]
                  && sizeof(T) <= sizeof(storage_union::stack)
                  && std::alignment_of<T>::value <= std::alignment_of<storage_union::stack_storage_t>::value)>
    {};

    /// Returns the pointer to the vtable of the type T.
    template<typename T>
    static vtable_type* vtable_for_type()
    {
        using VTableType = typename std::conditional<requires_allocation<T>::value, vtable_dynamic<T>, vtable_stack<T>>::type;
        static vtable_type table = {
#ifndef ANY_IMPL_NO_RTTI
            VTableType::type,
#endif
            VTableType::destroy,
            VTableType::copy, VTableType::move,
            VTableType::swap,
        };
        return &table;
    }

protected:
    template<typename T>
    friend const T* any_cast(const any* operand) noexcept;
    template<typename T>
    friend T* any_cast(any* operand) noexcept;

    /// Casts (with no type_info checks) the storage pointer as const T*.
    template<typename T>
    const T* cast() const noexcept
    {
        return requires_allocation<typename std::decay<T>::type>::value?
            reinterpret_cast<const T*>(storage.dynamic) :
            reinterpret_cast<const T*>(&storage.stack);
    }

    /// Casts (with no type_info checks) the storage pointer as T*.
    template<typename T>
    T* cast() noexcept
    {
        return requires_allocation<typename std::decay<T>::type>::value?
            reinterpret_cast<T*>(storage.dynamic) :
            reinterpret_cast<T*>(&storage.stack);
    }

private:
    storage_union storage; // on offset(0) so no padding for align
    vtable_type*  vtable;

    template<typename ValueType, typename T>
    typename std::enable_if<requires_allocation<T>::value>::type
    do_construct(ValueType&& value)
    {
        storage.dynamic = new T(std::forward<ValueType>(value));
    }

    template<typename ValueType, typename T>
    typename std::enable_if<!requires_allocation<T>::value>::type
    do_construct(ValueType&& value)
    {
        new (&storage.stack) T(std::forward<ValueType>(value));
    }

    /// Chooses between stack and dynamic allocation for the type decay_t<ValueType>,
    /// assigns the correct vtable, and constructs the object on our storage.
    template<typename ValueType>
    void construct(ValueType&& value)
    {
        using T = typename std::decay<ValueType>::type;

        this->vtable = vtable_for_type<T>();

        do_construct<ValueType,T>(std::forward<ValueType>(value));
    }
};



namespace detail
{
    template<typename ValueType>
    inline ValueType any_cast_move_if_true(typename std::remove_reference<ValueType>::type* p, std::true_type)
    {
        return std::move(*p);
    }

    template<typename ValueType>
    inline ValueType any_cast_move_if_true(typename std::remove_reference<ValueType>::type* p, std::false_type)
    {
        return *p;
    }
}

/// Performs *any_cast<add_const_t<remove_reference_t<ValueType>>>(&operand), or throws bad_any_cast on failure.
template<typename ValueType>
inline ValueType any_cast(const any& operand)
{
    auto p = any_cast<typename std::add_const<typename std::remove_reference<ValueType>::type>::type>(&operand);
#ifndef ANY_IMPL_NO_EXCEPTIONS
    if(p == nullptr) throw bad_any_cast();
#endif
    return *p;
}

/// Performs *any_cast<remove_reference_t<ValueType>>(&operand), or throws bad_any_cast on failure.
template<typename ValueType>
inline ValueType any_cast(any& operand)
{
    auto p = any_cast<typename std::remove_reference<ValueType>::type>(&operand);
#ifndef ANY_IMPL_NO_EXCEPTIONS
    if(p == nullptr) throw bad_any_cast();
#endif
    return *p;
}

///
/// If ValueType is MoveConstructible and isn't a lvalue reference, performs
/// std::move(*any_cast<remove_reference_t<ValueType>>(&operand)), otherwise
/// *any_cast<remove_reference_t<ValueType>>(&operand). Throws bad_any_cast on failure.
///
template<typename ValueType>
inline ValueType any_cast(any&& operand)
{
    using can_move = std::integral_constant<bool,
        std::is_move_constructible<ValueType>::value
        && !std::is_lvalue_reference<ValueType>::value>;

    auto p = any_cast<typename std::remove_reference<ValueType>::type>(&operand);
#ifndef ANY_IMPL_NO_EXCEPTIONS
    if(p == nullptr) throw bad_any_cast();
#endif
    return detail::any_cast_move_if_true<ValueType>(p, can_move());
}

/// If operand != nullptr && operand->type() == typeid(ValueType), a pointer to the object
/// contained by operand, otherwise nullptr.
template<typename ValueType>
inline const ValueType* any_cast(const any* operand) noexcept
{
    using T = typename std::decay<ValueType>::type;

    if (operand && operand->vtable == any::vtable_for_type<T>())
        return operand->cast<ValueType>();
    else
        return nullptr;
}

/// If operand != nullptr && operand->type() == typeid(ValueType), a pointer to the object
/// contained by operand, otherwise nullptr.
template<typename ValueType>
inline ValueType* any_cast(any* operand) noexcept
{
    using T = typename std::decay<ValueType>::type;

    if (operand && operand->vtable == any::vtable_for_type<T>())
        return operand->cast<ValueType>();
    else
        return nullptr;
}

}

namespace std
{
    inline void swap(linb::any& lhs, linb::any& rhs) noexcept
    {
        lhs.swap(rhs);
    }
}

#endif
#ifndef ARGH
#define ARGH
//#pragma once

#include <algorithm>
#include <sstream>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <cassert>

namespace argh
{
   // Terminology:
   // A command line is composed of 2 types of args:
   // 1. Positional args, i.e. free standing values
   // 2. Options: args beginning with '-'. We identify two kinds:
   //    2.1: Flags: boolean options =>  (exist ? true : false)
   //    2.2: Parameters: a name followed by a non-option value

#if !defined(__GNUC__) || (__GNUC__ >= 5)
   using string_stream = std::istringstream;
#else
    // Until GCC 5, istringstream did not have a move constructor.
    // stringstream_proxy is used instead, as a workaround.
   class stringstream_proxy
   {
   public:
      stringstream_proxy() = default;

      // Construct with a value.
      stringstream_proxy(std::string const& value) :
         stream_(value)
      {}

      // Copy constructor.
      stringstream_proxy(const stringstream_proxy& other) :
         stream_(other.stream_.str())
      {
         stream_.setstate(other.stream_.rdstate());
      }

      void setstate(std::ios_base::iostate state) { stream_.setstate(state); }

      // Stream out the value of the parameter.
      // If the conversion was not possible, the stream will enter the fail state,
      // and operator bool will return false.
      template<typename T>
      stringstream_proxy& operator >> (T& thing)
      {
         stream_ >> thing;
         return *this;
      }


      // Get the string value.
      std::string str() const { return stream_.str(); }

      std::stringbuf* rdbuf() const { return stream_.rdbuf(); }

      // Check the state of the stream. 
      // False when the most recent stream operation failed
      operator bool() const { return !!stream_; }

      ~stringstream_proxy() = default;
   private:
      std::istringstream stream_;
   };
   using string_stream = stringstream_proxy;
#endif

   class parser
   {
   public:
      enum Mode { PREFER_FLAG_FOR_UNREG_OPTION = 1 << 0,
                  PREFER_PARAM_FOR_UNREG_OPTION = 1 << 1,
                  NO_SPLIT_ON_EQUALSIGN = 1 << 2,
                  SINGLE_DASH_IS_MULTIFLAG = 1 << 3,
                };

      parser() = default;

      parser(std::initializer_list<char const* const> pre_reg_names)
      {  add_params(pre_reg_names); }

      parser(const char* const argv[], int mode = PREFER_FLAG_FOR_UNREG_OPTION)
      {  parse(argv, mode); }

      parser(int argc, const char* const argv[], int mode = PREFER_FLAG_FOR_UNREG_OPTION)
      {  parse(argc, argv, mode); }

      void add_param(std::string const& name);
      void add_params(std::initializer_list<char const* const> init_list);

      void parse(const char* const argv[], int mode = PREFER_FLAG_FOR_UNREG_OPTION);
      void parse(int argc, const char* const argv[], int mode = PREFER_FLAG_FOR_UNREG_OPTION);

      std::multiset<std::string>          const& flags()    const { return flags_;    }
      std::map<std::string, std::string>  const& params()   const { return params_;   }
      std::vector<std::string>            const& pos_args() const { return pos_args_; }

      // begin() and end() for using range-for over positional args.
      std::vector<std::string>::const_iterator begin() const { return pos_args_.cbegin(); }
      std::vector<std::string>::const_iterator end()   const { return pos_args_.cend();   }
      size_t size()                                    const { return pos_args_.size();   }

      //////////////////////////////////////////////////////////////////////////
      // Accessors

      // flag (boolean) accessors: return true if the flag appeared, otherwise false.
      bool operator[](std::string const& name) const;

      // multiple flag (boolean) accessors: return true if at least one of the flag appeared, otherwise false.
      bool operator[](std::initializer_list<char const* const> init_list) const;

      // returns positional arg string by order. Like argv[] but without the options
      std::string const& operator[](size_t ind) const;

      // returns a std::istream that can be used to convert a positional arg to a typed value.
      string_stream operator()(size_t ind) const;

      // same as above, but with a default value in case the arg is missing (index out of range).
      template<typename T>
      string_stream operator()(size_t ind, T&& def_val) const;

      // parameter accessors, give a name get an std::istream that can be used to convert to a typed value.
      // call .str() on result to get as string
      string_stream operator()(std::string const& name) const;

      // accessor for a parameter with multiple names, give a list of names, get an std::istream that can be used to convert to a typed value.
      // call .str() on result to get as string
      // returns the first value in the list to be found.
      string_stream operator()(std::initializer_list<char const* const> init_list) const;

      // same as above, but with a default value in case the param was missing.
      // Non-string def_val types must have an operator<<() (output stream operator)
      // If T only has an input stream operator, pass the string version of the type as in "3" instead of 3.
      template<typename T>
      string_stream operator()(std::string const& name, T&& def_val) const;

      // same as above but for a list of names. returns the first value to be found.
      template<typename T>
      string_stream operator()(std::initializer_list<char const* const> init_list, T&& def_val) const;

   private:
      string_stream bad_stream() const;
      std::string trim_leading_dashes(std::string const& name) const;
      bool is_number(std::string const& arg) const;
      bool is_option(std::string const& arg) const;
      bool got_flag(std::string const& name) const;
      bool is_param(std::string const& name) const;

   private:
      std::vector<std::string> args_;
      std::map<std::string, std::string> params_;
      std::vector<std::string> pos_args_;
      std::multiset<std::string> flags_;
      std::set<std::string> registeredParams_;
      std::string empty_;
   };


   //////////////////////////////////////////////////////////////////////////

   inline void parser::parse(const char * const argv[], int mode)
   {
      int argc = 0;
      for (auto argvp = argv; *argvp; ++argc, ++argvp);
      parse(argc, argv, mode);
   }

   //////////////////////////////////////////////////////////////////////////

   inline void parser::parse(int argc, const char* const argv[], int mode /*= PREFER_FLAG_FOR_UNREG_OPTION*/)
   {
      // convert to strings
      args_.resize(argc);
      std::transform(argv, argv + argc, args_.begin(), [](const char* const arg) { return arg;  });

      // parse line
      for (auto i = 0u; i < args_.size(); ++i)
      {
         if (!is_option(args_[i]))
         {
            pos_args_.emplace_back(args_[i]);
            continue;
         }

         auto name = trim_leading_dashes(args_[i]);

         if (!(mode & NO_SPLIT_ON_EQUALSIGN))
         {
            auto equalPos = name.find('=');
            if (equalPos != std::string::npos)
            {
               params_.insert({ name.substr(0, equalPos), name.substr(equalPos + 1) });
               continue;
            }
         }

         // if the option is unregistered and should be a multi-flag
         if (1 == (args_[i].size() - name.size()) &&         // single dash
            argh::parser::SINGLE_DASH_IS_MULTIFLAG & mode && // multi-flag mode
            !is_param(name))                                  // unregistered
         {
            std::string keep_param; 
            
            if (!name.empty() && is_param(std::string(1ul, name.back()))) // last char is param
            {
               keep_param += name.back();
               name.resize(name.size() - 1);
            }

            for (auto const& c : name)
            {
               flags_.emplace(std::string{ c });
            }

            if (!keep_param.empty())
            {
               name = keep_param;
            }
            else
            {
               continue; // do not consider other options for this arg
            }
         }

         // any potential option will get as its value the next arg, unless that arg is an option too
         // in that case it will be determined a flag.
         if (i == args_.size() - 1 || is_option(args_[i + 1]))
         {
            flags_.emplace(name);
            continue;
         }

         // if 'name' is a pre-registered option, then the next arg cannot be a free parameter to it is skipped
         // otherwise we have 2 modes:
         // PREFER_FLAG_FOR_UNREG_OPTION: a non-registered 'name' is determined a flag. 
         //                               The following value (the next arg) will be a free parameter.
         //
         // PREFER_PARAM_FOR_UNREG_OPTION: a non-registered 'name' is determined a parameter, the next arg
         //                                will be the value of that option.

         assert(!(mode & argh::parser::PREFER_FLAG_FOR_UNREG_OPTION)
             || !(mode & argh::parser::PREFER_PARAM_FOR_UNREG_OPTION));

         bool preferParam = mode & argh::parser::PREFER_PARAM_FOR_UNREG_OPTION;

         if (is_param(name) || preferParam)
         {
            params_.insert({ name, args_[i + 1] });
            ++i; // skip next value, it is not a free parameter
            continue;
         }
         else
         {
            flags_.emplace(name);
         }
      };
   }

   //////////////////////////////////////////////////////////////////////////

   inline string_stream parser::bad_stream() const
   {
      string_stream bad;
      bad.setstate(std::ios_base::failbit);
      return bad;
   }

   //////////////////////////////////////////////////////////////////////////

   inline bool parser::is_number(std::string const& arg) const
   {
      // inefficient but simple way to determine if a string is a number (which can start with a '-')
      std::istringstream istr(arg);
      double number;
      istr >> number;
      return !(istr.fail() || istr.bad());
   }

   //////////////////////////////////////////////////////////////////////////

   inline bool parser::is_option(std::string const& arg) const
   {
      assert(0 != arg.size());
      if (is_number(arg))
         return false;
      return '-' == arg[0];
   }

   //////////////////////////////////////////////////////////////////////////

   inline std::string parser::trim_leading_dashes(std::string const& name) const
   {
      auto pos = name.find_first_not_of('-');
      return std::string::npos != pos ? name.substr(pos) : name;
   }

   //////////////////////////////////////////////////////////////////////////

   inline bool argh::parser::got_flag(std::string const& name) const
   {
      return flags_.end() != flags_.find(trim_leading_dashes(name));
   }

   //////////////////////////////////////////////////////////////////////////

   inline bool argh::parser::is_param(std::string const& name) const
   {
      return registeredParams_.count(name);
   }

   //////////////////////////////////////////////////////////////////////////

   inline bool parser::operator[](std::string const& name) const
   {
      return got_flag(name);
   }

   //////////////////////////////////////////////////////////////////////////

   inline bool parser::operator[](std::initializer_list<char const* const> init_list) const
   {
      return std::any_of(init_list.begin(), init_list.end(), [&](char const* const name) { return got_flag(name); });
   }

   //////////////////////////////////////////////////////////////////////////

   inline std::string const& parser::operator[](size_t ind) const
   {
      if (ind < pos_args_.size())
         return pos_args_[ind];
      return empty_;
   }

   //////////////////////////////////////////////////////////////////////////

   inline string_stream parser::operator()(std::string const& name) const
   {
      auto optIt = params_.find(trim_leading_dashes(name));
      if (params_.end() != optIt)
         return string_stream(optIt->second);
      return bad_stream();
   }

   //////////////////////////////////////////////////////////////////////////

   inline string_stream parser::operator()(std::initializer_list<char const* const> init_list) const
   {
      for (auto& name : init_list)
      {
         auto optIt = params_.find(trim_leading_dashes(name));
         if (params_.end() != optIt)
            return string_stream(optIt->second);
      }
      return bad_stream();
   }

   //////////////////////////////////////////////////////////////////////////

   template<typename T>
   string_stream parser::operator()(std::string const& name, T&& def_val) const
   {
      auto optIt = params_.find(trim_leading_dashes(name));
      if (params_.end() != optIt)
         return string_stream(optIt->second);

      std::ostringstream ostr;
      ostr << def_val;
      return string_stream(ostr.str()); // use default
   }

   //////////////////////////////////////////////////////////////////////////

   // same as above but for a list of names. returns the first value to be found.
   template<typename T>
   string_stream parser::operator()(std::initializer_list<char const* const> init_list, T&& def_val) const
   {
      for (auto& name : init_list)
      {
         auto optIt = params_.find(trim_leading_dashes(name));
         if (params_.end() != optIt)
            return string_stream(optIt->second);
      }      
      std::ostringstream ostr;
      ostr << def_val;
      return string_stream(ostr.str()); // use default
   }

   //////////////////////////////////////////////////////////////////////////

   inline string_stream parser::operator()(size_t ind) const
   {
      if (pos_args_.size() <= ind)
         return bad_stream();

      return string_stream(pos_args_[ind]);
   }

   //////////////////////////////////////////////////////////////////////////

   template<typename T>
   string_stream parser::operator()(size_t ind, T&& def_val) const
   {
      if (pos_args_.size() <= ind)
      {
         std::ostringstream ostr;
         ostr << def_val;
         return string_stream(ostr.str());
      }

      return string_stream(pos_args_[ind]);
   }

   //////////////////////////////////////////////////////////////////////////

   inline void parser::add_param(std::string const& name)
   {
      registeredParams_.insert(trim_leading_dashes(name));
   }

   //////////////////////////////////////////////////////////////////////////

   inline void parser::add_params(std::initializer_list<char const* const> init_list)
   {
      for (auto& name : init_list)
         registeredParams_.insert(trim_leading_dashes(name));
   }
}


#endif
/*
 * MIT License
 *
 * Copyright (c) 2018-2020  Christian Berger
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef STRINGTOOLBOX_HPP
#define STRINGTOOLBOX_HPP

#include <algorithm>
#include <string>
#include <vector>

namespace stringtoolbox {

/**
 * @return std::string without trailing whitespace characters.
 */
inline std::string &rtrim(std::string &str) noexcept {
  str.erase(str.find_last_not_of(" \t") + 1);
  return str;
}

/**
 * @return std::tring without leading whitespace characters.
 */
inline std::string &ltrim(std::string &str) noexcept {
  str.erase(0, str.find_first_not_of(" \t"));
  return str;
}

/**
 * @return std:string without leading and trailing whitespace characters.
 */
inline std::string &trim(std::string &str) noexcept {
  return ltrim(rtrim(str));
}

/**
 * @return std:string where all occurrences of characters FROM are replaced with TO.
 */
inline std::string replaceAll(const std::string &str,
                              const char &FROM,
                              const char &TO) noexcept {
  std::string retVal{str};
  std::replace(retVal.begin(), retVal.end(), FROM, TO);
  return retVal;
}

/**
 * @return std::vector<std:string> where the given string is split along delimiter.
 */
inline std::vector<std::string> split(const std::string &str,
                               const char &delimiter) noexcept {
  std::vector<std::string> retVal{};
  std::string::size_type prev{0};
  for (std::string::size_type i{str.find_first_of(delimiter, prev)};
       i != std::string::npos;
       prev = i + 1, i = str.find_first_of(delimiter, prev)) {
    if (i != prev) {
      retVal.emplace_back(str.substr(prev, i - prev));
    }
    else {
      retVal.emplace_back("");
    }
  }
  if (prev < str.size()) {
    retVal.emplace_back(str.substr(prev, str.size() - prev));
  }
  else if (prev > 0) {
    retVal.emplace_back("");
  }
  return retVal;
}

} // namespace stringtoolbox

#endif
// "License": Public Domain
// I, Mathias Panzenböck, place this file hereby into the public domain. Use it at your own risk for whatever you like.
// In case there are jurisdictions that don't support putting things in the public domain you can also consider it to
// be "dual licensed" under the BSD, MIT and Apache licenses, if you want to. This code is trivial anyway. Consider it
// an example on how to get the endian conversion functions on different platforms.

// Updated for FreeBSD 10.1+, DragonFly 4.2+, NetBSD 6.1.5+, fixes for Win32,
// and support for emscripten; Christian Berger.

#ifndef CLUON_PORTABLEENDIAN_HPP
#define CLUON_PORTABLEENDIAN_HPP

// clang-format off
#if defined(__linux__) || defined(__CYGWIN__)
    #include <endian.h>
#elif defined(__APPLE__)
    #include <libkern/OSByteOrder.h>
    #define htobe16(x) OSSwapHostToBigInt16(x)
    #define htole16(x) OSSwapHostToLittleInt16(x)
    #define be16toh(x) OSSwapBigToHostInt16(x)
    #define le16toh(x) OSSwapLittleToHostInt16(x)

    #define htobe32(x) OSSwapHostToBigInt32(x)
    #define htole32(x) OSSwapHostToLittleInt32(x)
    #define be32toh(x) OSSwapBigToHostInt32(x)
    #define le32toh(x) OSSwapLittleToHostInt32(x)

    #define htobe64(x) OSSwapHostToBigInt64(x)
    #define htole64(x) OSSwapHostToLittleInt64(x)
    #define be64toh(x) OSSwapBigToHostInt64(x)
    #define le64toh(x) OSSwapLittleToHostInt64(x)
#elif defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__DragonFly__)
    #include <sys/endian.h>
#elif (defined(_WIN16) || defined(_WIN32) || defined(_WIN64))
    #if BYTE_ORDER == LITTLE_ENDIAN
        // Add missing definitions for MinGW.
        #ifndef htonll
            #define htonll(x) ((1==htonl(1)) ? (x) : (((uint64_t)htonl((x) & 0xFFFFFFFFUL)) << 32) | htonl((uint32_t)((x) >> 32)))
        #endif
        #ifndef ntohll
            #define ntohll(x) ((1==ntohl(1)) ? (x) : (((uint64_t)ntohl((x) & 0xFFFFFFFFUL)) << 32) | ntohl((uint32_t)((x) >> 32)))
        #endif

        #define htobe16(x) htons(x)
        #define htole16(x) (x)
        #define be16toh(x) ntohs(x)
        #define le16toh(x) (x)

        #define htobe32(x) htonl(x)
        #define htole32(x) (x)
        #define be32toh(x) ntohl(x)
        #define le32toh(x) (x)

        #define htobe64(x) htonll(x)
        #define htole64(x) (x)
        #define be64toh(x) ntohll(x)
        #define le64toh(x) (x)
    #elif BYTE_ORDER == BIG_ENDIAN
        /* that would be xbox 360 */
        #define htobe16(x) (x)
        #define htole16(x) __builtin_bswap16(x)
        #define be16toh(x) (x)
        #define le16toh(x) __builtin_bswap16(x)

        #define htobe32(x) (x)
        #define htole32(x) __builtin_bswap32(x)
        #define be32toh(x) (x)
        #define le32toh(x) __builtin_bswap32(x)

        #define htobe64(x) (x)
        #define htole64(x) __builtin_bswap64(x)
        #define be64toh(x) (x)
        #define le64toh(x) __builtin_bswap64(x)
    #else
        #error byte order not supported
    #endif
#else
    #ifdef __EMSCRIPTEN__
        #include <endian.h>
    #else
        #warning platform not supported
    #endif
#endif
// clang-format on
#endif
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#ifndef CLUON_CLUON_HPP
#define CLUON_CLUON_HPP

// clang-format off
#ifdef WIN32
    #ifdef _WIN64
        #define ssize_t __int64
    #else
        #define ssize_t long
    #endif

    // Disable warning "'<': signed/unsigned mismatch".
    #pragma warning(disable : 4018)
    // Disable warning "Unary minus operator applied to unsigned type, result still unsigned".
    #pragma warning(disable : 4146)
    // Disable warning "Possible loss of precision".
    #pragma warning(disable : 4244)
    // Disable warning "Conversion from 'size_t' to 'type', possible loss of data".
    #pragma warning(disable : 4267)
    // Disable warning "'static_cast': truncation of constant value".
    #pragma warning(disable : 4309)
    // Disable warning "'operator ""s': literal suffix identifiers that do not start with an underscore are reserved".
    #pragma warning(disable : 4455)
    // Disable deprecated API warnings.
    #pragma warning(disable : 4996)

    // Link against ws2_32.lib for networking.
    #pragma comment(lib, "ws2_32.lib")
    // Link against iphlpapi.lib for address resolving.
    #pragma comment(lib, "iphlpapi.lib")

    // Avoid include definitions from Winsock v1.
    #define WIN32_LEAN_AND_MEAN

    // Export symbols.
    #ifdef LIBCLUON_SHARED
        #ifdef LIBCLUON_EXPORTS
            #define LIBCLUON_API __declspec(dllexport)
        #else
            #define LIBCLUON_API __declspec(dllimport)
        #endif
    #else
        // If linking statically:
        #define LIBCLUON_API
    #endif
#else
    // Undefine define for non-Win32 systems:
    #define LIBCLUON_API
#endif
// clang-format on

//#include "cluon/PortableEndian.hpp"

#include <map>
#include <string>

namespace cluon {

/**
This class can be used to define hash keys.
*/
class UseUInt32ValueAsHashKey {
   public:
    inline std::size_t operator()(const uint32_t v) const noexcept {
        return static_cast<std::size_t>(v);
    }
};

/**
 * @return Map for command line parameters passed as --key=value into key->values.
 */
std::map<std::string, std::string> getCommandlineArguments(int32_t argc, char **argv) noexcept;

} // namespace cluon

#endif
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#ifndef CLUON_TERMINATEHANDLER_HPP
#define CLUON_TERMINATEHANDLER_HPP

//#include "cluon/cluon.hpp"

#include <atomic>
#include <csignal>

namespace cluon {

class LIBCLUON_API TerminateHandler {
   private:
    TerminateHandler(const TerminateHandler &) = delete;
    TerminateHandler(TerminateHandler &&)      = delete;
    TerminateHandler &operator=(const TerminateHandler &) = delete;
    TerminateHandler &operator=(TerminateHandler &&) = delete;

   public:
    /**
     * Define singleton behavior using static initializer (cf. http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2011/n3242.pdf, Sec. 6.7.4).
     * @return singleton for an instance of this class.
     */
    static TerminateHandler &instance() noexcept {
        static TerminateHandler instance;
        return instance;
    }

    ~TerminateHandler() = default;

   public:
    std::atomic<bool> isTerminated{false};

   private:
    TerminateHandler() noexcept;

#ifndef WIN32
    struct sigaction m_signalHandler {};
#endif
};
} // namespace cluon

#endif
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#ifndef CLUON_NOTIFYINGPIPELINE_HPP
#define CLUON_NOTIFYINGPIPELINE_HPP

//#include "cluon/cluon.hpp"

#include <atomic>
#include <condition_variable>
#include <deque>
#include <functional>
#include <mutex>
#include <thread>

namespace cluon {

template <class T>
class LIBCLUON_API NotifyingPipeline {
   private:
    NotifyingPipeline(const NotifyingPipeline &) = delete;
    NotifyingPipeline(NotifyingPipeline &&)      = delete;
    NotifyingPipeline &operator=(const NotifyingPipeline &) = delete;
    NotifyingPipeline &operator=(NotifyingPipeline &&) = delete;

   public:
    NotifyingPipeline(std::function<void(T &&)> delegate)
        : m_delegate(delegate) {
        m_pipelineThread = std::thread(&NotifyingPipeline::processPipeline, this);

        // Let the operating system spawn the thread.
        using namespace std::literals::chrono_literals; // NOLINT
        do { std::this_thread::sleep_for(1ms); } while (!m_pipelineThreadRunning.load());
    }

    ~NotifyingPipeline() {
        m_pipelineThreadRunning.store(false);

        // Wake any waiting threads.
        m_pipelineCondition.notify_all();

        // Joining the thread could fail.
        try {
            if (m_pipelineThread.joinable()) {
                m_pipelineThread.join();
            }
        } catch (...) {} // LCOV_EXCL_LINE
    }

   public:
    inline void add(T &&entry) noexcept {
        std::unique_lock<std::mutex> lck(m_pipelineMutex);
        m_pipeline.emplace_back(entry);
    }

    inline void notifyAll() noexcept { m_pipelineCondition.notify_all(); }

    inline bool isRunning() noexcept { return m_pipelineThreadRunning.load(); }

   private:
    inline void processPipeline() noexcept {
        // Indicate to caller that we are ready.
        m_pipelineThreadRunning.store(true);

        while (m_pipelineThreadRunning.load()) {
            std::unique_lock<std::mutex> lck(m_pipelineMutex);
            // Wait until the thread should stop or data is available.
            m_pipelineCondition.wait(lck, [this] { return (!this->m_pipelineThreadRunning.load() || !this->m_pipeline.empty()); });

            // The condition will automatically lock the mutex after waking up.
            // As we are locking per entry, we need to unlock the mutex first.
            lck.unlock();

            uint32_t entries{0};
            {
                lck.lock();
                entries = static_cast<uint32_t>(m_pipeline.size());
                lck.unlock();
            }
            for (uint32_t i{0}; i < entries; i++) {
                T entry;
                {
                    lck.lock();
                    entry = m_pipeline.front();
                    lck.unlock();
                }

                if (nullptr != m_delegate) {
                    m_delegate(std::move(entry));
                }

                {
                    lck.lock();
                    m_pipeline.pop_front();
                    lck.unlock();
                }
            }
        }
    }

   private:
    std::function<void(T &&)> m_delegate;

    std::atomic<bool> m_pipelineThreadRunning{false};
    std::thread m_pipelineThread{};
    std::mutex m_pipelineMutex{};
    std::condition_variable m_pipelineCondition{};

    std::deque<T> m_pipeline{};
};
} // namespace cluon

#endif
/*
 * Copyright (C) 2019  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#ifndef CLUON_IPV4TOOLS_HPP
#define CLUON_IPV4TOOLS_HPP

#include <string>

namespace cluon {

/**
 * @return IPv4-formatted string for the given hostname or the empty string.
 */
std::string getIPv4FromHostname(const std::string &hostname) noexcept;

} // namespace cluon

#endif
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#ifndef CLUON_UDPPACKETSIZECONSTRAINTS_H
#define CLUON_UDPPACKETSIZECONSTRAINTS_H

#include <cstdint>

// clang-format off
namespace cluon {
    enum class UDPPacketSizeConstraints : uint16_t {
        SIZE_IPv4_HEADER    = 20,
        SIZE_UDP_HEADER     = 8,
        MAX_SIZE_UDP_PACKET = 0xFFFF, };
}
// clang-format on

#endif
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#ifndef CLUON_UDPSENDER_HPP
#define CLUON_UDPSENDER_HPP

//#include "cluon/cluon.hpp"

// clang-format off
#ifdef WIN32
    #include <Winsock2.h> // for WSAStartUp
    #include <ws2tcpip.h> // for SOCKET
#else
    #include <netinet/in.h>
#endif
// clang-format on

#include <cstdint>
#include <mutex>
#include <string>
#include <utility>

namespace cluon {
/**
To send data using a UDP socket, simply include the header
`#include <cluon/UDPSender.hpp>`.

Next, create an instance of class `cluon::UDPSender` as follows:
`cluon::UDPSender sender("127.0.0.1", 1234);`. The first parameter is of type
`std::string` expecting a numerical IPv4 address and the second parameter
specifies the UDP port to which the data shall be sent to.

To finally send data, simply call the method `send` supplying the data to be
sent: `sender.send(std::move("Hello World!")`. Please note that the data is
supplied using the _move_-semantics. The method `send` returns a
`std::pair<ssize_t, int32_t>` where the first element returns the size of the
successfully sent bytes and the second element contains the error code in case
the transmission of the data failed.

\code{.cpp}
cluon::UDPSender sender("127.0.0.1", 1234);

std::pair<ssize_t, int32_t> retVal = sender.send(std::move("Hello World!"));

std::cout << "Send " << retVal.first << " bytes, error code = " << retVal.second << std::endl;
\endcode

A complete example is available
[here](https://github.com/chrberger/libcluon/blob/master/libcluon/examples/cluon-UDPSender.cpp).
*/
class LIBCLUON_API UDPSender {
   private:
    UDPSender(const UDPSender &) = delete;
    UDPSender(UDPSender &&)      = delete;
    UDPSender &operator=(const UDPSender &) = delete;
    UDPSender &operator=(UDPSender &&) = delete;

   public:
    /**
     * Constructor.
     *
     * @param sendToAddress Numerical IPv4 address to send a UDP packet to.
     * @param sendToPort Port to send a UDP packet to.
     */
    UDPSender(const std::string &sendToAddress, uint16_t sendToPort) noexcept;
    ~UDPSender() noexcept;

    /**
     * Send a given string.
     *
     * @param data Data to send.
     * @return Pair: Number of bytes sent and errno.
     */
    std::pair<ssize_t, int32_t> send(std::string &&data) const noexcept;

   public:
    /**
     * @return Port that this UDP sender will use for sending or 0 if no information available.
     */
    uint16_t getSendFromPort() const noexcept;

   private:
    mutable std::mutex m_socketMutex{};
    int32_t m_socket{-1};
    uint16_t m_portToSentFrom{0};
    struct sockaddr_in m_sendToAddress {};
};
} // namespace cluon

#endif
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#ifndef CLUON_UDPRECEIVER_HPP
#define CLUON_UDPRECEIVER_HPP

//#include "cluon/NotifyingPipeline.hpp"
//#include "cluon/cluon.hpp"

// clang-format off
#ifdef WIN32
    #include <Winsock2.h> // for WSAStartUp
    #include <ws2tcpip.h> // for SOCKET
#else
    #include <netinet/in.h>
#endif
// clang-format on

#include <cstdint>
#include <atomic>
#include <condition_variable>
#include <deque>
#include <chrono>
#include <functional>
#include <memory>
#include <mutex>
#include <set>
#include <string>
#include <thread>

namespace cluon {
/**
To receive data from a UDP socket, simply include the header
`#include <cluon/UDPReceiver.hpp>`.

Next, create an instance of class `cluon::UDPReceiver` as follows:
`cluon::UDPReceiver receiver("127.0.0.1", 1234, delegate);`.
The first parameter is of type `std::string` expecting a numerical IPv4 address,
the second parameter specifies the UDP port, from which data shall be received
from, and the last parameter is of type `std::function` that is called whenever
new bytes are available to be processed.

The complete signature for the delegate function is
`std::function<void(std::string &&, std::string &&, std::chrono::system_clock::time_point &&) noexcept>`:
The first parameter contains the bytes that have been received, the second
parameter containes the human-readable representation of the sender
(X.Y.Z.W:ABCD), and the last parameter is the time stamp when the data has been
received. An example using a C++ lambda expression would look as follows:

\code{.cpp}
cluon::UDPReceiver receiver("127.0.0.1", 1234,
    [](std::string &&data, std::string &&sender, std::chrono::system_clock::time_point &&ts) noexcept {
        const auto timestamp(std::chrono::system_clock::to_time_t(ts));
        std::cout << "Received " << data.size() << " bytes"
                  << " from " << sender
                  << " at " << std::put_time(std::localtime(&timestamp), "%Y-%m-%d %X")
                  << ", containing '" << data << "'." << std::endl;
    });
\endcode

After creating an instance of class `cluon::UDPReceiver`, it is immediately
activated and concurrently waiting for data in a separate thread. To check
whether the instance was created successfully and running, the method
`isRunning()` should be called.

A complete example is available
[here](https://github.com/chrberger/libcluon/blob/master/libcluon/examples/cluon-UDPReceiver.cpp).
*/
class LIBCLUON_API UDPReceiver {
   private:
    UDPReceiver(const UDPReceiver &) = delete;
    UDPReceiver(UDPReceiver &&)      = delete;
    UDPReceiver &operator=(const UDPReceiver &) = delete;
    UDPReceiver &operator=(UDPReceiver &&) = delete;

   public:
    /**
     * Constructor.
     *
     * @param receiveFromAddress Numerical IPv4 address to receive UDP packets from.
     * @param receiveFromPort Port to receive UDP packets from.
     * @param delegate Functional (noexcept) to handle received bytes; parameters are received data, sender, timestamp.
     * @param localSendFromPort Port that an application is using to send data. This port (> 0) is ignored when data is received.
     */
    UDPReceiver(const std::string &receiveFromAddress,
                uint16_t receiveFromPort,
                std::function<void(std::string &&, std::string &&, std::chrono::system_clock::time_point &&)> delegate,
                uint16_t localSendFromPort = 0) noexcept;
    ~UDPReceiver() noexcept;

    /**
     * @return true if the UDPReceiver could successfully be created and is able to receive data.
     */
    bool isRunning() const noexcept;

   private:
    /**
     * This method closes the socket.
     *
     * @param errorCode Error code that caused this closing.
     */
    void closeSocket(int errorCode) noexcept;

    void readFromSocket() noexcept;

   private:
    int32_t m_socket{-1};
    bool m_isBlockingSocket{true};
    std::set<unsigned long> m_listOfLocalIPAddresses{};
    uint16_t m_localSendFromPort;
    struct sockaddr_in m_receiveFromAddress {};
    struct ip_mreq m_mreq {};
    bool m_isMulticast{false};

    std::atomic<bool> m_readFromSocketThreadRunning{false};
    std::thread m_readFromSocketThread{};

   private:
    std::function<void(std::string &&, std::string &&, std::chrono::system_clock::time_point)> m_delegate{};

   private:
    class PipelineEntry {
       public:
        std::string m_data;
        std::string m_from;
        std::chrono::system_clock::time_point m_sampleTime;
    };

    std::shared_ptr<cluon::NotifyingPipeline<PipelineEntry>> m_pipeline{};
};
} // namespace cluon

#endif
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#ifndef CLUON_TCPCONNECTION_HPP
#define CLUON_TCPCONNECTION_HPP

//#include "cluon/NotifyingPipeline.hpp"
//#include "cluon/cluon.hpp"

// clang-format off
#ifdef WIN32
    #include <Winsock2.h> // for WSAStartUp
    #include <ws2tcpip.h> // for SOCKET
#else
    #include <netinet/in.h>
#endif
// clang-format on

#include <cstdint>
#include <atomic>
#include <chrono>
#include <functional>
#include <memory>
#include <mutex>
#include <string>
#include <thread>

namespace cluon {
/**
To exchange data via TCP, simply include the header
`#include <cluon/TCPConnection.hpp>`.

Next, create an instance of class `cluon::TCPConnection` as follows:
`cluon::TCPConnection connection("127.0.0.1", 1234, newDataDelegate, connectionLostDelegate);`.
The first parameter is of type `std::string` expecting a numerical IPv4 address,
the second parameter specifies the TCP port, from which data shall be received
from, the third paraemter is of type `std::function` that is called whenever
new bytes are available to be processed, and the last parameter is of type
`std::function` that is called when the connection is lost.

The complete signature for the newDataDelegate function is
`std::function<void(std::string &&, std::string &&, std::chrono::system_clock::time_point &&) noexcept>`:
The first parameter contains the bytes that have been received, the second
parameter containes the human-readable representation of the sender
(X.Y.Z.W:ABCD), and the last parameter is the time stamp when the data has been
received.

The complete signature for the connectionLostDelegate function is
`std::function<void() noexcept>`.

To finally send data, simply call the method `send` supplying the data to be
sent: `connection.send(std::move("Hello World!")`. Please note that the data is
supplied using the _move_-semantics. The method `send` returns a
`std::pair<ssize_t, int32_t>` where the first element returns the size of the
successfully sent bytes and the second element contains the error code in case
the transmission of the data failed.

An example using a C++ lambda expression would look as follows:

\code{.cpp}
cluon::TCPConnection connection("127.0.0.1", 1234,
    [](std::string &&data, std::string &&sender, std::chrono::system_clock::time_point &&ts) noexcept {
        const auto timestamp(std::chrono::system_clock::to_time_t(ts));
        std::cout << "Received " << data.size() << " bytes"
                  << " from " << sender
                  << " at " << std::put_time(std::localtime(&timestamp), "%Y-%m-%d %X")
                  << ", containing '" << data << "'." << std::endl;
    },
    [](){ std::cout << "Connection lost." << std::endl; });

std::pair<ssize_t, int32_t> retVal = connection.send(std::move("Hello World!"));
\endcode

After creating an instance of class `cluon::TCPConnection`, it is immediately
activated and concurrently waiting for data in a separate thread. To check
whether the instance was created successfully and running, the method
`isRunning()` should be called.
*/
class LIBCLUON_API TCPConnection {
   private:
    friend class TCPServer;

    /**
     * Constructor that is only accessible to TCPServer to manage incoming TCP connections.
     *
     * @param socket Socket to handle an existing TCP connection described by this socket.
     */
    TCPConnection(const int32_t &socket) noexcept;

   private:
    TCPConnection(const TCPConnection &) = delete;
    TCPConnection(TCPConnection &&)      = delete;
    TCPConnection &operator=(const TCPConnection &) = delete;
    TCPConnection &operator=(TCPConnection &&) = delete;

   public:
    /**
     * Constructor to connect to a TCP server.
     *
     * @param address Numerical IPv4 address to receive UDP packets from.
     * @param port Port to receive UDP packets from.
     * @param newDataDelegate Functional (noexcept) to handle received bytes; parameters are received data, timestamp.
     * @param connectionLostDelegate Functional (noexcept) to handle a lost connection.
     */
    TCPConnection(const std::string &address,
                  uint16_t port,
                  std::function<void(std::string &&, std::chrono::system_clock::time_point &&)> newDataDelegate = nullptr,
                  std::function<void()> connectionLostDelegate                                                  = nullptr) noexcept;

    ~TCPConnection() noexcept;

   public:
    void setOnNewData(std::function<void(std::string &&, std::chrono::system_clock::time_point &&)> newDataDelegate) noexcept;
    void setOnConnectionLost(std::function<void()> connectionLostDelegate) noexcept;

   public:
    /**
     * @return true if the TCPConnection could successfully be created and is able to receive data.
     */
    bool isRunning() const noexcept;

    /**
     * Send a given string.
     *
     * @param data Data to send.
     * @return Pair: Number of bytes sent and errno.
     */
    std::pair<ssize_t, int32_t> send(std::string &&data) const noexcept;

   private:
    /**
     * This method closes the socket.
     *
     * @param errorCode Error code that caused this closing.
     */
    void closeSocket(int errorCode) noexcept;
    void startReadingFromSocket() noexcept;
    void readFromSocket() noexcept;

   private:
    mutable std::mutex m_socketMutex{};
    int32_t m_socket{-1};
    bool m_cleanup{true};//if not created from TCPServer,call WSACleanup
    struct sockaddr_in m_address {};

    std::atomic<bool> m_readFromSocketThreadRunning{false};
    std::thread m_readFromSocketThread{};

    std::mutex m_newDataDelegateMutex{};
    std::function<void(std::string &&, std::chrono::system_clock::time_point)> m_newDataDelegate{};

    mutable std::mutex m_connectionLostDelegateMutex{};
    std::function<void()> m_connectionLostDelegate{};

   private:
    class PipelineEntry {
       public:
        std::string m_data;
        std::chrono::system_clock::time_point m_sampleTime;
    };

    std::shared_ptr<cluon::NotifyingPipeline<PipelineEntry>> m_pipeline{};
};
} // namespace cluon

#endif
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#ifndef CLUON_TCPSERVER_HPP
#define CLUON_TCPSERVER_HPP

//#include "cluon/TCPConnection.hpp"
//#include "cluon/cluon.hpp"

// clang-format off
#ifdef WIN32
    #include <Winsock2.h> // for WSAStartUp
    #include <ws2tcpip.h> // for SOCKET
#else
    #include <netinet/in.h>
#endif
// clang-format on

#include <cstdint>
#include <atomic>
#include <functional>
#include <mutex>
#include <string>
#include <thread>

namespace cluon {

class LIBCLUON_API TCPServer {
   private:
    TCPServer(const TCPServer &) = delete;
    TCPServer(TCPServer &&)      = delete;
    TCPServer &operator=(const TCPServer &) = delete;
    TCPServer &operator=(TCPServer &&) = delete;

   public:
    /**
     * Constructor to create a TCP server.
     *
     * @param port Port to receive UDP packets from.
     * @param newConnectionDelegate Functional to handle incoming TCP connections.
     */
    TCPServer(uint16_t port, std::function<void(std::string &&from, std::shared_ptr<cluon::TCPConnection> connection)> newConnectionDelegate) noexcept;

    ~TCPServer() noexcept;

    /**
     * @return true if the TCPServer could successfully be created and is able to receive data.
     */
    bool isRunning() const noexcept;

   private:
    /**
     * This method closes the socket.
     *
     * @param errorCode Error code that caused this closing.
     */
    void closeSocket(int errorCode) noexcept;
    void readFromSocket() noexcept;

   private:
    mutable std::mutex m_socketMutex{};
    int32_t m_socket{-1};

    std::atomic<bool> m_readFromSocketThreadRunning{false};
    std::thread m_readFromSocketThread{};

    std::mutex m_newConnectionDelegateMutex{};
    std::function<void(std::string &&from, std::shared_ptr<cluon::TCPConnection> connection)> m_newConnectionDelegate{};
};
} // namespace cluon

#endif
#ifndef BEGIN_HEADER_ONLY_IMPLEMENTATION
#define BEGIN_HEADER_ONLY_IMPLEMENTATION
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//#include "argh/argh.h"

//#include "cluon/cluon.hpp"

namespace cluon {

inline std::map<std::string, std::string> getCommandlineArguments(int32_t argc, char **argv) noexcept {
    argh::parser commandline{argc, argv};
    std::map<std::string, std::string> retVal;

    for (auto &positionalArgument : commandline.pos_args()) { retVal[positionalArgument] = ""; }

    for (auto &flag : commandline.flags()) { retVal[flag] = "1"; }

    for (auto &parameter : commandline.params()) { retVal[parameter.first] = parameter.second; }

    return retVal;
}

} // namespace cluon
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//#include "cluon/TerminateHandler.hpp"

#include <cstdlib>
#include <cstring>
#include <iostream>

namespace cluon {

inline void cluon_handleExit() {
    TerminateHandler::instance().isTerminated.store(true);
}

inline void cluon_handleSignal(int32_t /*signal*/) {       // LCOV_EXCL_LINE
    TerminateHandler::instance().isTerminated.store(true); // LCOV_EXCL_LINE
}

inline TerminateHandler::TerminateHandler() noexcept {
    if (0 != std::atexit(cluon_handleExit)) {
        std::cerr << "[cluon::TerminateHandler] Failed to register cluon_exitHandler()." << std::endl; // LCOV_EXCL_LINE
    }

#ifdef WIN32
    if (SIG_ERR == ::signal(SIGINT, &cluon_handleSignal)) {
        std::cerr << "[cluon::TerminateHandler] Failed to register signal SIGINT." << std::endl;
    }
    if (SIG_ERR == ::signal(SIGTERM, &cluon_handleSignal)) {
        std::cerr << "[cluon::TerminateHandler] Failed to register signal SIGTERM." << std::endl;
    }
#else
    std::memset(&m_signalHandler, 0, sizeof(m_signalHandler));
    m_signalHandler.sa_handler = &cluon_handleSignal;

    if (::sigaction(SIGINT, &m_signalHandler, NULL) < 0) {
        std::cerr << "[cluon::TerminateHandler] Failed to register signal SIGINT." << std::endl; // LCOV_EXCL_LINE
    }
    if (::sigaction(SIGTERM, &m_signalHandler, NULL) < 0) {
        std::cerr << "[cluon::TerminateHandler] Failed to register signal SIGTERM." << std::endl; // LCOV_EXCL_LINE
    }
#endif
}

} // namespace cluon
/*
 * Copyright (C) 2019  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//#include "cluon/IPv4Tools.hpp"

// clang-format off
#ifdef WIN32
    #include <Winsock2.h> // for WSAStartUp
    #include <ws2tcpip.h>
    #include <iostream>
#else
    #include <arpa/inet.h>
    #include <netdb.h>

    #if defined(BSD)
        #include <netinet/in.h>
        #include <sys/socket.h>
    #endif
#endif
// clang-format on

#include <cstring>

namespace cluon {

inline std::string getIPv4FromHostname(const std::string &hostname) noexcept {
#ifdef WIN32
    // Load Winsock 2.2 DLL.
    WSADATA wsaData;
    if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
        std::cerr << "[cluon::getIPv4FromHostname] Error while calling WSAStartUp: " << WSAGetLastError() << std::endl;
    }
#endif
    std::string result{""};
    if (!hostname.empty()) {
        struct addrinfo hint;
        {
            std::memset(&hint, 1, sizeof(struct addrinfo));
            hint.ai_flags = AI_CANONNAME;
            hint.ai_family = AF_INET;
            hint.ai_socktype = 0;
            hint.ai_protocol = 0;
            hint.ai_addrlen = 0;
            hint.ai_canonname = nullptr;
            hint.ai_addr = nullptr;
            hint.ai_next = nullptr;
        }

        struct addrinfo *listOfHosts{nullptr};
        if (0 == getaddrinfo(hostname.c_str(), nullptr, &hint, &listOfHosts)) {
            for(struct addrinfo *e = listOfHosts; nullptr != listOfHosts; listOfHosts = listOfHosts->ai_next) {
                if (nullptr != e) {
                    if (AF_INET == e->ai_family) {
                        struct sockaddr_in *sinp = reinterpret_cast<struct sockaddr_in*>(e->ai_addr);
                        char buf[INET_ADDRSTRLEN];
                        const char *addr = inet_ntop(AF_INET, &sinp->sin_addr, buf, INET_ADDRSTRLEN);
                        if ( (nullptr != addr) && (result.empty()) ) {
                            result = std::string(addr);
                            break;
                        }
                    }
                }
            }
        }

        if (nullptr != listOfHosts) {
            freeaddrinfo(listOfHosts);
        }
    }
#ifdef WIN32
    WSACleanup();
#endif
    return result;
}

} // namespace cluon
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//#include "cluon/UDPSender.hpp"
//#include "cluon/IPv4Tools.hpp"
//#include "cluon/UDPPacketSizeConstraints.hpp"

// clang-format off
#ifndef WIN32
    #include <arpa/inet.h>
    #include <ifaddrs.h>
    #include <netdb.h>
    #include <sys/socket.h>
    #include <sys/types.h>
    #include <unistd.h>
#endif
// clang-format on

#include <cerrno>
#include <cstring>
#include <algorithm>
#include <iostream>
#include <iterator>
#include <sstream>
#include <vector>

namespace cluon {

inline UDPSender::UDPSender(const std::string &sendToAddress, uint16_t sendToPort) noexcept
    : m_socketMutex()
    , m_sendToAddress() {
    // Decompose given address into tokens to check validity with numerical IPv4 address.
    std::string tmp{cluon::getIPv4FromHostname(sendToAddress)};
    std::replace(tmp.begin(), tmp.end(), '.', ' ');
    std::istringstream sstr{tmp};
    std::vector<int> sendToAddressTokens{std::istream_iterator<int>(sstr), std::istream_iterator<int>()};

    if (!sendToAddress.empty() && (4 == sendToAddressTokens.size())
        && !(std::end(sendToAddressTokens) != std::find_if(sendToAddressTokens.begin(), sendToAddressTokens.end(), [](int a) { return (a < 0) || (a > 255); }))
        && (0 < sendToPort)) {
        ::memset(&m_sendToAddress, 0, sizeof(m_sendToAddress));
        m_sendToAddress.sin_addr.s_addr = ::inet_addr(sendToAddress.c_str());
        m_sendToAddress.sin_family      = AF_INET;
        m_sendToAddress.sin_port        = htons(sendToPort);

#ifdef WIN32
        // Load Winsock 2.2 DLL.
        WSADATA wsaData;
        if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
            std::cerr << "[cluon::UDPSender] Error while calling WSAStartUp: " << WSAGetLastError() << std::endl;
        }
#endif

        m_socket = ::socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);

#ifndef WIN32
        // Check whether given address is a broadcast address.
        bool isBroadcast{false};
        {
            isBroadcast |= (sendToAddress == "255.255.255.255");
            if (!isBroadcast) {
                struct ifaddrs *ifaddr{nullptr};
                if (0 == getifaddrs(&ifaddr)) {
                    for (struct ifaddrs *ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next) {
                        if (NULL == ifa->ifa_addr) continue; // LCOV_EXCL_LINE
                        char broadcastAddress[NI_MAXHOST];
#ifdef __APPLE__
                        if (NULL == ifa->ifa_dstaddr) continue; // LCOV_EXCL_LINE
                        if (0 == ::getnameinfo(ifa->ifa_dstaddr,
                               sizeof(struct sockaddr_in),
                               broadcastAddress, NI_MAXHOST,
                               NULL, 0, NI_NUMERICHOST))
#else
                        if (NULL == ifa->ifa_ifu.ifu_broadaddr) continue; // LCOV_EXCL_LINE
                        if (0 == ::getnameinfo(ifa->ifa_ifu.ifu_broadaddr,
                               sizeof(struct sockaddr_in),
                               broadcastAddress, NI_MAXHOST,
                               NULL, 0, NI_NUMERICHOST))
#endif
                        {
                             std::string _tmp{broadcastAddress};
                             isBroadcast |= (_tmp.compare(sendToAddress) == 0);
                        }
                    }
                    freeifaddrs(ifaddr);
                }
            }
        }
#endif

#ifndef WIN32
        if (!(m_socket < 0) && isBroadcast) {
            // Enabling broadcast.
            uint32_t YES = 1;
            // clang-format off
            auto retVal = ::setsockopt(m_socket, SOL_SOCKET, SO_BROADCAST, reinterpret_cast<char *>(&YES), sizeof(YES)); // NOLINT
            // clang-format on
            if (0 > retVal) {
#ifdef WIN32 // LCOV_EXCL_LINE
                auto errorCode = WSAGetLastError();
#else
                auto errorCode = errno; // LCOV_EXCL_LINE
#endif                                  // LCOV_EXCL_LINE
                std::cerr << "[cluon::UDPSender] Failed to perform socket operation: "; // LCOV_EXCL_LINE
#ifdef WIN32 // LCOV_EXCL_LINE
                std::cerr << errorCode << std::endl; 
#else
                std::cerr << ::strerror(errorCode) << " (" << errorCode << ")" << std::endl; // LCOV_EXCL_LINE
#endif // LCOV_EXCL_LINE
            }
        }
#endif

        // Bind to random address/port but store sender port.
        if (!(m_socket < 0)) {
            struct sockaddr_in sendFromAddress;
            std::memset(&sendFromAddress, 0, sizeof(sendFromAddress));
            sendFromAddress.sin_family = AF_INET;
            sendFromAddress.sin_port   = 0;                                                                              // Randomly choose a port to bind.
            if (0 == ::bind(m_socket, reinterpret_cast<struct sockaddr *>(&sendFromAddress), sizeof(sendFromAddress))) { // NOLINT
                struct sockaddr tmpAddr;
                socklen_t length = sizeof(tmpAddr);
                if (0 == ::getsockname(m_socket, &tmpAddr, &length)) {
                    struct sockaddr_in tmpAddrIn;
                    std::memcpy(&tmpAddrIn, &tmpAddr, sizeof(tmpAddrIn)); /* Flawfinder: ignore */ // NOLINT
                    m_portToSentFrom = ntohs(tmpAddrIn.sin_port);
                }
            }
        }

#ifdef WIN32
        if (m_socket < 0) {
            std::cerr << "[cluon::UDPSender] Error while creating socket: " << WSAGetLastError() << std::endl;
            WSACleanup();
        }
#endif
    }
}

inline UDPSender::~UDPSender() noexcept {
    if (!(m_socket < 0)) {
#ifdef WIN32
        ::shutdown(m_socket, SD_BOTH);
        ::closesocket(m_socket);
        WSACleanup();
#else
        ::shutdown(m_socket, SHUT_RDWR); // Disallow further read/write operations.
        ::close(m_socket);
#endif
    }
    m_socket = -1;
}

inline uint16_t UDPSender::getSendFromPort() const noexcept {
    return m_portToSentFrom;
}

inline std::pair<ssize_t, int32_t> UDPSender::send(std::string &&data) const noexcept {
    if (-1 == m_socket) {
        return {-1, EBADF};
    }

    if (data.empty()) {
        return {0, 0};
    }

    constexpr uint16_t MAX_LENGTH = static_cast<uint16_t>(UDPPacketSizeConstraints::MAX_SIZE_UDP_PACKET)
                                    - static_cast<uint16_t>(UDPPacketSizeConstraints::SIZE_IPv4_HEADER)
                                    - static_cast<uint16_t>(UDPPacketSizeConstraints::SIZE_UDP_HEADER);
    if (MAX_LENGTH < data.size()) {
        return {-1, E2BIG};
    }

    std::lock_guard<std::mutex> lck(m_socketMutex);
    ssize_t bytesSent = ::sendto(m_socket,
                                 data.c_str(),
                                 data.length(),
                                 0,
                                 reinterpret_cast<const struct sockaddr *>(&m_sendToAddress), // NOLINT
                                 sizeof(m_sendToAddress));

    return {bytesSent, (0 > bytesSent ? errno : 0)};
}
} // namespace cluon
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//#include "cluon/UDPReceiver.hpp"
//#include "cluon/IPv4Tools.hpp"
//#include "cluon/TerminateHandler.hpp"
//#include "cluon/UDPPacketSizeConstraints.hpp"

// clang-format off
#ifdef WIN32
    #include <cstdio>
    #include <cerrno>

    #include <winsock2.h>
    #include <iphlpapi.h>
    #include <ws2tcpip.h>

    #include <iostream>
#else
    #ifdef __linux__
        #include <linux/sockios.h>
    #endif

    #include <arpa/inet.h>
    #include <fcntl.h>
    #include <sys/ioctl.h>
    #include <sys/socket.h>
    #include <sys/types.h>
    #include <unistd.h>
#endif

#ifndef WIN32
    #include <ifaddrs.h>
    #include <netdb.h>
#endif
// clang-format on

#include <cstring>
#include <algorithm>
#include <array>
#include <iostream>
#include <iterator>
#include <sstream>
#include <utility>
#include <vector>

namespace cluon {

inline UDPReceiver::UDPReceiver(const std::string &receiveFromAddress,
                         uint16_t receiveFromPort,
                         std::function<void(std::string &&, std::string &&, std::chrono::system_clock::time_point &&)> delegate,
                         uint16_t localSendFromPort) noexcept
    : m_localSendFromPort(localSendFromPort)
    , m_receiveFromAddress()
    , m_mreq()
    , m_readFromSocketThread()
    , m_delegate(std::move(delegate)) {
    // Decompose given address string to check validity with numerical IPv4 address.
    std::string tmp{cluon::getIPv4FromHostname(receiveFromAddress)};
    std::replace(tmp.begin(), tmp.end(), '.', ' ');
    std::istringstream sstr{tmp};
    std::vector<int> receiveFromAddressTokens{std::istream_iterator<int>(sstr), std::istream_iterator<int>()};

    if ((!receiveFromAddress.empty()) && (4 == receiveFromAddressTokens.size())
        && !(std::end(receiveFromAddressTokens)
             != std::find_if(receiveFromAddressTokens.begin(), receiveFromAddressTokens.end(), [](int a) { return (a < 0) || (a > 255); }))
        && (0 < receiveFromPort)) {
        // Check for valid IP address.
        struct sockaddr_in tmpSocketAddress {};
        const bool isValid = (0 < ::inet_pton(AF_INET, receiveFromAddress.c_str(), &(tmpSocketAddress.sin_addr))) && (224 > receiveFromAddressTokens[0] || 255 == receiveFromAddressTokens[0]); // Accept regular IP addresses (ie., non-multicast addesses and the network-wide broadcast address 255.255.255.255.

        // Check for UDP multicast, i.e., IP address range [225.0.0.1 - 239.255.255.255].
        m_isMulticast = (((224 < receiveFromAddressTokens[0]) && (receiveFromAddressTokens[0] <= 239))
                         && ((0 <= receiveFromAddressTokens[1]) && (receiveFromAddressTokens[1] <= 255))
                         && ((0 <= receiveFromAddressTokens[2]) && (receiveFromAddressTokens[2] <= 255))
                         && ((1 <= receiveFromAddressTokens[3]) && (receiveFromAddressTokens[3] <= 255)));

#ifndef WIN32
        // Check whether given address is a broadcast address.
        bool isBroadcast{false};
        {
            isBroadcast |= (receiveFromAddress == "255.255.255.255");
            if (!isBroadcast) {
                struct ifaddrs *ifaddr{nullptr};
                if (0 == getifaddrs(&ifaddr)) {
                    for (struct ifaddrs *ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next) {
                        if (NULL == ifa->ifa_addr) continue; // LCOV_EXCL_LINE

                        if (ifa->ifa_addr->sa_family == AF_INET) {
                            char broadcastAddress[NI_MAXHOST];
#ifdef __APPLE__
                            if (NULL == ifa->ifa_dstaddr) continue; // LCOV_EXCL_LINE
                            if (0 == ::getnameinfo(ifa->ifa_dstaddr,
                                   sizeof(struct sockaddr_in),
                                   broadcastAddress, NI_MAXHOST,
                                   NULL, 0, NI_NUMERICHOST))
#else
                            if (NULL == ifa->ifa_ifu.ifu_broadaddr) continue; // LCOV_EXCL_LINE
                            if (0 == ::getnameinfo(ifa->ifa_ifu.ifu_broadaddr,
                                   sizeof(struct sockaddr_in),
                                   broadcastAddress, NI_MAXHOST,
                                   NULL, 0, NI_NUMERICHOST))
#endif
                            {
                                 std::string _tmp{broadcastAddress};
                                 isBroadcast |= (_tmp.compare(receiveFromAddress) == 0);
                            }
                        }
                    }
                    freeifaddrs(ifaddr);
                }
            }
        }
#endif

        std::memset(&m_receiveFromAddress, 0, sizeof(m_receiveFromAddress));
#ifdef WIN32
        // According to http://www.sockets.com/err_lst1.htm, the binding is
        // different on Windows opposed to POSIX when using the real address
        // here; thus, we need to use INADDR_ANY.
        m_receiveFromAddress.sin_addr.s_addr = (m_isMulticast ? htonl(INADDR_ANY) : ::inet_addr(receiveFromAddress.c_str()));
#else
        m_receiveFromAddress.sin_addr.s_addr = ::inet_addr(receiveFromAddress.c_str());
#endif
        m_receiveFromAddress.sin_family = AF_INET;
        m_receiveFromAddress.sin_port   = htons(receiveFromPort);

#ifdef WIN32
        // Load Winsock 2.2 DLL.
        WSADATA wsaData;
        if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
            std::cerr << "[cluon::UDPReceiver] Error while calling WSAStartUp: " << WSAGetLastError() << std::endl;
        }
#endif

        m_socket = ::socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);

#ifdef WIN32
        if (m_socket < 0) {
            std::cerr << "[cluon::UDPReceiver] Error while creating socket: " << WSAGetLastError() << std::endl;
            WSACleanup();
        }
#endif

        if (!(m_socket < 0)) {
            // Allow reusing of ports by multiple calls with same address/port.
            uint32_t YES = 1;
            // clang-format off
            auto retVal = ::setsockopt(m_socket, SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<char *>(&YES), sizeof(YES)); // NOLINT
            // clang-format on
            if (0 > retVal) {
#ifdef WIN32 // LCOV_EXCL_LINE
                auto errorCode = WSAGetLastError();
#else
                auto errorCode = errno; // LCOV_EXCL_LINE
#endif                                  // LCOV_EXCL_LINE
                closeSocket(errorCode); // LCOV_EXCL_LINE
            }
        }

#ifndef WIN32
        if (!(m_socket < 0) && isBroadcast) {
            // Enabling broadcast.
            uint32_t YES = 1;
            // clang-format off
            auto retVal = ::setsockopt(m_socket, SOL_SOCKET, SO_BROADCAST, reinterpret_cast<char *>(&YES), sizeof(YES)); // NOLINT
            // clang-format on
            if (0 > retVal) {
#ifdef WIN32 // LCOV_EXCL_LINE
                auto errorCode = WSAGetLastError();
#else
                auto errorCode = errno; // LCOV_EXCL_LINE
#endif                                  // LCOV_EXCL_LINE
                closeSocket(errorCode); // LCOV_EXCL_LINE
            }
        }
#endif

        if (!(m_socket < 0)) {
            // Trying to enable non_blocking mode.
#ifdef WIN32 // LCOV_EXCL_LINE
            u_long nonBlocking = 1;
            m_isBlockingSocket = !(NO_ERROR == ::ioctlsocket(m_socket, FIONBIO, &nonBlocking));
#else
            const int FLAGS    = ::fcntl(m_socket, F_GETFL, 0);
            m_isBlockingSocket = !(0 == ::fcntl(m_socket, F_SETFL, FLAGS | O_NONBLOCK));
#endif
        }

        if (!(m_socket < 0)) {
            // Trying to enable non_blocking mode.
#ifdef WIN32 // LCOV_EXCL_LINE
            u_long nonBlocking = 1;
            m_isBlockingSocket = !(NO_ERROR == ::ioctlsocket(m_socket, FIONBIO, &nonBlocking));
#else
            const int FLAGS    = ::fcntl(m_socket, F_GETFL, 0);
            m_isBlockingSocket = !(0 == ::fcntl(m_socket, F_SETFL, FLAGS | O_NONBLOCK));
#endif
        }

        if (!(m_socket < 0)) {
            // Try setting receiving buffer.
            int recvBuffer{26214400};
            auto retVal = ::setsockopt(m_socket, SOL_SOCKET, SO_RCVBUF, reinterpret_cast<char *>(&recvBuffer), sizeof(recvBuffer));
            if (retVal < 0) {
#ifdef WIN32 // LCOV_EXCL_LINE
                auto errorCode = WSAGetLastError();
#else
                auto errorCode = errno; // LCOV_EXCL_LINE
#endif                                                                                                                                       // LCOV_EXCL_LINE
                std::cerr << "[cluon::UDPReceiver] Error while trying to set SO_RCVBUF to " << recvBuffer << ": " << errorCode << std::endl; // LCOV_EXCL_LINE
            }
        }

        if (!(m_socket < 0)) {
            // Bind to receive address/port.
            // clang-format off
            auto retVal = ::bind(m_socket, reinterpret_cast<struct sockaddr *>(&m_receiveFromAddress), sizeof(m_receiveFromAddress)); // NOLINT
            // clang-format on
            if (0 > retVal) {
#ifdef WIN32 // LCOV_EXCL_LINE
                auto errorCode = WSAGetLastError();
#else
                auto errorCode = errno; // LCOV_EXCL_LINE
#endif                                  // LCOV_EXCL_LINE
                closeSocket(errorCode); // LCOV_EXCL_LINE
            }
        }

        if (!(m_socket < 0)) {
            if (m_isMulticast) {
                // Join the multicast group.
                m_mreq.imr_multiaddr.s_addr = ::inet_addr(receiveFromAddress.c_str());
                m_mreq.imr_interface.s_addr = htonl(INADDR_ANY);
                // clang-format off
                auto retval                 = ::setsockopt(m_socket, IPPROTO_IP, IP_ADD_MEMBERSHIP, reinterpret_cast<char *>(&m_mreq), sizeof(m_mreq)); // NOLINT
                // clang-format on
                if (0 > retval) { // LCOV_EXCL_LINE
#ifdef WIN32                      // LCOV_EXCL_LINE
                    closeSocket(WSAGetLastError());
#else
                    closeSocket(errno); // LCOV_EXCL_LINE
#endif // LCOV_EXCL_LINE
                }
            } else if (!isValid) {
                closeSocket(EBADF);
            }
        }

        // Fill list of local IP address to avoid sending data to ourselves.
        if (!(m_socket < 0)) {
#ifdef WIN32
            DWORD size{0};
            if (ERROR_BUFFER_OVERFLOW == GetAdaptersAddresses(AF_UNSPEC, GAA_FLAG_INCLUDE_PREFIX, NULL, NULL, &size)) {
                PIP_ADAPTER_ADDRESSES adapters = reinterpret_cast<PIP_ADAPTER_ADDRESSES>(malloc(size));
                if (ERROR_SUCCESS == GetAdaptersAddresses(AF_UNSPEC, GAA_FLAG_INCLUDE_PREFIX, NULL, adapters, &size)) {
                    for (PIP_ADAPTER_ADDRESSES adapter = adapters; nullptr != adapter; adapter = adapter->Next) {
                        for (PIP_ADAPTER_UNICAST_ADDRESS unicastAddress = adapter->FirstUnicastAddress; unicastAddress != NULL;
                             unicastAddress                             = unicastAddress->Next) {
                            if (AF_INET == unicastAddress->Address.lpSockaddr->sa_family) {
                                ::getnameinfo(unicastAddress->Address.lpSockaddr, unicastAddress->Address.iSockaddrLength, nullptr, 0, NULL, 0, NI_NUMERICHOST);
                                std::memcpy(&tmpSocketAddress, unicastAddress->Address.lpSockaddr, sizeof(tmpSocketAddress)); /* Flawfinder: ignore */ // NOLINT
                                const unsigned long LOCAL_IP = tmpSocketAddress.sin_addr.s_addr;
                                m_listOfLocalIPAddresses.insert(LOCAL_IP);
                            }
                        }
                    }
                }
                free(adapters);
            }
#else
            struct ifaddrs *interfaceAddress;
            if (0 == ::getifaddrs(&interfaceAddress)) {
                for (struct ifaddrs *it = interfaceAddress; nullptr != it; it = it->ifa_next) {
                    if ((nullptr != it->ifa_addr) && (it->ifa_addr->sa_family == AF_INET)) {
                        if (0 == ::getnameinfo(it->ifa_addr, sizeof(struct sockaddr_in), nullptr, 0, nullptr, 0, NI_NUMERICHOST)) {
                            std::memcpy(&tmpSocketAddress, it->ifa_addr, sizeof(tmpSocketAddress)); /* Flawfinder: ignore */ // NOLINT
                            const unsigned long LOCAL_IP = tmpSocketAddress.sin_addr.s_addr;
                            m_listOfLocalIPAddresses.insert(LOCAL_IP);
                        }
                    }
                }
                ::freeifaddrs(interfaceAddress);
            }
#endif
        }

        if (!(m_socket < 0)) {
            // Constructing the receiving thread could fail.
            try {
                m_readFromSocketThread = std::thread(&UDPReceiver::readFromSocket, this);

                // Let the operating system spawn the thread.
                using namespace std::literals::chrono_literals; // NOLINT
                do { std::this_thread::sleep_for(1ms); } while (!m_readFromSocketThreadRunning.load());
            } catch (...) { closeSocket(ECHILD); } // LCOV_EXCL_LINE

            try {
                m_pipeline = std::make_shared<cluon::NotifyingPipeline<PipelineEntry>>(
                    [this](PipelineEntry &&entry) { this->m_delegate(std::move(entry.m_data), std::move(entry.m_from), std::move(entry.m_sampleTime)); });
                if (m_pipeline) {
                    // Let the operating system spawn the thread.
                    using namespace std::literals::chrono_literals; // NOLINT
                    do { std::this_thread::sleep_for(1ms); } while (!m_pipeline->isRunning());
                }
            } catch (...) { closeSocket(ECHILD); } // LCOV_EXCL_LINE
        }
    }
}

inline UDPReceiver::~UDPReceiver() noexcept {
    {
        m_readFromSocketThreadRunning.store(false);

        // Joining the thread could fail.
        try {
            if (m_readFromSocketThread.joinable()) {
                m_readFromSocketThread.join();
            }
        } catch (...) {} // LCOV_EXCL_LINE
    }

    m_pipeline.reset();

    closeSocket(0);
}

inline void UDPReceiver::closeSocket(int errorCode) noexcept {
    if (0 != errorCode) {
        std::cerr << "[cluon::UDPReceiver] Failed to perform socket operation: ";
#ifdef WIN32
        std::cerr << errorCode << std::endl;
#else
        std::cerr << ::strerror(errorCode) << " (" << errorCode << ")" << std::endl;
#endif
    }

    if (!(m_socket < 0)) {
        if (m_isMulticast) {
            // clang-format off
            auto retVal = ::setsockopt(m_socket, IPPROTO_IP, IP_DROP_MEMBERSHIP, reinterpret_cast<char *>(&m_mreq), sizeof(m_mreq)); // NOLINT
            // clang-format on
            if (0 > retVal) {                                                                         // LCOV_EXCL_LINE
                std::cerr << "[cluon::UDPReceiver] Failed to drop multicast membership" << std::endl; // LCOV_EXCL_LINE
            }
        }

#ifdef WIN32
        ::shutdown(m_socket, SD_BOTH);
        ::closesocket(m_socket);
        WSACleanup();
#else
        ::shutdown(m_socket, SHUT_RDWR); // Disallow further read/write operations.
        ::close(m_socket);
#endif
    }
    m_socket = -1;
}

inline bool UDPReceiver::isRunning() const noexcept {
    return (m_readFromSocketThreadRunning.load() && !TerminateHandler::instance().isTerminated.load());
}

inline void UDPReceiver::readFromSocket() noexcept {
    // Create buffer to store data from socket.
    constexpr uint16_t MAX_LENGTH = static_cast<uint16_t>(UDPPacketSizeConstraints::MAX_SIZE_UDP_PACKET)
                                    - static_cast<uint16_t>(UDPPacketSizeConstraints::SIZE_IPv4_HEADER)
                                    - static_cast<uint16_t>(UDPPacketSizeConstraints::SIZE_UDP_HEADER);
    std::array<char, MAX_LENGTH> buffer{};

    struct timeval timeout {};

    // Define file descriptor set to watch for read operations.
    fd_set setOfFiledescriptorsToReadFrom{};

    // Sender address and port.
    constexpr uint16_t MAX_ADDR_SIZE{1024};
    std::array<char, MAX_ADDR_SIZE> remoteAddress{};

    struct sockaddr_storage remote {};
    socklen_t addrLength{sizeof(remote)};

    // Indicate to main thread that we are ready.
    m_readFromSocketThreadRunning.store(true);

    while (m_readFromSocketThreadRunning.load()) {
        // Define timeout for select system call. The timeval struct must be
        // reinitialized for every select call as it might be modified containing
        // the actual time slept.
        timeout.tv_sec  = 0;
        timeout.tv_usec = 20 * 1000; // Check for new data with 50Hz.

        FD_ZERO(&setOfFiledescriptorsToReadFrom);          // NOLINT
        FD_SET(m_socket, &setOfFiledescriptorsToReadFrom); // NOLINT
        ::select(m_socket + 1, &setOfFiledescriptorsToReadFrom, nullptr, nullptr, &timeout);

        ssize_t totalBytesRead{0};
        if (FD_ISSET(m_socket, &setOfFiledescriptorsToReadFrom)) { // NOLINT
            ssize_t bytesRead{0};
            do {
                bytesRead = ::recvfrom(m_socket,
                                       buffer.data(),
                                       buffer.max_size(),
                                       0,
                                       reinterpret_cast<struct sockaddr *>(&remote), // NOLINT
                                       reinterpret_cast<socklen_t *>(&addrLength));  // NOLINT

                if ((0 < bytesRead) && (nullptr != m_delegate)) {
#ifdef __linux__
                    std::chrono::system_clock::time_point timestamp;
                    struct timeval receivedTimeStamp {};
                    if (0 == ::ioctl(m_socket, SIOCGSTAMP, &receivedTimeStamp)) { // NOLINT
                        // Transform struct timeval to C++ chrono.
                        std::chrono::time_point<std::chrono::system_clock, std::chrono::microseconds> transformedTimePoint(
                            std::chrono::microseconds(receivedTimeStamp.tv_sec * 1000000L + receivedTimeStamp.tv_usec));
                        timestamp = std::chrono::time_point_cast<std::chrono::system_clock::duration>(transformedTimePoint);
                    } else { // LCOV_EXCL_LINE
                        // In case the ioctl failed, fall back to chrono. // LCOV_EXCL_LINE
                        timestamp = std::chrono::system_clock::now(); // LCOV_EXCL_LINE
                    }
#else
                    std::chrono::system_clock::time_point timestamp = std::chrono::system_clock::now();
#endif

                    // Transform sender address to C-string.
                    ::inet_ntop(remote.ss_family,
                                &((reinterpret_cast<struct sockaddr_in *>(&remote))->sin_addr), // NOLINT
                                remoteAddress.data(),
                                remoteAddress.max_size());
                    const unsigned long RECVFROM_IP{reinterpret_cast<struct sockaddr_in *>(&remote)->sin_addr.s_addr}; // NOLINT
                    const uint16_t RECVFROM_PORT{ntohs(reinterpret_cast<struct sockaddr_in *>(&remote)->sin_port)};    // NOLINT

                    // Check if the bytes actually came from us.
                    bool sentFromUs{false};
                    {
                        auto pos                   = m_listOfLocalIPAddresses.find(RECVFROM_IP);
                        const bool sentFromLocalIP = (pos != m_listOfLocalIPAddresses.end() && (*pos == RECVFROM_IP));
                        sentFromUs                 = sentFromLocalIP && (m_localSendFromPort == RECVFROM_PORT);
                    }

                    // Create a pipeline entry to be processed concurrently.
                    if (!sentFromUs) {
                        PipelineEntry pe;
                        pe.m_data       = std::string(buffer.data(), static_cast<size_t>(bytesRead));
                        pe.m_from       = std::string(remoteAddress.data()) + ':' + std::to_string(RECVFROM_PORT);
                        pe.m_sampleTime = timestamp;

                        // Store entry in queue.
                        if (m_pipeline) {
                            m_pipeline->add(std::move(pe));
                        }
                    }
                    totalBytesRead += bytesRead;
                }
            } while (!m_isBlockingSocket && (bytesRead > 0));
        }

        if (static_cast<int32_t>(totalBytesRead) > 0) {
            if (m_pipeline) {
                m_pipeline->notifyAll();
            }
        }
    }
}
} // namespace cluon
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//#include "cluon/TCPConnection.hpp"
//#include "cluon/IPv4Tools.hpp"
//#include "cluon/TerminateHandler.hpp"

// clang-format off
#ifdef WIN32
    #include <errno.h>
    #include <iostream>
#else
    #ifdef __linux__
        #include <linux/sockios.h>
    #endif

    #include <arpa/inet.h>
    #include <sys/ioctl.h>
    #include <sys/socket.h>
    #include <sys/types.h>
    #include <unistd.h>
#endif
// clang-format on

#include <cstring>
#include <algorithm>
#include <array>
#include <iostream>
#include <iterator>
#include <sstream>
#include <utility>
#include <vector>

namespace cluon {

inline TCPConnection::TCPConnection(const int32_t &socket) noexcept
    : m_socket(socket)
    , m_cleanup(false)
    , m_newDataDelegate(nullptr)
    , m_connectionLostDelegate(nullptr) {
    if (!(m_socket < 0)) {
        startReadingFromSocket();
    }
}

inline TCPConnection::TCPConnection(const std::string &address,
                             uint16_t port,
                             std::function<void(std::string &&, std::chrono::system_clock::time_point &&)> newDataDelegate,
                             std::function<void()> connectionLostDelegate) noexcept
    : m_newDataDelegate(std::move(newDataDelegate))
    , m_connectionLostDelegate(std::move(connectionLostDelegate)) {
    // Decompose given address string to check validity with numerical IPv4 address.
    std::string resolvedHostname{cluon::getIPv4FromHostname(address)};
    std::string tmp{resolvedHostname};
    std::replace(tmp.begin(), tmp.end(), '.', ' ');
    std::istringstream sstr{tmp};
    std::vector<int> addressTokens{std::istream_iterator<int>(sstr), std::istream_iterator<int>()};

    if ((!addressTokens.empty()) && (4 == addressTokens.size())
        && !(std::end(addressTokens) != std::find_if(addressTokens.begin(), addressTokens.end(), [](int a) { return (a < 0) || (a > 255); })) && (0 < port)) {
        // Check for valid IP address.
        struct sockaddr_in tmpSocketAddress {};
        const bool isValid = (0 < ::inet_pton(AF_INET, resolvedHostname.c_str(), &(tmpSocketAddress.sin_addr)));
        if (isValid) {
            std::memset(&m_address, 0, sizeof(m_address));
            m_address.sin_addr.s_addr = ::inet_addr(resolvedHostname.c_str());
            m_address.sin_family      = AF_INET;
            m_address.sin_port        = htons(port);
#ifdef WIN32
            // Load Winsock 2.2 DLL.
            WSADATA wsaData;
            if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
                std::cerr << "[cluon::TCPConnection] Error while calling WSAStartUp: " << WSAGetLastError() << std::endl;
            }
#endif

            m_socket = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

#ifdef WIN32
            if (m_socket < 0) {
                std::cerr << "[cluon::TCPConnection] Error while creating socket: " << WSAGetLastError() << std::endl;
                WSACleanup();
            }
#endif

            if (!(m_socket < 0)) {
                auto retVal = ::connect(m_socket, reinterpret_cast<struct sockaddr *>(&m_address), sizeof(m_address));
                if (0 > retVal) {
#ifdef WIN32 // LCOV_EXCL_LINE
                    auto errorCode = WSAGetLastError();
#else
                    auto errorCode = errno;                                          // LCOV_EXCL_LINE
#endif                                      // LCOV_EXCL_LINE
                    closeSocket(errorCode); // LCOV_EXCL_LINE
                } else {
                    startReadingFromSocket();
                }
            }
        }
    }
}

inline TCPConnection::~TCPConnection() noexcept {
    {
        m_readFromSocketThreadRunning.store(false);

        // Joining the thread could fail.
        try {
            if (m_readFromSocketThread.joinable()) {
                m_readFromSocketThread.join();
            }
        } catch (...) {} // LCOV_EXCL_LINE
    }

    m_pipeline.reset();

    closeSocket(0);
}

inline void TCPConnection::closeSocket(int errorCode) noexcept {
    if (0 != errorCode) {
        std::cerr << "[cluon::TCPConnection] Failed to perform socket operation: "; // LCOV_EXCL_LINE
#ifdef WIN32                                                                        // LCOV_EXCL_LINE
        std::cerr << errorCode << std::endl;
#else
        std::cerr << ::strerror(errorCode) << " (" << errorCode << ")" << std::endl; // LCOV_EXCL_LINE
#endif // LCOV_EXCL_LINE
    }

    if (!(m_socket < 0)) {
#ifdef WIN32
        ::shutdown(m_socket, SD_BOTH);
        ::closesocket(m_socket);
        if(m_cleanup){
            WSACleanup();
        }
#else
        ::shutdown(m_socket, SHUT_RDWR);                                             // Disallow further read/write operations.
        ::close(m_socket);
#endif
    }
    m_socket = -1;
}

inline void TCPConnection::startReadingFromSocket() noexcept {
    // Constructing a thread could fail.
    try {
        m_readFromSocketThread = std::thread(&TCPConnection::readFromSocket, this);

        // Let the operating system spawn the thread.
        using namespace std::literals::chrono_literals;
        do { std::this_thread::sleep_for(1ms); } while (!m_readFromSocketThreadRunning.load());
    } catch (...) {          // LCOV_EXCL_LINE
        closeSocket(ECHILD); // LCOV_EXCL_LINE
    }

    try {
        m_pipeline = std::make_shared<cluon::NotifyingPipeline<PipelineEntry>>(
            [this](PipelineEntry &&entry) { this->m_newDataDelegate(std::move(entry.m_data), std::move(entry.m_sampleTime)); });
        if (m_pipeline) {
            // Let the operating system spawn the thread.
            using namespace std::literals::chrono_literals; // NOLINT
            do { std::this_thread::sleep_for(1ms); } while (!m_pipeline->isRunning());
        }
    } catch (...) { closeSocket(ECHILD); } // LCOV_EXCL_LINE
}

inline void TCPConnection::setOnNewData(std::function<void(std::string &&, std::chrono::system_clock::time_point &&)> newDataDelegate) noexcept {
    std::lock_guard<std::mutex> lck(m_newDataDelegateMutex);
    m_newDataDelegate = newDataDelegate;
}

inline void TCPConnection::setOnConnectionLost(std::function<void()> connectionLostDelegate) noexcept {
    std::lock_guard<std::mutex> lck(m_connectionLostDelegateMutex);
    m_connectionLostDelegate = connectionLostDelegate;
}

inline bool TCPConnection::isRunning() const noexcept {
    return (m_readFromSocketThreadRunning.load() && !TerminateHandler::instance().isTerminated.load());
}

inline std::pair<ssize_t, int32_t> TCPConnection::send(std::string &&data) const noexcept {
    if (-1 == m_socket) {
        return {-1, EBADF};
    }

    if (data.empty()) {
        return {0, 0};
    }

    if (!m_readFromSocketThreadRunning.load()) {
        std::lock_guard<std::mutex> lck(m_connectionLostDelegateMutex); // LCOV_EXCL_LINE
        if (nullptr != m_connectionLostDelegate) {                      // LCOV_EXCL_LINE
            m_connectionLostDelegate();                                 // LCOV_EXCL_LINE
        }
        return {-1, ENOTCONN}; // LCOV_EXCL_LINE
    }

    constexpr uint16_t MAX_LENGTH{65535};
    if (MAX_LENGTH < data.size()) {
        return {-1, E2BIG};
    }

    std::lock_guard<std::mutex> lck(m_socketMutex);
    ssize_t bytesSent = ::send(m_socket, data.c_str(), data.length(), 0);
    return {bytesSent, (0 > bytesSent ? errno : 0)};
}

inline void TCPConnection::readFromSocket() noexcept {
    // Create buffer to store data from socket.
    constexpr uint16_t MAX_LENGTH{65535};
    std::array<char, MAX_LENGTH> buffer{};

    struct timeval timeout {};

    // Define file descriptor set to watch for read operations.
    fd_set setOfFiledescriptorsToReadFrom{};

    // Indicate to main thread that we are ready.
    m_readFromSocketThreadRunning.store(true);

    // This flag is used to not read data from the socket until this TCPConnection has a proper onNewDataHandler set.
    bool hasNewDataDelegate{false};

    while (m_readFromSocketThreadRunning.load()) {
        // Define timeout for select system call. The timeval struct must be
        // reinitialized for every select call as it might be modified containing
        // the actual time slept.
        timeout.tv_sec  = 0;
        timeout.tv_usec = 20 * 1000; // Check for new data with 50Hz.

        FD_ZERO(&setOfFiledescriptorsToReadFrom);
        FD_SET(m_socket, &setOfFiledescriptorsToReadFrom);
        ::select(m_socket + 1, &setOfFiledescriptorsToReadFrom, nullptr, nullptr, &timeout);

        // Only read data when the newDataDelegate is set.
        if (!hasNewDataDelegate) {
            std::lock_guard<std::mutex> lck(m_newDataDelegateMutex);
            hasNewDataDelegate = (nullptr != m_newDataDelegate);
        }
        if (FD_ISSET(m_socket, &setOfFiledescriptorsToReadFrom) && hasNewDataDelegate) {
            ssize_t bytesRead = ::recv(m_socket, buffer.data(), buffer.max_size(), 0);
            if (0 >= bytesRead) {
                // 0 == bytesRead: peer shut down the connection; 0 > bytesRead: other error.
                m_readFromSocketThreadRunning.store(false);

                {
                    std::lock_guard<std::mutex> lck(m_connectionLostDelegateMutex);
                    if (nullptr != m_connectionLostDelegate) {
                        m_connectionLostDelegate();
                    }
                }
                break;
            }

            {
                std::lock_guard<std::mutex> lck(m_newDataDelegateMutex);
                if ((0 < bytesRead) && (nullptr != m_newDataDelegate)) {
                    // SIOCGSTAMP is not available for a stream-based socket,
                    // thus, falling back to regular chrono timestamping.
                    std::chrono::system_clock::time_point timestamp = std::chrono::system_clock::now();
                    {
                        PipelineEntry pe;
                        pe.m_data       = std::string(buffer.data(), static_cast<size_t>(bytesRead));
                        pe.m_sampleTime = timestamp;

                        // Store entry in queue.
                        if (m_pipeline) {
                            m_pipeline->add(std::move(pe));
                        }
                    }

                    if (m_pipeline) {
                        m_pipeline->notifyAll();
                    }
                }
            }
        }
    }
}
} // namespace cluon
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//#include "cluon/TCPServer.hpp"
//#include "cluon/TerminateHandler.hpp"

// clang-format off
#ifdef WIN32
    #include <errno.h>
    #include <iostream>
#else
    #include <arpa/inet.h>
    #include <sys/ioctl.h>
    #include <sys/socket.h>
    #include <sys/types.h>
    #include <unistd.h>
#endif
// clang-format on

#include <cstring>
#include <array>
#include <iostream>
#include <memory>
#include <sstream>

namespace cluon {

inline TCPServer::TCPServer(uint16_t port, std::function<void(std::string &&from, std::shared_ptr<cluon::TCPConnection> connection)> newConnectionDelegate) noexcept
    : m_newConnectionDelegate(newConnectionDelegate) {
    if (0 < port) {
#ifdef WIN32
        // Load Winsock 2.2 DLL.
        WSADATA wsaData;
        if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
            std::cerr << "[cluon::TCPServer] Error while calling WSAStartUp: " << WSAGetLastError() << std::endl;
        }
#endif
        m_socket = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

#ifdef WIN32
        if (m_socket < 0) {
            std::cerr << "[cluon::TCPServer] Error while creating socket: " << WSAGetLastError() << std::endl;
            WSACleanup();
        }
#endif

        if (!(m_socket < 0)) {
            // Allow reusing of ports by multiple calls with same address/port.
            uint32_t YES = 1;
            // clang-format off
            auto retVal = ::setsockopt(m_socket, SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<char *>(&YES), sizeof(YES)); // NOLINT
            // clang-format on
            if (0 > retVal) {
#ifdef WIN32 // LCOV_EXCL_LINE
                auto errorCode = WSAGetLastError();
#else
                auto errorCode = errno;                                              // LCOV_EXCL_LINE
#endif                                  // LCOV_EXCL_LINE
                closeSocket(errorCode); // LCOV_EXCL_LINE
            }
        }

        if (!(m_socket < 0)) {
            // Setup address and port.
            struct sockaddr_in address;
            ::memset(&address, 0, sizeof(address));
            address.sin_family      = AF_INET;
            address.sin_addr.s_addr = htonl(INADDR_ANY);
            address.sin_port        = htons(port);

            auto retVal = ::bind(m_socket, reinterpret_cast<struct sockaddr *>(&address), sizeof(address));
            if (-1 != retVal) {
                constexpr int32_t MAX_PENDING_CONNECTIONS{100};
                retVal = ::listen(m_socket, MAX_PENDING_CONNECTIONS);
                if (-1 != retVal) {
                    // Constructing a thread could fail.
                    try {
                        m_readFromSocketThread = std::thread(&TCPServer::readFromSocket, this);

                        // Let the operating system spawn the thread.
                        using namespace std::literals::chrono_literals;
                        do { std::this_thread::sleep_for(1ms); } while (!m_readFromSocketThreadRunning.load());
                    } catch (...) {          // LCOV_EXCL_LINE
                        closeSocket(ECHILD); // LCOV_EXCL_LINE
                    }
                } else { // LCOV_EXCL_LINE
#ifdef WIN32             // LCOV_EXCL_LINE
                    auto errorCode = WSAGetLastError();
#else
                    auto errorCode = errno;                                          // LCOV_EXCL_LINE
#endif                                      // LCOV_EXCL_LINE
                    closeSocket(errorCode); // LCOV_EXCL_LINE
                }
            } else { // LCOV_EXCL_LINE
#ifdef WIN32         // LCOV_EXCL_LINE
                auto errorCode = WSAGetLastError();
#else
                auto errorCode = errno;                                              // LCOV_EXCL_LINE
#endif                                  // LCOV_EXCL_LINE
                closeSocket(errorCode); // LCOV_EXCL_LINE
            }
        }
    }
}

inline TCPServer::~TCPServer() noexcept {
    m_readFromSocketThreadRunning.store(false);

    // Joining the thread could fail.
    try {
        if (m_readFromSocketThread.joinable()) {
            m_readFromSocketThread.join();
        }
    } catch (...) { // LCOV_EXCL_LINE
    }

    closeSocket(0);
}

inline void TCPServer::closeSocket(int errorCode) noexcept {
    if (0 != errorCode) {
        std::cerr << "[cluon::TCPServer] Failed to perform socket operation: "; // LCOV_EXCL_LINE
#ifdef WIN32                                                                    // LCOV_EXCL_LINE
        std::cerr << errorCode << std::endl;
#else
        std::cerr << ::strerror(errorCode) << " (" << errorCode << ")" << std::endl; // LCOV_EXCL_LINE
#endif // LCOV_EXCL_LINE
    }

    if (!(m_socket < 0)) {
#ifdef WIN32
        ::shutdown(m_socket, SD_BOTH);
        ::closesocket(m_socket);
        WSACleanup();
#else
        ::shutdown(m_socket, SHUT_RDWR);                                             // Disallow further read/write operations.
        ::close(m_socket);
#endif
    }
    m_socket = -1;
}

inline bool TCPServer::isRunning() const noexcept {
    return (m_readFromSocketThreadRunning.load() && !TerminateHandler::instance().isTerminated.load());
}

inline void TCPServer::readFromSocket() noexcept {
    struct timeval timeout {};

    // Define file descriptor set to watch for read operations.
    fd_set setOfFiledescriptorsToReadFrom{};

    // Indicate to main thread that we are ready.
    m_readFromSocketThreadRunning.store(true);

    constexpr uint16_t MAX_ADDR_SIZE{1024};
    std::array<char, MAX_ADDR_SIZE> remoteAddress{};

    while (m_readFromSocketThreadRunning.load()) {
        // Define timeout for select system call. The timeval struct must be
        // reinitialized for every select call as it might be modified containing
        // the actual time slept.
        timeout.tv_sec  = 0;
        timeout.tv_usec = 20 * 1000; // Check for new data with 50Hz.

        FD_ZERO(&setOfFiledescriptorsToReadFrom);
        FD_SET(m_socket, &setOfFiledescriptorsToReadFrom);
        ::select(m_socket + 1, &setOfFiledescriptorsToReadFrom, nullptr, nullptr, &timeout);
        if (FD_ISSET(m_socket, &setOfFiledescriptorsToReadFrom)) {
            struct sockaddr_storage remote;
            socklen_t addrLength     = sizeof(remote);
            int32_t connectingClient = ::accept(m_socket, reinterpret_cast<struct sockaddr *>(&remote), &addrLength);
            if ((0 <= connectingClient) && (nullptr != m_newConnectionDelegate)) {
                ::inet_ntop(remote.ss_family,
                            &((reinterpret_cast<struct sockaddr_in *>(&remote))->sin_addr), // NOLINT
                            remoteAddress.data(),
                            remoteAddress.max_size());
                const uint16_t RECVFROM_PORT{ntohs(reinterpret_cast<struct sockaddr_in *>(&remote)->sin_port)}; // NOLINT
                m_newConnectionDelegate(std::string(remoteAddress.data()) + ':' + std::to_string(RECVFROM_PORT),
                                        std::shared_ptr<cluon::TCPConnection>(new cluon::TCPConnection(connectingClient)));
            }
        }
    }
}
} // namespace cluon
#endif
#ifdef HAVE_CLUON_MSC
/*
 * Boost Software License - Version 1.0
 *
 * Mustache v4.1
 * Copyright 2015-2020 Kevin Wojniak
 *
 * Permission is hereby granted, free of charge, to any person or organization
 * obtaining a copy of the software and accompanying documentation covered by
 * this license (the "Software") to use, reproduce, display, distribute,
 * execute, and transmit the Software, and to prepare derivative works of the
 * Software, and to permit third-parties to whom the Software is furnished to
 * do so, all subject to the following:
 *
 * The copyright notices in the Software and this entire statement, including
 * the above license grant, this restriction and the following disclaimer,
 * must be included in all copies of the Software, in whole or in part, and
 * all derivative works of the Software, unless such copies or derivative
 * works are solely in the form of machine-executable object code generated by
 * a source language processor.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO EVENT
 * SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE
 * FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#ifndef KAINJOW_MUSTACHE_HPP
#define KAINJOW_MUSTACHE_HPP

#include <cassert>
#include <cctype>
#include <functional>
#include <iostream>
#include <memory>
#include <sstream>
#include <unordered_map>
#include <vector>

namespace kainjow {
namespace mustache {

template <typename string_type>
string_type trim(const string_type& s) {
    auto it = s.begin();
    while (it != s.end() && std::isspace(*it)) {
        it++;
    }
    auto rit = s.rbegin();
    while (rit.base() != it && std::isspace(*rit)) {
        rit++;
    }
    return {it, rit.base()};
}

template <typename string_type>
string_type html_escape(const string_type& s) {
    string_type ret;
    ret.reserve(s.size()*2);
    for (const auto ch : s) {
        switch (ch) {
            case '&':
                ret.append({'&','a','m','p',';'});
                break;
            case '<':
                ret.append({'&','l','t',';'});
                break;
            case '>':
                ret.append({'&','g','t',';'});
                break;
            case '\"':
                ret.append({'&','q','u','o','t',';'});
                break;
            case '\'':
                ret.append({'&','a','p','o','s',';'});
                break;
            default:
                ret.append(1, ch);
                break;
        }
    }
    return ret;
}

template <typename string_type>
std::vector<string_type> split(const string_type& s, typename string_type::value_type delim) {
    std::vector<string_type> elems;
    std::basic_stringstream<typename string_type::value_type> ss(s);
    string_type item;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

template <typename string_type>
class basic_renderer {
public:
    using type1 = std::function<string_type(const string_type&)>;
    using type2 = std::function<string_type(const string_type&, bool escaped)>;

    string_type operator()(const string_type& text) const {
        return type1_(text);
    }

    string_type operator()(const string_type& text, bool escaped) const {
        return type2_(text, escaped);
    }

private:
    basic_renderer(const type1& t1, const type2& t2)
        : type1_(t1)
        , type2_(t2)
    {}

    const type1& type1_;
    const type2& type2_;

    template <typename StringType>
    friend class basic_mustache;
};

template <typename string_type>
class basic_lambda_t {
public:
    using type1 = std::function<string_type(const string_type&)>;
    using type2 = std::function<string_type(const string_type&, const basic_renderer<string_type>& render)>;

    basic_lambda_t(const type1& t) : type1_(new type1(t)) {}
    basic_lambda_t(const type2& t) : type2_(new type2(t)) {}

    bool is_type1() const { return static_cast<bool>(type1_); }
    bool is_type2() const { return static_cast<bool>(type2_); }

    const type1& type1_value() const { return *type1_; }
    const type2& type2_value() const { return *type2_; }

    // Copying
    basic_lambda_t(const basic_lambda_t& l) {
        if (l.type1_) {
            type1_.reset(new type1(*l.type1_));
        } else if (l.type2_) {
            type2_.reset(new type2(*l.type2_));
        }
    }

    string_type operator()(const string_type& text) const {
        return (*type1_)(text);
    }

    string_type operator()(const string_type& text, const basic_renderer<string_type>& render) const {
        return (*type2_)(text, render);
    }

private:
    std::unique_ptr<type1> type1_;
    std::unique_ptr<type2> type2_;
};

template <typename string_type>
class basic_data;
template <typename string_type>
using basic_object = std::unordered_map<string_type, basic_data<string_type>>;
template <typename string_type>
using basic_list = std::vector<basic_data<string_type>>;
template <typename string_type>
using basic_partial = std::function<string_type()>;
template <typename string_type>
using basic_lambda = typename basic_lambda_t<string_type>::type1;
template <typename string_type>
using basic_lambda2 = typename basic_lambda_t<string_type>::type2;

template <typename string_type>
class basic_data {
public:
    enum class type {
        object,
        string,
        list,
        bool_true,
        bool_false,
        partial,
        lambda,
        lambda2,
        invalid,
    };

    // Construction
    basic_data() : basic_data(type::object) {
    }
    basic_data(const string_type& string) : type_{type::string} {
        str_.reset(new string_type(string));
    }
    basic_data(const typename string_type::value_type* string) : type_{type::string} {
        str_.reset(new string_type(string));
    }
    basic_data(const basic_object<string_type>& obj) : type_{type::object} {
        obj_.reset(new basic_object<string_type>(obj));
    }
    basic_data(const basic_list<string_type>& l) : type_{type::list} {
        list_.reset(new basic_list<string_type>(l));
    }
    basic_data(type t) : type_{t} {
        switch (type_) {
            case type::object:
                obj_.reset(new basic_object<string_type>);
                break;
            case type::string:
                str_.reset(new string_type);
                break;
            case type::list:
                list_.reset(new basic_list<string_type>);
                break;
            default:
                break;
        }
    }
    basic_data(const string_type& name, const basic_data& var) : basic_data{} {
        set(name, var);
    }
    basic_data(const basic_partial<string_type>& p) : type_{type::partial} {
        partial_.reset(new basic_partial<string_type>(p));
    }
    basic_data(const basic_lambda<string_type>& l) : type_{type::lambda} {
        lambda_.reset(new basic_lambda_t<string_type>(l));
    }
    basic_data(const basic_lambda2<string_type>& l) : type_{type::lambda2} {
        lambda_.reset(new basic_lambda_t<string_type>(l));
    }
    basic_data(const basic_lambda_t<string_type>& l) {
        if (l.is_type1()) {
            type_ = type::lambda;
        } else if (l.is_type2()) {
            type_ = type::lambda2;
        }
        lambda_.reset(new basic_lambda_t<string_type>(l));
    }
    basic_data(bool b) : type_{b ? type::bool_true : type::bool_false} {
    }

    // Copying
    basic_data(const basic_data& dat) : type_(dat.type_) {
        if (dat.obj_) {
            obj_.reset(new basic_object<string_type>(*dat.obj_));
        } else if (dat.str_) {
            str_.reset(new string_type(*dat.str_));
        } else if (dat.list_) {
            list_.reset(new basic_list<string_type>(*dat.list_));
        } else if (dat.partial_) {
            partial_.reset(new basic_partial<string_type>(*dat.partial_));
        } else if (dat.lambda_) {
            lambda_.reset(new basic_lambda_t<string_type>(*dat.lambda_));
        }
    }

    // Move
    basic_data(basic_data&& dat) : type_{dat.type_} {
        if (dat.obj_) {
            obj_ = std::move(dat.obj_);
        } else if (dat.str_) {
            str_ = std::move(dat.str_);
        } else if (dat.list_) {
            list_ = std::move(dat.list_);
        } else if (dat.partial_) {
            partial_ = std::move(dat.partial_);
        } else if (dat.lambda_) {
            lambda_ = std::move(dat.lambda_);
        }
        dat.type_ = type::invalid;
    }
    basic_data& operator= (basic_data&& dat) {
        if (this != &dat) {
            obj_.reset();
            str_.reset();
            list_.reset();
            partial_.reset();
            lambda_.reset();
            if (dat.obj_) {
                obj_ = std::move(dat.obj_);
            } else if (dat.str_) {
                str_ = std::move(dat.str_);
            } else if (dat.list_) {
                list_ = std::move(dat.list_);
            } else if (dat.partial_) {
                partial_ = std::move(dat.partial_);
            } else if (dat.lambda_) {
                lambda_ = std::move(dat.lambda_);
            }
            type_ = dat.type_;
            dat.type_ = type::invalid;
        }
        return *this;
    }

    // Type info
    bool is_object() const {
        return type_ == type::object;
    }
    bool is_string() const {
        return type_ == type::string;
    }
    bool is_list() const {
        return type_ == type::list;
    }
    bool is_bool() const {
        return is_true() || is_false();
    }
    bool is_true() const {
        return type_ == type::bool_true;
    }
    bool is_false() const {
        return type_ == type::bool_false;
    }
    bool is_partial() const {
        return type_ == type::partial;
    }
    bool is_lambda() const {
        return type_ == type::lambda;
    }
    bool is_lambda2() const {
        return type_ == type::lambda2;
    }
    bool is_invalid() const {
        return type_ == type::invalid;
    }

    // Object data
    bool is_empty_object() const {
        return is_object() && obj_->empty();
    }
    bool is_non_empty_object() const {
        return is_object() && !obj_->empty();
    }
    void set(const string_type& name, const basic_data& var) {
        if (is_object()) {
            auto it = obj_->find(name);
            if (it != obj_->end()) {
                obj_->erase(it);
            }
            obj_->insert(std::pair<string_type,basic_data>{name, var});
        }
    }
    const basic_data* get(const string_type& name) const {
        if (!is_object()) {
            return nullptr;
        }
        const auto& it = obj_->find(name);
        if (it == obj_->end()) {
            return nullptr;
        }
        return &it->second;
    }

    // List data
    void push_back(const basic_data& var) {
        if (is_list()) {
            list_->push_back(var);
        }
    }
    const basic_list<string_type>& list_value() const {
        return *list_;
    }
    bool is_empty_list() const {
        return is_list() && list_->empty();
    }
    bool is_non_empty_list() const {
        return is_list() && !list_->empty();
    }
    basic_data& operator<< (const basic_data& data) {
        push_back(data);
        return *this;
    }

    // String data
    const string_type& string_value() const {
        return *str_;
    }

    basic_data& operator[] (const string_type& key) {
        return (*obj_)[key];
    }

    const basic_partial<string_type>& partial_value() const {
        return (*partial_);
    }

    const basic_lambda<string_type>& lambda_value() const {
        return lambda_->type1_value();
    }

    const basic_lambda2<string_type>& lambda2_value() const {
        return lambda_->type2_value();
    }

private:
    type type_;
    std::unique_ptr<basic_object<string_type>> obj_;
    std::unique_ptr<string_type> str_;
    std::unique_ptr<basic_list<string_type>> list_;
    std::unique_ptr<basic_partial<string_type>> partial_;
    std::unique_ptr<basic_lambda_t<string_type>> lambda_;
};

template <typename string_type>
class delimiter_set {
public:
    string_type begin;
    string_type end;
    delimiter_set()
        : begin(default_begin)
        , end(default_end)
    {}
    bool is_default() const { return begin == default_begin && end == default_end; }
    static const string_type default_begin;
    static const string_type default_end;
};

template <typename string_type>
const string_type delimiter_set<string_type>::default_begin(2, '{');
template <typename string_type>
const string_type delimiter_set<string_type>::default_end(2, '}');

template <typename string_type>
class basic_context {
public:
    virtual ~basic_context() = default;
    virtual void push(const basic_data<string_type>* data) = 0;
    virtual void pop() = 0;

    virtual const basic_data<string_type>* get(const string_type& name) const = 0;
    virtual const basic_data<string_type>* get_partial(const string_type& name) const = 0;
};

template <typename string_type>
class context : public basic_context<string_type> {
public:
    context(const basic_data<string_type>* data) {
        push(data);
    }

    context() {
    }

    virtual void push(const basic_data<string_type>* data) override {
        items_.insert(items_.begin(), data);
    }

    virtual void pop() override {
        items_.erase(items_.begin());
    }

    virtual const basic_data<string_type>* get(const string_type& name) const override {
        // process {{.}} name
        if (name.size() == 1 && name.at(0) == '.') {
            return items_.front();
        }
        if (name.find('.') == string_type::npos) {
            // process normal name without having to split which is slower
            for (const auto& item : items_) {
                const auto var = item->get(name);
                if (var) {
                    return var;
                }
            }
            return nullptr;
        }
        // process x.y-like name
        const auto names = split(name, '.');
        for (const auto& item : items_) {
            auto var = item;
            for (const auto& n : names) {
                var = var->get(n);
                if (!var) {
                    break;
                }
            }
            if (var) {
                return var;
            }
        }
        return nullptr;
    }

    virtual const basic_data<string_type>* get_partial(const string_type& name) const override {
        for (const auto& item : items_) {
            const auto var = item->get(name);
            if (var) {
                return var;
            }
        }
        return nullptr;
    }

    context(const context&) = delete;
    context& operator= (const context&) = delete;

private:
    std::vector<const basic_data<string_type>*> items_;
};

template <typename string_type>
class line_buffer_state {
public:
    string_type data;
    bool contained_section_tag = false;

    bool is_empty_or_contains_only_whitespace() const {
        for (const auto ch : data) {
            // don't look at newlines
            if (ch != ' ' && ch != '\t') {
                return false;
            }
        }
        return true;
    }

    void clear() {
        data.clear();
        contained_section_tag = false;
    }
};

template <typename string_type>
class context_internal {
public:
    basic_context<string_type>& ctx;
    delimiter_set<string_type> delim_set;
    line_buffer_state<string_type> line_buffer;

    context_internal(basic_context<string_type>& a_ctx)
        : ctx(a_ctx)
    {
    }
};

enum class tag_type {
    text,
    variable,
    unescaped_variable,
    section_begin,
    section_end,
    section_begin_inverted,
    comment,
    partial,
    set_delimiter,
};

template <typename string_type>
class mstch_tag /* gcc doesn't allow "tag tag;" so rename the class :( */ {
public:
    string_type name;
    tag_type type = tag_type::text;
    std::shared_ptr<string_type> section_text;
    std::shared_ptr<delimiter_set<string_type>> delim_set;
    bool is_section_begin() const {
        return type == tag_type::section_begin || type == tag_type::section_begin_inverted;
    }
    bool is_section_end() const {
        return type == tag_type::section_end;
    }
};

template <typename string_type>
class context_pusher {
public:
    context_pusher(context_internal<string_type>& ctx, const basic_data<string_type>* data)
        : ctx_(ctx)
    {
        ctx.ctx.push(data);
    }
    ~context_pusher() {
        ctx_.ctx.pop();
    }
    context_pusher(const context_pusher&) = delete;
    context_pusher& operator= (const context_pusher&) = delete;
private:
    context_internal<string_type>& ctx_;
};

template <typename string_type>
class component {
private:
    using string_size_type = typename string_type::size_type;

public:
    string_type text;
    mstch_tag<string_type> tag;
    std::vector<component> children;
    string_size_type position = string_type::npos;

    enum class walk_control {
        walk, // "continue" is reserved :/
        stop,
        skip,
    };
    using walk_callback = std::function<walk_control(component&)>;

    component() {}
    component(const string_type& t, string_size_type p) : text(t), position(p) {}

    bool is_text() const {
        return tag.type == tag_type::text;
    }

    bool is_newline() const {
        return is_text() && ((text.size() == 2 && text[0] == '\r' && text[1] == '\n') ||
        (text.size() == 1 && (text[0] == '\n' || text[0] == '\r')));
    }

    bool is_non_newline_whitespace() const {
        return is_text() && !is_newline() && text.size() == 1 && (text[0] == ' ' || text[0] == '\t');
    }

    void walk_children(const walk_callback& callback) {
        for (auto& child : children) {
            if (child.walk(callback) != walk_control::walk) {
                break;
            }
        }
    }

private:
    walk_control walk(const walk_callback& callback) {
        walk_control control{callback(*this)};
        if (control == walk_control::stop) {
            return control;
        } else if (control == walk_control::skip) {
            return walk_control::walk;
        }
        for (auto& child : children) {
            control = child.walk(callback);
            if (control == walk_control::stop) {
                return control;
            }
        }
        return control;
    }
};

template <typename string_type>
class parser {
public:
    parser(const string_type& input, context_internal<string_type>& ctx, component<string_type>& root_component, string_type& error_message)
    {
        parse(input, ctx, root_component, error_message);
    }

private:
    void parse(const string_type& input, context_internal<string_type>& ctx, component<string_type>& root_component, string_type& error_message) const {
        using string_size_type = typename string_type::size_type;
        using streamstring = std::basic_ostringstream<typename string_type::value_type>;

        const string_type brace_delimiter_end_unescaped(3, '}');
        const string_size_type input_size{input.size()};

        bool current_delimiter_is_brace{ctx.delim_set.is_default()};

        std::vector<component<string_type>*> sections{&root_component};
        std::vector<string_size_type> section_starts;
        string_type current_text;
        string_size_type current_text_position = -1;

        current_text.reserve(input_size);

        const auto process_current_text = [&current_text, &current_text_position, &sections]() {
            if (!current_text.empty()) {
                const component<string_type> comp{current_text, current_text_position};
                sections.back()->children.push_back(comp);
                current_text.clear();
                current_text_position = -1;
            }
        };

        const std::vector<string_type> whitespace{
            string_type(1, '\r') + string_type(1, '\n'),
            string_type(1, '\n'),
            string_type(1, '\r'),
            string_type(1, ' '),
            string_type(1, '\t'),
        };

        for (string_size_type input_position = 0; input_position != input_size;) {
            bool parse_tag = false;

            if (input.compare(input_position, ctx.delim_set.begin.size(), ctx.delim_set.begin) == 0) {
                process_current_text();

                // Tag start delimiter
                parse_tag = true;
            } else {
                bool parsed_whitespace = false;
                for (const auto& whitespace_text : whitespace) {
                    if (input.compare(input_position, whitespace_text.size(), whitespace_text) == 0) {
                        process_current_text();

                        const component<string_type> comp{whitespace_text, input_position};
                        sections.back()->children.push_back(comp);
                        input_position += whitespace_text.size();

                        parsed_whitespace = true;
                        break;
                    }
                }

                if (!parsed_whitespace) {
                    if (current_text.empty()) {
                        current_text_position = input_position;
                    }
                    current_text.append(1, input[input_position]);
                    input_position++;
                }
            }

            if (!parse_tag) {
                continue;
            }

            // Find the next tag start delimiter
            const string_size_type tag_location_start = input_position;

            // Find the next tag end delimiter
            string_size_type tag_contents_location{tag_location_start + ctx.delim_set.begin.size()};
            const bool tag_is_unescaped_var{current_delimiter_is_brace && tag_location_start != (input_size - 2) && input.at(tag_contents_location) == ctx.delim_set.begin.at(0)};
            const string_type& current_tag_delimiter_end{tag_is_unescaped_var ? brace_delimiter_end_unescaped : ctx.delim_set.end};
            const auto current_tag_delimiter_end_size = current_tag_delimiter_end.size();
            if (tag_is_unescaped_var) {
                ++tag_contents_location;
            }
            const string_size_type tag_location_end{input.find(current_tag_delimiter_end, tag_contents_location)};
            if (tag_location_end == string_type::npos) {
                streamstring ss;
                ss << "Unclosed tag at " << tag_location_start;
                error_message.assign(ss.str());
                return;
            }

            // Parse tag
            const string_type tag_contents{trim(string_type{input, tag_contents_location, tag_location_end - tag_contents_location})};
            component<string_type> comp;
            if (!tag_contents.empty() && tag_contents[0] == '=') {
                if (!parse_set_delimiter_tag(tag_contents, ctx.delim_set)) {
                    streamstring ss;
                    ss << "Invalid set delimiter tag at " << tag_location_start;
                    error_message.assign(ss.str());
                    return;
                }
                current_delimiter_is_brace = ctx.delim_set.is_default();
                comp.tag.type = tag_type::set_delimiter;
                comp.tag.delim_set.reset(new delimiter_set<string_type>(ctx.delim_set));
            }
            if (comp.tag.type != tag_type::set_delimiter) {
                parse_tag_contents(tag_is_unescaped_var, tag_contents, comp.tag);
            }
            comp.position = tag_location_start;
            sections.back()->children.push_back(comp);

            // Start next search after this tag
            input_position = tag_location_end + current_tag_delimiter_end_size;

            // Push or pop sections
            if (comp.tag.is_section_begin()) {
                sections.push_back(&sections.back()->children.back());
                section_starts.push_back(input_position);
            } else if (comp.tag.is_section_end()) {
                if (sections.size() == 1) {
                    streamstring ss;
                    ss << "Unopened section \"" << comp.tag.name << "\" at " << comp.position;
                    error_message.assign(ss.str());
                    return;
                }
                sections.back()->tag.section_text.reset(new string_type(input.substr(section_starts.back(), tag_location_start - section_starts.back())));
                sections.pop_back();
                section_starts.pop_back();
            }
        }

        process_current_text();

        // Check for sections without an ending tag
        root_component.walk_children([&error_message](component<string_type>& comp) -> typename component<string_type>::walk_control {
            if (!comp.tag.is_section_begin()) {
                return component<string_type>::walk_control::walk;
            }
            if (comp.children.empty() || !comp.children.back().tag.is_section_end() || comp.children.back().tag.name != comp.tag.name) {
                streamstring ss;
                ss << "Unclosed section \"" << comp.tag.name << "\" at " << comp.position;
                error_message.assign(ss.str());
                return component<string_type>::walk_control::stop;
            }
            comp.children.pop_back(); // remove now useless end section component
            return component<string_type>::walk_control::walk;
        });
        if (!error_message.empty()) {
            return;
        }
    }

    bool is_set_delimiter_valid(const string_type& delimiter) const {
        // "Custom delimiters may not contain whitespace or the equals sign."
        for (const auto ch : delimiter) {
            if (ch == '=' || std::isspace(ch)) {
                return false;
            }
        }
        return true;
    }

    bool parse_set_delimiter_tag(const string_type& contents, delimiter_set<string_type>& delimiter_set) const {
        // Smallest legal tag is "=X X="
        if (contents.size() < 5) {
            return false;
        }
        if (contents.back() != '=') {
            return false;
        }
        const auto contents_substr = trim(contents.substr(1, contents.size() - 2));
        const auto spacepos = contents_substr.find(' ');
        if (spacepos == string_type::npos) {
            return false;
        }
        const auto nonspace = contents_substr.find_first_not_of(' ', spacepos + 1);
        assert(nonspace != string_type::npos);
        const string_type begin = contents_substr.substr(0, spacepos);
        const string_type end = contents_substr.substr(nonspace, contents_substr.size() - nonspace);
        if (!is_set_delimiter_valid(begin) || !is_set_delimiter_valid(end)) {
            return false;
        }
        delimiter_set.begin = begin;
        delimiter_set.end = end;
        return true;
    }

    void parse_tag_contents(bool is_unescaped_var, const string_type& contents, mstch_tag<string_type>& tag) const {
        if (is_unescaped_var) {
            tag.type = tag_type::unescaped_variable;
            tag.name = contents;
        } else if (contents.empty()) {
            tag.type = tag_type::variable;
            tag.name.clear();
        } else {
            switch (contents.at(0)) {
                case '#':
                    tag.type = tag_type::section_begin;
                    break;
                case '^':
                    tag.type = tag_type::section_begin_inverted;
                    break;
                case '/':
                    tag.type = tag_type::section_end;
                    break;
                case '>':
                    tag.type = tag_type::partial;
                    break;
                case '&':
                    tag.type = tag_type::unescaped_variable;
                    break;
                case '!':
                    tag.type = tag_type::comment;
                    break;
                default:
                    tag.type = tag_type::variable;
                    break;
            }
            if (tag.type == tag_type::variable) {
                tag.name = contents;
            } else {
                string_type name{contents};
                name.erase(name.begin());
                tag.name = trim(name);
            }
        }
    }
};

template <typename StringType>
class basic_mustache {
public:
    using string_type = StringType;

    basic_mustache(const string_type& input)
        : basic_mustache() {
        context<string_type> ctx;
        context_internal<string_type> context{ctx};
        parser<string_type> parser{input, context, root_component_, error_message_};
    }

    bool is_valid() const {
        return error_message_.empty();
    }

    const string_type& error_message() const {
        return error_message_;
    }

    using escape_handler = std::function<string_type(const string_type&)>;
    void set_custom_escape(const escape_handler& escape_fn) {
        escape_ = escape_fn;
    }

    template <typename stream_type>
    stream_type& render(const basic_data<string_type>& data, stream_type& stream) {
        render(data, [&stream](const string_type& str) {
            stream << str;
        });
        return stream;
    }

    string_type render(const basic_data<string_type>& data) {
        std::basic_ostringstream<typename string_type::value_type> ss;
        return render(data, ss).str();
    }

    template <typename stream_type>
    stream_type& render(basic_context<string_type>& ctx, stream_type& stream) {
        context_internal<string_type> context{ctx};
        render([&stream](const string_type& str) {
            stream << str;
        }, context);
        return stream;
    }

    string_type render(basic_context<string_type>& ctx) {
        std::basic_ostringstream<typename string_type::value_type> ss;
        return render(ctx, ss).str();
    }

    using render_handler = std::function<void(const string_type&)>;
    void render(const basic_data<string_type>& data, const render_handler& handler) {
        if (!is_valid()) {
            return;
        }
        context<string_type> ctx{&data};
        context_internal<string_type> context{ctx};
        render(handler, context);
    }

private:
    using string_size_type = typename string_type::size_type;

    basic_mustache()
        : escape_(html_escape<string_type>)
    {
    }

    basic_mustache(const string_type& input, context_internal<string_type>& ctx)
        : basic_mustache() {
        parser<string_type> parser{input, ctx, root_component_, error_message_};
    }

    string_type render(context_internal<string_type>& ctx) {
        std::basic_ostringstream<typename string_type::value_type> ss;
        render([&ss](const string_type& str) {
            ss << str;
        }, ctx);
        return ss.str();
    }

    void render(const render_handler& handler, context_internal<string_type>& ctx, bool root_renderer = true) {
        root_component_.walk_children([&handler, &ctx, this](component<string_type>& comp) -> typename component<string_type>::walk_control {
            return render_component(handler, ctx, comp);
        });
        // process the last line, but only for the top-level renderer
        if (root_renderer) {
            render_current_line(handler, ctx, nullptr);
        }
    }

    void render_current_line(const render_handler& handler, context_internal<string_type>& ctx, const component<string_type>* comp) const {
        // We're at the end of a line, so check the line buffer state to see
        // if the line had tags in it, and also if the line is now empty or
        // contains whitespace only. if this situation is true, skip the line.
        bool output = true;
        if (ctx.line_buffer.contained_section_tag && ctx.line_buffer.is_empty_or_contains_only_whitespace()) {
            output = false;
        }
        if (output) {
            handler(ctx.line_buffer.data);
            if (comp) {
                handler(comp->text);
            }
        }
        ctx.line_buffer.clear();
    }

    void render_result(context_internal<string_type>& ctx, const string_type& text) const {
        ctx.line_buffer.data.append(text);
    }

    typename component<string_type>::walk_control render_component(const render_handler& handler, context_internal<string_type>& ctx, component<string_type>& comp) {
        if (comp.is_text()) {
            if (comp.is_newline()) {
                render_current_line(handler, ctx, &comp);
            } else {
                render_result(ctx, comp.text);
            }
            return component<string_type>::walk_control::walk;
        }

        const mstch_tag<string_type>& tag{comp.tag};
        const basic_data<string_type>* var = nullptr;
        switch (tag.type) {
            case tag_type::variable:
            case tag_type::unescaped_variable:
                if ((var = ctx.ctx.get(tag.name)) != nullptr) {
                    if (!render_variable(handler, var, ctx, tag.type == tag_type::variable)) {
                        return component<string_type>::walk_control::stop;
                    }
                }
                break;
            case tag_type::section_begin:
                if ((var = ctx.ctx.get(tag.name)) != nullptr) {
                    if (var->is_lambda() || var->is_lambda2()) {
                        if (!render_lambda(handler, var, ctx, render_lambda_escape::optional, *comp.tag.section_text, true)) {
                            return component<string_type>::walk_control::stop;
                        }
                    } else if (!var->is_false() && !var->is_empty_list()) {
                        render_section(handler, ctx, comp, var);
                    }
                }
                return component<string_type>::walk_control::skip;
            case tag_type::section_begin_inverted:
                if ((var = ctx.ctx.get(tag.name)) == nullptr || var->is_false() || var->is_empty_list()) {
                    render_section(handler, ctx, comp, var);
                }
                return component<string_type>::walk_control::skip;
            case tag_type::partial:
                if ((var = ctx.ctx.get_partial(tag.name)) != nullptr && (var->is_partial() || var->is_string())) {
                    const auto& partial_result = var->is_partial() ? var->partial_value()() : var->string_value();
                    basic_mustache tmpl{partial_result};
                    tmpl.set_custom_escape(escape_);
                    if (!tmpl.is_valid()) {
                        error_message_ = tmpl.error_message();
                    } else {
                        tmpl.render(handler, ctx, false);
                        if (!tmpl.is_valid()) {
                            error_message_ = tmpl.error_message();
                        }
                    }
                    if (!tmpl.is_valid()) {
                        return component<string_type>::walk_control::stop;
                    }
                }
                break;
            case tag_type::set_delimiter:
                ctx.delim_set = *comp.tag.delim_set;
                break;
            default:
                break;
        }

        return component<string_type>::walk_control::walk;
    }

    enum class render_lambda_escape {
        escape,
        unescape,
        optional,
    };

    bool render_lambda(const render_handler& handler, const basic_data<string_type>* var, context_internal<string_type>& ctx, render_lambda_escape escape, const string_type& text, bool parse_with_same_context) {
        const typename basic_renderer<string_type>::type2 render2 = [this, &ctx, parse_with_same_context, escape](const string_type& text, bool escaped) {
            const auto process_template = [this, &ctx, escape, escaped](basic_mustache& tmpl) -> string_type {
                if (!tmpl.is_valid()) {
                    error_message_ = tmpl.error_message();
                    return {};
                }
                context_internal<string_type> render_ctx{ctx.ctx}; // start a new line_buffer
                const auto str = tmpl.render(render_ctx);
                if (!tmpl.is_valid()) {
                    error_message_ = tmpl.error_message();
                    return {};
                }
                bool do_escape = false;
                switch (escape) {
                    case render_lambda_escape::escape:
                        do_escape = true;
                        break;
                    case render_lambda_escape::unescape:
                        do_escape = false;
                        break;
                    case render_lambda_escape::optional:
                        do_escape = escaped;
                        break;
                }
                return do_escape ? escape_(str) : str;
            };
            if (parse_with_same_context) {
                basic_mustache tmpl{text, ctx};
                tmpl.set_custom_escape(escape_);
                return process_template(tmpl);
            }
            basic_mustache tmpl{text};
            tmpl.set_custom_escape(escape_);
            return process_template(tmpl);
        };
        const typename basic_renderer<string_type>::type1 render = [&render2](const string_type& text) {
            return render2(text, false);
        };
        if (var->is_lambda2()) {
            const basic_renderer<string_type> renderer{render, render2};
            render_result(ctx, var->lambda2_value()(text, renderer));
        } else {
            render_current_line(handler, ctx, nullptr);
            render_result(ctx, render(var->lambda_value()(text)));
        }
        return error_message_.empty();
    }

    bool render_variable(const render_handler& handler, const basic_data<string_type>* var, context_internal<string_type>& ctx, bool escaped) {
        if (var->is_string()) {
            const auto& varstr = var->string_value();
            render_result(ctx, escaped ? escape_(varstr) : varstr);
        } else if (var->is_lambda()) {
            const render_lambda_escape escape_opt = escaped ? render_lambda_escape::escape : render_lambda_escape::unescape;
            return render_lambda(handler, var, ctx, escape_opt, {}, false);
        } else if (var->is_lambda2()) {
            using streamstring = std::basic_ostringstream<typename string_type::value_type>;
            streamstring ss;
            ss << "Lambda with render argument is not allowed for regular variables";
            error_message_ = ss.str();
            return false;
        }
        return true;
    }

    void render_section(const render_handler& handler, context_internal<string_type>& ctx, component<string_type>& incomp, const basic_data<string_type>* var) {
        const auto callback = [&handler, &ctx, this](component<string_type>& comp) -> typename component<string_type>::walk_control {
            return render_component(handler, ctx, comp);
        };
        if (var && var->is_non_empty_list()) {
            for (const auto& item : var->list_value()) {
                // account for the section begin tag
                ctx.line_buffer.contained_section_tag = true;

                const context_pusher<string_type> ctxpusher{ctx, &item};
                incomp.walk_children(callback);

                // ctx may have been cleared. account for the section end tag
                ctx.line_buffer.contained_section_tag = true;
            }
        } else if (var) {
            // account for the section begin tag
            ctx.line_buffer.contained_section_tag = true;

            const context_pusher<string_type> ctxpusher{ctx, var};
            incomp.walk_children(callback);

            // ctx may have been cleared. account for the section end tag
            ctx.line_buffer.contained_section_tag = true;
        } else {
            // account for the section begin tag
            ctx.line_buffer.contained_section_tag = true;

            incomp.walk_children(callback);

            // ctx may have been cleared. account for the section end tag
            ctx.line_buffer.contained_section_tag = true;
        }
    }

private:
    string_type error_message_;
    component<string_type> root_component_;
    escape_handler escape_;
};

using mustache = basic_mustache<std::string>;
using data = basic_data<mustache::string_type>;
using object = basic_object<mustache::string_type>;
using list = basic_list<mustache::string_type>;
using partial = basic_partial<mustache::string_type>;
using renderer = basic_renderer<mustache::string_type>;
using lambda = basic_lambda<mustache::string_type>;
using lambda2 = basic_lambda2<mustache::string_type>;
using lambda_t = basic_lambda_t<mustache::string_type>;

using mustachew = basic_mustache<std::wstring>;
using dataw = basic_data<mustachew::string_type>;

} // namespace mustache
} // namespace kainjow

#endif // KAINJOW_MUSTACHE_HPP
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#ifndef CLUON_MSC_HPP
#define CLUON_MSC_HPP

//#include "cluon/MessageParser.hpp"
//#include "cluon/MetaMessage.hpp"
//#include "cluon/MetaMessageToCPPTransformator.hpp"
//#include "cluon/MetaMessageToProtoTransformator.hpp"

//#include "argh/argh.h"

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

inline int32_t cluon_msc(int32_t argc, char **argv) {
    const std::string PROGRAM{argv[0]}; // NOLINT
    argh::parser commandline(argc, argv);

    std::string inputFilename = commandline.pos_args().back();
    if (std::string::npos != inputFilename.find(PROGRAM)) {
        std::cerr << PROGRAM
                  << " transforms a given message specification file in .odvd format into C++." << std::endl;
        std::cerr << "Usage:   " << PROGRAM << " [--cpp] [--proto] [--out=<file>] <odvd file>" << std::endl;
        std::cerr << "         " << PROGRAM << " --cpp:   Generate C++14-compliant, self-contained header file." << std::endl;
        std::cerr << "         " << PROGRAM << " --proto: Generate Proto version2-compliant file." << std::endl;
        std::cerr << std::endl;
        std::cerr << "Example: " << PROGRAM << " --cpp --out=/tmp/myOutput.hpp myFile.odvd" << std::endl;
        return 1;
    }

    std::string outputFilename;
    commandline({"--out"}) >> outputFilename;

    const bool generateCPP = commandline[{"--cpp"}];
    const bool generateProto = commandline[{"--proto"}];

    int retVal = 1;
    std::ifstream inputFile(inputFilename, std::ios::in);
    if (inputFile.good()) {
        bool addHeaderForFirstProtoFile = true;
        std::string input(static_cast<std::stringstream const&>(std::stringstream() << inputFile.rdbuf()).str()); // NOLINT

        cluon::MessageParser mp;
        auto result = mp.parse(input);
        retVal = result.second;

        // Delete the content of a potentially existing file.
        if (!outputFilename.empty()) {
            std::ofstream outputFile(outputFilename, std::ios::out | std::ios::trunc);
            outputFile.close();
        }
        for (auto e : result.first) {
            std::string content;
            if (generateCPP) {
                cluon::MetaMessageToCPPTransformator transformation;
                e.accept([&trans = transformation](const cluon::MetaMessage &_mm){ trans.visit(_mm); });
                content = transformation.content();
            }
            if (generateProto) {
                cluon::MetaMessageToProtoTransformator transformation;
                e.accept([&trans = transformation](const cluon::MetaMessage &_mm){ trans.visit(_mm); });
                content = transformation.content(addHeaderForFirstProtoFile);
                addHeaderForFirstProtoFile = false;
            }

            if (!outputFilename.empty()) {
                std::ofstream outputFile(outputFilename, std::ios::out | std::ios::app);
                outputFile << content << std::endl;
                outputFile.close();
            }
            else { // LCOV_EXCL_LINE
                std::cout << content << std::endl; // LCOV_EXCL_LINE
            }
        }
    }
    else {
        std::cerr << "[" << PROGRAM << "] Could not find '" << inputFilename << "'." << std::endl;
    }

    return retVal;
}

#endif
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// This test for a compiler definition is necessary to preserve single-file, header-only compability.
#ifndef HAVE_CLUON_MSC
#include "cluon-msc.hpp"
#endif

#include <cstdint>

int32_t main(int32_t argc, char **argv) {
    return cluon_msc(argc, argv);
}
#endif
