// This is an auto-generated header-only single-file distribution of libcluon.
// Date: Sun, 08 Oct 2023 12:58:13 +0800
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
//
//  peglib.h
//
//  Copyright (c) 2020 Yuji Hirose. All rights reserved.
//  MIT License
//

#ifndef CPPPEGLIB_PEGLIB_H
#define CPPPEGLIB_PEGLIB_H

#ifndef PEGLIB_USE_STD_ANY
#ifdef _MSVC_LANG
#define PEGLIB_USE_STD_ANY _MSVC_LANG >= 201703L
#elif defined(__cplusplus)
#define PEGLIB_USE_STD_ANY __cplusplus >= 201703L
#endif
#endif // PEGLIB_USE_STD_ANY

#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstring>
#include <functional>
#include <initializer_list>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#if PEGLIB_USE_STD_ANY
#include <any>
#endif

// guard for older versions of VC++
#ifdef _MSC_VER
#if defined(_MSC_VER) && _MSC_VER < 1900 // Less than Visual Studio 2015
#error "Requires complete C+11 support"
#endif
#endif

namespace peg {

/*-----------------------------------------------------------------------------
 *  any
 *---------------------------------------------------------------------------*/

#if PEGLIB_USE_STD_ANY
using any = std::any;

// Define a function alias to std::any_cast using perfect forwarding
template <typename T, typename... Args>
auto any_cast(Args &&... args)
    -> decltype(std::any_cast<T>(std::forward<Args>(args)...)) {
  return std::any_cast<T>(std::forward<Args>(args)...);
}
#else
class any {
public:
  any() = default;

  any(const any &rhs) : content_(rhs.clone()) {}

  any(any &&rhs) : content_(rhs.content_) { rhs.content_ = nullptr; }

  template <typename T> any(const T &value) : content_(new holder<T>(value)) {}

  any &operator=(const any &rhs) {
    if (this != &rhs) {
      if (content_) { delete content_; }
      content_ = rhs.clone();
    }
    return *this;
  }

  any &operator=(any &&rhs) {
    if (this != &rhs) {
      if (content_) { delete content_; }
      content_ = rhs.content_;
      rhs.content_ = nullptr;
    }
    return *this;
  }

  ~any() { delete content_; }

  bool has_value() const { return content_ != nullptr; }

  template <typename T> friend T &any_cast(any &val);

  template <typename T> friend const T &any_cast(const any &val);

private:
  struct placeholder {
    virtual ~placeholder() {}
    virtual placeholder *clone() const = 0;
  };

  template <typename T> struct holder : placeholder {
    holder(const T &value) : value_(value) {}
    placeholder *clone() const override { return new holder(value_); }
    T value_;
  };

  placeholder *clone() const { return content_ ? content_->clone() : nullptr; }

  placeholder *content_ = nullptr;
};

template <typename T> T &any_cast(any &val) {
  if (!val.content_) { throw std::bad_cast(); }
  auto p = dynamic_cast<any::holder<T> *>(val.content_);
  assert(p);
  if (!p) { throw std::bad_cast(); }
  return p->value_;
}

template <> inline any &any_cast<any>(any &val) { return val; }

template <typename T> const T &any_cast(const any &val) {
  assert(val.content_);
  auto p = dynamic_cast<any::holder<T> *>(val.content_);
  assert(p);
  if (!p) { throw std::bad_cast(); }
  return p->value_;
}

template <> inline const any &any_cast<any>(const any &val) { return val; }
#endif

/*-----------------------------------------------------------------------------
 *  scope_exit
 *---------------------------------------------------------------------------*/

// This is based on
// "http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4189".

template <typename EF> struct scope_exit {
  explicit scope_exit(EF &&f)
      : exit_function(std::move(f)), execute_on_destruction{true} {}

  scope_exit(scope_exit &&rhs)
      : exit_function(std::move(rhs.exit_function)),
        execute_on_destruction{rhs.execute_on_destruction} {
    rhs.release();
  }

  ~scope_exit() {
    if (execute_on_destruction) { this->exit_function(); }
  }

  void release() { this->execute_on_destruction = false; }

private:
  scope_exit(const scope_exit &) = delete;
  void operator=(const scope_exit &) = delete;
  scope_exit &operator=(scope_exit &&) = delete;

  EF exit_function;
  bool execute_on_destruction;
};

template <typename EF>
auto make_scope_exit(EF &&exit_function) -> scope_exit<EF> {
  return scope_exit<typename std::remove_reference<EF>::type>(
      std::forward<EF>(exit_function));
}

/*-----------------------------------------------------------------------------
 *  UTF8 functions
 *---------------------------------------------------------------------------*/

inline size_t codepoint_length(const char *s8, size_t l) {
  if (l) {
    auto b = static_cast<uint8_t>(s8[0]);
    if ((b & 0x80) == 0) {
      return 1;
    } else if ((b & 0xE0) == 0xC0) {
      return 2;
    } else if ((b & 0xF0) == 0xE0) {
      return 3;
    } else if ((b & 0xF8) == 0xF0) {
      return 4;
    }
  }
  return 0;
}

inline size_t encode_codepoint(char32_t cp, char *buff) {
  if (cp < 0x0080) {
    buff[0] = static_cast<char>(cp & 0x7F);
    return 1;
  } else if (cp < 0x0800) {
    buff[0] = static_cast<char>(0xC0 | ((cp >> 6) & 0x1F));
    buff[1] = static_cast<char>(0x80 | (cp & 0x3F));
    return 2;
  } else if (cp < 0xD800) {
    buff[0] = static_cast<char>(0xE0 | ((cp >> 12) & 0xF));
    buff[1] = static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
    buff[2] = static_cast<char>(0x80 | (cp & 0x3F));
    return 3;
  } else if (cp < 0xE000) {
    // D800 - DFFF is invalid...
    return 0;
  } else if (cp < 0x10000) {
    buff[0] = static_cast<char>(0xE0 | ((cp >> 12) & 0xF));
    buff[1] = static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
    buff[2] = static_cast<char>(0x80 | (cp & 0x3F));
    return 3;
  } else if (cp < 0x110000) {
    buff[0] = static_cast<char>(0xF0 | ((cp >> 18) & 0x7));
    buff[1] = static_cast<char>(0x80 | ((cp >> 12) & 0x3F));
    buff[2] = static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
    buff[3] = static_cast<char>(0x80 | (cp & 0x3F));
    return 4;
  }
  return 0;
}

inline std::string encode_codepoint(char32_t cp) {
  char buff[4];
  auto l = encode_codepoint(cp, buff);
  return std::string(buff, l);
}

inline bool decode_codepoint(const char *s8, size_t l, size_t &bytes,
                             char32_t &cp) {
  if (l) {
    auto b = static_cast<uint8_t>(s8[0]);
    if ((b & 0x80) == 0) {
      bytes = 1;
      cp = b;
      return true;
    } else if ((b & 0xE0) == 0xC0) {
      if (l >= 2) {
        bytes = 2;
        cp = ((static_cast<char32_t>(s8[0] & 0x1F)) << 6) |
             (static_cast<char32_t>(s8[1] & 0x3F));
        return true;
      }
    } else if ((b & 0xF0) == 0xE0) {
      if (l >= 3) {
        bytes = 3;
        cp = ((static_cast<char32_t>(s8[0] & 0x0F)) << 12) |
             ((static_cast<char32_t>(s8[1] & 0x3F)) << 6) |
             (static_cast<char32_t>(s8[2] & 0x3F));
        return true;
      }
    } else if ((b & 0xF8) == 0xF0) {
      if (l >= 4) {
        bytes = 4;
        cp = ((static_cast<char32_t>(s8[0] & 0x07)) << 18) |
             ((static_cast<char32_t>(s8[1] & 0x3F)) << 12) |
             ((static_cast<char32_t>(s8[2] & 0x3F)) << 6) |
             (static_cast<char32_t>(s8[3] & 0x3F));
        return true;
      }
    }
  }
  return false;
}

inline size_t decode_codepoint(const char *s8, size_t l, char32_t &out) {
  size_t bytes;
  if (decode_codepoint(s8, l, bytes, out)) { return bytes; }
  return 0;
}

inline char32_t decode_codepoint(const char *s8, size_t l) {
  char32_t out = 0;
  decode_codepoint(s8, l, out);
  return out;
}

inline std::u32string decode(const char *s8, size_t l) {
  std::u32string out;
  size_t i = 0;
  while (i < l) {
    auto beg = i++;
    while (i < l && (s8[i] & 0xc0) == 0x80) {
      i++;
    }
    out += decode_codepoint(&s8[beg], (i - beg));
  }
  return out;
}

/*-----------------------------------------------------------------------------
 *  resolve_escape_sequence
 *---------------------------------------------------------------------------*/

inline bool is_hex(char c, int &v) {
  if ('0' <= c && c <= '9') {
    v = c - '0';
    return true;
  } else if ('a' <= c && c <= 'f') {
    v = c - 'a' + 10;
    return true;
  } else if ('A' <= c && c <= 'F') {
    v = c - 'A' + 10;
    return true;
  }
  return false;
}

inline bool is_digit(char c, int &v) {
  if ('0' <= c && c <= '9') {
    v = c - '0';
    return true;
  }
  return false;
}

inline std::pair<int, size_t> parse_hex_number(const char *s, size_t n,
                                               size_t i) {
  int ret = 0;
  int val;
  while (i < n && is_hex(s[i], val)) {
    ret = static_cast<int>(ret * 16 + val);
    i++;
  }
  return std::make_pair(ret, i);
}

inline std::pair<int, size_t> parse_octal_number(const char *s, size_t n,
                                                 size_t i) {
  int ret = 0;
  int val;
  while (i < n && is_digit(s[i], val)) {
    ret = static_cast<int>(ret * 8 + val);
    i++;
  }
  return std::make_pair(ret, i);
}

inline std::string resolve_escape_sequence(const char *s, size_t n) {
  std::string r;
  r.reserve(n);

  size_t i = 0;
  while (i < n) {
    auto ch = s[i];
    if (ch == '\\') {
      i++;
      if (i == n) { throw std::runtime_error("Invalid escape sequence..."); }
      switch (s[i]) {
      case 'n':
        r += '\n';
        i++;
        break;
      case 'r':
        r += '\r';
        i++;
        break;
      case 't':
        r += '\t';
        i++;
        break;
      case '\'':
        r += '\'';
        i++;
        break;
      case '"':
        r += '"';
        i++;
        break;
      case '[':
        r += '[';
        i++;
        break;
      case ']':
        r += ']';
        i++;
        break;
      case '\\':
        r += '\\';
        i++;
        break;
      case 'x':
      case 'u': {
        char32_t cp;
        std::tie(cp, i) = parse_hex_number(s, n, i + 1);
        r += encode_codepoint(cp);
        break;
      }
      default: {
        char32_t cp;
        std::tie(cp, i) = parse_octal_number(s, n, i);
        r += encode_codepoint(cp);
        break;
      }
      }
    } else {
      r += ch;
      i++;
    }
  }
  return r;
}

/*-----------------------------------------------------------------------------
 *  Trie
 *---------------------------------------------------------------------------*/

class Trie {
public:
  Trie() = default;
  Trie(const Trie &) = default;

  Trie(const std::vector<std::string> &items) {
    for (const auto &item : items) {
      for (size_t len = 1; len <= item.size(); len++) {
        auto last = len == item.size();
        std::string s(item.c_str(), len);
        auto it = dic_.find(s);
        if (it == dic_.end()) {
          dic_.emplace(s, Info{last, last});
        } else if (last) {
          it->second.match = true;
        } else {
          it->second.done = false;
        }
      }
    }
  }

  size_t match(const char *text, size_t text_len) const {
    size_t match_len = 0;
    {
      auto done = false;
      size_t len = 1;
      while (!done && len <= text_len) {
        std::string s(text, len);
        auto it = dic_.find(s);
        if (it == dic_.end()) {
          done = true;
        } else {
          if (it->second.match) { match_len = len; }
          if (it->second.done) { done = true; }
        }
        len += 1;
      }
    }
    return match_len;
  }

private:
  struct Info {
    bool done;
    bool match;
  };
  std::unordered_map<std::string, Info> dic_;
};

/*-----------------------------------------------------------------------------
 *  PEG
 *---------------------------------------------------------------------------*/

/*
 * Line information utility function
 */
inline std::pair<size_t, size_t> line_info(const char *start, const char *cur) {
  auto p = start;
  auto col_ptr = p;
  auto no = 1;

  while (p < cur) {
    if (*p == '\n') {
      no++;
      col_ptr = p + 1;
    }
    p++;
  }

  auto col = p - col_ptr + 1;

  return std::make_pair(no, col);
}

/*
 * String tag
 */
inline constexpr unsigned int str2tag(const char *str, unsigned int h = 0) {
  return (*str == '\0')
             ? h
             : str2tag(str + 1, (h * 33) ^ static_cast<unsigned char>(*str));
}

namespace udl {

inline constexpr unsigned int operator"" _(const char *s, size_t) {
  return str2tag(s);
}

} // namespace udl

/*
 * Semantic values
 */
struct SemanticValues : protected std::vector<any> {
  // Input text
  const char *path = nullptr;
  const char *ss = nullptr;
  const std::vector<size_t> *source_line_index = nullptr;

  // Matched string
  const char *c_str() const { return s_; }
  size_t length() const { return n_; }

  std::string str() const { return std::string(s_, n_); }

  // Definition name
  const std::string &name() const { return name_; }

  std::vector<unsigned int> tags;

  // Line number and column at which the matched string is
  std::pair<size_t, size_t> line_info() const {
    const auto &idx = *source_line_index;

    auto cur = static_cast<size_t>(std::distance(ss, s_));
    auto it = std::lower_bound(
        idx.begin(), idx.end(), cur,
        [](size_t element, size_t value) { return element < value; });

    auto id = static_cast<size_t>(std::distance(idx.begin(), it));
    auto off = cur - (id == 0 ? 0 : idx[id - 1] + 1);
    return std::make_pair(id + 1, off + 1);
  }

  // Choice count
  size_t choice_count() const { return choice_count_; }

  // Choice number (0 based index)
  size_t choice() const { return choice_; }

  // Tokens
  std::vector<std::pair<const char *, size_t>> tokens;

  std::string token(size_t id = 0) const {
    if (!tokens.empty()) {
      assert(id < tokens.size());
      const auto &tok = tokens[id];
      return std::string(tok.first, tok.second);
    }
    return std::string(s_, n_);
  }

  // Transform the semantic value vector to another vector
  template <typename T>
  auto transform(size_t beg = 0, size_t end = static_cast<size_t>(-1)) const
      -> vector<T> {
    return this->transform(beg, end,
                           [](const any &v) { return any_cast<T>(v); });
  }

  using std::vector<any>::iterator;
  using std::vector<any>::const_iterator;
  using std::vector<any>::size;
  using std::vector<any>::empty;
  using std::vector<any>::assign;
  using std::vector<any>::begin;
  using std::vector<any>::end;
  using std::vector<any>::rbegin;
  using std::vector<any>::rend;
  using std::vector<any>::operator[];
  using std::vector<any>::at;
  using std::vector<any>::resize;
  using std::vector<any>::front;
  using std::vector<any>::back;
  using std::vector<any>::push_back;
  using std::vector<any>::pop_back;
  using std::vector<any>::insert;
  using std::vector<any>::erase;
  using std::vector<any>::clear;
  using std::vector<any>::swap;
  using std::vector<any>::emplace;
  using std::vector<any>::emplace_back;

private:
  friend class Context;
  friend class Sequence;
  friend class PrioritizedChoice;
  friend class Holder;
  friend class PrecedenceClimbing;

  const char *s_ = nullptr;
  size_t n_ = 0;
  size_t choice_count_ = 0;
  size_t choice_ = 0;
  std::string name_;

  template <typename F>
  auto transform(F f) const
      -> vector<typename std::remove_const<decltype(f(any()))>::type> {
    vector<typename std::remove_const<decltype(f(any()))>::type> r;
    for (const auto &v : *this) {
      r.emplace_back(f(v));
    }
    return r;
  }

  template <typename F>
  auto transform(size_t beg, size_t end, F f) const
      -> vector<typename std::remove_const<decltype(f(any()))>::type> {
    vector<typename std::remove_const<decltype(f(any()))>::type> r;
    end = (std::min)(end, size());
    for (size_t i = beg; i < end; i++) {
      r.emplace_back(f((*this)[i]));
    }
    return r;
  }
};

/*
 * Semantic action
 */
template <typename R, typename F,
          typename std::enable_if<std::is_void<R>::value,
                                  std::nullptr_t>::type = nullptr,
          typename... Args>
any call(F fn, Args &&... args) {
  fn(std::forward<Args>(args)...);
  return any();
}

template <typename R, typename F,
          typename std::enable_if<
              std::is_same<typename std::remove_cv<R>::type, any>::value,
              std::nullptr_t>::type = nullptr,
          typename... Args>
any call(F fn, Args &&... args) {
  return fn(std::forward<Args>(args)...);
}

template <typename R, typename F,
          typename std::enable_if<
              !std::is_void<R>::value &&
                  !std::is_same<typename std::remove_cv<R>::type, any>::value,
              std::nullptr_t>::type = nullptr,
          typename... Args>
any call(F fn, Args &&... args) {
  return any(fn(std::forward<Args>(args)...));
}

class Action {
public:
  Action() = default;
  Action(const Action &rhs) = default;

  template <typename F,
            typename std::enable_if<!std::is_pointer<F>::value &&
                                        !std::is_same<F, std::nullptr_t>::value,
                                    std::nullptr_t>::type = nullptr>
  Action(F fn) : fn_(make_adaptor(fn, &F::operator())) {}

  template <typename F, typename std::enable_if<std::is_pointer<F>::value,
                                                std::nullptr_t>::type = nullptr>
  Action(F fn) : fn_(make_adaptor(fn, fn)) {}

  template <typename F,
            typename std::enable_if<std::is_same<F, std::nullptr_t>::value,
                                    std::nullptr_t>::type = nullptr>
  Action(F /*fn*/) {}

  template <typename F,
            typename std::enable_if<!std::is_pointer<F>::value &&
                                        !std::is_same<F, std::nullptr_t>::value,
                                    std::nullptr_t>::type = nullptr>
  void operator=(F fn) {
    fn_ = make_adaptor(fn, &F::operator());
  }

  template <typename F, typename std::enable_if<std::is_pointer<F>::value,
                                                std::nullptr_t>::type = nullptr>
  void operator=(F fn) {
    fn_ = make_adaptor(fn, fn);
  }

  template <typename F,
            typename std::enable_if<std::is_same<F, std::nullptr_t>::value,
                                    std::nullptr_t>::type = nullptr>
  void operator=(F /*fn*/) {}

  Action &operator=(const Action &rhs) = default;

  operator bool() const { return bool(fn_); }

  any operator()(SemanticValues &sv, any &dt) const { return fn_(sv, dt); }

private:
  template <typename R> struct TypeAdaptor_sv {
    TypeAdaptor_sv(std::function<R(SemanticValues &sv)> fn) : fn_(fn) {}
    any operator()(SemanticValues &sv, any & /*dt*/) {
      return call<R>(fn_, sv);
    }
    std::function<R(SemanticValues &sv)> fn_;
  };

  template <typename R> struct TypeAdaptor_csv {
    TypeAdaptor_csv(std::function<R(const SemanticValues &sv)> fn) : fn_(fn) {}
    any operator()(SemanticValues &sv, any & /*dt*/) {
      return call<R>(fn_, sv);
    }
    std::function<R(const SemanticValues &sv)> fn_;
  };

  template <typename R> struct TypeAdaptor_sv_dt {
    TypeAdaptor_sv_dt(std::function<R(SemanticValues &sv, any &dt)> fn)
        : fn_(fn) {}
    any operator()(SemanticValues &sv, any &dt) { return call<R>(fn_, sv, dt); }
    std::function<R(SemanticValues &sv, any &dt)> fn_;
  };

  template <typename R> struct TypeAdaptor_csv_dt {
    TypeAdaptor_csv_dt(std::function<R(const SemanticValues &sv, any &dt)> fn)
        : fn_(fn) {}
    any operator()(SemanticValues &sv, any &dt) { return call<R>(fn_, sv, dt); }
    std::function<R(const SemanticValues &sv, any &dt)> fn_;
  };

  typedef std::function<any(SemanticValues &sv, any &dt)> Fty;

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (F::*)(SemanticValues &sv) const) {
    return TypeAdaptor_sv<R>(fn);
  }

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (F::*)(const SemanticValues &sv) const) {
    return TypeAdaptor_csv<R>(fn);
  }

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (F::*)(SemanticValues &sv)) {
    return TypeAdaptor_sv<R>(fn);
  }

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (F::*)(const SemanticValues &sv)) {
    return TypeAdaptor_csv<R>(fn);
  }

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (*)(SemanticValues &sv)) {
    return TypeAdaptor_sv<R>(fn);
  }

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (*)(const SemanticValues &sv)) {
    return TypeAdaptor_csv<R>(fn);
  }

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (F::*)(SemanticValues &sv, any &dt) const) {
    return TypeAdaptor_sv_dt<R>(fn);
  }

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (F::*)(const SemanticValues &sv, any &dt) const) {
    return TypeAdaptor_csv_dt<R>(fn);
  }

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (F::*)(SemanticValues &sv, any &dt)) {
    return TypeAdaptor_sv_dt<R>(fn);
  }

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (F::*)(const SemanticValues &sv, any &dt)) {
    return TypeAdaptor_csv_dt<R>(fn);
  }

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (*)(SemanticValues &sv, any &dt)) {
    return TypeAdaptor_sv_dt<R>(fn);
  }

  template <typename F, typename R>
  Fty make_adaptor(F fn, R (*)(const SemanticValues &sv, any &dt)) {
    return TypeAdaptor_csv_dt<R>(fn);
  }

  Fty fn_;
};

/*
 * Semantic predicate
 */
// Note: 'parse_error' exception class should be be used in sematic action
// handlers to reject the rule.
struct parse_error {
  parse_error() = default;
  parse_error(const char *s) : s_(s) {}
  const char *what() const { return s_.empty() ? nullptr : s_.c_str(); }

private:
  std::string s_;
};

/*
 * Result
 */
inline bool success(size_t len) { return len != static_cast<size_t>(-1); }

inline bool fail(size_t len) { return len == static_cast<size_t>(-1); }

/*
 * Context
 */
class Context;
class Ope;
class Definition;

typedef std::function<void(const char *name, const char *s, size_t n,
                           const SemanticValues &sv, const Context &c,
                           const any &dt)>
    TracerEnter;

typedef std::function<void(const char *name, const char *s, size_t n,
                           const SemanticValues &sv, const Context &c,
                           const any &dt, size_t)>
    TracerLeave;

class Context {
public:
  const char *path;
  const char *s;
  const size_t l;
  std::vector<size_t> source_line_index;

  const char *error_pos = nullptr;
  const char *message_pos = nullptr;
  std::string message; // TODO: should be `int`.

  std::vector<std::shared_ptr<SemanticValues>> value_stack;
  size_t value_stack_size = 0;
  std::vector<std::vector<std::shared_ptr<Ope>>> args_stack;

  bool in_token = false;

  std::shared_ptr<Ope> whitespaceOpe;
  bool in_whitespace = false;

  std::shared_ptr<Ope> wordOpe;

  std::vector<std::map<std::string, std::string>> capture_scope_stack;
  size_t capture_scope_stack_size = 0;

  const size_t def_count;
  const bool enablePackratParsing;
  std::vector<bool> cache_registered;
  std::vector<bool> cache_success;

  std::map<std::pair<size_t, size_t>, std::tuple<size_t, any>> cache_values;

  TracerEnter tracer_enter;
  TracerLeave tracer_leave;

  Context(const char *a_path, const char *a_s, size_t a_l, size_t a_def_count,
          std::shared_ptr<Ope> a_whitespaceOpe, std::shared_ptr<Ope> a_wordOpe,
          bool a_enablePackratParsing, TracerEnter a_tracer_enter,
          TracerLeave a_tracer_leave)
      : path(a_path), s(a_s), l(a_l), whitespaceOpe(a_whitespaceOpe),
        wordOpe(a_wordOpe), def_count(a_def_count),
        enablePackratParsing(a_enablePackratParsing),
        cache_registered(enablePackratParsing ? def_count * (l + 1) : 0),
        cache_success(enablePackratParsing ? def_count * (l + 1) : 0),
        tracer_enter(a_tracer_enter), tracer_leave(a_tracer_leave) {

    for (size_t pos = 0; pos < l; pos++) {
      if (s[pos] == '\n') { source_line_index.push_back(pos); }
    }
    source_line_index.push_back(l);

    args_stack.resize(1);

    push_capture_scope();
  }

  ~Context() { assert(!value_stack_size); }

  Context(const Context &) = delete;
  Context(Context &&) = delete;
  Context operator=(const Context &) = delete;

  template <typename T>
  void packrat(const char *a_s, size_t def_id, size_t &len, any &val, T fn) {
    if (!enablePackratParsing) {
      fn(val);
      return;
    }

    auto col = a_s - s;
    auto idx = def_count * static_cast<size_t>(col) + def_id;

    if (cache_registered[idx]) {
      if (cache_success[idx]) {
        auto key = std::make_pair(col, def_id);
        std::tie(len, val) = cache_values[key];
        return;
      } else {
        len = static_cast<size_t>(-1);
        return;
      }
    } else {
      fn(val);
      cache_registered[idx] = true;
      cache_success[idx] = success(len);
      if (success(len)) {
        auto key = std::make_pair(col, def_id);
        cache_values[key] = std::make_pair(len, val);
      }
      return;
    }
  }

  SemanticValues &push() {
    assert(value_stack_size <= value_stack.size());
    if (value_stack_size == value_stack.size()) {
      value_stack.emplace_back(std::make_shared<SemanticValues>());
    } else {
      auto &sv = *value_stack[value_stack_size];
      if (!sv.empty()) {
        sv.clear();
        sv.tags.clear();
      }
      sv.s_ = nullptr;
      sv.n_ = 0;
      sv.choice_count_ = 0;
      sv.choice_ = 0;
      sv.tokens.clear();
    }

    auto &sv = *value_stack[value_stack_size++];
    sv.path = path;
    sv.ss = s;
    sv.source_line_index = &source_line_index;
    return sv;
  }

  void pop() { value_stack_size--; }

  void push_args(std::vector<std::shared_ptr<Ope>> &&args) {
    args_stack.emplace_back(args);
  }

  void pop_args() { args_stack.pop_back(); }

  const std::vector<std::shared_ptr<Ope>> &top_args() const {
    return args_stack[args_stack.size() - 1];
  }

  void push_capture_scope() {
    assert(capture_scope_stack_size <= capture_scope_stack.size());
    if (capture_scope_stack_size == capture_scope_stack.size()) {
      capture_scope_stack.emplace_back(std::map<std::string, std::string>());
    } else {
      auto &cs = capture_scope_stack[capture_scope_stack_size];
      cs.clear();
    }
    capture_scope_stack_size++;
  }

  void pop_capture_scope() { capture_scope_stack_size--; }

  void shift_capture_values() {
    assert(capture_scope_stack.size() >= 2);
    auto curr = &capture_scope_stack[capture_scope_stack_size - 1];
    auto prev = curr - 1;
    for (const auto &kv : *curr) {
      (*prev)[kv.first] = kv.second;
    }
  }

  void set_error_pos(const char *a_s) {
    if (error_pos < a_s) error_pos = a_s;
  }

  void trace_enter(const char *name, const char *a_s, size_t n,
                   SemanticValues &sv, any &dt) const;
  void trace_leave(const char *name, const char *a_s, size_t n,
                   SemanticValues &sv, any &dt, size_t len) const;
  bool is_traceable(const Ope &ope) const;

  mutable size_t next_trace_id = 0;
  mutable std::list<size_t> trace_ids;
};

/*
 * Parser operators
 */
class Ope {
public:
  struct Visitor;

  virtual ~Ope() {}
  size_t parse(const char *s, size_t n, SemanticValues &sv, Context &c,
               any &dt) const;
  virtual size_t parse_core(const char *s, size_t n, SemanticValues &sv,
                            Context &c, any &dt) const = 0;
  virtual void accept(Visitor &v) = 0;
};

class Sequence : public Ope {
public:
  template <typename... Args>
  Sequence(const Args &... args)
      : opes_{static_cast<std::shared_ptr<Ope>>(args)...} {}
  Sequence(const std::vector<std::shared_ptr<Ope>> &opes) : opes_(opes) {}
  Sequence(std::vector<std::shared_ptr<Ope>> &&opes) : opes_(opes) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override {
    auto &chldsv = c.push();
    auto pop_se = make_scope_exit([&]() { c.pop(); });
    size_t i = 0;
    for (const auto &ope : opes_) {
      const auto &rule = *ope;
      auto len = rule.parse(s + i, n - i, chldsv, c, dt);
      if (fail(len)) { return static_cast<size_t>(-1); }
      i += len;
    }
    if (!chldsv.empty()) {
      for (size_t j = 0; j < chldsv.size(); j++) {
        sv.emplace_back(std::move(chldsv[j]));
      }
    }
    if (!chldsv.tags.empty()) {
      for (size_t j = 0; j < chldsv.tags.size(); j++) {
        sv.tags.emplace_back(std::move(chldsv.tags[j]));
      }
    }
    sv.s_ = chldsv.c_str();
    sv.n_ = chldsv.length();
    if (!chldsv.tokens.empty()) {
      for (size_t j = 0; j < chldsv.tokens.size(); j++) {
        sv.tokens.emplace_back(std::move(chldsv.tokens[j]));
      }
    }
    return i;
  }

  void accept(Visitor &v) override;

  std::vector<std::shared_ptr<Ope>> opes_;
};

class PrioritizedChoice : public Ope {
public:
  template <typename... Args>
  PrioritizedChoice(const Args &... args)
      : opes_{static_cast<std::shared_ptr<Ope>>(args)...} {}
  PrioritizedChoice(const std::vector<std::shared_ptr<Ope>> &opes)
      : opes_(opes) {}
  PrioritizedChoice(std::vector<std::shared_ptr<Ope>> &&opes) : opes_(opes) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override {
    size_t id = 0;
    for (const auto &ope : opes_) {
      auto &chldsv = c.push();
      c.push_capture_scope();
      auto se = make_scope_exit([&]() {
        c.pop();
        c.pop_capture_scope();
      });
      const auto &rule = *ope;
      auto len = rule.parse(s, n, chldsv, c, dt);
      if (success(len)) {
        if (!chldsv.empty()) {
          for (size_t i = 0; i < chldsv.size(); i++) {
            sv.emplace_back(std::move(chldsv[i]));
          }
        }
        if (!chldsv.tags.empty()) {
          for (size_t i = 0; i < chldsv.tags.size(); i++) {
            sv.tags.emplace_back(std::move(chldsv.tags[i]));
          }
        }
        sv.s_ = chldsv.c_str();
        sv.n_ = chldsv.length();
        sv.choice_count_ = opes_.size();
        sv.choice_ = id;
        if (!chldsv.tokens.empty()) {
          for (size_t i = 0; i < chldsv.tokens.size(); i++) {
            sv.tokens.emplace_back(std::move(chldsv.tokens[i]));
          }
        }

        c.shift_capture_values();
        return len;
      }
      id++;
    }
    return static_cast<size_t>(-1);
  }

  void accept(Visitor &v) override;

  size_t size() const { return opes_.size(); }

  std::vector<std::shared_ptr<Ope>> opes_;
};

class Repetition : public Ope {
public:
  Repetition(const std::shared_ptr<Ope> &ope, size_t min, size_t max)
      : ope_(ope), min_(min), max_(max) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override {
    size_t count = 0;
    size_t i = 0;
    while (count < min_) {
      c.push_capture_scope();
      auto se = make_scope_exit([&]() { c.pop_capture_scope(); });
      const auto &rule = *ope_;
      auto len = rule.parse(s + i, n - i, sv, c, dt);
      if (success(len)) {
        c.shift_capture_values();
      } else {
        return static_cast<size_t>(-1);
      }
      i += len;
      count++;
    }

    auto save_error_pos = c.error_pos;
    while (n - i > 0 && count < max_) {
      c.push_capture_scope();
      auto se = make_scope_exit([&]() { c.pop_capture_scope(); });
      auto save_sv_size = sv.size();
      auto save_tok_size = sv.tokens.size();
      const auto &rule = *ope_;
      auto len = rule.parse(s + i, n - i, sv, c, dt);
      if (success(len)) {
        c.shift_capture_values();
      } else {
        if (sv.size() != save_sv_size) {
          sv.erase(sv.begin() + static_cast<std::ptrdiff_t>(save_sv_size));
          sv.tags.erase(sv.tags.begin() +
                        static_cast<std::ptrdiff_t>(save_sv_size));
        }
        if (sv.tokens.size() != save_tok_size) {
          sv.tokens.erase(sv.tokens.begin() +
                          static_cast<std::ptrdiff_t>(save_tok_size));
        }
        c.error_pos = save_error_pos;
        break;
      }
      i += len;
      count++;
    }
    return i;
  }

  void accept(Visitor &v) override;

  bool is_zom() const {
    return min_ == 0 && max_ == std::numeric_limits<size_t>::max();
  }

  static std::shared_ptr<Repetition> zom(const std::shared_ptr<Ope> &ope) {
    return std::make_shared<Repetition>(ope, 0,
                                        std::numeric_limits<size_t>::max());
  }

  static std::shared_ptr<Repetition> oom(const std::shared_ptr<Ope> &ope) {
    return std::make_shared<Repetition>(ope, 1,
                                        std::numeric_limits<size_t>::max());
  }

  static std::shared_ptr<Repetition> opt(const std::shared_ptr<Ope> &ope) {
    return std::make_shared<Repetition>(ope, 0, 1);
  }

  std::shared_ptr<Ope> ope_;
  size_t min_;
  size_t max_;
};

class AndPredicate : public Ope {
public:
  AndPredicate(const std::shared_ptr<Ope> &ope) : ope_(ope) {}

  size_t parse_core(const char *s, size_t n, SemanticValues & /*sv*/,
                    Context &c, any &dt) const override {
    auto &chldsv = c.push();
    c.push_capture_scope();
    auto se = make_scope_exit([&]() {
      c.pop();
      c.pop_capture_scope();
    });
    const auto &rule = *ope_;
    auto len = rule.parse(s, n, chldsv, c, dt);
    if (success(len)) {
      return 0;
    } else {
      return static_cast<size_t>(-1);
    }
  }

  void accept(Visitor &v) override;

  std::shared_ptr<Ope> ope_;
};

class NotPredicate : public Ope {
public:
  NotPredicate(const std::shared_ptr<Ope> &ope) : ope_(ope) {}

  size_t parse_core(const char *s, size_t n, SemanticValues & /*sv*/,
                    Context &c, any &dt) const override {
    auto save_error_pos = c.error_pos;
    auto &chldsv = c.push();
    c.push_capture_scope();
    auto se = make_scope_exit([&]() {
      c.pop();
      c.pop_capture_scope();
    });
    const auto &rule = *ope_;
    auto len = rule.parse(s, n, chldsv, c, dt);
    if (success(len)) {
      c.set_error_pos(s);
      return static_cast<size_t>(-1);
    } else {
      c.error_pos = save_error_pos;
      return 0;
    }
  }

  void accept(Visitor &v) override;

  std::shared_ptr<Ope> ope_;
};

class Dictionary : public Ope, public std::enable_shared_from_this<Dictionary> {
public:
  Dictionary(const std::vector<std::string> &v) : trie_(v) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override;

  void accept(Visitor &v) override;

  Trie trie_;
};

class LiteralString : public Ope,
                      public std::enable_shared_from_this<LiteralString> {
public:
  LiteralString(const std::string &s, bool ignore_case)
      : lit_(s), ignore_case_(ignore_case), init_is_word_(false),
        is_word_(false) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override;

  void accept(Visitor &v) override;

  std::string lit_;
  bool ignore_case_;
  mutable bool init_is_word_;
  mutable bool is_word_;
};

class CharacterClass : public Ope,
                       public std::enable_shared_from_this<CharacterClass> {
public:
  CharacterClass(const std::string &s, bool negated) : negated_(negated) {
    auto chars = decode(s.c_str(), s.length());
    auto i = 0u;
    while (i < chars.size()) {
      if (i + 2 < chars.size() && chars[i + 1] == '-') {
        auto cp1 = chars[i];
        auto cp2 = chars[i + 2];
        ranges_.emplace_back(std::make_pair(cp1, cp2));
        i += 3;
      } else {
        auto cp = chars[i];
        ranges_.emplace_back(std::make_pair(cp, cp));
        i += 1;
      }
    }
    assert(!ranges_.empty());
  }

  CharacterClass(const std::vector<std::pair<char32_t, char32_t>> &ranges,
                 bool negated)
      : ranges_(ranges), negated_(negated) {
    assert(!ranges_.empty());
  }

  size_t parse_core(const char *s, size_t n, SemanticValues & /*sv*/,
                    Context &c, any & /*dt*/) const override {
    if (n < 1) {
      c.set_error_pos(s);
      return static_cast<size_t>(-1);
    }

    char32_t cp = 0;
    auto len = decode_codepoint(s, n, cp);

    for (const auto &range : ranges_) {
      if (range.first <= cp && cp <= range.second) {
        if (negated_) {
          c.set_error_pos(s);
          return static_cast<size_t>(-1);
        } else {
          return len;
        }
      }
    }

    if (negated_) {
      return len;
    } else {
      c.set_error_pos(s);
      return static_cast<size_t>(-1);
    }
  }

  void accept(Visitor &v) override;

  std::vector<std::pair<char32_t, char32_t>> ranges_;
  bool negated_;
};

class Character : public Ope, public std::enable_shared_from_this<Character> {
public:
  Character(char ch) : ch_(ch) {}

  size_t parse_core(const char *s, size_t n, SemanticValues & /*sv*/,
                    Context &c, any & /*dt*/) const override {
    if (n < 1 || s[0] != ch_) {
      c.set_error_pos(s);
      return static_cast<size_t>(-1);
    }
    return 1;
  }

  void accept(Visitor &v) override;

  char ch_;
};

class AnyCharacter : public Ope,
                     public std::enable_shared_from_this<AnyCharacter> {
public:
  size_t parse_core(const char *s, size_t n, SemanticValues & /*sv*/,
                    Context &c, any & /*dt*/) const override {
    auto len = codepoint_length(s, n);
    if (len < 1) {
      c.set_error_pos(s);
      return static_cast<size_t>(-1);
    }
    return len;
  }

  void accept(Visitor &v) override;
};

class CaptureScope : public Ope {
public:
  CaptureScope(const std::shared_ptr<Ope> &ope) : ope_(ope) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override {
    c.push_capture_scope();
    auto se = make_scope_exit([&]() { c.pop_capture_scope(); });
    const auto &rule = *ope_;
    auto len = rule.parse(s, n, sv, c, dt);
    return len;
  }

  void accept(Visitor &v) override;

  std::shared_ptr<Ope> ope_;
};

class Capture : public Ope {
public:
  typedef std::function<void(const char *s, size_t n, Context &c)> MatchAction;

  Capture(const std::shared_ptr<Ope> &ope, MatchAction ma)
      : ope_(ope), match_action_(ma) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override {
    const auto &rule = *ope_;
    auto len = rule.parse(s, n, sv, c, dt);
    if (success(len) && match_action_) { match_action_(s, len, c); }
    return len;
  }

  void accept(Visitor &v) override;

  std::shared_ptr<Ope> ope_;
  MatchAction match_action_;
};

class TokenBoundary : public Ope {
public:
  TokenBoundary(const std::shared_ptr<Ope> &ope) : ope_(ope) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override;

  void accept(Visitor &v) override;

  std::shared_ptr<Ope> ope_;
};

class Ignore : public Ope {
public:
  Ignore(const std::shared_ptr<Ope> &ope) : ope_(ope) {}

  size_t parse_core(const char *s, size_t n, SemanticValues & /*sv*/,
                    Context &c, any &dt) const override {
    const auto &rule = *ope_;
    auto &chldsv = c.push();
    auto se = make_scope_exit([&]() { c.pop(); });
    return rule.parse(s, n, chldsv, c, dt);
  }

  void accept(Visitor &v) override;

  std::shared_ptr<Ope> ope_;
};

typedef std::function<size_t(const char *s, size_t n, SemanticValues &sv,
                             any &dt)>
    Parser;

class User : public Ope {
public:
  User(Parser fn) : fn_(fn) {}
  size_t parse_core(const char *s, size_t n, SemanticValues &sv,
                    Context & /*c*/, any &dt) const override {
    assert(fn_);
    return fn_(s, n, sv, dt);
  }
  void accept(Visitor &v) override;
  std::function<size_t(const char *s, size_t n, SemanticValues &sv, any &dt)>
      fn_;
};

class WeakHolder : public Ope {
public:
  WeakHolder(const std::shared_ptr<Ope> &ope) : weak_(ope) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override {
    auto ope = weak_.lock();
    assert(ope);
    const auto &rule = *ope;
    return rule.parse(s, n, sv, c, dt);
  }

  void accept(Visitor &v) override;

  std::weak_ptr<Ope> weak_;
};

class Holder : public Ope {
public:
  Holder(Definition *outer) : outer_(outer) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override;

  void accept(Visitor &v) override;

  any reduce(SemanticValues &sv, any &dt) const;

  const char *trace_name() const;

  std::shared_ptr<Ope> ope_;
  Definition *outer_;
  mutable std::string trace_name_;

  friend class Definition;
};

typedef std::unordered_map<std::string, Definition> Grammar;

class Reference : public Ope, public std::enable_shared_from_this<Reference> {
public:
  Reference(const Grammar &grammar, const std::string &name, const char *s,
            bool is_macro, const std::vector<std::shared_ptr<Ope>> &args)
      : grammar_(grammar), name_(name), s_(s), is_macro_(is_macro), args_(args),
        rule_(nullptr), iarg_(0) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override;

  void accept(Visitor &v) override;

  std::shared_ptr<Ope> get_core_operator() const;

  const Grammar &grammar_;
  const std::string name_;
  const char *s_;

  const bool is_macro_;
  const std::vector<std::shared_ptr<Ope>> args_;

  Definition *rule_;
  size_t iarg_;
};

class Whitespace : public Ope {
public:
  Whitespace(const std::shared_ptr<Ope> &ope) : ope_(ope) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override {
    if (c.in_whitespace) { return 0; }
    c.in_whitespace = true;
    auto se = make_scope_exit([&]() { c.in_whitespace = false; });
    const auto &rule = *ope_;
    return rule.parse(s, n, sv, c, dt);
  }

  void accept(Visitor &v) override;

  std::shared_ptr<Ope> ope_;
};

class BackReference : public Ope {
public:
  BackReference(const std::string &name) : name_(name) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override;

  void accept(Visitor &v) override;

  std::string name_;
};

class PrecedenceClimbing : public Ope {
public:
  using BinOpeInfo = std::map<std::string, std::pair<size_t, char>>;

  PrecedenceClimbing(const std::shared_ptr<Ope> &atom,
                     const std::shared_ptr<Ope> &binop, const BinOpeInfo &info,
                     const Definition &rule)
      : atom_(atom), binop_(binop), info_(info), rule_(rule) {}

  size_t parse_core(const char *s, size_t n, SemanticValues &sv, Context &c,
                    any &dt) const override {
    return parse_expression(s, n, sv, c, dt, 0);
  }

  void accept(Visitor &v) override;

  std::shared_ptr<Ope> atom_;
  std::shared_ptr<Ope> binop_;
  BinOpeInfo info_;
  const Definition &rule_;

private:
  size_t parse_expression(const char *s, size_t n, SemanticValues &sv,
                          Context &c, any &dt, size_t min_prec) const;

  Definition &get_reference_for_binop(Context &c) const;
};

/*
 * Factories
 */
template <typename... Args> std::shared_ptr<Ope> seq(Args &&... args) {
  return std::make_shared<Sequence>(static_cast<std::shared_ptr<Ope>>(args)...);
}

template <typename... Args> std::shared_ptr<Ope> cho(Args &&... args) {
  return std::make_shared<PrioritizedChoice>(
      static_cast<std::shared_ptr<Ope>>(args)...);
}

inline std::shared_ptr<Ope> zom(const std::shared_ptr<Ope> &ope) {
  return Repetition::zom(ope);
}

inline std::shared_ptr<Ope> oom(const std::shared_ptr<Ope> &ope) {
  return Repetition::oom(ope);
}

inline std::shared_ptr<Ope> opt(const std::shared_ptr<Ope> &ope) {
  return Repetition::opt(ope);
}

inline std::shared_ptr<Ope> rep(const std::shared_ptr<Ope> &ope, size_t min,
                                size_t max) {
  return std::make_shared<Repetition>(ope, min, max);
}

inline std::shared_ptr<Ope> apd(const std::shared_ptr<Ope> &ope) {
  return std::make_shared<AndPredicate>(ope);
}

inline std::shared_ptr<Ope> npd(const std::shared_ptr<Ope> &ope) {
  return std::make_shared<NotPredicate>(ope);
}

inline std::shared_ptr<Ope> dic(const std::vector<std::string> &v) {
  return std::make_shared<Dictionary>(v);
}

inline std::shared_ptr<Ope> lit(const std::string &s) {
  return std::make_shared<LiteralString>(s, false);
}

inline std::shared_ptr<Ope> liti(const std::string &s) {
  return std::make_shared<LiteralString>(s, true);
}

inline std::shared_ptr<Ope> cls(const std::string &s) {
  return std::make_shared<CharacterClass>(s, false);
}

inline std::shared_ptr<Ope>
cls(const std::vector<std::pair<char32_t, char32_t>> &ranges) {
  return std::make_shared<CharacterClass>(ranges, false);
}

inline std::shared_ptr<Ope> ncls(const std::string &s) {
  return std::make_shared<CharacterClass>(s, true);
}

inline std::shared_ptr<Ope>
ncls(const std::vector<std::pair<char32_t, char32_t>> &ranges) {
  return std::make_shared<CharacterClass>(ranges, true);
}

inline std::shared_ptr<Ope> chr(char dt) {
  return std::make_shared<Character>(dt);
}

inline std::shared_ptr<Ope> dot() { return std::make_shared<AnyCharacter>(); }

inline std::shared_ptr<Ope> csc(const std::shared_ptr<Ope> &ope) {
  return std::make_shared<CaptureScope>(ope);
}

inline std::shared_ptr<Ope> cap(const std::shared_ptr<Ope> &ope,
                                Capture::MatchAction ma) {
  return std::make_shared<Capture>(ope, ma);
}

inline std::shared_ptr<Ope> tok(const std::shared_ptr<Ope> &ope) {
  return std::make_shared<TokenBoundary>(ope);
}

inline std::shared_ptr<Ope> ign(const std::shared_ptr<Ope> &ope) {
  return std::make_shared<Ignore>(ope);
}

inline std::shared_ptr<Ope>
usr(std::function<size_t(const char *s, size_t n, SemanticValues &sv, any &dt)>
        fn) {
  return std::make_shared<User>(fn);
}

inline std::shared_ptr<Ope> ref(const Grammar &grammar, const std::string &name,
                                const char *s, bool is_macro,
                                const std::vector<std::shared_ptr<Ope>> &args) {
  return std::make_shared<Reference>(grammar, name, s, is_macro, args);
}

inline std::shared_ptr<Ope> wsp(const std::shared_ptr<Ope> &ope) {
  return std::make_shared<Whitespace>(std::make_shared<Ignore>(ope));
}

inline std::shared_ptr<Ope> bkr(const std::string &name) {
  return std::make_shared<BackReference>(name);
}

inline std::shared_ptr<Ope> pre(const std::shared_ptr<Ope> &atom,
                                const std::shared_ptr<Ope> &binop,
                                const PrecedenceClimbing::BinOpeInfo &info,
                                const Definition &rule) {
  return std::make_shared<PrecedenceClimbing>(atom, binop, info, rule);
}

/*
 * Visitor
 */
struct Ope::Visitor {
  virtual ~Visitor() {}
  virtual void visit(Sequence & /*ope*/) {}
  virtual void visit(PrioritizedChoice & /*ope*/) {}
  virtual void visit(Repetition & /*ope*/) {}
  virtual void visit(AndPredicate & /*ope*/) {}
  virtual void visit(NotPredicate & /*ope*/) {}
  virtual void visit(Dictionary & /*ope*/) {}
  virtual void visit(LiteralString & /*ope*/) {}
  virtual void visit(CharacterClass & /*ope*/) {}
  virtual void visit(Character & /*ope*/) {}
  virtual void visit(AnyCharacter & /*ope*/) {}
  virtual void visit(CaptureScope & /*ope*/) {}
  virtual void visit(Capture & /*ope*/) {}
  virtual void visit(TokenBoundary & /*ope*/) {}
  virtual void visit(Ignore & /*ope*/) {}
  virtual void visit(User & /*ope*/) {}
  virtual void visit(WeakHolder & /*ope*/) {}
  virtual void visit(Holder & /*ope*/) {}
  virtual void visit(Reference & /*ope*/) {}
  virtual void visit(Whitespace & /*ope*/) {}
  virtual void visit(BackReference & /*ope*/) {}
  virtual void visit(PrecedenceClimbing & /*ope*/) {}
};

struct IsReference : public Ope::Visitor {
  void visit(Reference & /*ope*/) override { is_reference = true; }
  bool is_reference = false;
};

struct TraceOpeName : public Ope::Visitor {
  void visit(Sequence & /*ope*/) override { name = "Sequence"; }
  void visit(PrioritizedChoice & /*ope*/) override {
    name = "PrioritizedChoice";
  }
  void visit(Repetition & /*ope*/) override { name = "Repetition"; }
  void visit(AndPredicate & /*ope*/) override { name = "AndPredicate"; }
  void visit(NotPredicate & /*ope*/) override { name = "NotPredicate"; }
  void visit(Dictionary & /*ope*/) override { name = "Dictionary"; }
  void visit(LiteralString & /*ope*/) override { name = "LiteralString"; }
  void visit(CharacterClass & /*ope*/) override { name = "CharacterClass"; }
  void visit(Character & /*ope*/) override { name = "Character"; }
  void visit(AnyCharacter & /*ope*/) override { name = "AnyCharacter"; }
  void visit(CaptureScope & /*ope*/) override { name = "CaptureScope"; }
  void visit(Capture & /*ope*/) override { name = "Capture"; }
  void visit(TokenBoundary & /*ope*/) override { name = "TokenBoundary"; }
  void visit(Ignore & /*ope*/) override { name = "Ignore"; }
  void visit(User & /*ope*/) override { name = "User"; }
  void visit(WeakHolder & /*ope*/) override { name = "WeakHolder"; }
  void visit(Holder &ope) override { name = ope.trace_name(); }
  void visit(Reference & /*ope*/) override { name = "Reference"; }
  void visit(Whitespace & /*ope*/) override { name = "Whitespace"; }
  void visit(BackReference & /*ope*/) override { name = "BackReference"; }
  void visit(PrecedenceClimbing & /*ope*/) override {
    name = "PrecedenceClimbing";
  }

  const char *name = nullptr;
};

struct AssignIDToDefinition : public Ope::Visitor {
  void visit(Sequence &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
    }
  }
  void visit(PrioritizedChoice &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
    }
  }
  void visit(Repetition &ope) override { ope.ope_->accept(*this); }
  void visit(AndPredicate &ope) override { ope.ope_->accept(*this); }
  void visit(NotPredicate &ope) override { ope.ope_->accept(*this); }
  void visit(CaptureScope &ope) override { ope.ope_->accept(*this); }
  void visit(Capture &ope) override { ope.ope_->accept(*this); }
  void visit(TokenBoundary &ope) override { ope.ope_->accept(*this); }
  void visit(Ignore &ope) override { ope.ope_->accept(*this); }
  void visit(WeakHolder &ope) override { ope.weak_.lock()->accept(*this); }
  void visit(Holder &ope) override;
  void visit(Reference &ope) override;
  void visit(Whitespace &ope) override { ope.ope_->accept(*this); }

  std::unordered_map<void *, size_t> ids;
};

struct IsLiteralToken : public Ope::Visitor {
  void visit(PrioritizedChoice &ope) override {
    for (auto op : ope.opes_) {
      if (!IsLiteralToken::check(*op)) { return; }
    }
    result_ = true;
  }

  void visit(Dictionary & /*ope*/) override { result_ = true; }
  void visit(LiteralString & /*ope*/) override { result_ = true; }

  static bool check(Ope &ope) {
    IsLiteralToken vis;
    ope.accept(vis);
    return vis.result_;
  }

private:
  bool result_ = false;
};

struct TokenChecker : public Ope::Visitor {
  void visit(Sequence &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
    }
  }
  void visit(PrioritizedChoice &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
    }
  }
  void visit(Repetition &ope) override { ope.ope_->accept(*this); }
  void visit(CaptureScope &ope) override { ope.ope_->accept(*this); }
  void visit(Capture &ope) override { ope.ope_->accept(*this); }
  void visit(TokenBoundary & /*ope*/) override { has_token_boundary_ = true; }
  void visit(Ignore &ope) override { ope.ope_->accept(*this); }
  void visit(WeakHolder &ope) override { ope.weak_.lock()->accept(*this); }
  void visit(Reference &ope) override;
  void visit(Whitespace &ope) override { ope.ope_->accept(*this); }
  void visit(PrecedenceClimbing &ope) override { ope.atom_->accept(*this); }

  static bool is_token(Ope &ope) {
    if (IsLiteralToken::check(ope)) { return true; }

    TokenChecker vis;
    ope.accept(vis);
    return vis.has_token_boundary_ || !vis.has_rule_;
  }

private:
  bool has_token_boundary_ = false;
  bool has_rule_ = false;
};

struct DetectLeftRecursion : public Ope::Visitor {
  DetectLeftRecursion(const std::string &name) : name_(name) {}

  void visit(Sequence &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
      if (done_) {
        break;
      } else if (error_s) {
        done_ = true;
        break;
      }
    }
  }
  void visit(PrioritizedChoice &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
      if (error_s) {
        done_ = true;
        break;
      }
    }
  }
  void visit(Repetition &ope) override {
    ope.ope_->accept(*this);
    done_ = ope.min_ > 0;
  }
  void visit(AndPredicate &ope) override {
    ope.ope_->accept(*this);
    done_ = false;
  }
  void visit(NotPredicate &ope) override {
    ope.ope_->accept(*this);
    done_ = false;
  }
  void visit(Dictionary & /*ope*/) override { done_ = true; }
  void visit(LiteralString &ope) override { done_ = !ope.lit_.empty(); }
  void visit(CharacterClass & /*ope*/) override { done_ = true; }
  void visit(Character & /*ope*/) override { done_ = true; }
  void visit(AnyCharacter & /*ope*/) override { done_ = true; }
  void visit(CaptureScope &ope) override { ope.ope_->accept(*this); }
  void visit(Capture &ope) override { ope.ope_->accept(*this); }
  void visit(TokenBoundary &ope) override { ope.ope_->accept(*this); }
  void visit(Ignore &ope) override { ope.ope_->accept(*this); }
  void visit(User & /*ope*/) override { done_ = true; }
  void visit(WeakHolder &ope) override { ope.weak_.lock()->accept(*this); }
  void visit(Holder &ope) override { ope.ope_->accept(*this); }
  void visit(Reference &ope) override;
  void visit(Whitespace &ope) override { ope.ope_->accept(*this); }
  void visit(BackReference & /*ope*/) override { done_ = true; }
  void visit(PrecedenceClimbing &ope) override { ope.atom_->accept(*this); }

  const char *error_s = nullptr;

private:
  std::string name_;
  std::set<std::string> refs_;
  bool done_ = false;
};

struct HasEmptyElement : public Ope::Visitor {
  HasEmptyElement(std::list<std::pair<const char *, std::string>> &refs)
      : refs_(refs) {}

  void visit(Sequence &ope) override {
    bool save_is_empty = false;
    const char *save_error_s = nullptr;
    std::string save_error_name;
    for (auto op : ope.opes_) {
      op->accept(*this);
      if (!is_empty) { return; }
      save_is_empty = is_empty;
      save_error_s = error_s;
      save_error_name = error_name;
      is_empty = false;
      error_name.clear();
    }
    is_empty = save_is_empty;
    error_s = save_error_s;
    error_name = save_error_name;
  }
  void visit(PrioritizedChoice &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
      if (is_empty) { return; }
    }
  }
  void visit(Repetition &ope) override {
    if (ope.min_ == 0) {
      set_error();
    } else {
      ope.ope_->accept(*this);
    }
  }
  void visit(AndPredicate & /*ope*/) override { set_error(); }
  void visit(NotPredicate & /*ope*/) override { set_error(); }
  void visit(CaptureScope &ope) override { ope.ope_->accept(*this); }
  void visit(Capture &ope) override { ope.ope_->accept(*this); }
  void visit(TokenBoundary &ope) override { ope.ope_->accept(*this); }
  void visit(Ignore &ope) override { ope.ope_->accept(*this); }
  void visit(WeakHolder &ope) override { ope.weak_.lock()->accept(*this); }
  void visit(Holder &ope) override { ope.ope_->accept(*this); }
  void visit(Reference &ope) override;
  void visit(Whitespace &ope) override { ope.ope_->accept(*this); }
  void visit(PrecedenceClimbing &ope) override { ope.atom_->accept(*this); }

  bool is_empty = false;
  const char *error_s = nullptr;
  std::string error_name;

private:
  void set_error() {
    is_empty = true;
    error_s = refs_.back().first;
    error_name = refs_.back().second;
  }
  std::list<std::pair<const char *, std::string>> &refs_;
};

struct DetectInfiniteLoop : public Ope::Visitor {
  DetectInfiniteLoop(const char *s, const std::string &name) {
    refs_.emplace_back(s, name);
  }

  void visit(Sequence &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
      if (has_error) { return; }
    }
  }
  void visit(PrioritizedChoice &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
      if (has_error) { return; }
    }
  }
  void visit(Repetition &ope) override {
    if (ope.max_ == std::numeric_limits<size_t>::max()) {
      HasEmptyElement vis(refs_);
      ope.ope_->accept(vis);
      if (vis.is_empty) {
        has_error = true;
        error_s = vis.error_s;
        error_name = vis.error_name;
      }
    } else {
      ope.ope_->accept(*this);
    }
  }
  void visit(AndPredicate &ope) override { ope.ope_->accept(*this); }
  void visit(NotPredicate &ope) override { ope.ope_->accept(*this); }
  void visit(CaptureScope &ope) override { ope.ope_->accept(*this); }
  void visit(Capture &ope) override { ope.ope_->accept(*this); }
  void visit(TokenBoundary &ope) override { ope.ope_->accept(*this); }
  void visit(Ignore &ope) override { ope.ope_->accept(*this); }
  void visit(WeakHolder &ope) override { ope.weak_.lock()->accept(*this); }
  void visit(Holder &ope) override { ope.ope_->accept(*this); }
  void visit(Reference &ope) override;
  void visit(Whitespace &ope) override { ope.ope_->accept(*this); }
  void visit(PrecedenceClimbing &ope) override { ope.atom_->accept(*this); }

  bool has_error = false;
  const char *error_s = nullptr;
  std::string error_name;

private:
  std::list<std::pair<const char *, std::string>> refs_;
};

struct ReferenceChecker : public Ope::Visitor {
  ReferenceChecker(const Grammar &grammar,
                   const std::vector<std::string> &params)
      : grammar_(grammar), params_(params) {}

  void visit(Sequence &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
    }
  }
  void visit(PrioritizedChoice &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
    }
  }
  void visit(Repetition &ope) override { ope.ope_->accept(*this); }
  void visit(AndPredicate &ope) override { ope.ope_->accept(*this); }
  void visit(NotPredicate &ope) override { ope.ope_->accept(*this); }
  void visit(CaptureScope &ope) override { ope.ope_->accept(*this); }
  void visit(Capture &ope) override { ope.ope_->accept(*this); }
  void visit(TokenBoundary &ope) override { ope.ope_->accept(*this); }
  void visit(Ignore &ope) override { ope.ope_->accept(*this); }
  void visit(WeakHolder &ope) override { ope.weak_.lock()->accept(*this); }
  void visit(Holder &ope) override { ope.ope_->accept(*this); }
  void visit(Reference &ope) override;
  void visit(Whitespace &ope) override { ope.ope_->accept(*this); }
  void visit(PrecedenceClimbing &ope) override { ope.atom_->accept(*this); }

  std::unordered_map<std::string, const char *> error_s;
  std::unordered_map<std::string, std::string> error_message;

private:
  const Grammar &grammar_;
  const std::vector<std::string> &params_;
};

struct LinkReferences : public Ope::Visitor {
  LinkReferences(Grammar &grammar, const std::vector<std::string> &params)
      : grammar_(grammar), params_(params) {}

  void visit(Sequence &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
    }
  }
  void visit(PrioritizedChoice &ope) override {
    for (auto op : ope.opes_) {
      op->accept(*this);
    }
  }
  void visit(Repetition &ope) override { ope.ope_->accept(*this); }
  void visit(AndPredicate &ope) override { ope.ope_->accept(*this); }
  void visit(NotPredicate &ope) override { ope.ope_->accept(*this); }
  void visit(CaptureScope &ope) override { ope.ope_->accept(*this); }
  void visit(Capture &ope) override { ope.ope_->accept(*this); }
  void visit(TokenBoundary &ope) override { ope.ope_->accept(*this); }
  void visit(Ignore &ope) override { ope.ope_->accept(*this); }
  void visit(WeakHolder &ope) override { ope.weak_.lock()->accept(*this); }
  void visit(Holder &ope) override { ope.ope_->accept(*this); }
  void visit(Reference &ope) override;
  void visit(Whitespace &ope) override { ope.ope_->accept(*this); }
  void visit(PrecedenceClimbing &ope) override { ope.atom_->accept(*this); }

private:
  Grammar &grammar_;
  const std::vector<std::string> &params_;
};

struct FindReference : public Ope::Visitor {
  FindReference(const std::vector<std::shared_ptr<Ope>> &args,
                const std::vector<std::string> &params)
      : args_(args), params_(params) {}

  void visit(Sequence &ope) override {
    std::vector<std::shared_ptr<Ope>> opes;
    for (auto o : ope.opes_) {
      o->accept(*this);
      opes.push_back(found_ope);
    }
    found_ope = std::make_shared<Sequence>(opes);
  }
  void visit(PrioritizedChoice &ope) override {
    std::vector<std::shared_ptr<Ope>> opes;
    for (auto o : ope.opes_) {
      o->accept(*this);
      opes.push_back(found_ope);
    }
    found_ope = std::make_shared<PrioritizedChoice>(opes);
  }
  void visit(Repetition &ope) override {
    ope.ope_->accept(*this);
    found_ope = rep(found_ope, ope.min_, ope.max_);
  }
  void visit(AndPredicate &ope) override {
    ope.ope_->accept(*this);
    found_ope = apd(found_ope);
  }
  void visit(NotPredicate &ope) override {
    ope.ope_->accept(*this);
    found_ope = npd(found_ope);
  }
  void visit(Dictionary &ope) override { found_ope = ope.shared_from_this(); }
  void visit(LiteralString &ope) override {
    found_ope = ope.shared_from_this();
  }
  void visit(CharacterClass &ope) override {
    found_ope = ope.shared_from_this();
  }
  void visit(Character &ope) override { found_ope = ope.shared_from_this(); }
  void visit(AnyCharacter &ope) override { found_ope = ope.shared_from_this(); }
  void visit(CaptureScope &ope) override {
    ope.ope_->accept(*this);
    found_ope = csc(found_ope);
  }
  void visit(Capture &ope) override {
    ope.ope_->accept(*this);
    found_ope = cap(found_ope, ope.match_action_);
  }
  void visit(TokenBoundary &ope) override {
    ope.ope_->accept(*this);
    found_ope = tok(found_ope);
  }
  void visit(Ignore &ope) override {
    ope.ope_->accept(*this);
    found_ope = ign(found_ope);
  }
  void visit(WeakHolder &ope) override { ope.weak_.lock()->accept(*this); }
  void visit(Holder &ope) override { ope.ope_->accept(*this); }
  void visit(Reference &ope) override;
  void visit(Whitespace &ope) override {
    ope.ope_->accept(*this);
    found_ope = wsp(found_ope);
  }
  void visit(PrecedenceClimbing &ope) override {
    ope.atom_->accept(*this);
    found_ope = csc(found_ope);
  }

  std::shared_ptr<Ope> found_ope;

private:
  const std::vector<std::shared_ptr<Ope>> &args_;
  const std::vector<std::string> &params_;
};

struct IsPrioritizedChoice : public Ope::Visitor {
  void visit(PrioritizedChoice & /*ope*/) override { result_ = true; }

  static bool check(Ope &ope) {
    IsPrioritizedChoice vis;
    ope.accept(vis);
    return vis.result_;
  }

private:
  bool result_ = false;
};

/*
 * Keywords
 */
static const char *WHITESPACE_DEFINITION_NAME = "%whitespace";
static const char *WORD_DEFINITION_NAME = "%word";

/*
 * Definition
 */
class Definition {
public:
  struct Result {
    bool ret;
    size_t len;
    const char *error_pos;
    const char *message_pos;
    const std::string message;
  };

  Definition() : holder_(std::make_shared<Holder>(this)) {}

  Definition(const Definition &rhs) : name(rhs.name), holder_(rhs.holder_) {
    holder_->outer_ = this;
  }

  Definition(const std::shared_ptr<Ope> &ope)
      : holder_(std::make_shared<Holder>(this)) {
    *this <= ope;
  }

  operator std::shared_ptr<Ope>() {
    return std::make_shared<WeakHolder>(holder_);
  }

  Definition &operator<=(const std::shared_ptr<Ope> &ope) {
    holder_->ope_ = ope;
    return *this;
  }

  Result parse(const char *s, size_t n, const char *path = nullptr) const {
    SemanticValues sv;
    any dt;
    return parse_core(s, n, sv, dt, path);
  }

  Result parse(const char *s, const char *path = nullptr) const {
    auto n = strlen(s);
    return parse(s, n, path);
  }

  Result parse(const char *s, size_t n, any &dt,
               const char *path = nullptr) const {
    SemanticValues sv;
    return parse_core(s, n, sv, dt, path);
  }

  Result parse(const char *s, any &dt, const char *path = nullptr) const {
    auto n = strlen(s);
    return parse(s, n, dt, path);
  }

  template <typename T>
  Result parse_and_get_value(const char *s, size_t n, T &val,
                             const char *path = nullptr) const {
    SemanticValues sv;
    any dt;
    auto r = parse_core(s, n, sv, dt, path);
    if (r.ret && !sv.empty() && sv.front().has_value()) {
      val = any_cast<T>(sv[0]);
    }
    return r;
  }

  template <typename T>
  Result parse_and_get_value(const char *s, T &val,
                             const char *path = nullptr) const {
    auto n = strlen(s);
    return parse_and_get_value(s, n, val, path);
  }

  template <typename T>
  Result parse_and_get_value(const char *s, size_t n, any &dt, T &val,
                             const char *path = nullptr) const {
    SemanticValues sv;
    auto r = parse_core(s, n, sv, dt, path);
    if (r.ret && !sv.empty() && sv.front().has_value()) {
      val = any_cast<T>(sv[0]);
    }
    return r;
  }

  template <typename T>
  Result parse_and_get_value(const char *s, any &dt, T &val,
                             const char *path = nullptr) const {
    auto n = strlen(s);
    return parse_and_get_value(s, n, dt, val, path);
  }

  Action operator=(Action a) {
    action = a;
    return a;
  }

  template <typename T> Definition &operator,(T fn) {
    operator=(fn);
    return *this;
  }

  Definition &operator~() {
    ignoreSemanticValue = true;
    return *this;
  }

  void accept(Ope::Visitor &v) { holder_->accept(v); }

  std::shared_ptr<Ope> get_core_operator() const { return holder_->ope_; }

  bool is_token() const {
    std::call_once(is_token_init_, [this]() {
      is_token_ = TokenChecker::is_token(*get_core_operator());
    });
    return is_token_;
  }

  std::string name;
  const char *s_ = nullptr;

  size_t id = 0;
  Action action;
  std::function<void(const char *s, size_t n, any &dt)> enter;
  std::function<void(const char *s, size_t n, size_t matchlen, any &value,
                     any &dt)>
      leave;
  std::function<std::string()> error_message;
  bool ignoreSemanticValue = false;
  std::shared_ptr<Ope> whitespaceOpe;
  std::shared_ptr<Ope> wordOpe;
  bool enablePackratParsing = false;
  bool is_macro = false;
  std::vector<std::string> params;
  TracerEnter tracer_enter;
  TracerLeave tracer_leave;
  bool disable_action = false;

private:
  friend class Reference;
  friend class ParserGenerator;

  Definition &operator=(const Definition &rhs);
  Definition &operator=(Definition &&rhs);

  void initialize_definition_ids() const {
    std::call_once(definition_ids_init_, [&]() {
      AssignIDToDefinition vis;
      holder_->accept(vis);
      if (whitespaceOpe) { whitespaceOpe->accept(vis); }
      if (wordOpe) { wordOpe->accept(vis); }
      definition_ids_.swap(vis.ids);
    });
  }

  Result parse_core(const char *s, size_t n, SemanticValues &sv, any &dt,
                    const char *path) const {
    initialize_definition_ids();

    std::shared_ptr<Ope> ope = holder_;
    if (whitespaceOpe) { ope = std::make_shared<Sequence>(whitespaceOpe, ope); }

    Context cxt(path, s, n, definition_ids_.size(), whitespaceOpe, wordOpe,
                enablePackratParsing, tracer_enter, tracer_leave);

    auto len = ope->parse(s, n, sv, cxt, dt);
    return Result{success(len), len, cxt.error_pos, cxt.message_pos,
                  cxt.message};
  }

  std::shared_ptr<Holder> holder_;
  mutable std::once_flag is_token_init_;
  mutable bool is_token_ = false;
  mutable std::once_flag assign_id_to_definition_init_;
  mutable std::once_flag definition_ids_init_;
  mutable std::unordered_map<void *, size_t> definition_ids_;
};

/*
 * Implementations
 */

inline size_t parse_literal(const char *s, size_t n, SemanticValues &sv,
                            Context &c, any &dt, const std::string &lit,
                            bool &init_is_word, bool &is_word,
                            bool ignore_case) {
  size_t i = 0;
  for (; i < lit.size(); i++) {
    if (i >= n || (ignore_case ? (std::tolower(s[i]) != std::tolower(lit[i]))
                               : (s[i] != lit[i]))) {
      c.set_error_pos(s);
      return static_cast<size_t>(-1);
    }
  }

  // Word check
  static Context dummy_c(nullptr, c.s, c.l, 0, nullptr, nullptr, false, nullptr,
                         nullptr);
  static SemanticValues dummy_sv;
  static any dummy_dt;

  if (!init_is_word) { // TODO: Protect with mutex
    if (c.wordOpe) {
      auto len =
          c.wordOpe->parse(lit.data(), lit.size(), dummy_sv, dummy_c, dummy_dt);
      is_word = success(len);
    }
    init_is_word = true;
  }

  if (is_word) {
    auto ope = std::make_shared<NotPredicate>(c.wordOpe);
    auto len = ope->parse(s + i, n - i, dummy_sv, dummy_c, dummy_dt);
    if (fail(len)) { return static_cast<size_t>(-1); }
    i += len;
  }

  // Skip whiltespace
  if (!c.in_token) {
    if (c.whitespaceOpe) {
      auto len = c.whitespaceOpe->parse(s + i, n - i, sv, c, dt);
      if (fail(len)) { return static_cast<size_t>(-1); }
      i += len;
    }
  }

  return i;
}

inline void Context::trace_enter(const char *name, const char *a_s, size_t n,
                                 SemanticValues &sv, any &dt) const {
  trace_ids.push_back(next_trace_id++);
  tracer_enter(name, a_s, n, sv, *this, dt);
}

inline void Context::trace_leave(const char *name, const char *a_s, size_t n,
                                 SemanticValues &sv, any &dt,
                                 size_t len) const {
  tracer_leave(name, a_s, n, sv, *this, dt, len);
  trace_ids.pop_back();
}

inline bool Context::is_traceable(const Ope &ope) const {
  if (tracer_enter && tracer_leave) {
    IsReference vis;
    const_cast<Ope &>(ope).accept(vis);
    return !vis.is_reference;
  }
  return false;
}

inline size_t Ope::parse(const char *s, size_t n, SemanticValues &sv,
                         Context &c, any &dt) const {
  if (c.is_traceable(*this)) {
    TraceOpeName vis;
    const_cast<Ope &>(*this).accept(vis);
    c.trace_enter(vis.name, s, n, sv, dt);
    auto len = parse_core(s, n, sv, c, dt);
    c.trace_leave(vis.name, s, n, sv, dt, len);
    return len;
  }
  return parse_core(s, n, sv, c, dt);
}

inline size_t Dictionary::parse_core(const char *s, size_t n,
                                     SemanticValues & /*sv*/, Context &c,
                                     any & /*dt*/) const {
  auto len = trie_.match(s, n);
  if (len > 0) { return len; }
  c.set_error_pos(s);
  return static_cast<size_t>(-1);
}

inline size_t LiteralString::parse_core(const char *s, size_t n,
                                        SemanticValues &sv, Context &c,
                                        any &dt) const {
  return parse_literal(s, n, sv, c, dt, lit_, init_is_word_, is_word_,
                       ignore_case_);
}

inline size_t TokenBoundary::parse_core(const char *s, size_t n,
                                        SemanticValues &sv, Context &c,
                                        any &dt) const {
  c.in_token = true;
  auto se = make_scope_exit([&]() { c.in_token = false; });
  const auto &rule = *ope_;
  auto len = rule.parse(s, n, sv, c, dt);
  if (success(len)) {
    sv.tokens.push_back(std::make_pair(s, len));

    if (c.whitespaceOpe) {
      auto l = c.whitespaceOpe->parse(s + len, n - len, sv, c, dt);
      if (fail(l)) { return static_cast<size_t>(-1); }
      len += l;
    }
  }
  return len;
}

inline size_t Holder::parse_core(const char *s, size_t n, SemanticValues &sv,
                                 Context &c, any &dt) const {
  if (!ope_) {
    throw std::logic_error("Uninitialized definition ope was used...");
  }

  // Macro reference
  // TODO: need packrat support
  if (outer_->is_macro) { return ope_->parse(s, n, sv, c, dt); }

  size_t len;
  any val;

  c.packrat(s, outer_->id, len, val, [&](any &a_val) {
    if (outer_->enter) { outer_->enter(s, n, dt); }

    auto se2 = make_scope_exit([&]() {
      c.pop();

      if (outer_->leave) { outer_->leave(s, n, len, a_val, dt); }
    });

    auto &chldsv = c.push();

    len = ope_->parse(s, n, chldsv, c, dt);

    // Invoke action
    if (success(len)) {
      chldsv.s_ = s;
      chldsv.n_ = len;
      chldsv.name_ = outer_->name;

      if (!IsPrioritizedChoice::check(*ope_)) {
        chldsv.choice_count_ = 0;
        chldsv.choice_ = 0;
      }

      try {
        a_val = reduce(chldsv, dt);
      } catch (const parse_error &e) {
        if (e.what()) {
          if (c.message_pos < s) {
            c.message_pos = s;
            c.message = e.what();
          }
        }
        len = static_cast<size_t>(-1);
      }
    }
  });

  if (success(len)) {
    if (!outer_->ignoreSemanticValue) {
      sv.emplace_back(val);
      sv.tags.emplace_back(str2tag(outer_->name.c_str()));
    }
  } else {
    if (outer_->error_message) {
      if (c.message_pos < s) {
        c.message_pos = s;
        c.message = outer_->error_message();
      }
    }
  }

  return len;
}

inline any Holder::reduce(SemanticValues &sv, any &dt) const {
  if (outer_->action && !outer_->disable_action) {
    return outer_->action(sv, dt);
  } else if (sv.empty()) {
    return any();
  } else {
    return std::move(sv.front());
  }
}

inline const char *Holder::trace_name() const {
  if (trace_name_.empty()) { trace_name_ = "[" + outer_->name + "]"; }
  return trace_name_.c_str();
}

inline size_t Reference::parse_core(const char *s, size_t n, SemanticValues &sv,
                                    Context &c, any &dt) const {
  if (rule_) {
    // Reference rule
    if (rule_->is_macro) {
      // Macro
      FindReference vis(c.top_args(), rule_->params);

      // Collect arguments
      std::vector<std::shared_ptr<Ope>> args;
      for (auto arg : args_) {
        arg->accept(vis);
        args.push_back(vis.found_ope);
      }

      c.push_args(std::move(args));
      auto se = make_scope_exit([&]() { c.pop_args(); });
      auto ope = get_core_operator();
      return ope->parse(s, n, sv, c, dt);
    } else {
      // Definition
      auto ope = get_core_operator();
      return ope->parse(s, n, sv, c, dt);
    }
  } else {
    // Reference parameter in macro
    const auto &args = c.top_args();
    return args[iarg_]->parse(s, n, sv, c, dt);
  }
}

inline std::shared_ptr<Ope> Reference::get_core_operator() const {
  return rule_->holder_;
}

inline size_t BackReference::parse_core(const char *s, size_t n,
                                        SemanticValues &sv, Context &c,
                                        any &dt) const {
  auto size = static_cast<int>(c.capture_scope_stack_size);
  for (auto i = size - 1; i >= 0; i--) {
    auto index = static_cast<size_t>(i);
    const auto &cs = c.capture_scope_stack[index];
    if (cs.find(name_) != cs.end()) {
      const auto &lit = cs.at(name_);
      auto init_is_word = false;
      auto is_word = false;
      return parse_literal(s, n, sv, c, dt, lit, init_is_word, is_word, false);
    }
  }
  throw std::runtime_error("Invalid back reference...");
}

inline Definition &
PrecedenceClimbing::get_reference_for_binop(Context &c) const {
  if (rule_.is_macro) {
    // Reference parameter in macro
    const auto &args = c.top_args();
    auto iarg = dynamic_cast<Reference &>(*binop_).iarg_;
    auto arg = args[iarg];
    return *dynamic_cast<Reference &>(*arg).rule_;
  }

  return *dynamic_cast<Reference &>(*binop_).rule_;
}

inline size_t PrecedenceClimbing::parse_expression(const char *s, size_t n,
                                                   SemanticValues &sv,
                                                   Context &c, any &dt,
                                                   size_t min_prec) const {
  auto len = atom_->parse(s, n, sv, c, dt);
  if (fail(len)) { return len; }

  std::string tok;
  auto &rule = get_reference_for_binop(c);
  auto action = rule.action;

  rule.action = [&](SemanticValues &sv2, any &dt2) -> any {
    tok = sv2.token();
    if (action) {
      return action(sv2, dt2);
    } else if (!sv2.empty()) {
      return sv2[0];
    }
    return any();
  };
  auto action_se = make_scope_exit([&]() { rule.action = action; });

  auto save_error_pos = c.error_pos;

  auto i = len;
  while (i < n) {
    std::vector<any> save_values(sv.begin(), sv.end());
    auto save_tokens = sv.tokens;

    auto chv = c.push();
    auto chl = binop_->parse(s + i, n - i, chv, c, dt);
    c.pop();

    if (fail(chl)) {
      c.error_pos = save_error_pos;
      break;
    }

    auto it = info_.find(tok);
    if (it == info_.end()) { break; }

    auto level = std::get<0>(it->second);
    auto assoc = std::get<1>(it->second);

    if (level < min_prec) { break; }

    sv.emplace_back(std::move(chv[0]));
    i += chl;

    auto next_min_prec = level;
    if (assoc == 'L') { next_min_prec = level + 1; }

    chv = c.push();
    chl = parse_expression(s + i, n - i, chv, c, dt, next_min_prec);
    c.pop();

    if (fail(chl)) {
      sv.assign(save_values.begin(), save_values.end());
      sv.tokens = save_tokens;
      c.error_pos = save_error_pos;
      break;
    }

    sv.emplace_back(std::move(chv[0]));
    i += chl;

    any val;
    if (rule_.action) {
      sv.s_ = s;
      sv.n_ = i;
      val = rule_.action(sv, dt);
    } else if (!sv.empty()) {
      val = sv[0];
    }
    sv.clear();
    sv.emplace_back(std::move(val));
  }

  return i;
}

inline void Sequence::accept(Visitor &v) { v.visit(*this); }
inline void PrioritizedChoice::accept(Visitor &v) { v.visit(*this); }
inline void Repetition::accept(Visitor &v) { v.visit(*this); }
inline void AndPredicate::accept(Visitor &v) { v.visit(*this); }
inline void NotPredicate::accept(Visitor &v) { v.visit(*this); }
inline void Dictionary::accept(Visitor &v) { v.visit(*this); }
inline void LiteralString::accept(Visitor &v) { v.visit(*this); }
inline void CharacterClass::accept(Visitor &v) { v.visit(*this); }
inline void Character::accept(Visitor &v) { v.visit(*this); }
inline void AnyCharacter::accept(Visitor &v) { v.visit(*this); }
inline void CaptureScope::accept(Visitor &v) { v.visit(*this); }
inline void Capture::accept(Visitor &v) { v.visit(*this); }
inline void TokenBoundary::accept(Visitor &v) { v.visit(*this); }
inline void Ignore::accept(Visitor &v) { v.visit(*this); }
inline void User::accept(Visitor &v) { v.visit(*this); }
inline void WeakHolder::accept(Visitor &v) { v.visit(*this); }
inline void Holder::accept(Visitor &v) { v.visit(*this); }
inline void Reference::accept(Visitor &v) { v.visit(*this); }
inline void Whitespace::accept(Visitor &v) { v.visit(*this); }
inline void BackReference::accept(Visitor &v) { v.visit(*this); }
inline void PrecedenceClimbing::accept(Visitor &v) { v.visit(*this); }

inline void AssignIDToDefinition::visit(Holder &ope) {
  auto p = static_cast<void *>(ope.outer_);
  if (ids.count(p)) { return; }
  auto id = ids.size();
  ids[p] = id;
  ope.outer_->id = id;
  ope.ope_->accept(*this);
}

inline void AssignIDToDefinition::visit(Reference &ope) {
  if (ope.rule_) {
    for (auto arg : ope.args_) {
      arg->accept(*this);
    }
    ope.rule_->accept(*this);
  }
}

inline void TokenChecker::visit(Reference &ope) {
  if (ope.is_macro_) {
    ope.rule_->accept(*this);
    for (auto arg : ope.args_) {
      arg->accept(*this);
    }
  } else {
    has_rule_ = true;
  }
}

inline void DetectLeftRecursion::visit(Reference &ope) {
  if (ope.name_ == name_) {
    error_s = ope.s_;
  } else if (!refs_.count(ope.name_)) {
    refs_.insert(ope.name_);
    if (ope.rule_) {
      ope.rule_->accept(*this);
      if (done_ == false) { return; }
    }
  }
  done_ = true;
}

inline void HasEmptyElement::visit(Reference &ope) {
  auto it = std::find_if(refs_.begin(), refs_.end(),
                         [&](const std::pair<const char *, std::string> &ref) {
                           return ope.name_ == ref.second;
                         });
  if (it != refs_.end()) { return; }

  if (ope.rule_) {
    refs_.emplace_back(ope.s_, ope.name_);
    ope.rule_->accept(*this);
    refs_.pop_back();
  }
}

inline void DetectInfiniteLoop::visit(Reference &ope) {
  auto it = std::find_if(refs_.begin(), refs_.end(),
                         [&](const std::pair<const char *, std::string> &ref) {
                           return ope.name_ == ref.second;
                         });
  if (it != refs_.end()) { return; }

  if (ope.rule_) {
    refs_.emplace_back(ope.s_, ope.name_);
    ope.rule_->accept(*this);
    refs_.pop_back();
  }
}

inline void ReferenceChecker::visit(Reference &ope) {
  auto it = std::find(params_.begin(), params_.end(), ope.name_);
  if (it != params_.end()) { return; }

  if (!grammar_.count(ope.name_)) {
    error_s[ope.name_] = ope.s_;
    error_message[ope.name_] = "'" + ope.name_ + "' is not defined.";
  } else {
    const auto &rule = grammar_.at(ope.name_);
    if (rule.is_macro) {
      if (!ope.is_macro_ || ope.args_.size() != rule.params.size()) {
        error_s[ope.name_] = ope.s_;
        error_message[ope.name_] = "incorrect number of arguments.";
      }
    } else if (ope.is_macro_) {
      error_s[ope.name_] = ope.s_;
      error_message[ope.name_] = "'" + ope.name_ + "' is not macro.";
    }
  }
}

inline void LinkReferences::visit(Reference &ope) {
  // Check if the reference is a macro parameter
  auto found_param = false;
  for (size_t i = 0; i < params_.size(); i++) {
    const auto &param = params_[i];
    if (param == ope.name_) {
      ope.iarg_ = i;
      found_param = true;
      break;
    }
  }

  // Check if the reference is a definition rule
  if (!found_param && grammar_.count(ope.name_)) {
    auto &rule = grammar_.at(ope.name_);
    ope.rule_ = &rule;
  }

  for (auto arg : ope.args_) {
    arg->accept(*this);
  }
}

inline void FindReference::visit(Reference &ope) {
  for (size_t i = 0; i < args_.size(); i++) {
    const auto &name = params_[i];
    if (name == ope.name_) {
      found_ope = args_[i];
      return;
    }
  }
  found_ope = ope.shared_from_this();
}

/*-----------------------------------------------------------------------------
 *  PEG parser generator
 *---------------------------------------------------------------------------*/

typedef std::unordered_map<std::string, std::shared_ptr<Ope>> Rules;
typedef std::function<void(size_t, size_t, const std::string &)> Log;

class ParserGenerator {
public:
  static std::shared_ptr<Grammar> parse(const char *s, size_t n,
                                        const Rules &rules, std::string &start,
                                        Log log) {
    return get_instance().perform_core(s, n, rules, start, log);
  }

  static std::shared_ptr<Grammar> parse(const char *s, size_t n,
                                        std::string &start, Log log) {
    Rules dummy;
    return parse(s, n, dummy, start, log);
  }

  // For debuging purpose
  static Grammar &grammar() { return get_instance().g; }

private:
  static ParserGenerator &get_instance() {
    static ParserGenerator instance;
    return instance;
  }

  ParserGenerator() {
    make_grammar();
    setup_actions();
  }

  struct Instruction {
    std::string type;
    any data;
  };

  struct Data {
    std::shared_ptr<Grammar> grammar;
    std::string start;
    const char *start_pos = nullptr;
    std::vector<std::pair<std::string, const char *>> duplicates;
    std::map<std::string, Instruction> instructions;

    Data() : grammar(std::make_shared<Grammar>()) {}
  };

  void make_grammar() {
    // Setup PEG syntax parser
    g["Grammar"] <= seq(g["Spacing"], oom(g["Definition"]), g["EndOfFile"]);
    g["Definition"] <=
        cho(seq(g["Ignore"], g["IdentCont"], g["Parameters"], g["LEFTARROW"],
                g["Expression"], opt(g["Instruction"])),
            seq(g["Ignore"], g["Identifier"], g["LEFTARROW"], g["Expression"],
                opt(g["Instruction"])));
    g["Expression"] <= seq(g["Sequence"], zom(seq(g["SLASH"], g["Sequence"])));
    g["Sequence"] <= zom(g["Prefix"]);
    g["Prefix"] <= seq(opt(cho(g["AND"], g["NOT"])), g["Suffix"]);
    g["Suffix"] <= seq(g["Primary"], opt(g["Loop"]));
    g["Loop"] <= cho(g["QUESTION"], g["STAR"], g["PLUS"], g["Repetition"]);
    g["Primary"] <=
        cho(seq(g["Ignore"], g["IdentCont"], g["Arguments"],
                npd(g["LEFTARROW"])),
            seq(g["Ignore"], g["Identifier"],
                npd(seq(opt(g["Parameters"]), g["LEFTARROW"]))),
            seq(g["OPEN"], g["Expression"], g["CLOSE"]),
            seq(g["BeginTok"], g["Expression"], g["EndTok"]),
            seq(g["BeginCapScope"], g["Expression"], g["EndCapScope"]),
            seq(g["BeginCap"], g["Expression"], g["EndCap"]), g["BackRef"],
            g["LiteralI"], g["Dictionary"], g["Literal"], g["NegatedClass"],
            g["Class"], g["DOT"]);

    g["Identifier"] <= seq(g["IdentCont"], g["Spacing"]);
    g["IdentCont"] <= seq(g["IdentStart"], zom(g["IdentRest"]));

    const static std::vector<std::pair<char32_t, char32_t>> range = {
        {0x0080, 0xFFFF}};
    g["IdentStart"] <= cho(cls("a-zA-Z_%"), cls(range));

    g["IdentRest"] <= cho(g["IdentStart"], cls("0-9"));

    g["Dictionary"] <= seq(g["LiteralD"], oom(seq(g["PIPE"], g["LiteralD"])));

    auto lit_ope = cho(seq(cls("'"), tok(zom(seq(npd(cls("'")), g["Char"]))),
                           cls("'"), g["Spacing"]),
                       seq(cls("\""), tok(zom(seq(npd(cls("\"")), g["Char"]))),
                           cls("\""), g["Spacing"]));
    g["Literal"] <= lit_ope;
    g["LiteralD"] <= lit_ope;

    g["LiteralI"] <=
        cho(seq(cls("'"), tok(zom(seq(npd(cls("'")), g["Char"]))), lit("'i"),
                g["Spacing"]),
            seq(cls("\""), tok(zom(seq(npd(cls("\"")), g["Char"]))), lit("\"i"),
                g["Spacing"]));

    // NOTE: The original Brian Ford's paper uses 'zom' instead of 'oom'.
    g["Class"] <= seq(chr('['), npd(chr('^')),
                      tok(oom(seq(npd(chr(']')), g["Range"]))), chr(']'),
                      g["Spacing"]);
    g["NegatedClass"] <= seq(lit("[^"),
                             tok(oom(seq(npd(chr(']')), g["Range"]))), chr(']'),
                             g["Spacing"]);

    g["Range"] <= cho(seq(g["Char"], chr('-'), g["Char"]), g["Char"]);
    g["Char"] <= cho(seq(chr('\\'), cls("nrt'\"[]\\^")),
                     seq(chr('\\'), cls("0-3"), cls("0-7"), cls("0-7")),
                     seq(chr('\\'), cls("0-7"), opt(cls("0-7"))),
                     seq(lit("\\x"), cls("0-9a-fA-F"), opt(cls("0-9a-fA-F"))),
                     seq(lit("\\u"), cls("0-9a-fA-F"), cls("0-9a-fA-F"),
                         cls("0-9a-fA-F"), cls("0-9a-fA-F")),
                     seq(npd(chr('\\')), dot()));

    g["Repetition"] <=
        seq(g["BeginBlacket"], g["RepetitionRange"], g["EndBlacket"]);
    g["RepetitionRange"] <= cho(seq(g["Number"], g["COMMA"], g["Number"]),
                                seq(g["Number"], g["COMMA"]), g["Number"],
                                seq(g["COMMA"], g["Number"]));
    g["Number"] <= seq(oom(cls("0-9")), g["Spacing"]);

    g["LEFTARROW"] <=
        seq(cho(lit("<-"), lit(reinterpret_cast<const char *>(u8"←"))),
            g["Spacing"]);
    ~g["SLASH"] <= seq(chr('/'), g["Spacing"]);
    ~g["PIPE"] <= seq(chr('|'), g["Spacing"]);
    g["AND"] <= seq(chr('&'), g["Spacing"]);
    g["NOT"] <= seq(chr('!'), g["Spacing"]);
    g["QUESTION"] <= seq(chr('?'), g["Spacing"]);
    g["STAR"] <= seq(chr('*'), g["Spacing"]);
    g["PLUS"] <= seq(chr('+'), g["Spacing"]);
    ~g["OPEN"] <= seq(chr('('), g["Spacing"]);
    ~g["CLOSE"] <= seq(chr(')'), g["Spacing"]);
    g["DOT"] <= seq(chr('.'), g["Spacing"]);

    ~g["Spacing"] <= zom(cho(g["Space"], g["Comment"]));
    g["Comment"] <=
        seq(chr('#'), zom(seq(npd(g["EndOfLine"]), dot())), g["EndOfLine"]);
    g["Space"] <= cho(chr(' '), chr('\t'), g["EndOfLine"]);
    g["EndOfLine"] <= cho(lit("\r\n"), chr('\n'), chr('\r'));
    g["EndOfFile"] <= npd(dot());

    ~g["BeginTok"] <= seq(chr('<'), g["Spacing"]);
    ~g["EndTok"] <= seq(chr('>'), g["Spacing"]);

    ~g["BeginCapScope"] <= seq(chr('$'), chr('('), g["Spacing"]);
    ~g["EndCapScope"] <= seq(chr(')'), g["Spacing"]);

    g["BeginCap"] <= seq(chr('$'), tok(g["IdentCont"]), chr('<'), g["Spacing"]);
    ~g["EndCap"] <= seq(chr('>'), g["Spacing"]);

    g["BackRef"] <= seq(chr('$'), tok(g["IdentCont"]), g["Spacing"]);

    g["IGNORE"] <= chr('~');

    g["Ignore"] <= opt(g["IGNORE"]);
    g["Parameters"] <= seq(g["OPEN"], g["Identifier"],
                           zom(seq(g["COMMA"], g["Identifier"])), g["CLOSE"]);
    g["Arguments"] <= seq(g["OPEN"], g["Expression"],
                          zom(seq(g["COMMA"], g["Expression"])), g["CLOSE"]);
    ~g["COMMA"] <= seq(chr(','), g["Spacing"]);

    // Instruction grammars
    g["Instruction"] <=
        seq(g["BeginBlacket"], cho(g["PrecedenceClimbing"]), g["EndBlacket"]);

    ~g["SpacesZom"] <= zom(g["Space"]);
    ~g["SpacesOom"] <= oom(g["Space"]);
    ~g["BeginBlacket"] <= seq(chr('{'), g["Spacing"]);
    ~g["EndBlacket"] <= seq(chr('}'), g["Spacing"]);

    // PrecedenceClimbing instruction
    g["PrecedenceClimbing"] <=
        seq(lit("precedence"), g["SpacesZom"], g["PrecedenceInfo"],
            zom(seq(g["SpacesOom"], g["PrecedenceInfo"])), g["SpacesZom"]);
    g["PrecedenceInfo"] <=
        seq(g["PrecedenceAssoc"],
            oom(seq(ign(g["SpacesOom"]), g["PrecedenceOpe"])));
    g["PrecedenceOpe"] <=
        tok(oom(
            seq(npd(cho(g["PrecedenceAssoc"], g["Space"], chr('}'))), dot())));
    g["PrecedenceAssoc"] <= cls("LR");

    // Set definition names
    for (auto &x : g) {
      x.second.name = x.first;
    }
  }

  void setup_actions() {
    g["Definition"] = [&](const SemanticValues &sv, any &dt) {
      Data &data = *any_cast<Data *>(dt);

      auto is_macro = sv.choice() == 0;
      auto ignore = any_cast<bool>(sv[0]);
      auto name = any_cast<std::string>(sv[1]);

      std::vector<std::string> params;
      std::shared_ptr<Ope> ope;
      if (is_macro) {
        params = any_cast<std::vector<std::string>>(sv[2]);
        ope = any_cast<std::shared_ptr<Ope>>(sv[4]);
        if (sv.size() == 6) {
          data.instructions[name] = any_cast<Instruction>(sv[5]);
        }
      } else {
        ope = any_cast<std::shared_ptr<Ope>>(sv[3]);
        if (sv.size() == 5) {
          data.instructions[name] = any_cast<Instruction>(sv[4]);
        }
      }

      auto &grammar = *data.grammar;
      if (!grammar.count(name)) {
        auto &rule = grammar[name];
        rule <= ope;
        rule.name = name;
        rule.s_ = sv.c_str();
        rule.ignoreSemanticValue = ignore;
        rule.is_macro = is_macro;
        rule.params = params;

        if (data.start.empty()) {
          data.start = name;
          data.start_pos = sv.c_str();
        }
      } else {
        data.duplicates.emplace_back(name, sv.c_str());
      }
    };

    g["Expression"] = [&](const SemanticValues &sv) {
      if (sv.size() == 1) {
        return any_cast<std::shared_ptr<Ope>>(sv[0]);
      } else {
        std::vector<std::shared_ptr<Ope>> opes;
        for (auto i = 0u; i < sv.size(); i++) {
          opes.emplace_back(any_cast<std::shared_ptr<Ope>>(sv[i]));
        }
        const std::shared_ptr<Ope> ope =
            std::make_shared<PrioritizedChoice>(opes);
        return ope;
      }
    };

    g["Sequence"] = [&](const SemanticValues &sv) {
      if (sv.size() == 1) {
        return any_cast<std::shared_ptr<Ope>>(sv[0]);
      } else {
        std::vector<std::shared_ptr<Ope>> opes;
        for (const auto &x : sv) {
          opes.emplace_back(any_cast<std::shared_ptr<Ope>>(x));
        }
        const std::shared_ptr<Ope> ope = std::make_shared<Sequence>(opes);
        return ope;
      }
    };

    g["Prefix"] = [&](const SemanticValues &sv) {
      std::shared_ptr<Ope> ope;
      if (sv.size() == 1) {
        ope = any_cast<std::shared_ptr<Ope>>(sv[0]);
      } else {
        assert(sv.size() == 2);
        auto tok = any_cast<char>(sv[0]);
        ope = any_cast<std::shared_ptr<Ope>>(sv[1]);
        if (tok == '&') {
          ope = apd(ope);
        } else { // '!'
          ope = npd(ope);
        }
      }
      return ope;
    };

    struct Loop {
      enum class Type { opt = 0, zom, oom, rep };
      Type type;
      std::pair<size_t, size_t> range;
    };

    g["Suffix"] = [&](const SemanticValues &sv) {
      auto ope = any_cast<std::shared_ptr<Ope>>(sv[0]);
      if (sv.size() == 1) {
        return ope;
      } else {
        assert(sv.size() == 2);
        auto loop = any_cast<Loop>(sv[1]);
        switch (loop.type) {
        case Loop::Type::opt: return opt(ope);
        case Loop::Type::zom: return zom(ope);
        case Loop::Type::oom: return oom(ope);
        case Loop::Type::rep: // Regex-like repetition
          return rep(ope, loop.range.first, loop.range.second);
        }
      }
    };

    g["Loop"] = [&](const SemanticValues &sv) {
      switch (sv.choice()) {
      case 0: // Option
        return Loop{Loop::Type::opt, std::pair<size_t, size_t>()};
      case 1: // Zero or More
        return Loop{Loop::Type::zom, std::pair<size_t, size_t>()};
      case 2: // One or More
        return Loop{Loop::Type::oom, std::pair<size_t, size_t>()};
      default: // Regex-like repetition
        return Loop{Loop::Type::rep,
                    any_cast<std::pair<size_t, size_t>>(sv[0])};
      }
    };

    g["RepetitionRange"] = [&](const SemanticValues &sv) {
      switch (sv.choice()) {
      case 0: { // Number COMMA Number
        auto min = any_cast<size_t>(sv[0]);
        auto max = any_cast<size_t>(sv[1]);
        return std::make_pair(min, max);
      }
      case 1: // Number COMMA
        return std::make_pair(any_cast<size_t>(sv[0]),
                              std::numeric_limits<size_t>::max());
      case 2: { // Number
        auto n = any_cast<size_t>(sv[0]);
        return std::make_pair(n, n);
      }
      default: // COMMA Number
        return std::make_pair(std::numeric_limits<size_t>::min(),
                              any_cast<size_t>(sv[0]));
      }
    };
    g["Number"] = [&](const SemanticValues &sv) {
      std::stringstream ss(sv.str());
      size_t n;
      ss >> n;
      return n;
    };

    g["Primary"] = [&](const SemanticValues &sv, any &dt) {
      Data &data = *any_cast<Data *>(dt);

      switch (sv.choice()) {
      case 0:   // Macro Reference
      case 1: { // Reference
        auto is_macro = sv.choice() == 0;
        auto ignore = any_cast<bool>(sv[0]);
        const auto &ident = any_cast<std::string>(sv[1]);

        std::vector<std::shared_ptr<Ope>> args;
        if (is_macro) {
          args = any_cast<std::vector<std::shared_ptr<Ope>>>(sv[2]);
        }

        std::shared_ptr<Ope> ope =
            ref(*data.grammar, ident, sv.c_str(), is_macro, args);

        if (ignore) {
          return ign(ope);
        } else {
          return ope;
        }
      }
      case 2: { // (Expression)
        return any_cast<std::shared_ptr<Ope>>(sv[0]);
      }
      case 3: { // TokenBoundary
        return tok(any_cast<std::shared_ptr<Ope>>(sv[0]));
      }
      case 4: { // CaptureScope
        return csc(any_cast<std::shared_ptr<Ope>>(sv[0]));
      }
      case 5: { // Capture
        const auto &name = any_cast<std::string>(sv[0]);
        auto ope = any_cast<std::shared_ptr<Ope>>(sv[1]);
        return cap(ope, [name](const char *a_s, size_t a_n, Context &c) {
          auto &cs = c.capture_scope_stack[c.capture_scope_stack_size - 1];
          cs[name] = std::string(a_s, a_n);
        });
      }
      default: {
        return any_cast<std::shared_ptr<Ope>>(sv[0]);
      }
      }
    };

    g["IdentCont"] = [](const SemanticValues &sv) {
      return std::string(sv.c_str(), sv.length());
    };

    g["Dictionary"] = [](const SemanticValues &sv) {
      auto items = sv.transform<std::string>();
      return dic(items);
    };

    g["Literal"] = [](const SemanticValues &sv) {
      const auto &tok = sv.tokens.front();
      return lit(resolve_escape_sequence(tok.first, tok.second));
    };
    g["LiteralI"] = [](const SemanticValues &sv) {
      const auto &tok = sv.tokens.front();
      return liti(resolve_escape_sequence(tok.first, tok.second));
    };
    g["LiteralD"] = [](const SemanticValues &sv) {
      auto &tok = sv.tokens.front();
      return resolve_escape_sequence(tok.first, tok.second);
    };

    g["Class"] = [](const SemanticValues &sv) {
      auto ranges = sv.transform<std::pair<char32_t, char32_t>>();
      return cls(ranges);
    };
    g["NegatedClass"] = [](const SemanticValues &sv) {
      auto ranges = sv.transform<std::pair<char32_t, char32_t>>();
      return ncls(ranges);
    };
    g["Range"] = [](const SemanticValues &sv) {
      switch (sv.choice()) {
      case 0: {
        auto s1 = any_cast<std::string>(sv[0]);
        auto s2 = any_cast<std::string>(sv[1]);
        auto cp1 = decode_codepoint(s1.c_str(), s1.length());
        auto cp2 = decode_codepoint(s2.c_str(), s2.length());
        return std::make_pair(cp1, cp2);
      }
      case 1: {
        auto s = any_cast<std::string>(sv[0]);
        auto cp = decode_codepoint(s.c_str(), s.length());
        return std::make_pair(cp, cp);
      }
      }
      return std::make_pair<char32_t, char32_t>(0, 0);
    };
    g["Char"] = [](const SemanticValues &sv) {
      return resolve_escape_sequence(sv.c_str(), sv.length());
    };

    g["AND"] = [](const SemanticValues &sv) { return *sv.c_str(); };
    g["NOT"] = [](const SemanticValues &sv) { return *sv.c_str(); };
    g["QUESTION"] = [](const SemanticValues &sv) { return *sv.c_str(); };
    g["STAR"] = [](const SemanticValues &sv) { return *sv.c_str(); };
    g["PLUS"] = [](const SemanticValues &sv) { return *sv.c_str(); };

    g["DOT"] = [](const SemanticValues & /*sv*/) { return dot(); };

    g["BeginCap"] = [](const SemanticValues &sv) { return sv.token(); };

    g["BackRef"] = [&](const SemanticValues &sv) { return bkr(sv.token()); };

    g["Ignore"] = [](const SemanticValues &sv) { return sv.size() > 0; };

    g["Parameters"] = [](const SemanticValues &sv) {
      return sv.transform<std::string>();
    };

    g["Arguments"] = [](const SemanticValues &sv) {
      return sv.transform<std::shared_ptr<Ope>>();
    };

    g["PrecedenceClimbing"] = [](const SemanticValues &sv) {
      PrecedenceClimbing::BinOpeInfo binOpeInfo;
      size_t level = 1;
      for (auto v : sv) {
        auto tokens = any_cast<std::vector<std::string>>(v);
        auto assoc = tokens[0][0];
        for (size_t i = 1; i < tokens.size(); i++) {
          const auto &tok = tokens[i];
          binOpeInfo[tok] = std::make_pair(level, assoc);
        }
        level++;
      }
      Instruction instruction;
      instruction.type = "precedence";
      instruction.data = binOpeInfo;
      return instruction;
    };
    g["PrecedenceInfo"] = [](const SemanticValues &sv) {
      return sv.transform<std::string>();
    };
    g["PrecedenceOpe"] = [](const SemanticValues &sv) { return sv.token(); };
    g["PrecedenceAssoc"] = [](const SemanticValues &sv) { return sv.token(); };
  }

  bool apply_precedence_instruction(Definition &rule,
                                    const PrecedenceClimbing::BinOpeInfo &info,
                                    const char *s, Log log) {
    try {
      auto &seq = dynamic_cast<Sequence &>(*rule.get_core_operator());
      auto atom = seq.opes_[0];
      auto &rep = dynamic_cast<Repetition &>(*seq.opes_[1]);
      auto &seq1 = dynamic_cast<Sequence &>(*rep.ope_);
      auto binop = seq1.opes_[0];
      auto atom1 = seq1.opes_[1];

      auto atom_name = dynamic_cast<Reference &>(*atom).name_;
      auto binop_name = dynamic_cast<Reference &>(*binop).name_;
      auto atom1_name = dynamic_cast<Reference &>(*atom1).name_;

      if (!rep.is_zom() || atom_name != atom1_name || atom_name == binop_name) {
        if (log) {
          auto line = line_info(s, rule.s_);
          log(line.first, line.second,
              "'precedence' instruction cannt be applied to '" + rule.name +
                  "'.");
        }
        return false;
      }

      rule.holder_->ope_ = pre(atom, binop, info, rule);
      rule.disable_action = true;
    } catch (...) {
      if (log) {
        auto line = line_info(s, rule.s_);
        log(line.first, line.second,
            "'precedence' instruction cannt be applied to '" + rule.name +
                "'.");
      }
      return false;
    }
    return true;
  }

  std::shared_ptr<Grammar> perform_core(const char *s, size_t n,
                                        const Rules &rules, std::string &start,
                                        Log log) {
    Data data;
    any dt = &data;
    auto r = g["Grammar"].parse(s, n, dt);

    if (!r.ret) {
      if (log) {
        if (r.message_pos) {
          auto line = line_info(s, r.message_pos);
          log(line.first, line.second, r.message);
        } else {
          auto line = line_info(s, r.error_pos);
          log(line.first, line.second, "syntax error");
        }
      }
      return nullptr;
    }

    auto &grammar = *data.grammar;

    // User provided rules
    for (const auto &x : rules) {
      auto name = x.first;
      bool ignore = false;
      if (!name.empty() && name[0] == '~') {
        ignore = true;
        name.erase(0, 1);
      }
      if (!name.empty()) {
        auto &rule = grammar[name];
        rule <= x.second;
        rule.name = name;
        rule.ignoreSemanticValue = ignore;
      }
    }

    // Check duplicated definitions
    bool ret = data.duplicates.empty();

    for (const auto &x : data.duplicates) {
      if (log) {
        const auto &name = x.first;
        auto ptr = x.second;
        auto line = line_info(s, ptr);
        log(line.first, line.second, "'" + name + "' is already defined.");
      }
    }

    // Check missing definitions
    for (auto &x : grammar) {
      auto &rule = x.second;

      ReferenceChecker vis(*data.grammar, rule.params);
      rule.accept(vis);
      for (const auto &y : vis.error_s) {
        const auto &name = y.first;
        const auto ptr = y.second;
        if (log) {
          auto line = line_info(s, ptr);
          log(line.first, line.second, vis.error_message[name]);
        }
        ret = false;
      }
    }

    if (!ret) { return nullptr; }

    // Link references
    for (auto &x : grammar) {
      auto &rule = x.second;
      LinkReferences vis(*data.grammar, rule.params);
      rule.accept(vis);
    }

    // Check left recursion
    ret = true;

    for (auto &x : grammar) {
      const auto &name = x.first;
      auto &rule = x.second;

      DetectLeftRecursion vis(name);
      rule.accept(vis);
      if (vis.error_s) {
        if (log) {
          auto line = line_info(s, vis.error_s);
          log(line.first, line.second, "'" + name + "' is left recursive.");
        }
        ret = false;
      }
    }

    if (!ret) { return nullptr; }

    // Set root definition
    auto &start_rule = (*data.grammar)[data.start];

    // Check infinite loop
    {
      DetectInfiniteLoop vis(data.start_pos, data.start);
      start_rule.accept(vis);
      if (vis.has_error) {
        if (log) {
          auto line = line_info(s, vis.error_s);
          log(line.first, line.second,
              "infinite loop is detected in '" + vis.error_name + "'.");
        }
        return nullptr;
      }
    }

    // Automatic whitespace skipping
    if (grammar.count(WHITESPACE_DEFINITION_NAME)) {
      for (auto &x : grammar) {
        auto &rule = x.second;
        auto ope = rule.get_core_operator();
        if (IsLiteralToken::check(*ope)) { rule <= tok(ope); }
      }

      start_rule.whitespaceOpe =
          wsp((*data.grammar)[WHITESPACE_DEFINITION_NAME].get_core_operator());
    }

    // Word expression
    if (grammar.count(WORD_DEFINITION_NAME)) {
      start_rule.wordOpe =
          (*data.grammar)[WORD_DEFINITION_NAME].get_core_operator();
    }

    // Apply instructions
    for (const auto &item : data.instructions) {
      const auto &name = item.first;
      const auto &instruction = item.second;
      auto &rule = grammar[name];

      if (instruction.type == "precedence") {
        const auto &info =
            any_cast<PrecedenceClimbing::BinOpeInfo>(instruction.data);

        if (!apply_precedence_instruction(rule, info, s, log)) {
          return nullptr;
        }
      }
    }

    // Set root definition
    start = data.start;

    return data.grammar;
  }

  Grammar g;
};

/*-----------------------------------------------------------------------------
 *  AST
 *---------------------------------------------------------------------------*/

template <typename Annotation> struct AstBase : public Annotation {
  AstBase(const char *a_path, size_t a_line, size_t a_column,
          const char *a_name, size_t a_position, size_t a_length,
          size_t a_choice_count, size_t a_choice,
          const std::vector<std::shared_ptr<AstBase>> &a_nodes)
      : path(a_path ? a_path : ""), line(a_line), column(a_column),
        name(a_name), position(a_position), length(a_length),
        choice_count(a_choice_count), choice(a_choice), original_name(a_name),
        original_choice_count(a_choice_count), original_choice(a_choice),
        tag(str2tag(a_name)), original_tag(tag), is_token(false),
        nodes(a_nodes) {}

  AstBase(const char *a_path, size_t a_line, size_t a_column,
          const char *a_name, size_t a_position, size_t a_length,
          size_t a_choice_count, size_t a_choice, const std::string &a_token)
      : path(a_path ? a_path : ""), line(a_line), column(a_column),
        name(a_name), position(a_position), length(a_length),
        choice_count(a_choice_count), choice(a_choice), original_name(a_name),
        original_choice_count(a_choice_count), original_choice(a_choice),
        tag(str2tag(a_name)), original_tag(tag), is_token(true),
        token(a_token) {}

  AstBase(const AstBase &ast, const char *a_original_name, size_t a_position,
          size_t a_length, size_t a_original_choice_count,
          size_t a_original_choise)
      : path(ast.path), line(ast.line), column(ast.column), name(ast.name),
        position(a_position), length(a_length), choice_count(ast.choice_count),
        choice(ast.choice), original_name(a_original_name),
        original_choice_count(a_original_choice_count),
        original_choice(a_original_choise), tag(ast.tag),
        original_tag(str2tag(a_original_name)), is_token(ast.is_token),
        token(ast.token), nodes(ast.nodes), parent(ast.parent) {}

  const std::string path;
  const size_t line = 1;
  const size_t column = 1;

  const std::string name;
  size_t position;
  size_t length;
  const size_t choice_count;
  const size_t choice;
  const std::string original_name;
  const size_t original_choice_count;
  const size_t original_choice;
  const unsigned int tag;
  const unsigned int original_tag;

  const bool is_token;
  const std::string token;

  std::vector<std::shared_ptr<AstBase<Annotation>>> nodes;
  std::weak_ptr<AstBase<Annotation>> parent;
};

template <typename T>
void ast_to_s_core(const std::shared_ptr<T> &ptr, std::string &s, int level,
                   std::function<std::string(const T &ast, int level)> fn) {
  const auto &ast = *ptr;
  for (auto i = 0; i < level; i++) {
    s += "  ";
  }
  auto name = ast.original_name;
  if (ast.original_choice_count > 0) {
    name += "/" + std::to_string(ast.original_choice);
  }
  if (ast.name != ast.original_name) { name += "[" + ast.name + "]"; }
  if (ast.is_token) {
    s += "- " + name + " (" + ast.token + ")\n";
  } else {
    s += "+ " + name + "\n";
  }
  if (fn) { s += fn(ast, level + 1); }
  for (auto node : ast.nodes) {
    ast_to_s_core(node, s, level + 1, fn);
  }
}

template <typename T>
std::string
ast_to_s(const std::shared_ptr<T> &ptr,
         std::function<std::string(const T &ast, int level)> fn = nullptr) {
  std::string s;
  ast_to_s_core(ptr, s, 0, fn);
  return s;
}

struct AstOptimizer {
  AstOptimizer(bool optimize_nodes,
               const std::vector<std::string> &filters = {})
      : optimize_nodes_(optimize_nodes), filters_(filters) {}

  template <typename T>
  std::shared_ptr<T> optimize(std::shared_ptr<T> original,
                              std::shared_ptr<T> parent = nullptr) {
    auto found = std::find(filters_.begin(), filters_.end(), original->name) !=
                 filters_.end();
    bool opt = optimize_nodes_ ? !found : found;

    if (opt && original->nodes.size() == 1) {
      auto child = optimize(original->nodes[0], parent);
      return std::make_shared<T>(*child, original->name.c_str(),
                                 original->choice_count, original->position,
                                 original->length, original->choice);
    }

    auto ast = std::make_shared<T>(*original);
    ast->parent = parent;
    ast->nodes.clear();
    for (auto node : original->nodes) {
      auto child = optimize(node, ast);
      ast->nodes.push_back(child);
    }
    return ast;
  }

private:
  const bool optimize_nodes_;
  const std::vector<std::string> filters_;
};

struct EmptyType {};
typedef AstBase<EmptyType> Ast;

/*-----------------------------------------------------------------------------
 *  parser
 *---------------------------------------------------------------------------*/

class parser {
public:
  parser() = default;

  parser(const char *s, size_t n, const Rules &rules) {
    load_grammar(s, n, rules);
  }

  parser(const char *s, const Rules &rules) : parser(s, strlen(s), rules) {}

  parser(const char *s, size_t n) : parser(s, n, Rules()) {}

  parser(const char *s) : parser(s, strlen(s), Rules()) {}

  operator bool() { return grammar_ != nullptr; }

  bool load_grammar(const char *s, size_t n, const Rules &rules) {
    grammar_ = ParserGenerator::parse(s, n, rules, start_, log);
    return grammar_ != nullptr;
  }

  bool load_grammar(const char *s, size_t n) {
    return load_grammar(s, n, Rules());
  }

  bool load_grammar(const char *s, const Rules &rules) {
    auto n = strlen(s);
    return load_grammar(s, n, rules);
  }

  bool load_grammar(const char *s) {
    auto n = strlen(s);
    return load_grammar(s, n);
  }

  bool parse_n(const char *s, size_t n, const char *path = nullptr) const {
    if (grammar_ != nullptr) {
      const auto &rule = (*grammar_)[start_];
      auto r = rule.parse(s, n, path);
      output_log(s, n, r);
      return r.ret && r.len == n;
    }
    return false;
  }

  bool parse(const char *s, const char *path = nullptr) const {
    auto n = strlen(s);
    return parse_n(s, n, path);
  }

  bool parse_n(const char *s, size_t n, any &dt,
               const char *path = nullptr) const {
    if (grammar_ != nullptr) {
      const auto &rule = (*grammar_)[start_];
      auto r = rule.parse(s, n, dt, path);
      output_log(s, n, r);
      return r.ret && r.len == n;
    }
    return false;
  }

  bool parse(const char *s, any &dt, const char *path = nullptr) const {
    auto n = strlen(s);
    return parse_n(s, n, dt, path);
  }

  template <typename T>
  bool parse_n(const char *s, size_t n, T &val,
               const char *path = nullptr) const {
    if (grammar_ != nullptr) {
      const auto &rule = (*grammar_)[start_];
      auto r = rule.parse_and_get_value(s, n, val, path);
      output_log(s, n, r);
      return r.ret && r.len == n;
    }
    return false;
  }

  template <typename T>
  bool parse(const char *s, T &val, const char *path = nullptr) const {
    auto n = strlen(s);
    return parse_n(s, n, val, path);
  }

  template <typename T>
  bool parse_n(const char *s, size_t n, any &dt, T &val,
               const char *path = nullptr) const {
    if (grammar_ != nullptr) {
      const auto &rule = (*grammar_)[start_];
      auto r = rule.parse_and_get_value(s, n, dt, val, path);
      output_log(s, n, r);
      return r.ret && r.len == n;
    }
    return false;
  }

  template <typename T>
  bool parse(const char *s, any &dt, T &val,
             const char * /*path*/ = nullptr) const {
    auto n = strlen(s);
    return parse_n(s, n, dt, val);
  }

  Definition &operator[](const char *s) { return (*grammar_)[s]; }

  const Definition &operator[](const char *s) const { return (*grammar_)[s]; }

  std::vector<std::string> get_rule_names() {
    std::vector<std::string> rules;
    rules.reserve(grammar_->size());
    for (auto const &r : *grammar_) {
      rules.emplace_back(r.first);
    }
    return rules;
  }

  void enable_packrat_parsing() {
    if (grammar_ != nullptr) {
      auto &rule = (*grammar_)[start_];
      rule.enablePackratParsing = true;
    }
  }

  template <typename T = Ast> parser &enable_ast() {
    for (auto &x : *grammar_) {
      const auto &name = x.first;
      auto &rule = x.second;

      if (!rule.action) {
        rule.action = [&](const SemanticValues &sv) {
          auto line = sv.line_info();

          if (rule.is_token()) {
            return std::make_shared<T>(
                sv.path, line.first, line.second, name.c_str(),
                std::distance(sv.ss, sv.c_str()), sv.length(),
                sv.choice_count(), sv.choice(), sv.token());
          }

          auto ast = std::make_shared<T>(
              sv.path, line.first, line.second, name.c_str(),
              std::distance(sv.ss, sv.c_str()), sv.length(), sv.choice_count(),
              sv.choice(), sv.transform<std::shared_ptr<T>>());

          for (auto node : ast->nodes) {
            node->parent = ast;
          }
          return ast;
        };
      }
    }
    return *this;
  }

  void enable_trace(TracerEnter tracer_enter, TracerLeave tracer_leave) {
    if (grammar_ != nullptr) {
      auto &rule = (*grammar_)[start_];
      rule.tracer_enter = tracer_enter;
      rule.tracer_leave = tracer_leave;
    }
  }

  Log log;

private:
  void output_log(const char *s, size_t n, const Definition::Result &r) const {
    if (log) {
      if (!r.ret) {
        if (r.message_pos) {
          auto line = line_info(s, r.message_pos);
          log(line.first, line.second, r.message);
        } else {
          auto line = line_info(s, r.error_pos);
          log(line.first, line.second, "syntax error");
        }
      } else if (r.len != n) {
        auto line = line_info(s, s + r.len);
        log(line.first, line.second, "syntax error");
      }
    }
  }

  std::shared_ptr<Grammar> grammar_;
  std::string start_;
};

} // namespace peg

#endif

// vim: et ts=2 sw=2 cin cino={1s ff=unix
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

#ifndef CLUON_METAMESSAGE_HPP
#define CLUON_METAMESSAGE_HPP

//#include "cluon/cluon.hpp"

#include <cstdint>
#include <functional>
#include <string>
#include <vector>

namespace cluon {
/**
This class provides a generic description for a message. It is internally
used during the processing of message specifications to generate targets
like C++ or .proto files.
*/
class LIBCLUON_API MetaMessage {
   public:
    /**
     * This inner class provides a generic description for a message's fields.
     * It is internally used during the processing of message specifications.
     */
    class MetaField {
       public:
        enum MetaFieldDataTypes : uint16_t {
            BOOL_T      = 0,
            UINT8_T     = 2,
            INT8_T      = 3,
            UINT16_T    = 4,
            INT16_T     = 5,
            UINT32_T    = 6,
            INT32_T     = 7,
            UINT64_T    = 8,
            INT64_T     = 9,
            CHAR_T      = 11,
            FLOAT_T     = 13,
            DOUBLE_T    = 14,
            BYTES_T     = 49,
            STRING_T    = 51,
            MESSAGE_T   = 53,
            UNDEFINED_T = 0xFFFF,
        };

       private:
        MetaField &operator=(MetaField &&) = delete;

       public:
        MetaField()                  = default;
        MetaField(const MetaField &) = default;
        MetaField(MetaField &&)      = default;
        MetaField &operator=(const MetaField &) = default;

       public:
        /**
         * @return Type of this field.
         */
        MetaFieldDataTypes fieldDataType() const noexcept;
        /**
         * This method sets the type for this field.
         *
         * @param v Type for this field.
         * @return Reference to this instance.
         */
        MetaField &fieldDataType(const MetaFieldDataTypes &v) noexcept;

        /**
         * @return Type name of this field.
         */
        std::string fieldDataTypeName() const noexcept;
        /**
         * This method sets the type name for this field.
         *
         * @param v Type name for this field.
         * @return Reference to this instance.
         */
        MetaField &fieldDataTypeName(const std::string &v) noexcept;

        /**
         * @return Name of this field.
         */
        std::string fieldName() const noexcept;
        /**
         * This method sets the name for this field.
         *
         * @param v Name for this field.
         * @return Reference to this instance.
         */
        MetaField &fieldName(const std::string &v) noexcept;

        /**
         * @return Identifier of this field.
         */
        uint32_t fieldIdentifier() const noexcept;
        /**
         * This method sets the identifier for this field.
         *
         * @param v Identifier for this field.
         * @return Reference to this instance.
         */
        MetaField &fieldIdentifier(uint32_t v) noexcept;

        /**
         * @return Field's default initialization value.
         */
        std::string defaultInitializationValue() const noexcept;
        /**
         * This method sets the field's default initialization value for this field.
         *
         * @param v Field's default initialization value for this field.
         * @return Reference to this instance.
         */
        MetaField &defaultInitializationValue(const std::string &v) noexcept;

       private:
        MetaFieldDataTypes m_fieldDataType{UNDEFINED_T};
        std::string m_fieldDataTypeName{""};
        std::string m_fieldName{""};
        uint32_t m_fieldIdentifier{0};
        std::string m_defaultInitializationValue{""};
    };

   public:
    MetaMessage() noexcept;
    MetaMessage(const MetaMessage &) = default;
    MetaMessage(MetaMessage &&)      = default;
    MetaMessage &operator=(const MetaMessage &) = default;
    MetaMessage &operator=(MetaMessage &&) = default;

    /**
     * This method adds a metafield to this meta message.
     *
     * @param mf Meta field to be added.
     * @return Reference to this instance.
     */
    MetaMessage &add(MetaField &&mf) noexcept;

    /**
     * This method returns a vector of current meta fields.
     *
     * @return Meta fields from this meta message.
     */
    const std::vector<MetaField> &listOfMetaFields() const noexcept;

    /**
     * This method can be used to visit this instance and propagate information
     * details about the contained fields.
     *
     * @param visit std::function object to be called to visit this MetaMessage.
     */
    void accept(const std::function<void(const MetaMessage &)> &visit);

    /**
     * @return Package name.
     */
    std::string packageName() const noexcept;
    /**
     * This method sets the package name.
     *
     * @param v Package name for this message.
     * @return Reference to this instance.
     */
    MetaMessage &packageName(const std::string &v) noexcept;

    /**
     * @return Message name.
     */
    std::string messageName() const noexcept;
    /**
     * This method sets the message name.
     *
     * @param v Message name for this message.
     * @return Reference to this instance.
     */
    MetaMessage &messageName(const std::string &v) noexcept;

    /**
     * @return Message identifier.
     */
    int32_t messageIdentifier() const noexcept;
    /**
     * This method sets the message identifier.
     *
     * @param v Message identifier for this message.
     * @return Reference to this instance.
     */
    MetaMessage &messageIdentifier(int32_t v) noexcept;

   private:
    std::string m_packageName{""};
    std::string m_messageName{""};
    int32_t m_messageIdentifier{0};
    std::vector<MetaField> m_listOfMetaFields{};
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

#ifndef CLUON_MESSAGEPARSER_HPP
#define CLUON_MESSAGEPARSER_HPP

//#include "cluon/MetaMessage.hpp"
//#include "cluon/cluon.hpp"

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

namespace cluon {
/**
This class provides a parser for message specifications in .odvd format. The
format is inspired by Google Protobuf (https://developers.google.com/protocol-buffers/)
but simplified to enforce backwards and forwards compatibility next to
numerical message identifiers.

This message specification format is also used by OpenDaVINCI (http://code.opendavinci.org).

The parser is based on https://github.com/yhirose/cpp-peglib.

An example for a .odvd compliant message is demonstrated in the following:

\code{.cpp}
const char *spec = R"(
message myMessage.SubName [id = 1] {
    uint8 field1 [id = 1];
    uint32 field2 [id = 2];
    int64 field3 [id = 3];
    string field4 [id = 4];
}
)";

cluon::MessageParser mp;
auto retVal = mp.parse(std::string(spec));
if (retVal.second == cluon::MessageParser::MessageParserErrorCodes::NO_MESSAGEPARSER_ERROR) {
    auto listOfMessages = retVal.first;
    for (auto message : listOfMessages) {
        message.accept([](const cluon::MetaMessage &mm){ std::cout << "Message name = " << mm.messageName() <<
std::endl; });
    }
}
\endcode
*/
class LIBCLUON_API MessageParser {
   public:
    enum MessageParserErrorCodes : uint8_t { NO_MESSAGEPARSER_ERROR = 0, SYNTAX_ERROR = 1, DUPLICATE_IDENTIFIERS = 2 };

   private:
    MessageParser(const MessageParser &) = delete;
    MessageParser(MessageParser &&)      = delete;
    MessageParser &operator=(const MessageParser &) = delete;
    MessageParser &operator=(MessageParser &&) = delete;

   public:
    MessageParser() = default;

    /**
     * This method tries to parse the given message specification.
     *
     * @param input Message specification.
     * @return Pair: List of cluon::MetaMessages describing the specified messages and error code:
     *         NO_MESSAGEPARSER_ERROR: The given specification could be parsed successfully (list moght be non-empty).
     *         SYNTAX_ERROR: The given specification could not be parsed successfully (list is empty).
     *         DUPLICATE_IDENTIFIERS: The given specification contains ambiguous names or identifiers (list is empty).
     */
    std::pair<std::vector<MetaMessage>, MessageParserErrorCodes> parse(const std::string &input);
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

//#include "cluon/MetaMessage.hpp"

namespace cluon {

inline MetaMessage::MetaField::MetaFieldDataTypes MetaMessage::MetaField::fieldDataType() const noexcept {
    return m_fieldDataType;
}

inline MetaMessage::MetaField &MetaMessage::MetaField::fieldDataType(const MetaMessage::MetaField::MetaFieldDataTypes &v) noexcept {
    m_fieldDataType = v;
    return *this;
}

inline std::string MetaMessage::MetaField::fieldDataTypeName() const noexcept {
    return m_fieldDataTypeName;
}

inline MetaMessage::MetaField &MetaMessage::MetaField::fieldDataTypeName(const std::string &v) noexcept {
    m_fieldDataTypeName = v;
    return *this;
}

inline std::string MetaMessage::MetaField::fieldName() const noexcept {
    return m_fieldName;
}

inline MetaMessage::MetaField &MetaMessage::MetaField::fieldName(const std::string &v) noexcept {
    m_fieldName = v;
    return *this;
}

inline uint32_t MetaMessage::MetaField::fieldIdentifier() const noexcept {
    return m_fieldIdentifier;
}

inline MetaMessage::MetaField &MetaMessage::MetaField::fieldIdentifier(uint32_t v) noexcept {
    m_fieldIdentifier = v;
    return *this;
}

inline std::string MetaMessage::MetaField::defaultInitializationValue() const noexcept {
    return m_defaultInitializationValue;
}

inline MetaMessage::MetaField &MetaMessage::MetaField::defaultInitializationValue(const std::string &v) noexcept {
    m_defaultInitializationValue = v;
    return *this;
}

////////////////////////////////////////////////////////////////////////////////

inline MetaMessage::MetaMessage() noexcept {}

inline std::string MetaMessage::packageName() const noexcept {
    return m_packageName;
}

inline std::string MetaMessage::messageName() const noexcept {
    return m_messageName;
}

inline MetaMessage &MetaMessage::packageName(const std::string &v) noexcept {
    m_packageName = v;
    return *this;
}

inline MetaMessage &MetaMessage::messageName(const std::string &v) noexcept {
    m_messageName = v;
    return *this;
}

inline int32_t MetaMessage::messageIdentifier() const noexcept {
    return m_messageIdentifier;
}

inline MetaMessage &MetaMessage::messageIdentifier(int32_t v) noexcept {
    m_messageIdentifier = v;
    return *this;
}

inline MetaMessage &MetaMessage::add(MetaMessage::MetaField &&mf) noexcept {
    m_listOfMetaFields.emplace_back(std::move(mf));
    return *this;
}

inline const std::vector<MetaMessage::MetaField> &MetaMessage::listOfMetaFields() const noexcept {
    return m_listOfMetaFields;
}

inline void MetaMessage::accept(const std::function<void(const MetaMessage &)> &visit) {
    visit(*this);
}
} // namespace cluon
/*
 * Copyright (C) 2017-2018  Christian Berger
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//#include "cluon/MessageParser.hpp"
//#include "cluon/stringtoolbox.hpp"

//#include "cpp-peglib/peglib.h"

#include <algorithm>
#include <functional>
#include <iostream>
#include <memory>
#include <regex>
#include <string>
#include <vector>

namespace cluon {

inline std::pair<std::vector<MetaMessage>, MessageParser::MessageParserErrorCodes> MessageParser::parse(const std::string &input) {
    const char *grammarMessageSpecificationLanguage = R"(
        MESSAGES_SPECIFICATION      <- PACKAGE_DECLARATION? MESSAGE_DECLARATION*
        PACKAGE_DECLARATION         <- 'package' PACKAGE_NAME ';'
        PACKAGE_NAME                <- < NAME ('.' NAME)* >

        MESSAGE_DECLARATION         <- 'message' MESSAGE_NAME '[' IDENTIFIER ','? ']' '{' FIELD* '}'
        MESSAGE_NAME                <- < NAME ('.' NAME)* >

        FIELD                       <- PRIMITIVE_TYPE NAME ('[' (((DEFAULT / IDENTIFIER) ','?)+)? ']')? ';'
        DEFAULT                     <- 'default' '=' (FLOAT_NUMBER / BOOL / CHARACTER / STRING)
        PRIMITIVE_TYPE              <- < 'bool' / 'float' / 'double' /
                                         'char' /
                                         'bytes' / 'string' /
                                         'int8' / 'uint8' / 
                                         'int16' / 'uint16' / 
                                         'int32' / 'uint32' / 
                                         'int64' / 'uint64' /
                                         MESSAGE_TYPE >

        MESSAGE_TYPE                <- < NAME ('.' NAME)* >

        IDENTIFIER                  <- 'id' '=' NATURAL_NUMBER

        NAME                        <- < [a-zA-Z][a-zA-Z0-9_]* >
        DIGIT                       <- < [0-9] >
        NATURAL_NUMBER              <- < [1-9] DIGIT* >
        FLOAT_NUMBER                <- < ('+' / '-')? DIGIT DIGIT* (('.') DIGIT*)? >
        BOOL                        <- < 'true' > / < 'false' >
        STRING                      <- '"' < (!'"'.)* > '"'
        CHARACTER                   <- '\'' < (!'\'' .) > '\''

        %whitespace                 <- [ \t\r\n]*
    )";

    ////////////////////////////////////////////////////////////////////////////

    // Function to check for unique field names.
    std::function<bool(const peg::Ast &, std::string &, std::vector<std::string> &, std::vector<std::string> &, std::vector<int32_t> &, std::vector<int32_t> &)>
        check4UniqueFieldNames = [&checkForUniqueFieldNames = check4UniqueFieldNames](const peg::Ast &ast,
                                                                                      std::string &prefix,
                                                                                      std::vector<std::string> &messageNames,
                                                                                      std::vector<std::string> &fieldNames,
                                                                                      std::vector<int32_t> &numericalMessageIdentifiers,
                                                                                      std::vector<int32_t> &numericalFieldIdentifiers) {
            bool retVal = true;
            // First, we need to visit the children of AST node MESSAGES_SPECIFICATION.

            if ("MESSAGES_SPECIFICATION" == ast.name) {
                for (const auto &node : ast.nodes) {
                    retVal &= checkForUniqueFieldNames(*node, prefix, messageNames, fieldNames, numericalMessageIdentifiers, numericalFieldIdentifiers);
                }
                // Try finding duplicated message identifiers.
                if (retVal) {
                    std::sort(std::begin(numericalMessageIdentifiers), std::end(numericalMessageIdentifiers));
                    int32_t duplicatedMessageIdentifier{-1};
                    for (auto it{std::begin(numericalMessageIdentifiers)}; it != std::end(numericalMessageIdentifiers); it++) {
                        if (it + 1 != std::end(numericalMessageIdentifiers)) {
                            if (std::find(it + 1, std::end(numericalMessageIdentifiers), *it) != std::end(numericalMessageIdentifiers)) {
                                duplicatedMessageIdentifier = *it;
                            }
                        }
                    }
                    retVal &= (-1 == duplicatedMessageIdentifier);
                    if (!retVal) {
                        std::cerr << "[cluon::MessageParser] Found duplicated numerical message identifier: " << duplicatedMessageIdentifier << '\n';
                    }
                }
                // Try finding duplicated message names.
                if (retVal) {
                    std::sort(std::begin(messageNames), std::end(messageNames));
                    std::string duplicatedMessageName;
                    for (auto it{std::begin(messageNames)}; it != std::end(messageNames); it++) {
                        if (it + 1 != std::end(messageNames)) {
                            if (std::find(it + 1, std::end(messageNames), *it) != std::end(messageNames)) {
                                duplicatedMessageName = *it;
                            }
                        }
                    }
                    retVal &= (duplicatedMessageName.empty());
                    if (!retVal) {
                        std::cerr << "[cluon::MessageParser] Found duplicated message name '" << duplicatedMessageName << "'" << '\n';
                    }
                }
            }
            // Second, we need to visit the children of AST node MESSAGE_DECLARATION.
            if ("MESSAGE_DECLARATION" == ast.name) {
                fieldNames.clear();
                numericalFieldIdentifiers.clear();
                prefix = "";
                retVal = true;

                for (const auto &node : ast.nodes) {
                    if ("MESSAGE_NAME" == node->original_name) {
                        prefix = node->token;
                        messageNames.push_back(::stringtoolbox::trim(prefix));
                    } else if ("IDENTIFIER" == node->original_name) {
                        numericalMessageIdentifiers.push_back(std::stoi(node->token));
                    } else if ("FIELD" == node->original_name) {
                        retVal &= checkForUniqueFieldNames(*node, prefix, messageNames, fieldNames, numericalMessageIdentifiers, numericalFieldIdentifiers);
                    }
                }

                // Try finding duplicated numerical field identifiers.
                if (retVal) {
                    std::sort(std::begin(numericalFieldIdentifiers), std::end(numericalFieldIdentifiers));
                    int32_t duplicatedFieldIdentifier{-1};
                    for (auto it{std::begin(numericalFieldIdentifiers)}; it != std::end(numericalFieldIdentifiers); it++) {
                        if (it + 1 != std::end(numericalFieldIdentifiers)) {
                            if (std::find(it + 1, std::end(numericalFieldIdentifiers), *it) != std::end(numericalFieldIdentifiers)) {
                                duplicatedFieldIdentifier = *it;
                            }
                        }
                    }
                    retVal &= (-1 == duplicatedFieldIdentifier);
                    if (!retVal) {
                        std::cerr << "[cluon::MessageParser] Found duplicated numerical field identifier in message "
                                  << "'" << ::stringtoolbox::trim(prefix) << "': " << duplicatedFieldIdentifier << '\n';
                    }
                }
                // Try finding duplicated field names.
                if (retVal) {
                    std::sort(std::begin(fieldNames), std::end(fieldNames));
                    std::string duplicatedFieldName;
                    for (auto it{std::begin(fieldNames)}; it != std::end(fieldNames); it++) {
                        if (it + 1 != std::end(fieldNames)) {
                            if (std::find(it + 1, std::end(fieldNames), *it) != std::end(fieldNames)) {
                                duplicatedFieldName = *it;
                            }
                        }
                    }
                    retVal &= (duplicatedFieldName.empty());
                    if (!retVal) {
                        std::cerr << "[cluon::MessageParser] Found duplicated field name in message '" << ::stringtoolbox::trim(prefix) << "': '"
                                  << duplicatedFieldName << "'" << '\n';
                    }
                }
            }
            // Within AST node MESSAGE_DECLARATION, we have FIELD from
            // which we need to extract the field "token".
            if (ast.original_name == "FIELD") {
                // Extract the value of entry "NAME".
                auto nodeName = std::find_if(std::begin(ast.nodes), std::end(ast.nodes), [](auto a) { return (a->original_name == "NAME"); });
                if (nodeName != std::end(ast.nodes)) {
                    fieldNames.push_back((*nodeName)->token);
                }

                // Extract the value of entry "IDENTIFIER".
                auto nodeNumericalFieldIdentifier
                    = std::find_if(std::begin(ast.nodes), std::end(ast.nodes), [](auto a) { return (a->original_name == "IDENTIFIER"); });
                if (nodeNumericalFieldIdentifier != std::end(ast.nodes)) {
                    numericalFieldIdentifiers.push_back(std::stoi((*nodeNumericalFieldIdentifier)->token));
                }
            }

            return retVal;
        };

    ////////////////////////////////////////////////////////////////////////////

    // Function to transform AST into list of MetaMessages.
    std::function<void(const peg::Ast &, std::vector<MetaMessage> &)> transform2MetaMessages
        = [](const peg::Ast &ast, std::vector<MetaMessage> &listOfMetaMessages) {
              // "Inner"-lambda to handle various types of message declarations.
              auto createMetaMessage = [](const peg::Ast &_node, std::string _packageName) -> MetaMessage {
                  MetaMessage mm;
                  mm.packageName(::stringtoolbox::trim(_packageName));
                  uint32_t fieldIdentifierCounter{0};
                  for (const auto &e : _node.nodes) {
                      if ("MESSAGE_NAME" == e->original_name) {
                          std::string _messageName = e->token;
                          mm.messageName(::stringtoolbox::trim(_messageName));
                      } else if ("IDENTIFIER" == e->original_name) {
                          mm.messageIdentifier(std::stoi(e->token));
                      } else if ("FIELD" == e->original_name) {
                          std::string _fieldDataType;
                          std::string _fieldName;
                          std::string _fieldDefaultInitializerValue;
                          std::string _fieldIdentifier;
                          for (const auto &f : e->nodes) {
                              if ("PRIMITIVE_TYPE" == f->original_name) {
                                  _fieldDataType = f->token;
                              } else if ("NAME" == f->original_name) {
                                  _fieldName = f->token;
                              } else if ("DEFAULT" == f->original_name) {
                                  if ("STRING" == f->name) {
                                      _fieldDefaultInitializerValue = "\"" + f->token + "\""; // NOLINT
                                  } else if ("CHARACTER" == f->name) {
                                      _fieldDefaultInitializerValue = "'" + f->token + "'";
                                  } else {
                                      _fieldDefaultInitializerValue = f->token;
                                  }
                              } else if ("IDENTIFIER" == f->original_name) {
                                  _fieldIdentifier = f->token;
                              }
                          }

                          if (_fieldIdentifier.empty()) {
                              // Automatically count expected field identifiers in case of missing field options.
                              fieldIdentifierCounter++;
                          }

                          std::map<std::string, MetaMessage::MetaField::MetaFieldDataTypes> STRING_TO_DATATYPE_MAP = {
                              {"bool", MetaMessage::MetaField::BOOL_T},
                              {"char", MetaMessage::MetaField::CHAR_T},
                              {"uint8", MetaMessage::MetaField::UINT8_T},
                              {"int8", MetaMessage::MetaField::INT8_T},
                              {"uint16", MetaMessage::MetaField::UINT16_T},
                              {"int16", MetaMessage::MetaField::INT16_T},
                              {"uint32", MetaMessage::MetaField::UINT32_T},
                              {"int32", MetaMessage::MetaField::INT32_T},
                              {"uint64", MetaMessage::MetaField::UINT64_T},
                              {"int64", MetaMessage::MetaField::INT64_T},
                              {"float", MetaMessage::MetaField::FLOAT_T},
                              {"double", MetaMessage::MetaField::DOUBLE_T},
                              {"string", MetaMessage::MetaField::STRING_T},
                              {"bytes", MetaMessage::MetaField::BYTES_T},
                          };

                          MetaMessage::MetaField mf;
                          if (0 < STRING_TO_DATATYPE_MAP.count(_fieldDataType)) {
                              mf.fieldDataType(STRING_TO_DATATYPE_MAP[_fieldDataType]);
                          } else {
                              mf.fieldDataType(MetaMessage::MetaField::MESSAGE_T);
                          }
                          mf.fieldDataTypeName(::stringtoolbox::trim(_fieldDataType));
                          mf.fieldName(::stringtoolbox::trim(_fieldName));
                          mf.fieldIdentifier(
                              (!_fieldIdentifier.empty() ? static_cast<uint32_t>(std::stoi(::stringtoolbox::trim(_fieldIdentifier))) : fieldIdentifierCounter));
                          mf.defaultInitializationValue(_fieldDefaultInitializerValue);
                          mm.add(std::move(mf));
                      }
                  }
                  return mm;
              };

              ////////////////////////////////////////////////////////////////////////

              // Case: "package XYZ" present.
              if ("MESSAGES_SPECIFICATION" == ast.name) {
                  // Extract the value of entry "PACKAGE_NAME".
                  auto nodeIdentifier = std::find_if(std::begin(ast.nodes), std::end(ast.nodes), [](auto a) { return (a->name == "PACKAGE_NAME"); });
                  std::string packageName;
                  if (nodeIdentifier != std::end(ast.nodes)) {
                      packageName = (*nodeIdentifier)->token;
                  }

                  // Extract the value of entry "MESSAGE_DECLARATION".
                  for (const auto &node : ast.nodes) {
                      if (node->name == "MESSAGE_DECLARATION") {
                          listOfMetaMessages.emplace_back(createMetaMessage(*node, packageName));
                      }
                  }
              } else {
                  // In case we only have one single message and no package.
                  listOfMetaMessages.emplace_back(createMetaMessage(ast, ""));
              }
          };

    ////////////////////////////////////////////////////////////////////////////

    peg::parser p(grammarMessageSpecificationLanguage);
    p.enable_ast();
    p.log = [](size_t row, size_t col, const std::string &msg) {
        std::cerr << "[cluon::MessageParser] Parsing error:" << row << ":" << col << ": " << msg << '\n';
    };

    std::pair<std::vector<MetaMessage>, MessageParserErrorCodes> retVal{};
    std::string inputWithoutComments{input};
    try {
        const std::string MATCH_COMMENTS_REGEX = R"(/\*([\s\S]*?)\*/|//.*)";
        inputWithoutComments                   = std::regex_replace(input, std::regex(MATCH_COMMENTS_REGEX), ""); // NOLINT
    } catch (std::regex_error &) {                                                                                // LCOV_EXCL_LINE
    } catch (std::bad_cast &) {                                                                                   // LCOV_EXCL_LINE
    }
    try {
        std::vector<MetaMessage> listOfMetaMessages{};
        std::shared_ptr<peg::Ast> ast{};
        if (p.parse(inputWithoutComments.c_str(), ast)) {
            ast = peg::AstOptimizer(true).optimize(ast);
            {
                std::string tmpPrefix;
                std::vector<std::string> tmpMessageNames{};
                std::vector<std::string> tmpFieldNames{};
                std::vector<int32_t> tmpNumericalMessageIdentifiers{};
                std::vector<int32_t> tmpNumericalFieldIdentifiers{};
                if (check4UniqueFieldNames(*ast, tmpPrefix, tmpMessageNames, tmpFieldNames, tmpNumericalMessageIdentifiers, tmpNumericalFieldIdentifiers)) {
                    transform2MetaMessages(*ast, listOfMetaMessages);
                    retVal = {listOfMetaMessages, MessageParserErrorCodes::NO_MESSAGEPARSER_ERROR};
                } else {
                    retVal = {listOfMetaMessages, MessageParserErrorCodes::DUPLICATE_IDENTIFIERS};
                }
            }
        } else {
            retVal = {listOfMetaMessages, MessageParserErrorCodes::SYNTAX_ERROR};
        }
    } catch (std::bad_cast &) { // LCOV_EXCL_LINE
    }
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
