#pragma once

#include "tcp_server.hpp"
#include <boost/asio/buffers_iterator.hpp>
#include <boost/asio/read_until.hpp>
#include <boost/asio/streambuf.hpp>

struct syncword_frame_condition
{
    std::vector<uint8_t> syncword;
    size_t frame_len;

    template <typename Iterator>
    std::pair<Iterator, bool> operator()(Iterator begin, Iterator end) const
    {
        auto len = end - begin;
        if (len < frame_len)
        {
            return { begin, false };
        }
        auto ptr_begin = (uint8_t*)(begin.operator->());
        for (auto i = 0; i < syncword.size(); ++i)
        {
            if (syncword.at(i) != *(ptr_begin + i))
            {
                return { begin, true };
            }
        }
        return { begin + frame_len, true };

        // auto ptr_begin = (uint8_t *)(begin.operator->());
        // auto pos = std::search(ptr_begin, ptr_begin + len, syncword.begin(), syncword.end());
        // auto rest = len - (pos - ptr_begin);
        // if (rest < frame_len)
        //{
        //    return { begin, false };
        //}
        // return { begin + (pos - ptr_begin) + frame_len, true };
    }
};

namespace boost::asio
{
    template <>
    struct is_match_condition<syncword_frame_condition> : public boost::true_type
    {
    };
}  // namespace boost::asio

class syncword_tcp_session : public tcp_session
{
public:
    using tcp_session::tcp_session;

public:
    void set_syncword(const std::vector<uint8_t>& syncword)
    {
        syncword_ = syncword;
    }

    void set_frame_len(size_t len)
    {
        frame_len_ = len;
    }

protected:
    void do_read() override
    {
        auto self(shared_from_this());

        static auto match_syncword = [](auto&& begin, auto&& end) {
            return std::pair(begin + 10, true);
        };

        boost::asio::async_read_until(socket_, buf_, syncword_frame_condition(syncword_, frame_len_), [this, self](auto&& ec, auto&& length) {
            if (!ec)
            {
                if (length == 0)
                {
                    monitor_->trigger(monitor_->on_disconnect, self);
                    return;
                }
                monitor_->trigger(monitor_->on_recv, self, std::string_view((char*)buf_.data().data(), length));
                buf_.consume(length);
                do_read();
            }
            else
            {
                monitor_->trigger(monitor_->on_disconnect, self);
            }
            });
    }

private:
    std::vector<uint8_t> syncword_;
    size_t frame_len_;
    boost::asio::streambuf buf_;
};

using syncword_tcp_server = detail::tcp_server<syncword_tcp_session>;

/**
    syncword_tcp_server s(io, 9000);
    s.on_connect([](auto &ss) {
        ss->set_syncword({ 0xeb, 0x90 });
        ss->set_frame_len(10);
    });
    s.on_received([](auto &&ss, auto &&msg) {
        std::stringstream str;
        for (uint8_t c : msg)
        {
            str << std::format("{:02X}", c);
        }
        fmt::print("{}", str.str());
    });
 */
