#pragma once

#include "tcp_server.hpp"

class fixed_length_tcp_session : public detail::tcp_session
{
public:
    using tcp_session::tcp_session;

public:
    void set_fixed_length(size_t len)
    {
        len_ = len;
    }

protected:
    void do_read() override
    {
        data_.resize(len_);
        auto self(shared_from_this());
        boost::asio::async_read(socket_, boost::asio::buffer(data_, data_.size()), [this, self](boost::system::error_code ec, std::size_t length) {
            if (!ec)
            {
                monitor_->trigger(monitor_->on_recv, self, std::string_view(data_.data(), length));
                do_read();
            }
            else
            {
                monitor_->trigger(monitor_->on_disconnect, self);
            }
        });
    }

private:
    size_t len_{ 0 };
};

using fixed_length_tcp_session = tcp_server<fixed_length_tcp_session>;
