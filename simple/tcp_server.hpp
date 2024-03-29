#pragma once

#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/write.hpp>
#include <mutex>
#include <deque>
#include <memory>
#include <utility>

namespace detail
{
    using boost::asio::ip::tcp;

    class tcp_session;

    struct monitor
    {
        std::function<void(const std::shared_ptr<tcp_session>&)> on_connect{ nullptr };
        std::function<void(const std::shared_ptr<tcp_session>&)> on_disconnect{ nullptr };
        std::function<void(const std::shared_ptr<tcp_session>&, const std::string_view&)> on_recv{ nullptr };

        template <class Func, class... Args>
        void trigger(Func&& func, Args &&...args)
        {
            if (func != nullptr)
            {
                func(args...);
            }
        }
    };

    class tcp_session : public std::enable_shared_from_this<tcp_session>
    {
    public:
        tcp_session(tcp::socket socket, const std::shared_ptr<detail::monitor>& monitor, size_t capacity = 4096)
            : socket_(std::move(socket))
            , monitor_(monitor)
        {
            read_buffer_.resize(capacity);
        }

        virtual ~tcp_session()
        {
            socket_.close();
        }

    public:
        void start()
        {
            monitor_->trigger(monitor_->on_connect, shared_from_this());
            do_read();
        }

        void send(const std::string& msg)
        {
            bool write_in_progress = false;
            {
                std::scoped_lock lock(mutex_);
                if (write_msgs_.size() > 4096)
                {
                    write_msgs_.clear();
                }
                write_in_progress = !write_msgs_.empty();
                write_msgs_.push_back(msg);
            }
            if (!write_in_progress)
            {
                do_write();
            }
        }

        std::string peer_address()
        {
            auto ep = socket_.remote_endpoint();
            return std::format("{}:{}", ep.address().to_string(), ep.port());
        }

        tcp::socket& socket()
        {
            return socket_;
        }

    protected:
        virtual void do_read()
        {
            auto self(shared_from_this());
            socket_.async_read_some(boost::asio::buffer(read_buffer_, read_buffer_.size()), [this, self](boost::system::error_code ec, std::size_t length) {
                if (!ec)
                {
                    monitor_->trigger(monitor_->on_recv, self, std::string_view(read_buffer_.data(), length));
                    do_read();
                }
                else
                {
                    monitor_->trigger(monitor_->on_disconnect, self);
                }
                });
        }

        virtual void do_write()
        {
            auto self(shared_from_this());
            {
                std::scoped_lock lock(mutex_);
                if (write_msgs_.empty())
                {
                    return;
                }
                write_buffer_ = write_msgs_.front();
                write_msgs_.pop_front();
            }
            boost::asio::async_write(socket_, boost::asio::buffer(write_buffer_.data(), write_buffer_.length()),
                [this, self](boost::system::error_code ec, std::size_t /*length*/) {
                    if (!ec)
                    {
                        do_write();
                    }
                    else
                    {
                        monitor_->trigger(monitor_->on_disconnect, self);
                    }
                });
        }

    protected:
        tcp::socket socket_;
        std::shared_ptr<detail::monitor> monitor_{ nullptr };

        std::string read_buffer_;
        std::string write_buffer_;
        std::mutex mutex_;
        std::deque<std::string> write_msgs_;
    };

    template <class Session>
    class tcp_server
    {
        friend typename Session;

    public:
        tcp_server(boost::asio::io_context& io, const tcp::endpoint& ep)
            : acceptor_(io, ep, true)
            , monitor_(std::make_shared<detail::monitor>())
        {
        }

        tcp_server(boost::asio::io_context& io, short port)
            : tcp_server(io, tcp::endpoint(tcp::v4(), port))
        {
        }

        tcp_server(boost::asio::io_context& io, const std::string_view& host, short port)
            : tcp_server(io, tcp::endpoint(boost::asio::ip::address::from_string(host.data()), port))
        {
        }

        ~tcp_server()
        {
        }

    public:
        tcp_server& on_connect(const std::function<void(const std::shared_ptr<Session>&)>& func)
        {
            monitor_->on_connect = [func = std::move(func)](auto&& ss) {
                func(std::dynamic_pointer_cast<Session>(ss));
            };
            return *this;
        }

        tcp_server& on_disconnect(const std::function<void(const std::shared_ptr<Session>&)>& func)
        {
            monitor_->on_disconnect = [func = std::move(func)](auto&& ss) {
                func(std::dynamic_pointer_cast<Session>(ss));
            };
            return *this;
        }

        tcp_server& on_received(const std::function<void(const std::shared_ptr<Session>&, const std::string_view&)>& func)
        {
            monitor_->on_recv = [func = std::move(func)](auto&& ss, auto&& msg) {
                func(std::dynamic_pointer_cast<Session>(ss), msg);
            };
            return *this;
        }

        void start()
        {
            do_accept();
        }

    private:
        void do_accept()
        {
            acceptor_.async_accept(
                [this](boost::system::error_code ec, tcp::socket socket) {
                    if (!ec)
                    {
                        std::make_shared<Session>(std::move(socket), monitor_)->start();
                    }
                    do_accept();
                }
            );
        }

    private:
        tcp::acceptor acceptor_;
        std::shared_ptr<detail::monitor> monitor_{ nullptr };
    };
}  // namespace detail

using detail::tcp_session;
using tcp_server = detail::tcp_server<detail::tcp_session>;
