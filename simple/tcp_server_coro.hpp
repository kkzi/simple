#pragma once

#include <boost/asio/awaitable.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/read.hpp>
#include <boost/asio/redirect_error.hpp>
#include <boost/asio/write.hpp>
#include <deque>
#include <memory>
#include <string_view>
#include <utility>

namespace detail
{
    using namespace boost::asio;
    using boost::asio::ip::tcp;

    class tcp_session;

    struct monitor
    {
        std::function<void(const std::shared_ptr<tcp_session>&)> on_connect{ nullptr };
        std::function<void(const std::shared_ptr<tcp_session>&)> on_disconnect{ nullptr };
        std::function<void(const std::shared_ptr<tcp_session>&, const string_view&)> on_recv{ nullptr };

        template <class Func, class... Args> void trigger(Func&& func, Args &&...args)
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
        tcp_session(tcp::socket socket, const std::shared_ptr<monitor>& monitor, size_t capacity = 4096)
            : sock_(std::move(socket)), monitor_(monitor), timer_(sock_.get_executor())
        {
            timer_.expires_at(std::chrono::steady_clock::time_point::max());
            data_.resize(capacity);
        }

        virtual ~tcp_session()
        {
        }

    public:
        void start()
        {
            monitor_->trigger(monitor_->on_connect, shared_from_this());
            auto exec = sock_.get_executor();
            auto self = shared_from_this();
            co_spawn(
                exec, [self] { return self->reader(); }, detached);

            co_spawn(
                exec, [self] { return self->writer(); }, detached);
        }

        void send(const std::string_view& msg)
        {
            write_msgs_.push_back(msg.data());
            timer_.cancel_one();
        }

        std::string peer_address()
        {
            auto ep = sock_.remote_endpoint();
            return std::format("{}:{}", ep.address().to_string(), ep.port());
        }

        tcp::socket& socket()
        {
            return sock_;
        }

    protected:
        awaitable<void> reader()
        {
            for (std::vector<uint8_t> frame(4096);;)
            {
                try
                {
                    auto n = co_await sock_.async_read_some(buffer(frame), use_awaitable);
                    monitor_->trigger(monitor_->on_recv, shared_from_this(), std::string_view((char*)frame.data(), n));
                }
                catch (std::exception&)
                {
                    break;
                }
            }
            stop();
        }

        awaitable<void> writer()
        {
            while (sock_.is_open())
            {
                if (write_msgs_.empty())
                {
                    boost::system::error_code ec;
                    co_await timer_.async_wait(redirect_error(use_awaitable, ec));
                }
                else
                {
                    try
                    {
                        co_await async_write(sock_,
                            boost::asio::buffer(write_msgs_.front().data(), write_msgs_.front().length()),
                            use_awaitable);
                        write_msgs_.pop_front();
                    }
                    catch (const std::exception& /*e*/)
                    {
                        break;
                    }
                }
            }
            stop();
        }

        void stop()
        {
            sock_.close();
            timer_.cancel();
        }

    protected:
        tcp::socket sock_;
        steady_timer timer_;
        std::shared_ptr<monitor> monitor_{ nullptr };

        std::string data_;
        std::deque<std::string> write_msgs_;
    };

    template <class Session> class tcp_server
    {
        friend typename Session;

    public:
        tcp_server(boost::asio::io_context& io, const tcp::endpoint& ep)
            : acceptor_(io, ep), monitor_(std::make_shared<monitor>())
        {
        }

        tcp_server(boost::asio::io_context& io, short port) : tcp_server(io, tcp::endpoint(tcp::v4(), port))
        {
        }

        tcp_server(boost::asio::io_context& io, const std::string_view& host, short port)
            : tcp_server(io, tcp::endpoint(boost::asio::ip::address::from_string(host.data()), port))
        {
        }

    public:
        tcp_server& on_connect(const std::function<void(const std::shared_ptr<Session>&)>& func)
        {
            monitor_->on_connect = [func = std::move(func)](auto&& ss) { func(std::dynamic_pointer_cast<Session>(ss)); };
            return *this;
        }

        tcp_server& on_disconnect(const std::function<void(const std::shared_ptr<Session>&)>& func)
        {
            monitor_->on_disconnect = [func = std::move(func)](auto&& ss) { func(std::dynamic_pointer_cast<Session>(ss)); };
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
            co_spawn(
                acceptor_.get_executor(),
                [this]() -> awaitable<void> {
                    for (;;)
                    {
                        auto sock = co_await acceptor_.async_accept(use_awaitable);
                        std::make_shared<Session>(std::move(sock), monitor_)->start();
                    }
                },
                detached);
        }

    private:
        tcp::acceptor acceptor_;
        std::shared_ptr<monitor> monitor_{ nullptr };
    };
} // namespace detail

using detail::tcp_session;
using tcp_server = detail::tcp_server<detail::tcp_session>;
