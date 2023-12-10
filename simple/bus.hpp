#pragma once

#include <algorithm>
#include <cassert>
#include <execution>
#include <memory>
#include <mutex>
#include <unordered_map>

template <class T>
class bus
{
public:
    static constexpr auto ALL = "*";
    using processor = std::function<void(const T &)>;

private:
    struct recv_stub
    {
        size_t uid{ 0 };
        std::string topic;
        processor process{ nullptr };
    };

public:
    void send(const T &msg)
    {
        static auto dispatch = [this](auto &&topic, auto &&msg) {
            auto &&arr = topic2handlers_[topic];
            std::for_each(std::execution::par_unseq, std::begin(arr), std::end(arr), [&msg](auto &&recp) {
                recp->process(msg);
            });
        };
        std::scoped_lock lock(mutex_);
        dispatch(msg.topic, msg);
        dispatch(ALL, msg);
    }

    size_t registra(const std::string &topic, const processor &func)
    {
        assert(func != nullptr);
        static size_t uid = 0;
        auto recp = std::make_shared<recv_stub>(++uid, topic, func);
        std::scoped_lock lock(mutex_);
        topic2handlers_[recp->topic].push_back(recp);
        uid2handlers_[recp->uid] = recp;
        return recp->uid;
    }

    void unregistra(size_t uid)
    {
        std::scoped_lock lock(mutex_);
        if (!uid2handlers_.contains(uid))
        {
            return;
        }
        const auto &recp = uid2handlers_.at(uid);
        topic2handlers_.at(recp->topic).remove(recp);
        topic2handlers_.at(ALL).remove(recp);
        uid2handlers_.erase(uid);
    }

private:
    std::recursive_mutex mutex_;
    std::unordered_map<size_t, std::shared_ptr<recv_stub>> uid2handlers_;
    std::unordered_map<std::string, std::list<std::shared_ptr<recv_stub>>> topic2handlers_;
};
