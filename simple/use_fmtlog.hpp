#pragma once

#if not defined(FMT_HEADER_ONLY)
#define FMT_HEADER_ONLY
#endif

#include "fmtlog/fmtlog-inl.h"
#include <chrono>
#include <string_view>
#include <unordered_map>

static inline void init_logger(std::string_view path, std::string_view level = "debug")
{
    using namespace std::literals;

    if (!path.empty())
    {
        fmtlog::setLogFile(path.data(), true);
    }

    static std::unordered_map<std::string, fmtlog::LogLevel> str2level{
        {"debug", fmtlog::LogLevel::DBG},
        {"info", fmtlog::LogLevel::INF},
        {"warn", fmtlog::LogLevel::WRN},
        {"error", fmtlog::LogLevel::ERR},
    };
    fmtlog::setLogLevel(str2level.contains(level.data()) ? str2level.at(level.data()) : fmtlog::LogLevel::DBG);

    fmtlog::setHeaderPattern("{l} {YmdHMSe} {s}, {t} | ");
    auto delay = std::chrono::duration_cast<std::chrono::nanoseconds>(1s).count();
    // fmtlog::flushOn(fmtlog::LogLevel::DBG);
    // fmtlog::setFlushDelay((int64_t)delay);
    fmtlog::startPollingThread((int64_t)delay);
}

#define LOG_DEBUG logd
#define LOG_INFO logi
#define LOG_WARN logw
#define LOG_ERROR loge
#define LOG_FATAL loge

#define LOGD LOG_DEBUG
#define LOGI LOG_INFO
#define LOGW LOG_WARN
#define LOGE LOG_ERROR
#define LOGF LOG_FATAL

#define log_debug LOG_DEBUG
#define log_info LOG_INFO
#define log_warn LOG_WARN
#define log_error LOG_ERROR
#define log_fatal LOG_FATAL
