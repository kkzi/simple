#pragma once

#if not defined(FMT_HEADER_ONLY)
#define FMT_HEADER_ONLY
#endif
#if not defined(SPDLOG_FMT_EXTERNAL)
#define SPDLOG_FMT_EXTERNAL
#endif
#if not defined(SPDLOG_FMT_EXTERNAL_HO)
#define SPDLOG_FMT_EXTERNAL_HO
#endif

#define SPDLOG_ACTIVE_LEVEL SPDLOG_LEVEL_DEBUG

#include <spdlog/sinks/rotating_file_sink.h>
#include <spdlog/sinks/stdout_color_sinks.h>
#include <spdlog/spdlog.h>
#include <string_view>

static inline void init_logger(std::string_view path, std::string_view level = "debug")
{
    std::vector<spdlog::sink_ptr> sinks{std::make_shared<spdlog::sinks::stdout_color_sink_mt>()};
    if (!path.empty())
    {
        sinks.push_back(std::make_shared<spdlog::sinks::rotating_file_sink_mt>(SPDLOG_FILENAME_T(path.data()), 100 * 1024 * 1024, 10, true));
    }
    auto logger = std::make_shared<spdlog::logger>("default", sinks.begin(), sinks.end());

    // logger->set_pattern("[%^%L%$][%Y-%m-%d %H:%M:%S.%e] %v. [%s:%#, %t]");
    logger->set_pattern("[%Y-%m-%d %H:%M:%S.%e][%^%l%$] %v. [%s:%#, %t]");
    // glog/absl log patern
    // logger->set_pattern("%L%m%d %H:%M:%S.%f %7t %s:%#] %v");

    logger->set_level(spdlog::level::from_str(level.data()));
    logger->flush_on(spdlog::level::warn);

    spdlog::flush_every(std::chrono::seconds(2));
    spdlog::set_default_logger(logger);
}

#define LOG_DEBUG SPDLOG_DEBUG
#define LOG_INFO SPDLOG_INFO
#define LOG_WARN SPDLOG_WARN
#define LOG_ERROR SPDLOG_ERROR
#define LOG_FATAL SPDLOG_CRITICAL

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
