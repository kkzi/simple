#pragma once

// Specify Win32xx specific predefinitions here
// #define NO_USING_NAMESPACE        // Don't use Win32xx namespace

#if defined(_MSC_VER) && _MSC_VER == 1200   // For Visual Studio 6
#pragma warning (disable : 4786)            // identifier was truncated
#pragma warning (disable : 4702)            // unreachable code (bugs in Microsoft's STL)
#endif

                                            // Rarely modified header files should be included here
#include <vector>                           // Add support for std::vector
#include <map>                              // Add support for std::map
#include <string>                           // Add support for std::string
#include <sstream>                          // Add support for stringstream
#include <cassert>                          // Add support for the assert macro
#include <cstdio>                           // Add support for C style printf, sprintf, etc.
#include <cstdlib>                          // Add support for C style character conversions atoi etc.
#include <tchar.h>                          // Add support for C style TCHARs.
#include <functional>
#include <memory>
#include <cassert>


#include "wxx/wxx_cstring.h"
#include "wxx/wxx_gdi.h"
#include "wxx/wxx_appcore.h"
#include "wxx/wxx_wincore.h"


namespace wxx
{
    class App final : public CWinApp
    {
    public:
        App(std::shared_ptr<CWnd> view)
            : CWinApp()
            , view_(view)
        {
            assert(view_);
        }

        ~App()
        {
        }

        App(const App&) = delete;
        App(App&&) = delete;
        App& operator=(const App&) = delete;
        App& operator=(App&&) = delete;

    protected:
        BOOL InitInstance() override
        {
            assert(view_);
            view_->Create();
            return TRUE;
        }

    private:
        std::shared_ptr<CWnd> view_{ nullptr };
    };
}
