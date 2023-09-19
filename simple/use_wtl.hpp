#pragma once

#if defined _M_IX86
#pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='x86' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_IA64
#pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='ia64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_X64
#pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='amd64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#else
#pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")
#endif

#include <atldef.h>
#include <atlbase.h>
#include <atlapp.h>
#include <atlframe.h>
#include <atlctrls.h>
#include <atldlgs.h>
#include <atlctrlw.h>
#include <atltheme.h>
#include <concepts>


extern CAppModule _Module;

namespace WTL
{
    template<class T> concept CanCreate = requires(T obj) { { obj.Create(NULL) } -> std::convertible_to<HWND>; };
    template<class T> concept CanCreateEx = requires(T obj) { { obj.CreateEx(NULL) }-> std::convertible_to<HWND>; };


    template<class Win, class... Args>
    static int Run(HINSTANCE hInstance, int cmdshow, Args &&...args)
    {
        HRESULT res = ::CoInitialize(NULL);
        ATLASSERT(SUCCEEDED(res));

        ::DefWindowProc(NULL, 0, 0, 0L);
        AtlInitCommonControls(ICC_COOL_CLASSES | ICC_BAR_CLASSES | ICC_USEREX_CLASSES);

        res = _Module.Init(NULL, hInstance);
        ATLASSERT(SUCCEEDED(res));

        CMessageLoop loop;
        _Module.AddMessageLoop(&loop);

        Win win(std::forward<Args>(args)...);
        HWND hwnd = 0;
        if constexpr (CanCreateEx<Win>) {
            hwnd = win.CreateEx(NULL);
        }
        else if constexpr (CanCreate<Win>) {
            hwnd = win.Create(NULL);
        }
        if (hwnd == NULL)
        {
            ATLTRACE(_T("Main window creation failed!\n"));
            return 0;
        }
        win.ShowWindow(cmdshow);

        int ret = loop.Run();
        _Module.RemoveMessageLoop();
        _Module.Term();

        ::CoUninitialize();
        return ret;
    }

    static CMessageLoop* Loop()
    {
        auto loop = _Module.GetMessageLoop();
        ATLASSERT(loop != NULL);
        return loop;
    }
}

namespace wtl = WTL;

#define WTL_MAIN(__MAIN_WINDOW__) \
WTL::CAppModule _Module; \
int WINAPI _tWinMain(HINSTANCE hInstance, HINSTANCE, LPTSTR lpstrCmdLine, int nCmdShow)\
{\
    return wtl::Run<__MAIN_WINDOW__>(hInstance, lpstrCmdLine, nCmdShow);\
}
