#pragma once

#include <atldef.h>
#include <atlapp.h>
#include <atlframe.h>
#include <atlctrls.h>
#include <atldlgs.h>
#include <atlctrlw.h>
#include <atltheme.h>
#include <concepts>

namespace WTL
{
    template<class T> concept CanCreate = requires(T obj) { { obj.Create(NULL) } -> std::convertible_to<HWND>; };
    template<class T> concept CanCreateEx = requires(T obj) { { obj.CreateEx(NULL) }-> std::convertible_to<HWND>; };
    template<CanCreate T> HWND Create(T& t) { return t.Create(NULL); }
    template<CanCreateEx T> HWND Create(T& t) { return t.CreateEx(NULL); }

    template<class Win>
    static int Run(HINSTANCE hInstance, LPTSTR /*lpstrCmdLine*/ = NULL, int nCmdShow = SW_SHOWDEFAULT)
    {
        HRESULT res = ::CoInitialize(NULL);
        ATLASSERT(SUCCEEDED(res));

        ::DefWindowProc(NULL, 0, 0, 0L);
        WTL::AtlInitCommonControls(ICC_COOL_CLASSES | ICC_BAR_CLASSES);

        res = _Module.Init(NULL, hInstance);
        ATLASSERT(SUCCEEDED(res));

        CMessageLoop loop;
        _Module.AddMessageLoop(&loop);

        Win win;
        if (Create(win) == NULL)
        {
            ATLTRACE(_T("Main window creation failed!\n"));
            return 0;
        }
        win.ShowWindow(nCmdShow);

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
