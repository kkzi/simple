#pragma once

#include <atldef.h>
#include <atlapp.h>
#include <atlframe.h>
#include <atlctrls.h>
#include <atldlgs.h>
#include <atlctrlw.h>
#include <atltheme.h>

CAppModule app_;

namespace WTL
{

    template<class Win>
    int Run(HINSTANCE hInstance, LPTSTR /*lpstrCmdLine*/ = NULL, int nCmdShow = SW_SHOWDEFAULT)
    {
        HRESULT res = ::CoInitialize(NULL);
        ATLASSERT(SUCCEEDED(res));

        ::DefWindowProc(NULL, 0, 0, 0L);
        WTL::AtlInitCommonControls(ICC_COOL_CLASSES | ICC_BAR_CLASSES);

        res = app_.Init(NULL, hInstance);
        ATLASSERT(SUCCEEDED(res));

        CMessageLoop loop;
        app_.AddMessageLoop(&loop);

        Win win;
        if (win.CreateEx() == NULL)
        {
            ATLTRACE(_T("Main window creation failed!\n"));
            return 0;
        }
        win.ShowWindow(nCmdShow);

        int ret = loop.Run();
        app_.RemoveMessageLoop();
        app_.Term();

        ::CoUninitialize();
        return ret;
    }
}

namespace wtl = WTL;

template<class Win>
static int wtl_run(HINSTANCE hInstance, LPTSTR lpstrCmdLine = NULL, int nCmdShow = SW_SHOWDEFAULT)
{
    return wtl::Run<Win>(hInstance, lpstrCmdLine, nCmdShow);
}

