#pragma once

#include <Windows.h>
#include <functional>
#include <string>

struct create_window_option
{
    HINSTANCE inst{ 0 };
    HICON icon{ 0 };
#ifdef UNICODE
    std::wstring title{ TEXT("MainWindow") };
#else
    std::string title{ TEXT("MainWindow") };
#endif
    HBRUSH background{ (HBRUSH)COLOR_WINDOWFRAME };
    int style{ CS_HREDRAW | CS_VREDRAW };
    int cmdshow{ SW_SHOW };
    int width{ 1280 };
    int height{ 720 };
    WNDPROC process{ nullptr };
};

static HWND create_window(const create_window_option& opt)
{
    WNDCLASSEX wcex{  };
    wcex.cbSize = sizeof(WNDCLASSEX);
    wcex.hInstance = opt.inst;
    wcex.style = opt.style;
    wcex.lpfnWndProc = opt.process;
    wcex.hIcon = opt.icon;
    wcex.hIconSm = opt.icon;
    wcex.hbrBackground = opt.background;
    wcex.lpszClassName = opt.title.c_str();
    //wcex.lpszMenuName = NULL;
    //wcex.cbClsExtra = 0;
    //wcex.cbWndExtra = 0;
    //wcex.hCursor = 0;

    if (!RegisterClassEx(&wcex))
    {
        return 0;
    }

    HWND hWnd = CreateWindow(wcex.lpszClassName, opt.title.c_str(), WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, opt.width, opt.height, NULL, NULL, opt.inst, NULL);
    ShowWindow(hWnd, opt.cmdshow);
    UpdateWindow(hWnd);
    return hWnd;
}

static int exec_main_loop()
{
    MSG msg;
    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    return (int)msg.wParam;
}

