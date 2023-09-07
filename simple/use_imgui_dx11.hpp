#pragma once

#include <imgui.h>
#include "imgui_impl_dx11.h"
#include "imgui_impl_win32.h"

#include <d3d11.h>
#include <dxgi1_4.h>
#include <functional>
#include <optional>
#include <string>
#include <tuple>
#include <unordered_map>

#pragma comment(lib, "dxguid.lib")
#pragma comment(lib, "dxgi.lib")
#pragma comment(lib, "d3d11.lib")

// Forward declare message handler from imgui_impl_win32.cpp
extern IMGUI_IMPL_API LRESULT ImGui_ImplWin32_WndProcHandler(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);

namespace ImGuiDx::detail
{
    enum class ImGuiTheme
    {
        DefaultDark,
        DefaultLight,
    };

    struct ImGuiFont
    {
        std::string Path{};
        float Pixel{ 16.0f };

        enum Range_
        {
            Default,
            Chinese,
        } Range{ Default };
    };

    struct RunOptions
    {
        HICON Icon{ 0 };
#ifdef UNICODE
        std::wstring Title
#else
        std::string Title
#endif
        { TEXT("Untitled") };

        int Width{ 960 };
        int Height{ 640 };
        int DwStyle{ WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU /*WS_OVERLAPPEDWINDOW */ };
        int CmdShow{ SW_SHOWDEFAULT };
        ImGuiTheme Theme{ ImGuiTheme::DefaultLight };
        std::optional<ImVec4> BackgroundColor;
        // std::optional<ImGuiFont> Font{ "C:/Windows/Fonts/segoeui.ttf" };
        std::optional<ImGuiFont> Font{ "C:/Windows/Fonts/calibri.ttf" };
    };

    // Data
    static ID3D11Device* g_pd3dDevice{ nullptr };
    static ID3D11DeviceContext* g_pd3dDeviceContext{ nullptr };
    static IDXGISwapChain* g_pSwapChain{ nullptr };
    static UINT g_ResizeWidth{ 0 }, g_ResizeHeight{ 0 };
    static ID3D11RenderTargetView* g_mainRenderTargetView{ nullptr };
    static std::unordered_map<uint32_t, WNDPROC> g_msg2proc{};

    static void CreateRenderTarget()
    {
        ID3D11Texture2D* pBackBuffer;
        g_pSwapChain->GetBuffer(0, IID_PPV_ARGS(&pBackBuffer));
        g_pd3dDevice->CreateRenderTargetView(pBackBuffer, nullptr, &g_mainRenderTargetView);
        pBackBuffer->Release();
    }

    static bool CreateDeviceD3D(HWND hWnd)
    {
        // Setup swap chain
        DXGI_SWAP_CHAIN_DESC sd;
        ZeroMemory(&sd, sizeof(sd));
        sd.BufferCount = 2;
        sd.BufferDesc.Width = 0;
        sd.BufferDesc.Height = 0;
        sd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
        sd.BufferDesc.RefreshRate.Numerator = 60;
        sd.BufferDesc.RefreshRate.Denominator = 1;
        sd.Flags = DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH;
        sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
        sd.OutputWindow = hWnd;
        sd.SampleDesc.Count = 1;
        sd.SampleDesc.Quality = 0;
        sd.Windowed = TRUE;
        sd.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;

        UINT createDeviceFlags = 0;
        //createDeviceFlags |= D3D11_CREATE_DEVICE_DEBUG;
        D3D_FEATURE_LEVEL featureLevel;
        const D3D_FEATURE_LEVEL featureLevelArray[2] = { D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_10_0, };
        HRESULT res = D3D11CreateDeviceAndSwapChain(nullptr, D3D_DRIVER_TYPE_HARDWARE, nullptr, createDeviceFlags, featureLevelArray, 2, D3D11_SDK_VERSION, &sd, &g_pSwapChain, &g_pd3dDevice, &featureLevel, &g_pd3dDeviceContext);
        if (res == DXGI_ERROR_UNSUPPORTED) // Try high-performance WARP software driver if hardware is not available.
            res = D3D11CreateDeviceAndSwapChain(nullptr, D3D_DRIVER_TYPE_WARP, nullptr, createDeviceFlags, featureLevelArray, 2, D3D11_SDK_VERSION, &sd, &g_pSwapChain, &g_pd3dDevice, &featureLevel, &g_pd3dDeviceContext);
        if (res != S_OK)
            return false;

        CreateRenderTarget();
        return true;
    }


    static void CleanupRenderTarget()
    {
        if (g_mainRenderTargetView) { g_mainRenderTargetView->Release(); g_mainRenderTargetView = nullptr; }
    }

    static void CleanupDeviceD3D()
    {
        CleanupRenderTarget();
        if (g_pSwapChain) { g_pSwapChain->Release(); g_pSwapChain = nullptr; }
        if (g_pd3dDeviceContext) { g_pd3dDeviceContext->Release(); g_pd3dDeviceContext = nullptr; }
        if (g_pd3dDevice) { g_pd3dDevice->Release(); g_pd3dDevice = nullptr; }
    }

    static LRESULT WINAPI WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
    {
        if (ImGui_ImplWin32_WndProcHandler(hWnd, msg, wParam, lParam)) return true;

        if (g_msg2proc.contains(msg))
        {
            return g_msg2proc.at(msg)(hWnd, msg, wParam, lParam);
        }

        return ::DefWindowProcW(hWnd, msg, wParam, lParam);
    }

    static std::tuple<HWND, WNDCLASSEX> CreateAppWindow(const RunOptions& opts)
    {
        WNDCLASSEX wc{
            sizeof(wc),
            CS_CLASSDC,
            WndProc,
            0L,
            0L,
            GetModuleHandle(NULL),
            NULL,
            NULL,
            NULL,
            NULL,
            TEXT("APP"),
            opts.Icon,
        };
        ::RegisterClassEx(&wc);

        int ScreenWidth = GetSystemMetrics(SM_CXSCREEN);
        int ScreenHeight = GetSystemMetrics(SM_CYSCREEN);
        int Left = (ScreenWidth - opts.Width) / 2;
        int Top = (ScreenHeight - opts.Height) / 2;

        HWND hwnd =
            ::CreateWindowEx(NULL, wc.lpszClassName, opts.Title.c_str(), opts.DwStyle, Left, Top, opts.Width, opts.Height, NULL, NULL, wc.hInstance, NULL);

        // Initialize Direct3D
        if (!CreateDeviceD3D(hwnd))
        {
            CleanupDeviceD3D();
            ::UnregisterClass(wc.lpszClassName, wc.hInstance);
            return { 0, wc };
        }

        // Show the window
        ::ShowWindow(hwnd, opts.CmdShow);
        ::UpdateWindow(hwnd);

        return { hwnd, wc };
    }

    static void InitImGuiContext(HWND hwnd, const RunOptions& opts)
    {
        IMGUI_CHECKVERSION();
        ImGui_ImplWin32_EnableDpiAwareness();
        ImGui::CreateContext();
        ImGuiIO& io = ImGui::GetIO();
        io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;  // Enable Keyboard Controls
        io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;   // Enable Gamepad Controls
        io.IniFilename = NULL;

        // Setup Platform/Renderer backends
        ImGui_ImplWin32_Init(hwnd);
        ImGui_ImplDX11_Init(g_pd3dDevice, g_pd3dDeviceContext);

        // Load Fonts
        // - If no fonts are loaded, dear imgui will use the default font. You can also load multiple fonts and use ImGui::PushFont()/PopFont() to select them.
        // - AddFontFromFileTTF() will return the ImFont* so you can store it if you need to select the font among multiple.
        // - If the file cannot be loaded, the function will return NULL. Please handle those errors in your application (e.g. use an assertion, or display an
        // error and quit).
        // - The fonts will be rasterized at a given size (w/ oversampling) and stored into a texture when calling ImFontAtlas::Build()/GetTexDataAsXXXX(),
        // which ImGui_ImplXXXX_NewFrame below will call.
        // - Use '#define IMGUI_ENABLE_FREETYPE' in your imconfig file to use Freetype for higher quality font rendering.
        // - Read 'docs/FONTS.md' for more instructions and details.
        // - Remember that in C/C++ if you want to include a backslash \ in a string literal you need to write a double backslash \\ !
        // io.Fonts->AddFontDefault();
        // io.Fonts->AddFontFromFileTTF("c:\\Windows\\Fonts\\segoeui.ttf", 18.0f);
        // io.Fonts->AddFontFromFileTTF("../../misc/fonts/DroidSans.ttf", 16.0f);
        // io.Fonts->AddFontFromFileTTF("../../misc/fonts/Roboto-Medium.ttf", 16.0f);
        // io.Fonts->AddFontFromFileTTF("../../misc/fonts/Cousine-Regular.ttf", 15.0f);
        // ImFont* font = io.Fonts->AddFontFromFileTTF("c:\\Windows\\Fonts\\ArialUni.ttf", 18.0f, NULL, io.Fonts->GetGlyphRangesJapanese());
        // IM_ASSERT(font != NULL);
        if (!opts.Font)
        {
            return;
        }

        const ImWchar* range = nullptr;
        switch (opts.Font->Range)
        {
        case ImGuiFont::Chinese:
            range = io.Fonts->GetGlyphRangesChineseFull();
            break;
        default:
            break;
        }

        io.Fonts->AddFontFromFileTTF(opts.Font->Path.c_str(), opts.Font->Pixel, NULL, range);
        // IM_ASSERT(font != NULL);
    }

    static void InitStyle(const RunOptions& opts)
    {
        switch (opts.Theme)
        {
        case ImGuiTheme::DefaultDark:
            ImGui::StyleColorsDark();
            break;
        case ImGuiTheme::DefaultLight:
            ImGui::StyleColorsLight();
            break;
        default:
            break;
        }

        ImGuiStyle& style = ImGui::GetStyle();
        style.WindowBorderSize = 0.0f;
    }

    // Poll and handle messages (inputs, window resize, etc.)
    // See the WndProc() function below for our to dispatch events to the Win32 backend.
    static void PollMessageInLoop(bool& done)
    {
        MSG msg;
        while (::PeekMessage(&msg, NULL, 0U, 0U, PM_REMOVE))
        {
            ::TranslateMessage(&msg);
            ::DispatchMessage(&msg);
            if (msg.message == WM_QUIT)
            {
                done = true;
            }
        }
    }

    static void RenderImGui()
    {
        // Rendering
        ImGui::Render();

        constexpr ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);
        const float clear_color_with_alpha[4] = { clear_color.x * clear_color.w, clear_color.y * clear_color.w, clear_color.z * clear_color.w, clear_color.w };
        g_pd3dDeviceContext->OMSetRenderTargets(1, &g_mainRenderTargetView, nullptr);
        g_pd3dDeviceContext->ClearRenderTargetView(g_mainRenderTargetView, clear_color_with_alpha);
        ImGui_ImplDX11_RenderDrawData(ImGui::GetDrawData());

        g_pSwapChain->Present(1, 0); // Present with vsync
    }

    static void DestoryImGuiContext()
    {
        ImGui_ImplDX11_Shutdown();
        ImGui_ImplWin32_Shutdown();
        ImGui::DestroyContext();
    }

    static void DestoryAppWindow(HWND hwnd, const WNDCLASSEX& wc)
    {
        CleanupDeviceD3D();
        ::DestroyWindow(hwnd);
        ::UnregisterClass(wc.lpszClassName, wc.hInstance);
    }

    static void OnMessage(uint32_t msg, WNDPROC proc, bool overridded = false)
    {
        if (overridded || !g_msg2proc.contains(msg)) {
            g_msg2proc[msg] = proc;
        }
    }

    static void RegisterDefaultMsgProcs()
    {
        OnMessage(WM_SIZE,
            [](HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) -> LRESULT {
                if (g_pd3dDevice != NULL && wParam != SIZE_MINIMIZED)
                {
                    CleanupRenderTarget();
                    HRESULT result = g_pSwapChain->ResizeBuffers(
                        0, (UINT)LOWORD(lParam), (UINT)HIWORD(lParam), DXGI_FORMAT_UNKNOWN, DXGI_SWAP_CHAIN_FLAG_FRAME_LATENCY_WAITABLE_OBJECT);
                    assert(SUCCEEDED(result) && "Failed to resize swapchain.");
                    CreateRenderTarget();
                }
                return 0;
            });

        OnMessage(WM_SYSCOMMAND,
            [](HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) -> LRESULT {
                if ((wParam & 0xfff0) == SC_KEYMENU)  // Disable ALT application menu
                    return 0;
                return ::DefWindowProcW(hWnd, msg, wParam, lParam);
            });
        OnMessage(WM_DESTROY,
            [](HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) -> LRESULT {
                ::PostQuitMessage(0);
                return 0;
            });
    }

    // Main code
    static int Run(const RunOptions& opts, std::function<bool(HWND)> func)
    {
        RegisterDefaultMsgProcs();
        auto&& [hwnd, wc] = CreateAppWindow(opts);
        InitImGuiContext(hwnd, opts);
        InitStyle(opts);

        // Main loop
        bool done = false;
        while (!done)
        {
            PollMessageInLoop(done);
            if (done) break;

            // Handle window resize (we don't resize directly in the WM_SIZE handler)
            //if (g_ResizeWidth != 0 && g_ResizeHeight != 0)
            //{
            //    CleanupRenderTarget();
            //    g_pSwapChain->ResizeBuffers(0, g_ResizeWidth, g_ResizeHeight, DXGI_FORMAT_UNKNOWN, 0);
            //    g_ResizeWidth = g_ResizeHeight = 0;
            //    CreateRenderTarget();
            //}

            ImGui_ImplDX11_NewFrame();
            ImGui_ImplWin32_NewFrame();
            ImGui::NewFrame();

            auto viewport = ImGui::GetMainViewport();
            ImGui::SetNextWindowPos(viewport->WorkPos);
            ImGui::SetNextWindowSize(viewport->WorkSize);
            if (ImGui::Begin("MainWindow", 0, ImGuiWindowFlags_NoDecoration))
            {
                done = !func(hwnd);
                ImGui::End();
            }

            RenderImGui();
        }

        DestoryImGuiContext();
        DestoryAppWindow(hwnd, wc);

        return EXIT_SUCCESS;
    }
}

namespace ImGuiDx
{
    using detail::ImGuiTheme;
    using detail::OnMessage;
    using detail::Run;
    using detail::RunOptions;
}
