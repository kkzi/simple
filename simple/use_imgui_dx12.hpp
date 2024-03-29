#pragma once

#include <imgui.h>
#include "imgui_impl_dx12.h"
#include "imgui_impl_win32.h"

#include <d3d12.h>
#include <dxgi1_4.h>
#include <functional>
#include <optional>
#include <string>
#include <tuple>
#include <unordered_map>

#ifdef _DEBUG
#define DX12_ENABLE_DEBUG_LAYER
#endif

#ifdef DX12_ENABLE_DEBUG_LAYER
#include <dxgidebug.h>
#pragma comment(lib, "dxguid.lib")
#endif

#pragma comment(lib, "dxguid.lib")
#pragma comment(lib, "dxgi.lib")
#pragma comment(lib, "d3d12.lib")

// Forward declare message handler from imgui_impl_win32.cpp
extern IMGUI_IMPL_API LRESULT ImGui_ImplWin32_WndProcHandler(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);

namespace ImGuiDx12::detail
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
        int CmdShow{ SW_SHOWDEFAULT };
        ImGuiTheme Theme{ ImGuiTheme::DefaultLight };
        std::optional<ImVec4> BackgroundColor;
        // std::optional<ImGuiFont> Font{ "C:/Windows/Fonts/segoeui.ttf" };
        std::optional<ImGuiFont> Font{ "C:/Windows/Fonts/calibri.ttf" };
    };

    struct FrameContext
    {
        ID3D12CommandAllocator* CommandAllocator;
        UINT64 FenceValue;
    };

    // Data
    static int const NUM_FRAMES_IN_FLIGHT{ 3 };
    static FrameContext g_frameContext[NUM_FRAMES_IN_FLIGHT]{};
    static UINT g_frameIndex{ 0 };

    static int const NUM_BACK_BUFFERS{ 3 };
    static ID3D12Device* g_pd3dDevice{ NULL };
    static ID3D12DescriptorHeap* g_pd3dRtvDescHeap{ NULL };
    static ID3D12DescriptorHeap* g_pd3dSrvDescHeap{ NULL };
    static ID3D12CommandQueue* g_pd3dCommandQueue{ NULL };
    static ID3D12GraphicsCommandList* g_pd3dCommandList{ NULL };
    static ID3D12Fence* g_fence{ NULL };
    static HANDLE g_fenceEvent{ NULL };
    static UINT64 g_fenceLastSignaledValue{ 0 };
    static IDXGISwapChain3* g_pSwapChain{ NULL };
    static HANDLE g_hSwapChainWaitableObject{ NULL };
    static ID3D12Resource* g_mainRenderTargetResource[NUM_BACK_BUFFERS]{};
    static D3D12_CPU_DESCRIPTOR_HANDLE g_mainRenderTargetDescriptor[NUM_BACK_BUFFERS]{};
    static std::unordered_map<uint32_t, WNDPROC> g_msg2proc{};

    static void CreateRenderTarget()
    {
        for (UINT i = 0; i < NUM_BACK_BUFFERS; i++)
        {
            ID3D12Resource* pBackBuffer = NULL;
            g_pSwapChain->GetBuffer(i, IID_PPV_ARGS(&pBackBuffer));
            g_pd3dDevice->CreateRenderTargetView(pBackBuffer, NULL, g_mainRenderTargetDescriptor[i]);
            g_mainRenderTargetResource[i] = pBackBuffer;
        }
    }

    static bool CreateDeviceD3D(HWND hWnd)
    {
        // Setup swap chain
        DXGI_SWAP_CHAIN_DESC1 sd;
        {
            ZeroMemory(&sd, sizeof(sd));
            sd.BufferCount = NUM_BACK_BUFFERS;
            sd.Width = 0;
            sd.Height = 0;
            sd.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
            sd.Flags = DXGI_SWAP_CHAIN_FLAG_FRAME_LATENCY_WAITABLE_OBJECT;
            sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
            sd.SampleDesc.Count = 1;
            sd.SampleDesc.Quality = 0;
            sd.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD;
            sd.AlphaMode = DXGI_ALPHA_MODE_UNSPECIFIED;
            sd.Scaling = DXGI_SCALING_STRETCH;
            sd.Stereo = FALSE;
        }

        // [DEBUG] Enable debug interface
#ifdef DX12_ENABLE_DEBUG_LAYER
        ID3D12Debug* pdx12Debug = NULL;
        if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&pdx12Debug)))) pdx12Debug->EnableDebugLayer();
#endif

        // Create device
        D3D_FEATURE_LEVEL featureLevel = D3D_FEATURE_LEVEL_11_0;
        if (D3D12CreateDevice(NULL, featureLevel, IID_PPV_ARGS(&g_pd3dDevice)) != S_OK) return false;

        // [DEBUG] Setup debug interface to break on any warnings/errors
#ifdef DX12_ENABLE_DEBUG_LAYER
        if (pdx12Debug != NULL)
        {
            ID3D12InfoQueue* pInfoQueue = NULL;
            g_pd3dDevice->QueryInterface(IID_PPV_ARGS(&pInfoQueue));
            pInfoQueue->SetBreakOnSeverity(D3D12_MESSAGE_SEVERITY_ERROR, true);
            pInfoQueue->SetBreakOnSeverity(D3D12_MESSAGE_SEVERITY_CORRUPTION, true);
            pInfoQueue->SetBreakOnSeverity(D3D12_MESSAGE_SEVERITY_WARNING, true);
            pInfoQueue->Release();
            pdx12Debug->Release();
    }
#endif

        {
            D3D12_DESCRIPTOR_HEAP_DESC desc = {};
            desc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
            desc.NumDescriptors = NUM_BACK_BUFFERS;
            desc.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_NONE;
            desc.NodeMask = 1;
            if (g_pd3dDevice->CreateDescriptorHeap(&desc, IID_PPV_ARGS(&g_pd3dRtvDescHeap)) != S_OK) return false;

            SIZE_T rtvDescriptorSize = g_pd3dDevice->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
            D3D12_CPU_DESCRIPTOR_HANDLE rtvHandle = g_pd3dRtvDescHeap->GetCPUDescriptorHandleForHeapStart();
            for (UINT i = 0; i < NUM_BACK_BUFFERS; i++)
            {
                g_mainRenderTargetDescriptor[i] = rtvHandle;
                rtvHandle.ptr += rtvDescriptorSize;
            }
        }

        {
            D3D12_DESCRIPTOR_HEAP_DESC desc = {};
            desc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
            desc.NumDescriptors = 1;
            desc.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
            if (g_pd3dDevice->CreateDescriptorHeap(&desc, IID_PPV_ARGS(&g_pd3dSrvDescHeap)) != S_OK) return false;
        }

        {
            D3D12_COMMAND_QUEUE_DESC desc = {};
            desc.Type = D3D12_COMMAND_LIST_TYPE_DIRECT;
            desc.Flags = D3D12_COMMAND_QUEUE_FLAG_NONE;
            desc.NodeMask = 1;
            if (g_pd3dDevice->CreateCommandQueue(&desc, IID_PPV_ARGS(&g_pd3dCommandQueue)) != S_OK) return false;
        }

        for (UINT i = 0; i < NUM_FRAMES_IN_FLIGHT; i++)
            if (g_pd3dDevice->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&g_frameContext[i].CommandAllocator)) != S_OK) return false;

        if (g_pd3dDevice->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT, g_frameContext[0].CommandAllocator, NULL, IID_PPV_ARGS(&g_pd3dCommandList)) !=
            S_OK ||
            g_pd3dCommandList->Close() != S_OK)
            return false;

        if (g_pd3dDevice->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&g_fence)) != S_OK) return false;

        g_fenceEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
        if (g_fenceEvent == NULL) return false;

        {
            IDXGIFactory4* dxgiFactory = NULL;
            IDXGISwapChain1* swapChain1 = NULL;
            if (CreateDXGIFactory1(IID_PPV_ARGS(&dxgiFactory)) != S_OK) return false;
            if (dxgiFactory->CreateSwapChainForHwnd(g_pd3dCommandQueue, hWnd, &sd, NULL, NULL, &swapChain1) != S_OK) return false;
            if (swapChain1->QueryInterface(IID_PPV_ARGS(&g_pSwapChain)) != S_OK) return false;
            swapChain1->Release();
            dxgiFactory->Release();
            g_pSwapChain->SetMaximumFrameLatency(NUM_BACK_BUFFERS);
            g_hSwapChainWaitableObject = g_pSwapChain->GetFrameLatencyWaitableObject();
        }

        CreateRenderTarget();
        return true;
}

    static void WaitForLastSubmittedFrame()
    {
        FrameContext* frameCtx = &g_frameContext[g_frameIndex % NUM_FRAMES_IN_FLIGHT];

        UINT64 fenceValue = frameCtx->FenceValue;
        if (fenceValue == 0) return;  // No fence was signaled

        frameCtx->FenceValue = 0;
        if (g_fence->GetCompletedValue() >= fenceValue) return;

        g_fence->SetEventOnCompletion(fenceValue, g_fenceEvent);
        WaitForSingleObject(g_fenceEvent, INFINITE);
    }

    static void CleanupRenderTarget()
    {
        WaitForLastSubmittedFrame();

        for (UINT i = 0; i < NUM_BACK_BUFFERS; i++)
            if (g_mainRenderTargetResource[i])
            {
                g_mainRenderTargetResource[i]->Release();
                g_mainRenderTargetResource[i] = NULL;
            }
    }

    static void CleanupDeviceD3D()
    {
        CleanupRenderTarget();
        if (g_pSwapChain)
        {
            g_pSwapChain->SetFullscreenState(false, NULL);
            g_pSwapChain->Release();
            g_pSwapChain = NULL;
        }
        if (g_hSwapChainWaitableObject != NULL)
        {
            CloseHandle(g_hSwapChainWaitableObject);
        }
        for (UINT i = 0; i < NUM_FRAMES_IN_FLIGHT; i++)
            if (g_frameContext[i].CommandAllocator)
            {
                g_frameContext[i].CommandAllocator->Release();
                g_frameContext[i].CommandAllocator = NULL;
            }
        if (g_pd3dCommandQueue)
        {
            g_pd3dCommandQueue->Release();
            g_pd3dCommandQueue = NULL;
        }
        if (g_pd3dCommandList)
        {
            g_pd3dCommandList->Release();
            g_pd3dCommandList = NULL;
        }
        if (g_pd3dRtvDescHeap)
        {
            g_pd3dRtvDescHeap->Release();
            g_pd3dRtvDescHeap = NULL;
        }
        if (g_pd3dSrvDescHeap)
        {
            g_pd3dSrvDescHeap->Release();
            g_pd3dSrvDescHeap = NULL;
        }
        if (g_fence)
        {
            g_fence->Release();
            g_fence = NULL;
        }
        if (g_fenceEvent)
        {
            CloseHandle(g_fenceEvent);
            g_fenceEvent = NULL;
        }
        if (g_pd3dDevice)
        {
            g_pd3dDevice->Release();
            g_pd3dDevice = NULL;
        }

#ifdef DX12_ENABLE_DEBUG_LAYER
        IDXGIDebug1* pDebug = NULL;
        if (SUCCEEDED(DXGIGetDebugInterface1(0, IID_PPV_ARGS(&pDebug))))
        {
            pDebug->ReportLiveObjects(DXGI_DEBUG_ALL, DXGI_DEBUG_RLO_SUMMARY);
            pDebug->Release();
        }
#endif
    }

    static FrameContext* WaitForNextFrameResources()
    {
        UINT nextFrameIndex = g_frameIndex + 1;
        g_frameIndex = nextFrameIndex;

        HANDLE waitableObjects[] = { g_hSwapChainWaitableObject, NULL };
        DWORD numWaitableObjects = 1;

        FrameContext* frameCtx = &g_frameContext[nextFrameIndex % NUM_FRAMES_IN_FLIGHT];
        UINT64 fenceValue = frameCtx->FenceValue;
        if (fenceValue != 0)  // means no fence was signaled
        {
            frameCtx->FenceValue = 0;
            g_fence->SetEventOnCompletion(fenceValue, g_fenceEvent);
            waitableObjects[1] = g_fenceEvent;
            numWaitableObjects = 2;
        }

        WaitForMultipleObjects(numWaitableObjects, waitableObjects, TRUE, INFINITE);

        return frameCtx;
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
            ::CreateWindow(wc.lpszClassName, opts.Title.c_str(), WS_OVERLAPPEDWINDOW, Left, Top, opts.Width, opts.Height, NULL, NULL, wc.hInstance, NULL);

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
        ImGui_ImplDX12_Init(g_pd3dDevice, NUM_FRAMES_IN_FLIGHT, DXGI_FORMAT_R8G8B8A8_UNORM, g_pd3dSrvDescHeap,
            g_pd3dSrvDescHeap->GetCPUDescriptorHandleForHeapStart(), g_pd3dSrvDescHeap->GetGPUDescriptorHandleForHeapStart());

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

        FrameContext* frameCtx = WaitForNextFrameResources();
        UINT backBufferIdx = g_pSwapChain->GetCurrentBackBufferIndex();
        frameCtx->CommandAllocator->Reset();

        D3D12_RESOURCE_BARRIER barrier = {};
        barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        barrier.Flags = D3D12_RESOURCE_BARRIER_FLAG_NONE;
        barrier.Transition.pResource = g_mainRenderTargetResource[backBufferIdx];
        barrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        barrier.Transition.StateBefore = D3D12_RESOURCE_STATE_PRESENT;
        barrier.Transition.StateAfter = D3D12_RESOURCE_STATE_RENDER_TARGET;
        g_pd3dCommandList->Reset(frameCtx->CommandAllocator, NULL);
        g_pd3dCommandList->ResourceBarrier(1, &barrier);

        // Render Dear ImGui graphics
        constexpr static ImVec4 BgColor{ 0.45f, 0.55f, 0.60f, 1.00f };
        constexpr float ClearColor[4] = {
            BgColor.x * BgColor.w,
            BgColor.y * BgColor.w,
            BgColor.z * BgColor.w,
            BgColor.w,
        };
        g_pd3dCommandList->ClearRenderTargetView(g_mainRenderTargetDescriptor[backBufferIdx], ClearColor, 0, NULL);
        g_pd3dCommandList->OMSetRenderTargets(1, &g_mainRenderTargetDescriptor[backBufferIdx], FALSE, NULL);
        g_pd3dCommandList->SetDescriptorHeaps(1, &g_pd3dSrvDescHeap);
        ImGui_ImplDX12_RenderDrawData(ImGui::GetDrawData(), g_pd3dCommandList);
        barrier.Transition.StateBefore = D3D12_RESOURCE_STATE_RENDER_TARGET;
        barrier.Transition.StateAfter = D3D12_RESOURCE_STATE_PRESENT;
        g_pd3dCommandList->ResourceBarrier(1, &barrier);
        g_pd3dCommandList->Close();

        g_pd3dCommandQueue->ExecuteCommandLists(1, (ID3D12CommandList* const*)&g_pd3dCommandList);

        g_pSwapChain->Present(1, 0);  // Present with vsync
        // g_pSwapChain->Present(0, 0); // Present without vsync

        UINT64 fenceValue = g_fenceLastSignaledValue + 1;
        g_pd3dCommandQueue->Signal(g_fence, fenceValue);
        g_fenceLastSignaledValue = fenceValue;
        frameCtx->FenceValue = fenceValue;
    }

    static void DestoryImGuiContext()
    {
        ImGui_ImplDX12_Shutdown();
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

            ImGui_ImplDX12_NewFrame();
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

        WaitForLastSubmittedFrame();
        DestoryImGuiContext();
        DestoryAppWindow(hwnd, wc);

        return EXIT_SUCCESS;
    }
}  // namespace ImGuiDx12::detail

namespace ImGuiDx12
{
    using detail::ImGuiTheme;
    using detail::OnMessage;
    using detail::Run;
    using detail::RunOptions;
}  // namespace ImGuiDx12
