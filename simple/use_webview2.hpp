#pragma once


#include <wrl.h>
#include <wil/com.h>
#include <WebView2/WebView2.h>
#include <string_view>
#include <functional>
#include <filesystem>

namespace wrl = Microsoft::WRL;

class WebViewAgent
{
public:
    using on_view_ready = std::function<void(const WebViewAgent&)>;

public:
    WebViewAgent(HWND hwnd)
        : hwnd_(hwnd)
    {
    }

public:
    void load_url(std::wstring_view url, on_view_ready ready = nullptr)
    {
        assert(!url.empty());
        initialize([url, ready](auto&& agent) {
            agent.view_->Navigate(url.data());
            if (ready)
            {
                ready(agent);
            }
            });
    }

    void load_html(std::wstring_view content, on_view_ready ready = nullptr)
    {
        assert(!content.empty());
        initialize([content, ready](auto&& agent) {
            agent.view_->NavigateToString(content.data());
            if (ready)
            {
                ready(agent);
            }
            });
    }

    void update_bounds(RECT bounds = { 0,0,0,0 })
    {
        if (bounds.bottom == bounds.top && bounds.left == bounds.right)
        {
            GetClientRect(hwnd_, &bounds);
        }
        ctrl_->put_Bounds(bounds);
    }

    wil::com_ptr<ICoreWebView2Controller> ctrl() const { return ctrl_; }
    wil::com_ptr<ICoreWebView2> view() const { return view_; }
    wil::com_ptr<ICoreWebView2Settings> settings() const { return settings_; };

private:
    void initialize(on_view_ready func)
    {
        auto udfpath = std::filesystem::temp_directory_path();
        CreateCoreWebView2EnvironmentWithOptions(nullptr, udfpath.c_str(), nullptr, wrl::Callback<ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler>(
            [this, func](HRESULT result, ICoreWebView2Environment* env) -> HRESULT {
                env->CreateCoreWebView2Controller(this->hwnd_, wrl::Callback<ICoreWebView2CreateCoreWebView2ControllerCompletedHandler>(
                    [this, func](HRESULT result, ICoreWebView2Controller* ctrl) -> HRESULT {
                        this->ctrl_ = ctrl;
                        if (ctrl != nullptr)
                        {
                            ctrl->get_CoreWebView2(&this->view_);
                        }
                        if (this->view_ != nullptr)
                        {
                            this->view_->get_Settings(&this->settings_);
                            this->settings_->put_IsScriptEnabled(TRUE);
                            this->settings_->put_AreDefaultScriptDialogsEnabled(TRUE);
                            this->settings_->put_IsWebMessageEnabled(TRUE);
                            this->update_bounds();
                            if (func)
                            {
                                func(*this);
                            }
                        }

                        return S_OK;
                    }).Get());
                return S_OK;
            }).Get());


        //if (!url_.empty())
        //{
        //    view_->Navigate(url_.data());
        //}

#if 0
        // <NavigationEvents>
        // Step 4 - Navigation events
        // register an ICoreWebView2NavigationStartingEventHandler to cancel any non-https navigation
        EventRegistrationToken token;
        view_->add_NavigationStarting(wrl::Callback<ICoreWebView2NavigationStartingEventHandler>(
            [](ICoreWebView2* webview, ICoreWebView2NavigationStartingEventArgs* args) -> HRESULT {
                wil::unique_cotaskmem_string uri;
                args->get_Uri(&uri);
                std::wstring source(uri.get());
                if (source.substr(0, 5) != L"https") {
                    args->put_Cancel(true);
                }
                return S_OK;
            }).Get(), &token);
        // </NavigationEvents>

        // <Scripting>
        // Step 5 - Scripting
        // Schedule an async task to add initialization script that freezes the Object object
        view_->AddScriptToExecuteOnDocumentCreated(L"Object.freeze(Object);", nullptr);
        // Schedule an async task to get the document URL
        view_->ExecuteScript(L"window.document.URL;", wrl::Callback<ICoreWebView2ExecuteScriptCompletedHandler>(
            [](HRESULT errorCode, LPCWSTR resultObjectAsJson) -> HRESULT {
                LPCWSTR URL = resultObjectAsJson;
                //doSomethingWithURL(URL);
                return S_OK;
            }).Get());
        // </Scripting>

        // <CommunicationHostWeb>
        // Step 6 - Communication between host and web content
        // Set an event handler for the host to return received message back to the web content
        view_->add_WebMessageReceived(wrl::Callback<ICoreWebView2WebMessageReceivedEventHandler>(
            [](ICoreWebView2* webview, ICoreWebView2WebMessageReceivedEventArgs* args) -> HRESULT {
                wil::unique_cotaskmem_string message;
                args->TryGetWebMessageAsString(&message);
                // processMessage(&message);
                webview->PostWebMessageAsString(message.get());
                return S_OK;
            }).Get(), &token);

        // Schedule an async task to add initialization script that
        // 1) Add an listener to print message from the host
        // 2) Post document URL to the host
        view_->AddScriptToExecuteOnDocumentCreated(
            L"window.chrome.webview.addEventListener(\'message\', event => alert(event.data));" \
            L"window.chrome.webview.postMessage(window.document.URL);",
            nullptr);
        // </CommunicationHostWeb>
#endif
    }

private:
    HWND hwnd_{ 0 };
    std::wstring url_;
    wil::com_ptr<ICoreWebView2> view_{ nullptr };
    wil::com_ptr<ICoreWebView2Controller> ctrl_{ nullptr };
    wil::com_ptr<ICoreWebView2Settings> settings_{ nullptr };
};

