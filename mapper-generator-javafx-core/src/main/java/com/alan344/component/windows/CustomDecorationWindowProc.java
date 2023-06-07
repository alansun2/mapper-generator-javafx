// Copyright 2020 Kalkidan Betre Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package com.alan344.component.windows;

import com.sun.jna.Native;
import com.sun.jna.platform.win32.BaseTSD;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinUser;
import com.sun.jna.win32.W32APIOptions;

import static com.sun.jna.platform.win32.WinUser.*;

public class CustomDecorationWindowProc implements WinUser.WindowProc {
    final int WM_NCCALCSIZE = 0x0083;
    final int WM_NCHITTEST = 0x0084;

    final User32Ex INSTANCEEx;
    WinDef.HWND hwnd = new WinDef.HWND();
    BaseTSD.LONG_PTR defWndProc;

    public CustomDecorationWindowProc() {
        INSTANCEEx = (User32Ex) Native.load("user32", User32Ex.class, W32APIOptions.DEFAULT_OPTIONS);
    }

    public void init(WinDef.HWND hwnd) {
        this.hwnd = hwnd;
        if (is64Bit())
            defWndProc = INSTANCEEx.SetWindowLongPtr(hwnd, User32Ex.GWLP_WNDPROC, this);
        else
            defWndProc = INSTANCEEx.SetWindowLong(hwnd, User32Ex.GWLP_WNDPROC, this);
        INSTANCEEx.SetWindowPos(hwnd, hwnd, 0, 0, 0, 0,
                SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_FRAMECHANGED);
    }

    @Override
    public LRESULT callback(HWND hwnd, int uMsg, WPARAM wparam, LPARAM lparam) {
        LRESULT lresult;
        switch (uMsg) {
            case WM_NCCALCSIZE:
                return new LRESULT(0);
            case WM_NCHITTEST:
                lresult = this.BorderLessHitTest(hwnd, uMsg, wparam, lparam);
                if (lresult.intValue() == new LRESULT(0).intValue()) {
                    return INSTANCEEx.CallWindowProc(defWndProc, hwnd, uMsg, wparam, lparam);
                }
                return lresult;
            case WM_DESTROY:
                if (is64Bit())
                    INSTANCEEx.SetWindowLongPtr(hwnd, User32Ex.GWLP_WNDPROC, defWndProc);
                else
                    INSTANCEEx.SetWindowLong(hwnd, User32Ex.GWLP_WNDPROC, defWndProc);
                return new LRESULT(0);
            default:
                lresult = INSTANCEEx.CallWindowProc(defWndProc, hwnd, uMsg, wparam, lparam);
                return lresult;
        }
    }

    LRESULT BorderLessHitTest(HWND hWnd, int message, WPARAM wParam, LPARAM lParam) {
        int borderOffset = CustomDecorationParameters.getMaximizedWindowFrameThickness();
        int borderThickness = CustomDecorationParameters.getFrameResizeBorderThickness();

        WinDef.POINT ptMouse = new WinDef.POINT();
        RECT rcWindow = new RECT();
        User32.INSTANCE.GetCursorPos(ptMouse);
        User32.INSTANCE.GetWindowRect(hWnd, rcWindow);

        int uRow = 1, uCol = 1;
        boolean fOnResizeBorder = false, fOnFrameDrag = false;

        int topOffset = CustomDecorationParameters.getTitleBarHeight() == 0 ? borderThickness : CustomDecorationParameters.getTitleBarHeight();
        if (ptMouse.y >= rcWindow.top && ptMouse.y < rcWindow.top + topOffset + borderOffset) {
            fOnResizeBorder = (ptMouse.y < (rcWindow.top + borderThickness));  // Top Resizing
            if (!fOnResizeBorder) {
                fOnFrameDrag = (ptMouse.y <= rcWindow.top + CustomDecorationParameters.getTitleBarHeight() + borderOffset)
                        && (ptMouse.x < (rcWindow.right - (CustomDecorationParameters.getControlBoxWidth()
                        + borderOffset + CustomDecorationParameters.getExtraRightReservedWidth())))
                        && (ptMouse.x > (rcWindow.left + CustomDecorationParameters.getIconWidth()
                        + borderOffset + CustomDecorationParameters.getExtraLeftReservedWidth()));
            }
            uRow = 0; // Top Resizing or Caption Moving
        } else if (ptMouse.y < rcWindow.bottom && ptMouse.y >= rcWindow.bottom - borderThickness)
            uRow = 2; // Bottom Resizing
        if (ptMouse.x >= rcWindow.left && ptMouse.x < rcWindow.left + borderThickness)
            uCol = 0; // Left Resizing
        else if (ptMouse.x < rcWindow.right && ptMouse.x >= rcWindow.right - borderThickness)
            uCol = 2; // Right Resizing

        final int HTTOPLEFT = 13, HTTOP = 12, HTCAPTION = 2, HTTOPRIGHT = 14, HTLEFT = 10, HTNOWHERE = 0,
                HTRIGHT = 11, HTBOTTOMLEFT = 16, HTBOTTOM = 15, HTBOTTOMRIGHT = 17, HTSYSMENU = 3;

        int[][] hitTests = {
                {HTTOPLEFT, fOnResizeBorder ? HTTOP : fOnFrameDrag ? HTCAPTION : HTNOWHERE, HTTOPRIGHT},
                {HTLEFT, HTNOWHERE, HTRIGHT},
                {HTBOTTOMLEFT, HTBOTTOM, HTBOTTOMRIGHT},
        };

        return new LRESULT(hitTests[uRow][uCol]);
    }

    public static final boolean is64Bit() {
        String model = System.getProperty("sun.arch.data.model",
                System.getProperty("com.ibm.vm.bitmode"));
        if (model != null) {
            return "64".equals(model);
        }
        return false;
    }
}