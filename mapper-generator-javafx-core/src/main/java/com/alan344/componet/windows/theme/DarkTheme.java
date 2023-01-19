// Copyright 2020 Kalkidan Betre Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package com.alan344.componet.windows.theme;


import javafx.scene.paint.Color;

public class DarkTheme implements Theme {
    @Override
    public Color getFrameBorderColor() {
        return new Color(0, 255, 255, 1);
    }

    @Override
    public Color getDefaultBackgroundColor() {
        return new Color(64, 64, 64, 1);
    }

    @Override
    public Color getDefaultForegroundColor() {
        return new Color(255, 255, 255, 1);
    }

    @Override
    public Color getLightForegroundColor() {
        return new Color(192, 192, 192, 1);
    }

    @Override
    public Color getDefaultButtonHoverColor() {
        return new Color(101, 101, 101, 1);
    }

    @Override
    public Color getDefaultButtonPressedColor() {
        return new Color(101, 101, 101, 1);
    }

    @Override
    public Color getCloseButtonHoverColor() {
        return new Color(232, 17, 35, 1);
    }

    @Override
    public Color getCloseButtonPressedColor() {
        return new Color(241, 112, 122, 1);
    }
}