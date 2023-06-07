// Copyright 2020 Kalkidan Betre Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package com.alan344.component.windows.theme;


import javafx.scene.paint.Color;

public interface Theme {
    Color getFrameBorderColor();

    Color getDefaultBackgroundColor();

    Color getDefaultForegroundColor();

    Color getLightForegroundColor();

    Color getDefaultButtonHoverColor();

    Color getDefaultButtonPressedColor();

    Color getCloseButtonHoverColor();

    Color getCloseButtonPressedColor();
}