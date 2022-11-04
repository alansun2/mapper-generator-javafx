package com.alan344.utils;

import javafx.scene.control.Control;
import javafx.scene.control.Tooltip;

/**
 * @author AlanSun
 * @date 2022/11/4 14:15
 */
public class TooltipWrapper {

    public static <C extends Control> C wrap(C c, String text) {
        c.setTooltip(new Tooltip(text));
        return c;
    }
}
