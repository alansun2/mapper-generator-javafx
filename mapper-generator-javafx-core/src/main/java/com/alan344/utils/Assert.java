package com.alan344.utils;

import com.alan344.exception.BizException;
import javafx.stage.Stage;

/**
 * @author AlanSun
 * @since 2020/4/3 9:53
 */
public class Assert {
    public static void isTrue(boolean expression, String content, Stage primaryStage) {
        if (!expression) {
            Toast.makeTextDefault(primaryStage, content);
            throw new BizException(content);
        }
    }
}
