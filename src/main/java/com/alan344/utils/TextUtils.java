package com.alan344.utils;

import com.alan344happyframework.util.StringUtils;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

/**
 * @author AlanSun
 * @date 2019/8/8 18:17
 */
public class TextUtils {
    public static void checkText(Stage primaryStage, TextField textField) {
        String text = textField.getText();
        if (StringUtils.isEmpty(text)) {
            Toast.Companion.makeText(primaryStage, "不能为空", 3000, 500, 500, 15, 5);
        }
    }

}
