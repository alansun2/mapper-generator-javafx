package com.alan344.utils;

import com.alan344happyframework.util.StringUtils;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

/**
 * @author AlanSun
 * @date 2019/8/8 18:17
 */
public class TextUtils {

    private static boolean checkText(Stage primaryStage, TextField textField) {
        String text = textField.getText();
        if (StringUtils.isEmpty(text)) {
            Toast.makeText(primaryStage, textField.getPromptText() + "不能为空", 3000, 500, 500, 15, 5);
            return false;
        } else {
            return true;
        }
    }

    /**
     * 检查多个文本框是否为空，如果为空则toast
     *
     * @param primaryStage primaryStage
     * @param textFields   文本框
     * @return true: 都不为空， false：有一个值为空
     */
    public static boolean checkTexts(Stage primaryStage, TextField... textFields) {
        for (TextField textField : textFields) {
            if (!checkText(primaryStage, textField)) {
                return false;
            }
        }
        return true;
    }

}
