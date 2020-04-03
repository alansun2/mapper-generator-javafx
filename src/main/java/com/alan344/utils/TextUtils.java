package com.alan344.utils;

import com.alan344happyframework.util.StringUtils;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

/**
 * @author AlanSun
 * @date 2019/8/8 18:17
 */
public class TextUtils {

    /**
     * 检查文本框是否为空
     *
     * @param primaryStage toast 的 stage
     * @param textField    要检查的文本框
     * @return true 为空; false 非空
     */
    private static boolean checkTextIsEmpty(Stage primaryStage, TextField textField) {
        String text = textField.getText();
        if (StringUtils.isEmpty(text)) {
            Toast.makeText(primaryStage, textField.getPromptText() + "不能为空", 3000, 500, 500, 15, 5);
            return true;
        } else {
            return false;
        }
    }

    /**
     * 检查多个文本框是否为空，如果有一个为空则toast
     *
     * @param primaryStage primaryStage
     * @param textFields   文本框
     * @return true: 都不为空， false：有一个值为空
     */
    public static boolean checkTextsHasEmpty(Stage primaryStage, TextField... textFields) {
        for (TextField textField : textFields) {
            if (!checkTextIsEmpty(primaryStage, textField)) {
                return false;
            }
        }
        return true;
    }

}
