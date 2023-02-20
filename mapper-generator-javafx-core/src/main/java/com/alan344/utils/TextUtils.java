package com.alan344.utils;

import com.alan344.exception.BizException;
import javafx.scene.control.TextField;
import javafx.stage.Stage;
import com.alan344.utils.StringUtils;

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
     */
    private static void checkTextIsEmpty(Stage primaryStage, TextField textField) {
        String text = textField.getText();
        if (!StringUtils.isNotEmpty(text)) {
            Toast.makeText(primaryStage, textField.getPromptText() + "不能为空", 3000, 500, 500, 15, 5);
            throw new BizException(text + "不能为空");
        }
    }

    /**
     * 检查多个文本框是否为空，如果有一个为空则toast
     *
     * @param primaryStage primaryStage
     * @param textFields   文本框
     */
    public static void checkTextsHasEmpty(Stage primaryStage, TextField... textFields) {
        for (TextField textField : textFields) {
            checkTextIsEmpty(primaryStage, textField);
        }
    }
}
