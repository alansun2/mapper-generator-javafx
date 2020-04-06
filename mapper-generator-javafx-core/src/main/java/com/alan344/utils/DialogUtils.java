package com.alan344.utils;

import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.stage.Stage;

import java.util.Optional;

/**
 * @author AlanSun
 * @date 2019/8/23 14:18
 */
public class DialogUtils {
    public static void closeDialog(Stage primaryStage) {
        Dialog<ButtonType> dialog = new Dialog<>();

        dialog.setTitle("关闭");

        dialog.setContentText("确认关闭吗？");

        dialog.initOwner(primaryStage);

        dialog.getDialogPane().getButtonTypes().add(ButtonType.APPLY);
        dialog.getDialogPane().getButtonTypes().add(ButtonType.CLOSE);

        dialog.getDialogPane().setPrefSize(350, 100);

        Optional<ButtonType> s = dialog.showAndWait();
        s.ifPresent(s1 -> {
            if (s1.equals(ButtonType.APPLY)) {
                primaryStage.close();
            } else if (s1.equals(ButtonType.CLOSE)) {
                dialog.close();
            }
        });
    }

    public static void successDialog(Stage primaryStage) {
        Dialog<ButtonType> dialog = new Dialog<>();

        dialog.setTitle("导出成功");

        dialog.setContentText("导出成功");

        dialog.initOwner(primaryStage);

        dialog.getDialogPane().getButtonTypes().add(ButtonType.APPLY);
        dialog.getDialogPane().getButtonTypes().add(ButtonType.CLOSE);

        dialog.getDialogPane().setPrefSize(350, 100);

        Optional<ButtonType> s = dialog.showAndWait();
        s.ifPresent(s1 -> {
            if (s1.equals(ButtonType.APPLY)) {
                primaryStage.close();
            } else if (s1.equals(ButtonType.CLOSE)) {
                dialog.close();
            }
        });
    }
}
