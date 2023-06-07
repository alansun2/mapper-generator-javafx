package com.alan344.factory;

import com.alan344.component.FileOpenAndSuccessDialog;
import com.alan344.component.SuccessDialog;
import com.jfoenix.controls.JFXDialog;
import com.jfoenix.controls.JFXDialogLayout;
import javafx.scene.control.Button;
import javafx.scene.image.ImageView;
import javafx.scene.layout.StackPane;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import org.controlsfx.dialog.ExceptionDialog;

/**
 * @author AlanSun
 * @date 2019/8/23 14:18
 */
public class DialogFactory {
    public static void closeDialog(Stage stage, StackPane stackPane) {
        JFXDialog dialog = new JFXDialog();

        JFXDialogLayout jfxDialogLayout = new JFXDialogLayout();
        jfxDialogLayout.setPrefWidth(350);
        jfxDialogLayout.setPrefHeight(130);
        jfxDialogLayout.getStylesheets().add("css/common.css");
        jfxDialogLayout.setStyle("-fx-background-color: #f1efef");
        jfxDialogLayout.setBody(new Text("确定关闭吗？"));
        Button button = new Button("确定");
        button.setOnAction(event -> stage.close());
        jfxDialogLayout.setActions(button);

        dialog.setDialogContainer(stackPane);
        dialog.setContent(jfxDialogLayout);
        dialog.setTransitionType(JFXDialog.DialogTransition.CENTER);
        dialog.show();
    }

    public static void successDialog(Stage primaryStage, String textContent) {
        SuccessDialog successDialog = new SuccessDialog(primaryStage, textContent);
    }

    public static void successAndOpenFileDialog(Stage primaryStage, String textContent, String path) {
        FileOpenAndSuccessDialog successDialog = new FileOpenAndSuccessDialog(primaryStage, textContent, path);
    }

    public static void exceptionDialog(Throwable e) {
        ExceptionDialog exceptionDialog = new ExceptionDialog(e);
        exceptionDialog.setWidth(700);
        exceptionDialog.setGraphic(new ImageView("/image/icon.png"));
        exceptionDialog.setHeaderText(e.getMessage());
        exceptionDialog.show();
    }
}
