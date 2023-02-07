package com.alan344.factory;

import com.jfoenix.controls.JFXAlert;
import com.jfoenix.controls.JFXDialog;
import com.jfoenix.controls.JFXDialogLayout;
import javafx.scene.Node;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.layout.StackPane;
import javafx.scene.text.Text;
import javafx.stage.Stage;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2019/8/23 14:18
 */
public class DialogUtils {
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

        // dialog.setTitle("关闭");
        //
        // dialog.setContentText("确认关闭吗？");
        //
        // dialog.initOwner(primaryStage);
        //
        // dialog.getDialogPane().getButtonTypes().add(ButtonType.APPLY);
        // dialog.getDialogPane().getButtonTypes().add(ButtonType.CANCEL);
        //
        // dialog.getDialogPane().setPrefSize(350, 100);
        //
        // Optional<ButtonType> s = dialog.showAndWait();
        // s.ifPresent(s1 -> {
        //     if (s1.equals(ButtonType.APPLY)) {
        //         primaryStage.close();
        //     } else if (s1.equals(ButtonType.CLOSE)) {
        //         dialog.close();
        //     }
        // });
    }

    public static void successDialog(Stage primaryStage, String content, Node... btns) {
        Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
        // alert.setSize(350, 130);

        JFXDialogLayout jfxDialogLayout = new JFXDialogLayout();
        jfxDialogLayout.getStylesheets().add("css/common.css");
        // jfxDialogLayout.setStyle("-fx-background-color: #f1efef");
        jfxDialogLayout.setBody(new Text(content));
        Button okBtn = new Button("确定");
        okBtn.setOnAction(event -> alert.close());
        final ArrayList<Node> collect = Arrays.stream(btns).collect(Collectors.toCollection(ArrayList::new));
        collect.add(okBtn);
        jfxDialogLayout.setActions(collect);

        alert.setGraphic(jfxDialogLayout);
        alert.show();
    }
}
