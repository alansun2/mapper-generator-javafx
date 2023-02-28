package com.alan344.componet;

import com.alan344.constants.NodeConstants;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;

import java.util.LinkedHashMap;
import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2023/2/28 16:13
 */
public class PropertyPane extends BorderPane {

    public static void open(LinkedHashMap<String, String> customProperties) {
        Stage stage = new Stage();

        BorderPane borderPane = new BorderPane();
        borderPane.getStyleClass().add("border-pane-padding");
        borderPane.getStylesheets().add("css/common.css");
        borderPane.setPrefWidth(400);
        borderPane.setPrefHeight(400);

        ListView<CustomPropertyHBox> lv = new ListView<>();

        customProperties.forEach((key, value) -> {
            CustomPropertyHBox customPropertyHBox = new CustomPropertyHBox(key, value);
            customPropertyHBox.delOnAction(event -> {
                lv.getItems().remove(customPropertyHBox);
            });
            lv.getItems().add(customPropertyHBox);
        });

        borderPane.setCenter(lv);

        // 按钮
        Button addBtn = new Button("添加");
        addBtn.setOnAction(event -> addCustomProperty(customPropertyHBox -> {
            lv.getItems().add(customPropertyHBox);
            customPropertyHBox.delOnAction(event1 -> lv.getItems().remove(customPropertyHBox));
        }));
        Button cancelBtn = new Button("取消");
        cancelBtn.setOnAction(event -> stage.close());
        Button applyBtn = new Button("应用");
        applyBtn.setOnAction(event -> {
            customProperties.clear();
            lv.getItems().forEach(customPropertyHBox -> customProperties.put(customPropertyHBox.getKey(), customPropertyHBox.getValue()));
            stage.close();
        });
        HBox btnHbox = new HBox(10, addBtn, cancelBtn, applyBtn);
        btnHbox.setAlignment(Pos.CENTER_RIGHT);

        borderPane.setBottom(btnHbox);

        stage.setScene(new Scene(borderPane));
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/icon.png"));
        stage.setTitle("设置自定义属性");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(NodeConstants.primaryStage);
        stage.show();
    }

    private static void addCustomProperty(Consumer<CustomPropertyHBox> consumer) {
        Stage stage = new Stage();

        BorderPane borderPane = new BorderPane();
        borderPane.getStyleClass().add("border-pane-padding");
        borderPane.getStylesheets().add("css/common.css");
        borderPane.setPrefWidth(400);
        borderPane.setPrefHeight(150);

        int labelWidth = 80;

        VBox vBox = new VBox();
        vBox.setSpacing(10);

        // key
        TextField keyTextField = new TextField();
        keyTextField.setPromptText("key");
        PropertyHBox keyHbox = new PropertyHBox("key", labelWidth, keyTextField);
        vBox.getChildren().add(keyHbox);

        // value
        TextField valueTextField = new TextField();
        valueTextField.setPromptText("value");
        PropertyHBox valueHbox = new PropertyHBox("value", labelWidth, valueTextField);
        vBox.getChildren().add(valueHbox);

        borderPane.setCenter(vBox);

        // 按钮
        Button cancelBtn = new Button("取消");
        cancelBtn.setOnAction(event -> stage.close());
        Button applyBtn = new Button("应用");
        applyBtn.setOnAction(event -> {
            CustomPropertyHBox customPropertyHBox = new CustomPropertyHBox(keyTextField.getText(), valueTextField.getText());
            consumer.accept(customPropertyHBox);
            stage.close();
        });
        HBox btnHbox = new HBox(10, cancelBtn, applyBtn);
        btnHbox.setAlignment(Pos.CENTER_RIGHT);

        borderPane.setBottom(btnHbox);

        stage.setScene(new Scene(borderPane));
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/icon.png"));
        stage.setTitle("编辑");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(NodeConstants.primaryStage);
        stage.show();
    }
}
