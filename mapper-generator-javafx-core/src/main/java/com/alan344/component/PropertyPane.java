package com.alan344.component;

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
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

import java.util.LinkedHashMap;
import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2023/2/28 16:13
 */
public class PropertyPane extends BorderPane {

    private static Stage stage;

    private static BorderPane borderPane;

    private static Button addBtn, applyBtn;

    public static void open(LinkedHashMap<String, String> customProperties) {
        if (stage == null) {
            stage = new Stage();
            // 按钮
            addBtn = new Button("添加");
            Button cancelBtn = new Button("取消");
            cancelBtn.setOnAction(event -> stage.close());
            applyBtn = new Button("应用");
            applyBtn.getStyleClass().add("apply-btn");
            HBox btnHbox = new HBox(10, addBtn, cancelBtn, applyBtn);
            btnHbox.setAlignment(Pos.CENTER_RIGHT);

            borderPane = new BorderPane();
            borderPane.getStyleClass().add("border-pane-padding");
            borderPane.getStylesheets().add("css/common.css");
            borderPane.setPrefWidth(400);
            borderPane.setPrefHeight(400);
            borderPane.setBottom(btnHbox);

            stage.setScene(new Scene(borderPane));
            stage.setResizable(false);
            stage.getIcons().add(new Image("/image/icon.png"));
            stage.setTitle("设置自定义属性");
            stage.initModality(Modality.WINDOW_MODAL);
            stage.initOwner(NodeConstants.primaryStage);
        }
        ListView<CustomPropertyHBox> lv = new ListView<>();
        customProperties.forEach((key, value) -> {
            CustomPropertyHBox customPropertyHbox = new CustomPropertyHBox(key, value);
            customPropertyHbox.delOnAction(event -> {
                lv.getItems().remove(customPropertyHbox);
            });
            lv.getItems().add(customPropertyHbox);
        });
        borderPane.setCenter(lv);


        addBtn.setOnAction(event -> addCustomProperty(customPropertyHbox -> {
            lv.getItems().add(customPropertyHbox);
            customPropertyHbox.delOnAction(event1 -> lv.getItems().remove(customPropertyHbox));
        }));

        applyBtn.setOnAction(event -> {
            customProperties.clear();
            lv.getItems().forEach(customPropertyHbox -> customProperties.put(customPropertyHbox.getKey(),
                    customPropertyHbox.getValue()));
            stage.close();
        });
        stage.show();
    }

    private static Stage stage1;

    private static TextField keyTextField, valueTextField;

    private static Button applyBtn1;

    private static final ValidationSupport validationSupport = new ValidationSupport();

    private static void addCustomProperty(Consumer<CustomPropertyHBox> consumer) {
        if (stage1 == null) {
            validationSupport.setValidationDecorator(new StyleClassValidationDecoration());
            stage1 = new Stage();

            int labelWidth = 80;

            VBox vBox = new VBox();
            vBox.setSpacing(10);

            // key
            keyTextField = new TextField();
            keyTextField.setPromptText("key");
            PropertyHBox keyHbox = new PropertyHBox("key", labelWidth, keyTextField);
            validationSupport.registerValidator(keyTextField, Validator.createEmptyValidator("key不能为空"));
            vBox.getChildren().add(keyHbox);

            // value
            valueTextField = new TextField();
            valueTextField.setPromptText("value");
            PropertyHBox valueHbox = new PropertyHBox("value", labelWidth, valueTextField);
            validationSupport.registerValidator(valueTextField, Validator.createEmptyValidator("value不能为空"));
            vBox.getChildren().add(valueHbox);

            // 按钮
            Button cancelBtn = new Button("取消");
            cancelBtn.setOnAction(event -> stage1.close());
            applyBtn1 = new Button("应用");
            applyBtn1.getStyleClass().add("apply-btn");
            HBox btnHbox = new HBox(10, cancelBtn, applyBtn1);
            btnHbox.setAlignment(Pos.CENTER_RIGHT);

            BorderPane borderPane = new BorderPane();
            borderPane.getStyleClass().add("border-pane-padding");
            borderPane.getStylesheets().add("css/common.css");
            borderPane.setPrefWidth(350);
            borderPane.setPrefHeight(110);
            borderPane.setCenter(vBox);
            borderPane.setBottom(btnHbox);

            stage1.setScene(new Scene(borderPane));
            stage1.setResizable(false);
            stage1.getIcons().add(new Image("/image/icon.png"));
            stage1.setTitle("新增");
            stage1.initModality(Modality.WINDOW_MODAL);
            stage1.initOwner(NodeConstants.primaryStage);
        }
        keyTextField.setText(null);
        valueTextField.setText(null);
        applyBtn1.setOnAction(event -> {
            if (validationSupport.isInvalid()) {
                return;
            }
            CustomPropertyHBox customPropertyHbox = new CustomPropertyHBox(keyTextField.getText(),
                    valueTextField.getText());
            consumer.accept(customPropertyHbox);
            stage1.close();
        });
        stage1.show();
    }
}
