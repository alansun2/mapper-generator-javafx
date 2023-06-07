package com.alan344.component;

import javafx.event.ActionEvent;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;

import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/8/21 15:01
 */
public class FileSelectTextHBox extends HBox {

    private final TextField textField;
    private final Button button;

    public FileSelectTextHBox() {
        this("浏览", null);
    }


    public FileSelectTextHBox(String btnName, String initText) {
        textField = new TextField(initText);
        textField.prefHeightProperty().bind(this.heightProperty());
        textField.prefWidthProperty().bind(this.widthProperty().subtract(64));

        button = new Button(btnName);
        button.getStylesheets().add("css/common.css");
        button.getStyleClass().add("mf-scan");
        button.setPrefWidth(64);
        button.prefHeightProperty().bind(this.heightProperty());

        this.getChildren().addAll(textField, button);
        this.setAlignment(Pos.CENTER);
        this.setStyle("-fx-border-width: 0; -fx-background-insets:0; -fx-background-color: #FFF");

        this.getStylesheets().add("/css/text-button.css");
    }

    public String getText() {
        return this.textField.getText();
    }

    public void setText(String text) {
        this.textField.setText(text);
    }

    public TextField getTextField() {
        return this.textField;
    }

    public void onAction(Consumer<ActionEvent> consumer) {
        this.button.setOnAction(consumer::accept);
    }

    public final String getPromptText() {
        return this.textField.getPromptText();
    }

    public final void setPromptText(String value) {
        this.textField.setPromptText(value);
    }

    public final void setTextTooltip(String textTooltip) {
        this.textField.setTooltip(new Tooltip(textTooltip));
    }
}
