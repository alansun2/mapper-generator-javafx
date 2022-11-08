package com.alan344.componet;

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
public class FileSelectText extends HBox {

    private final TextField textField;
    private final Button button;

    public FileSelectText() {
        this("浏览", null);
    }


    public FileSelectText(String btnName, String initText) {
        textField = new TextField(initText);
        textField.focusedProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue) {
                this.setStyle("-fx-border-width: 1;" +
                        "-fx-border-color: #38b1da;");
            } else {
                this.setStyle("-fx-border-width: 1;" +
                        "-fx-border-color: #BABABA;");
            }
        });
        textField.setStyle("-fx-background-insets: 0");
        textField.prefHeightProperty().bind(this.heightProperty());
        textField.prefWidthProperty().bind(this.widthProperty().subtract(64));
        button = new Button(btnName);
        button.setStyle("-fx-background-insets: 0");
        button.setPrefWidth(64);
        button.prefHeightProperty().bind(this.heightProperty());
        this.getChildren().addAll(textField, button);
        this.setAlignment(Pos.CENTER);
        this.setStyle("-fx-border-width: 1;" +
                "-fx-border-color: #BABABA;");
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
