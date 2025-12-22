package com.alan344.component;

import com.jfoenix.controls.JFXToggleButton;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import lombok.Getter;

import java.util.function.Consumer;

/**
 * @author AlanSun
 * @since 2022/8/21 15:01
 */
public class FileSelectTextToggleHBox extends HBox {

    @Getter
    private final TextField textField;
    private final Button button;

    public FileSelectTextToggleHBox(String btnName, SimpleBooleanProperty curToggle, String initText) {
        JFXToggleButton jfxToggleButton = new JFXToggleButton();
        jfxToggleButton.setSize(7);
        jfxToggleButton.setPadding(new Insets(-25, 0, -25, 0));
        jfxToggleButton.setSelected(curToggle.get());
        jfxToggleButton.setStyle("-fx-background-color: blue; -fx-border-width: 1");
        curToggle.bindBidirectional(jfxToggleButton.selectedProperty());

        button = new Button(btnName);
        button.setPrefWidth(64);
        button.prefHeightProperty().bind(this.heightProperty());

        textField = new TextField(initText);
        textField.prefHeightProperty().bind(this.heightProperty());
        textField.prefWidthProperty().bind(this.widthProperty().subtract(64 + 28));

        this.getChildren().addAll(textField, button, jfxToggleButton);
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

    public final void disable(boolean disable) {
        this.textField.setDisable(disable);
        this.button.setDisable(disable);
    }
}
