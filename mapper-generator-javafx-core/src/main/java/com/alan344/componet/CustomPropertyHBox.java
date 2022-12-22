package com.alan344.componet;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;

/**
 * @author AlanSun
 * @date 2022/12/21 18:58
 */
public class CustomPropertyHBox extends HBox {

    private final Button delBtn;

    private final Label keyLabel;

    private final TextField valueTF;

    public CustomPropertyHBox(String key, String value) {
        keyLabel = new Label(key);
        keyLabel.prefWidthProperty().bind(this.widthProperty().multiply(0.2));
        Label label = new Label(":");
        label.prefWidthProperty().bind(this.widthProperty().multiply(0.1));
        valueTF = new TextField(value);
        valueTF.prefWidthProperty().bind(this.widthProperty().multiply(0.45));
        delBtn = new Button("Del");
        delBtn.prefWidthProperty().bind(this.widthProperty().multiply(0.15));
        this.setSpacing(10);
        this.getChildren().addAll(keyLabel, label, valueTF, delBtn);
        this.setAlignment(Pos.CENTER);
    }

    public void delOnAction(EventHandler<ActionEvent> value) {
        this.delBtn.setOnAction(value);
    }

    public String getKey() {
        return keyLabel.getText();
    }

    public String getValue() {
        return valueTF.getText();
    }
}
