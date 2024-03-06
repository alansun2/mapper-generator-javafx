package com.alan344.component.dialog;

import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Paint;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import org.kordamp.ikonli.javafx.FontIcon;

/**
 * @author AlanSun
 * @date 2023/2/19 23:59
 */
public class SuccessDialog extends AlDialog {

    public SuccessDialog(Stage stage1, String title, String textContent) {
        super(stage1);
        this.setPrefHeight(150);
        this.setPrefWidth(300);
        this.actionHBox.setStyle("-fx-background-color: white");

        Button button = new Button("确定");
        button.setPrefWidth(60);
        button.setOnAction(event -> stage.close());
        this.setActions(button);

        final FontIcon fontIcon = new FontIcon("unis-check-circle:40:#81E069");

        Text text = new Text(textContent);
        text.setFont(Font.font(15));
        text.setFill(Paint.valueOf("#81E069"));

        HBox hBox = new HBox(10, fontIcon, text);
        hBox.setStyle("-fx-background-color: white;");
        hBox.setAlignment(Pos.CENTER);

        this.setCenter(hBox);

        this.stage.setTitle(title);
        this.stage.show();
    }
}
