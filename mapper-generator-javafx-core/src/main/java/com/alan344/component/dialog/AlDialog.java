package com.alan344.component.dialog;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.stage.Modality;
import javafx.stage.Stage;

/**
 * @author AlanSun
 * @date 2023/2/8 9:44
 */
public class AlDialog extends BorderPane {
    protected HBox actionHBox = new HBox();

    protected final Stage stage;

    public AlDialog(Stage stage1) {
        this.getStylesheets().add("css/common.css");
        this.setBottom(actionHBox);

        actionHBox.setSpacing(10);
        actionHBox.setPadding(new Insets(5));
        actionHBox.setAlignment(Pos.CENTER_RIGHT);

        stage = new Stage();
        stage.setScene(new Scene(this));
        stage.setResizable(false);
        stage.initModality(Modality.WINDOW_MODAL);
        stage.getIcons().add(new Image("image/icon.png"));
        stage.initOwner(stage1);

        this.addEventHandler(KeyEvent.KEY_PRESSED, event -> {
            if (event.getCode() == KeyCode.ESCAPE) {
                stage.close();
            }
        });
    }

    public void setActions(Button... buttons) {
        for (int i = buttons.length - 1; i >= 0; i--) {
            actionHBox.getChildren().add(0, buttons[i]);
        }
    }
}
