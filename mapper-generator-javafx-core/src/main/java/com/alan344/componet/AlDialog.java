package com.alan344.componet;

import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

/**
 * @author AlanSun
 * @date 2023/2/8 9:44
 */
public class AlDialog extends BorderPane {
    private HBox actionHBox = new HBox();

    private StackPane stackPane = new StackPane();

    public AlDialog() {
        this.getStylesheets().add("css/common.css");
        this.setBottom(actionHBox);
        this.setCenter(stackPane);

        actionHBox.setSpacing(10);

        Stage stage = new Stage();
        stage.setScene(new Scene(this));
        stage.initStyle(StageStyle.UTILITY);
        stage.getIcons().add(new Image("image/icon.png"));
        stage.show();
    }

    public void setActions(Button... buttons) {
        actionHBox.getChildren().addAll(buttons);
    }

    public void setContent(Node content) {
        this.stackPane.getChildren().add(content);
    }
}
