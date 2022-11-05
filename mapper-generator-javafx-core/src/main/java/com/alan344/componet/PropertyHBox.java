package com.alan344.componet;

import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;

/**
 * @author AlanSun
 * @date 2022/11/5 11:35
 */
public class PropertyHBox extends HBox {

    public PropertyHBox(String name, int nameWidth, Region c) {
        Label nameLabel = new Label(name);
        nameLabel.setPrefWidth(nameWidth);
        nameLabel.setTooltip(new Tooltip(name));
        c.prefWidthProperty().bind(this.widthProperty().subtract(nameWidth));
        this.setAlignment(Pos.CENTER);
        this.setStyle("-fx-padding: 0 10");
        this.getChildren().addAll(nameLabel, c);
    }
}
