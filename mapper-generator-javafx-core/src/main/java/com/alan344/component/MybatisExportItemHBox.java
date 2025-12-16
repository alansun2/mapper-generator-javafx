package com.alan344.component;

import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;

/**
 * @author AlanSun
 * @since 2023/2/22 0:40
 */
public class MybatisExportItemHBox extends HBox {
    private final Label label;

    public MybatisExportItemHBox(String name, Region node) {
        this.setSpacing(10);
        this.setAlignment(Pos.CENTER);

        label = new Label(name);
        label.setTooltip(new Tooltip(name));
        label.setPrefWidth(130);

        node.prefWidthProperty().bind(this.widthProperty().subtract(200));
        node.prefHeightProperty().bind(this.heightProperty());

        this.getChildren().addAll(label, node);
    }

    public void disable(boolean disable) {
        this.label.setDisable(disable);
    }
}
