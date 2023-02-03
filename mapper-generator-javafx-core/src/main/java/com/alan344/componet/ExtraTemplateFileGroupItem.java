package com.alan344.componet;

import com.alan344.bean.config.ExtraTemplateFileGroupConfig;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.HBox;

/**
 * @author AlanSun
 * @date 2022/11/21 15:33
 */
public class ExtraTemplateFileGroupItem extends HBox {
    private final Label label;

    private final ExtraTemplateFileGroupConfig extraTemplateFileGroupConfig;

    private static final ToggleGroup toggleGroup = new ToggleGroup();

    public ExtraTemplateFileGroupItem(ExtraTemplateFileGroupConfig extraTemplateFileGroupConfig) {
        this.extraTemplateFileGroupConfig = extraTemplateFileGroupConfig;
        label = new Label(extraTemplateFileGroupConfig.getGroupName());
        label.setPrefWidth(70);

        this.getChildren().addAll(label);
        this.setPrefHeight(20);
        this.setSpacing(5);
        this.setAlignment(Pos.CENTER);
    }

    public String getName() {
        return label.getText();
    }

    public void setName(String name) {
        label.setText(name);
        extraTemplateFileGroupConfig.setGroupName(name);
    }

    public ExtraTemplateFileGroupConfig getExtraTemplateFileConfigGroup() {
        return this.extraTemplateFileGroupConfig;
    }
}
