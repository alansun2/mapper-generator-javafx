package com.alan344.component;

import com.alan344.bean.config.ExtraTemplateFileGroupConfig;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;

/**
 * @author AlanSun
 * @date 2022/11/21 15:33
 */
public class ExtraTemplateFileGroupItemHBox extends HBox implements LeftRightLinkageBorderPane.Item<ExtraTemplateFileGroupConfig> {
    private final Label label;

    private final ExtraTemplateFileGroupConfig extraTemplateFileGroupConfig;

    public ExtraTemplateFileGroupItemHBox(ExtraTemplateFileGroupConfig extraTemplateFileGroupConfig) {
        this.extraTemplateFileGroupConfig = extraTemplateFileGroupConfig;
        label = new Label(extraTemplateFileGroupConfig.getGroupName());
        label.prefWidthProperty().bind(this.widthProperty().subtract(100));

        this.getChildren().addAll(label);
        this.setPrefHeight(20);
        this.setSpacing(5);
        this.setAlignment(Pos.CENTER);
    }

    @Override
    public String getName() {
        return label.getText();
    }

    @Override
    public void setName(String name) {
        label.setText(name);
        extraTemplateFileGroupConfig.setGroupName(name);
    }

    @Override
    public ExtraTemplateFileGroupConfig getConfig() {
        return this.extraTemplateFileGroupConfig;
    }
}
