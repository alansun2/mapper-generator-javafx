package com.alan344.componet;

import com.alan344.bean.config.ExtraFileGroupConfig;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;

import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/11/21 15:33
 */
public class ExtraFileGroup extends HBox {
    private final Label label;

    private final ExtraFileGroupConfig extraFileGroupConfig;

    public ExtraFileGroup(ExtraFileGroupConfig extraFileGroupConfig, Consumer<MouseEvent> consumer) {
        this.extraFileGroupConfig = extraFileGroupConfig;
        label = new Label(extraFileGroupConfig.getGroupName());
        label.setPrefWidth(90);
        ToggleSwitch button = new ToggleSwitch(40, extraFileGroupConfig.isEnable(), extraFileGroupConfig::setEnable);
        button.setPrefHeight(10);
        this.getChildren().addAll(label, button);
        this.setSpacing(5);
        this.addEventHandler(MouseEvent.MOUSE_RELEASED, consumer::accept);
        this.setAlignment(Pos.CENTER);
    }

    public String getName() {
        return label.getText();
    }

    public void setName(String name) {
        label.setText(name);
    }

    public ExtraFileGroupConfig getExtraFileGroupConfig() {
        return this.extraFileGroupConfig;
    }
}
