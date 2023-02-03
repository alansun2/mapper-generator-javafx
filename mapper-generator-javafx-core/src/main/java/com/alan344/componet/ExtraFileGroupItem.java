package com.alan344.componet;

import com.alan344.bean.config.ExtraFileGroupConfig;
import com.alan344.utils.TooltipWrapper;
import com.jfoenix.controls.JFXToggleButton;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.control.ToggleGroup;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;

import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/11/21 15:33
 */
public class ExtraFileGroupItem extends HBox {
    private final Label label;

    private final ExtraFileGroupConfig extraFileGroupConfig;

    private static final ToggleGroup TOGGLE_GROUP = new ToggleGroup();

    public ExtraFileGroupItem(ExtraFileGroupConfig extraFileGroupConfig, Consumer<MouseEvent> consumer) {
        this.extraFileGroupConfig = extraFileGroupConfig;
        label = new Label(extraFileGroupConfig.getGroupName());
        label.setPrefWidth(100);
        TooltipWrapper.wrap(label, extraFileGroupConfig.getGroupName());
        label.setAlignment(Pos.CENTER);
        JFXToggleButton toggleButton = new JFXToggleButton();
        toggleButton.setToggleGroup(TOGGLE_GROUP);
        toggleButton.setSize(8);
        toggleButton.setSelected(extraFileGroupConfig.isEnable());
        toggleButton.selectedProperty().addListener((observable, oldValue, newValue) -> extraFileGroupConfig.setEnable(newValue));
        this.getChildren().addAll(label, toggleButton);
        this.setPrefHeight(20);
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
