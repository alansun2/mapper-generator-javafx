package com.alan344.componet;

import com.alan344.constants.ExtraFileTypeEnum;
import javafx.event.ActionEvent;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;

import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/8/21 15:01
 */
public class ExtraFileLabel extends HBox {
    private final Button scanButton;

    public ExtraFileLabel(ExtraFileTypeEnum extraFileTypeEnum, boolean curToggle, Consumer<Boolean> consumer) {
        int toggleWidth = 64, btnWidth = 64;
        Label label = new Label(extraFileTypeEnum.name());
        label.setStyle("-fx-background-insets: 0");
        label.prefWidthProperty().bind(this.widthProperty().subtract(toggleWidth + btnWidth));

        ToggleSwitch toggleSwitch = new ToggleSwitch(toggleWidth, curToggle, consumer);
        toggleSwitch.prefHeightProperty().bind(this.heightProperty());

        scanButton = new Button("设置");
        scanButton.setStyle("-fx-background-insets: 0");
        scanButton.setPrefWidth(btnWidth);
        scanButton.prefHeightProperty().bind(this.heightProperty());
        this.getChildren().addAll(label, toggleSwitch, scanButton);
        this.setSpacing(10);
        this.setAlignment(Pos.CENTER);
        this.setStyle("-fx-background-insets: 0");
    }

    public void onAction(Consumer<ActionEvent> consumer) {
        this.scanButton.setOnAction(consumer::accept);
    }
}
