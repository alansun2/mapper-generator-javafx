package com.alan344.componet;

import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.constants.enums.ExtraFileTypeEnum;
import com.jfoenix.controls.JFXToggleButton;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;

import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/8/21 15:01
 */
public class ExtraFileItemHBox extends HBox {
    private final Button deleteBtn;

    private final Button editBtn;
    private final Label nameLabel;
    private ExtraTemplateFileConfig extraTemplateFileConfig;

    public ExtraFileItemHBox(String name, ExtraFileTypeEnum extraFileTypeEnum, boolean curToggle, Consumer<Boolean> consumer) {
        int nameWidth = 130, toggleWidth = 64, btnWidth = 64;

        nameLabel = new Label(name);
        nameLabel.setStyle("-fx-background-insets: 0");
        nameLabel.setPrefWidth(nameWidth);

        Label extraFileTypeLabel = new Label(extraFileTypeEnum.name());
        extraFileTypeLabel.setStyle("-fx-background-insets: 0");
        extraFileTypeLabel.prefWidthProperty().bind(this.widthProperty().subtract(nameWidth + toggleWidth + (btnWidth * 2)).subtract(45));

        JFXToggleButton toggleSwitch = new JFXToggleButton();
        toggleSwitch.setSize(7);
        toggleSwitch.setPadding(new Insets(-10.5, 0, -10.5, 0));
        toggleSwitch.setSelected(curToggle);
        toggleSwitch.selectedProperty().addListener((observable, oldValue, newValue) -> consumer.accept(newValue));

        editBtn = new Button("Edit");
        editBtn.setStyle("-fx-background-insets: 0; -fx-background-color: #7070fd; -fx-text-fill: white");
        editBtn.setPrefWidth(btnWidth);

        deleteBtn = new Button("Del");
        deleteBtn.setStyle("-fx-background-insets: 0; -fx-background-color: #E35252; -fx-text-fill: white");
        deleteBtn.setPrefWidth(btnWidth);

        this.getChildren().addAll(nameLabel, extraFileTypeLabel, toggleSwitch, editBtn, deleteBtn);

        this.setSpacing(10);
        this.setAlignment(Pos.CENTER);
        this.setStyle("-fx-background-insets: 0");
    }

    public void onDelAction(Consumer<ActionEvent> consumer) {
        this.deleteBtn.setOnAction(consumer::accept);
    }

    public void onEditAction(Consumer<ActionEvent> consumer) {
        this.editBtn.setOnAction(consumer::accept);
    }


    public void disable(boolean disable) {
        deleteBtn.setDisable(disable);
    }

    public void setLabelText(String text) {
        nameLabel.setText(text);
    }

    public ExtraTemplateFileConfig getExtraFileConfig() {
        return this.extraTemplateFileConfig;
    }

    public void setExtraFileConfig(ExtraTemplateFileConfig extraTemplateFileConfig) {
        this.extraTemplateFileConfig = extraTemplateFileConfig;
    }
}
