package com.alan344.component;

import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.jfoenix.controls.JFXCheckBox;
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
public class ExtraTemplateFileItemHBox extends HBox implements SelectBtnBarHBox.Selected {
    private final Button editButton;
    private final Button deleteButton;
    private final Button copyBtn;
    private final Label nameLabel;
    private final ExtraTemplateFileConfig extraTemplateFileConfig;

    private final JFXCheckBox jfxCheckBox;

    private final boolean isSystem;

    public ExtraTemplateFileItemHBox(boolean isSystem, ExtraTemplateFileConfig extraTemplateFileConfig) {
        this.extraTemplateFileConfig = extraTemplateFileConfig;
        this.isSystem = isSystem;
        int jfxCheckBoxWidth = 30, extraFileTypeWidth = 130, btnWidth = 64;

        jfxCheckBox = new JFXCheckBox();
        jfxCheckBox.setPrefWidth(jfxCheckBoxWidth);

        nameLabel = new Label(extraTemplateFileConfig.getName());
        nameLabel.setStyle("-fx-background-insets: 0");
        nameLabel.prefHeightProperty().bind(this.heightProperty());
        nameLabel.prefWidthProperty().bind(this.widthProperty().subtract(jfxCheckBoxWidth + extraFileTypeWidth + (btnWidth * 3)));

        Label extraFileTypeLabel = new Label(extraTemplateFileConfig.getExtraFileType().name());
        extraFileTypeLabel.setStyle("-fx-background-insets: 0");
        extraFileTypeLabel.setPrefWidth(extraFileTypeWidth);

        editButton = new Button("Edit");
        editButton.setStyle("-fx-background-insets: 0; -fx-background-color: #7070fd; -fx-text-fill: white");
        editButton.setPrefWidth(btnWidth);
        editButton.prefHeightProperty().bind(this.heightProperty());

        deleteButton = new Button("Del");
        deleteButton.setStyle("-fx-background-insets: 0; -fx-background-color: #E35252; -fx-text-fill: white");
        deleteButton.setPrefWidth(btnWidth);
        deleteButton.prefHeightProperty().bind(this.heightProperty());

        copyBtn = new Button("Copy");
        copyBtn.setStyle("-fx-background-insets: 0");
        copyBtn.setPrefWidth(btnWidth);
        copyBtn.prefHeightProperty().bind(this.heightProperty());

        this.disable(isSystem);
        this.getChildren().addAll(jfxCheckBox, nameLabel, extraFileTypeLabel, editButton, deleteButton, copyBtn);
        this.setSpacing(10);
        this.setAlignment(Pos.CENTER);
        this.setStyle("-fx-background-insets: 0");
    }

    public void onEditAction(Consumer<ActionEvent> consumer) {
        this.editButton.setOnAction(consumer::accept);
    }

    public void onDelAction(Consumer<ActionEvent> consumer) {
        this.deleteButton.setOnAction(consumer::accept);
    }

    public void onCopyAction(Consumer<ActionEvent> consumer) {
        this.copyBtn.setOnAction(consumer::accept);
    }

    public void setLabelText(String text) {
        nameLabel.setText(text);
    }

    public ExtraTemplateFileConfig getExtraTemplateFileConfig() {
        return this.extraTemplateFileConfig;
    }

    @Override
    public boolean isSelected() {
        return this.jfxCheckBox.isSelected();
    }

    @Override
    public void setSelect(boolean select) {
        this.jfxCheckBox.setSelected(select);
    }

    public boolean isSystem() {
        return isSystem;
    }

    private void disable(boolean disable) {
        deleteButton.setDisable(disable);
        copyBtn.setDisable(disable);
    }
}
