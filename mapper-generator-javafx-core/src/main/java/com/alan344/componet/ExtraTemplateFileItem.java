package com.alan344.componet;

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
public class ExtraTemplateFileItem extends HBox {
    private final Button scanButton;
    private final Button deleteButton;
    private final Button copyBtn;
    private final Label nameLabel;
    private final ExtraTemplateFileConfig extraTemplateFileConfig;

    private final JFXCheckBox jfxCheckBox;

    private final boolean showCheckBox;

    public ExtraTemplateFileItem(boolean showCheckBox, ExtraTemplateFileConfig extraTemplateFileConfig) {
        this.extraTemplateFileConfig = extraTemplateFileConfig;
        this.showCheckBox = showCheckBox;
        int jfxCheckBoxWidth = 30, nameWidth = 130, btnWidth = 64;

        jfxCheckBox = new JFXCheckBox();
        jfxCheckBox.setDisable(!showCheckBox);
        jfxCheckBox.setPrefWidth(jfxCheckBoxWidth);

        nameLabel = new Label(extraTemplateFileConfig.getName());
        nameLabel.setStyle("-fx-background-insets: 0");
        nameLabel.setPrefWidth(nameWidth);
        nameLabel.prefHeightProperty().bind(this.heightProperty());

        Label extraFileTypeLabel = new Label(extraTemplateFileConfig.getExtraFileType().name());
        extraFileTypeLabel.setStyle("-fx-background-insets: 0");
        extraFileTypeLabel.prefWidthProperty().bind(this.widthProperty().subtract(jfxCheckBoxWidth + nameWidth + (btnWidth * 3)));

        scanButton = new Button("Edit");
        scanButton.setStyle("-fx-background-insets: 0; -fx-background-color: #7070fd; -fx-text-fill: white");
        scanButton.setPrefWidth(btnWidth);
        scanButton.prefHeightProperty().bind(this.heightProperty());

        deleteButton = new Button("Del");
        deleteButton.setStyle("-fx-background-insets: 0; -fx-background-color: #E35252; -fx-text-fill: white");
        deleteButton.setPrefWidth(btnWidth);
        deleteButton.prefHeightProperty().bind(this.heightProperty());

        copyBtn = new Button("Copy");
        copyBtn.setStyle("-fx-background-insets: 0");
        copyBtn.setPrefWidth(btnWidth);
        copyBtn.prefHeightProperty().bind(this.heightProperty());

        this.getChildren().addAll(jfxCheckBox, nameLabel, extraFileTypeLabel, scanButton, deleteButton, copyBtn);
        this.setSpacing(10);
        this.setAlignment(Pos.CENTER);
        this.setStyle("-fx-background-insets: 0");
    }

    public void onEditAction(Consumer<ActionEvent> consumer) {
        this.scanButton.setOnAction(consumer::accept);
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

    public boolean isSelected() {
        return this.jfxCheckBox.isSelected();
    }

    public boolean isShowCheckBox() {
        return showCheckBox;
    }
}
