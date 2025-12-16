package com.alan344.component;

import com.alan344.bean.config.ExtraFileGroupConfig;
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
 * @since 2022/8/21 15:01
 */
public class ExtraFileItemHBox extends HBox implements SelectBtnBarHBox.Selected {
    private final Button deleteBtn;
    private final Button editBtn;
    private final JFXToggleButton toggleSwitch;

    public ExtraFileItemHBox(ExtraFileGroupConfig.ExtraFileConfig extraFileConfigNew) {
        int serialNumberWidth = 20, nameWidth = 130, toggleWidth = 64, btnWidth = 64;

        Label serialNumberLabel = new Label(extraFileConfigNew.getSerialNumber());
        serialNumberLabel.textProperty().bindBidirectional(extraFileConfigNew.serialNumberProperty());
        serialNumberLabel.setPrefWidth(serialNumberWidth);

        Label nameLabel = new Label(extraFileConfigNew.getName());
        nameLabel.textProperty().bindBidirectional(extraFileConfigNew.nameProperty());
        nameLabel.setStyle("-fx-background-insets: 0");
        nameLabel.setPrefWidth(nameWidth);

        Label extraFileTypeLabel = new Label(extraFileConfigNew.getName());
        extraFileTypeLabel.textProperty().bindBidirectional(extraFileConfigNew.extraFileTypeProperty());
        extraFileTypeLabel.setStyle("-fx-background-insets: 0");
        extraFileTypeLabel.prefWidthProperty().bind(this.widthProperty().subtract(serialNumberWidth + nameWidth + toggleWidth + (btnWidth * 2)).subtract(45));

        toggleSwitch = new JFXToggleButton();
        toggleSwitch.selectedProperty().bindBidirectional(extraFileConfigNew.enableProperty());
        toggleSwitch.setSize(7);
        toggleSwitch.setPadding(new Insets(-10.5, 0, -10.5, 0));
        toggleSwitch.setSelected(extraFileConfigNew.isEnable());

        editBtn = new Button("Edit");
        editBtn.setStyle("-fx-background-insets: 0; -fx-background-color: #7070fd; -fx-text-fill: white");
        editBtn.setPrefWidth(btnWidth);

        deleteBtn = new Button("Del");
        deleteBtn.setStyle("-fx-background-insets: 0; -fx-background-color: #E35252; -fx-text-fill: white");
        deleteBtn.setPrefWidth(btnWidth);

        this.getChildren().addAll(serialNumberLabel, nameLabel, extraFileTypeLabel, toggleSwitch, editBtn, deleteBtn);

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

    @Override
    public boolean isSelected() {
        return toggleSwitch.isSelected();
    }

    @Override
    public void setSelect(boolean selected) {
        toggleSwitch.setSelected(selected);
    }
}
