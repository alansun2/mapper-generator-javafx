package com.alan344.componet;

import com.alan344.bean.config.ExtraFileConfig;
import com.alan344.constants.ExtraFileTypeEnum;
import javafx.event.ActionEvent;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import org.controlsfx.control.ToggleSwitch;

import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/8/21 15:01
 */
public class ExtraFileOfGroupLabel extends HBox {
    private final Button deleteBtn;

    private final Button editBtn;
    private final Label nameLabel;
    private ExtraFileConfig extraFileConfig;

    public ExtraFileOfGroupLabel(String name, ExtraFileTypeEnum extraFileTypeEnum, boolean curToggle, Consumer<Boolean> consumer) {
        int nameWidth = 130, toggleWidth = 64, btnWidth = 64;

        nameLabel = new Label(name);
        nameLabel.setStyle("-fx-background-insets: 0");
        nameLabel.setPrefWidth(nameWidth);
        nameLabel.prefHeightProperty().bind(this.heightProperty());

        Label extraFileTypeLabel = new Label(extraFileTypeEnum.name());
        extraFileTypeLabel.setStyle("-fx-background-insets: 0");
        extraFileTypeLabel.prefWidthProperty().bind(this.widthProperty().subtract(nameWidth + toggleWidth + (btnWidth * 2)));

        ToggleSwitch toggleSwitch = new ToggleSwitch();
        toggleSwitch.setPrefWidth(toggleWidth);
        toggleSwitch.setSelected(curToggle);
        toggleSwitch.selectedProperty().addListener((observable, oldValue, newValue) -> consumer.accept(newValue));
        toggleSwitch.prefHeightProperty().bind(this.heightProperty());

        editBtn = new Button("Edit");
        editBtn.setStyle("-fx-background-insets: 0; -fx-background-color: #7070fd; -fx-text-fill: white");
        editBtn.setPrefWidth(btnWidth);
        editBtn.prefHeightProperty().bind(this.heightProperty());

        deleteBtn = new Button("Del");
        deleteBtn.setStyle("-fx-background-insets: 0; -fx-background-color: #E35252; -fx-text-fill: white");
        deleteBtn.setPrefWidth(btnWidth);
        deleteBtn.prefHeightProperty().bind(this.heightProperty());

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

    public void setLabelText(String text) {
        nameLabel.setText(text);
    }

    public ExtraFileConfig getExtraFileConfig() {
        return this.extraFileConfig;
    }

    public void setExtraFileConfig(ExtraFileConfig extraFileConfig) {
        this.extraFileConfig = extraFileConfig;
    }
}
