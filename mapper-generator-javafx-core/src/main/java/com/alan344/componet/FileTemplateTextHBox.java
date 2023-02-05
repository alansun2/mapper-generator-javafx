package com.alan344.componet;

import javafx.event.ActionEvent;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;

import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/8/21 15:01
 */
public class FileTemplateTextHBox extends HBox {

    private final TextField textField;
    private final Button importBtn;
    private final Button exportBtn;

    public FileTemplateTextHBox(String initText) {
        textField = new TextField(initText);
        textField.focusedProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue) {
                this.setStyle("-fx-border-width: 1;" +
                        "-fx-border-color: #38b1da;");
            } else {
                this.setStyle("-fx-border-width: 1;" +
                        "-fx-border-color: #BABABA;");
            }
        });
        textField.setStyle("-fx-background-insets: 0");
        textField.prefHeightProperty().bind(this.heightProperty());
        textField.prefWidthProperty().bind(this.widthProperty().subtract(64));

        importBtn = new Button("导入");
        importBtn.getStylesheets().add("css/common.css");
        importBtn.getStyleClass().add("mf-scan");
        importBtn.setPrefWidth(64);
        importBtn.prefHeightProperty().bind(this.heightProperty());

        exportBtn = new Button("导出");
        exportBtn.getStylesheets().add("css/common.css");
        exportBtn.setPrefWidth(64);
        exportBtn.prefHeightProperty().bind(this.heightProperty());

        this.getChildren().addAll(textField, importBtn, exportBtn);
        this.setAlignment(Pos.CENTER);
        this.setStyle("-fx-border-width: 1;" +
                "-fx-border-color: #BABABA;");

        this.getStylesheets().add("/css/text-button.css");
    }


    public String getText() {
        return this.textField.getText();
    }

    public void setText(String text) {
        this.textField.setText(text);
    }

    public TextField getTextField() {
        return this.textField;
    }

    public void importAction(Consumer<ActionEvent> consumer) {
        this.importBtn.setOnAction(consumer::accept);
    }

    public void exportAction(Consumer<ActionEvent> consumer) {
        this.exportBtn.setOnAction(consumer::accept);
    }

    public final void setPromptText(String value) {
        this.textField.setPromptText(value);
    }

    public void disable(boolean disable) {
        textField.setDisable(disable);
        this.importBtn.setDisable(disable);
    }
}
