package com.alan344.component;

import com.alan344.utils.FileExploreUtils;
import com.alan344.utils.StringUtils;
import javafx.event.ActionEvent;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import lombok.Getter;

import java.io.File;
import java.util.function.Consumer;

/**
 * @author AlanSun
 * @since 2022/8/21 15:01
 */
public class FileTemplateTextHBox extends HBox {
    @Getter
    private final TextField textField;
    private final Button importBtn;
    private final Button exportBtn;
    private final Button editBtn;
    private final Button openFileLocationBtn;

    public FileTemplateTextHBox(String initText) {
        textField = new TextField(initText);
        textField.prefHeightProperty().bind(this.heightProperty());
        textField.prefWidthProperty().bind(this.widthProperty().subtract(256));

        importBtn = new Button("导入");
        importBtn.getStyleClass().add("mf-scan");
        importBtn.setPrefWidth(64);
        importBtn.prefHeightProperty().bind(this.heightProperty());

        exportBtn = new Button("导出");
        exportBtn.getStylesheets().add("css/common.css");
        exportBtn.setStyle("-fx-background-insets: 0; -fx-border-radius: 0em; -fx-border-width: 1;");
        exportBtn.setPrefWidth(64);
        exportBtn.prefHeightProperty().bind(this.heightProperty());
        this.exportBtn.setDisable(StringUtils.isEmpty(initText));

        editBtn = new Button("编辑");
        editBtn.getStyleClass().add("mf-scan");
        editBtn.setPrefWidth(64);
        editBtn.prefHeightProperty().bind(this.heightProperty());

        openFileLocationBtn = new Button("打开位置");
        openFileLocationBtn.getStylesheets().add("css/common.css");
        openFileLocationBtn.setStyle("-fx-background-insets: 0; -fx-border-radius: 0em; -fx-border-width: 1;");
        openFileLocationBtn.setPrefWidth(80);
        openFileLocationBtn.prefHeightProperty().bind(this.heightProperty());
        // 默认禁用，当有文件路径时启用
        openFileLocationBtn.setDisable(StringUtils.isEmpty(initText));
        // 打开文件位置按钮点击事件
        openFileLocationBtn.setOnAction(event -> {
            String filePath = this.getText();
            if (StringUtils.isNotEmpty(filePath)) {
                FileExploreUtils.open(filePath);
            }
        });

        textField.textProperty().addListener((observable, oldValue, newValue) -> {
            this.exportBtn.setDisable(StringUtils.isEmpty(newValue));
            // 检查是否为类路径资源或文件系统路径并存在
            boolean isClasspathResource = StringUtils.isNotEmpty(newValue) && newValue.startsWith("classpath:");
            boolean isFileExists = StringUtils.isNotEmpty(newValue) && !isClasspathResource && new File(newValue).exists();
            // 打开位置按钮仅在文件系统路径且文件存在时启用
            this.openFileLocationBtn.setDisable(!isFileExists);
        });

        this.getChildren().addAll(textField, importBtn, exportBtn, editBtn, openFileLocationBtn);
        this.setAlignment(Pos.CENTER);
        this.setStyle("-fx-border-width: 0; -fx-background-insets:0; -fx-background-color: #FFF");

        this.getStylesheets().add("css/text-button.css");
    }


    public String getText() {
        return this.textField.getText();
    }

    public void setText(String text) {
        this.textField.setText(text);
    }

    public void importAction(Consumer<ActionEvent> consumer) {
        this.importBtn.setOnAction(consumer::accept);
    }

    public void exportAction(Consumer<ActionEvent> consumer) {
        this.exportBtn.setOnAction(consumer::accept);
    }

    public void editAction(Consumer<ActionEvent> consumer) {
        this.editBtn.setOnAction(consumer::accept);
    }

    public final void setPromptText(String value) {
        this.textField.setPromptText(value);
    }

    public void disable(boolean disable) {
        textField.setDisable(disable);
        this.importBtn.setDisable(disable);
        this.editBtn.setDisable(disable);
        this.openFileLocationBtn.setDisable(disable);
    }
}