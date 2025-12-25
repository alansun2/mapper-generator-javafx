package com.alan344.component;

import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

import java.util.function.Consumer;

/**
 * 文本编辑器对话框
 * 用于编辑模板文件内容
 *
 * @author AlanSun
 * @since 2023/12/17
 */
public class TextEditorDialog extends BorderPane {
    private final Stage stage;
    private final TextArea textArea;
    private final Button cancelBtn;
    private final Button applyBtn;

    public TextEditorDialog(String title, String initText) {
        // 创建舞台
        stage = new Stage();
        stage.initStyle(StageStyle.DECORATED);
        stage.setTitle(title);
        stage.initModality(Modality.APPLICATION_MODAL);
        stage.setResizable(true);
        stage.setMinWidth(600);
        stage.setMinHeight(400);

        // 文本编辑器
        textArea = new TextArea(initText);
        textArea.setWrapText(false);
        textArea.setStyle("-fx-font-family: Consolas, Monaco, 'Courier New', monospace; -fx-font-size: 14px;");

        // 按钮
        cancelBtn = new Button("取消");
        applyBtn = new Button("应用");
        applyBtn.getStyleClass().add("apply-btn");

        HBox btnHbox = new HBox(10, cancelBtn, applyBtn);
        btnHbox.setAlignment(javafx.geometry.Pos.CENTER_RIGHT);
        btnHbox.setPadding(new Insets(10));

        // 设置布局
        this.setCenter(textArea);
        this.setBottom(btnHbox);
        this.getStylesheets().add("css/common.css");

        // 设置场景
        Scene scene = new Scene(this);
        stage.setScene(scene);
    }

    /**
     * 显示对话框
     */
    public void show() {
        stage.show();
    }

    /**
     * 设置取消按钮动作
     */
    public void setCancelAction(Consumer<ActionEvent> consumer) {
        cancelBtn.setOnAction(consumer::accept);
    }

    /**
     * 设置应用按钮动作
     */
    public void setApplyAction(Consumer<ActionEvent> consumer) {
        applyBtn.setOnAction(consumer::accept);
    }

    /**
     * 获取文本内容
     */
    public String getText() {
        return textArea.getText();
    }

    /**
     * 关闭对话框
     */
    public void close() {
        stage.close();
    }
}