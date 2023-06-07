package com.alan344.component;

import com.alan344.utils.FileExploreUtils;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Paint;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import org.kordamp.ikonli.javafx.FontIcon;

/**
 * @author AlanSun
 * @date 2023/2/19 23:59
 * <p>
 * 打开文件 dialog
 */
public class FileOpenAndSuccessDialog extends AlDialog {

    public FileOpenAndSuccessDialog(Stage stage1, String textContent, String path) {
        super(stage1);
        this.setPrefHeight(150);
        this.setPrefWidth(300);
        this.actionHBox.setStyle("-fx-background-color: white");

        Button open = new Button("打开");
        open.setPrefWidth(60);
        open.setOnAction(event -> FileExploreUtils.open(path));

        Button button = new Button("确定");
        button.setPrefWidth(60);
        button.setOnAction(event -> stage.close());
        this.setActions(open, button);

        final FontIcon fontIcon = new FontIcon("unis-check-circle:40:#81E069");

        Text text = new Text(textContent);
        text.setFont(Font.font(15));
        text.setFill(Paint.valueOf("#81E069"));

        HBox hBox = new HBox(10, fontIcon, text);
        hBox.setStyle("-fx-background-color: white;");
        hBox.setAlignment(Pos.CENTER);

        this.setCenter(hBox);

        this.stage.show();
    }
}
