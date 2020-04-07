package com.alan344.controller;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.AnchorPane;
import javafx.scene.paint.Paint;
import javafx.scene.text.Text;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.io.IOException;

/**
 * @author AlanSun
 * @date 2020/4/6 18:29
 */
@Controller
public class ExportSuccessAlertController {

    @FXML
    private Text text;

    @FXML
    private ImageView image;

    @Resource
    private BeanFactory beanFactory;

    private Stage tableAdvanceSetUpStage;

    /**
     * 打开表高级设置
     *
     * @param stage 主窗口
     * @throws IOException e
     */
    void openTableAdvancedSetUP(Stage stage, boolean isExportSuccess) throws IOException {
        if (tableAdvanceSetUpStage == null) {
            FXMLLoader fxmlLoader = new FXMLLoader();

            fxmlLoader.setLocation(getClass().getResource("/fxml/export-success-alert.fxml"));
            fxmlLoader.setControllerFactory(beanFactory::getBean);

            AnchorPane tableAdvanceSetUpPane = fxmlLoader.load();
            tableAdvanceSetUpStage = new Stage();
            tableAdvanceSetUpStage.setScene(new Scene(tableAdvanceSetUpPane));
            tableAdvanceSetUpStage.setTitle("导出成功");
            tableAdvanceSetUpStage.getIcons().add(new Image("/image/advanced-set-up.png"));
            tableAdvanceSetUpStage.setResizable(false);
            tableAdvanceSetUpStage.initModality(Modality.WINDOW_MODAL);
            tableAdvanceSetUpStage.initOwner(stage);
        }

        if (!isExportSuccess) {
            tableAdvanceSetUpStage.setTitle("导出成功");
            text.setText("successful!!!");
            text.setFill(Paint.valueOf("#25ae20"));
            image.setImage(new Image("/image/export-success.png"));
        } else {
            tableAdvanceSetUpStage.setTitle("导出失败");
            text.setText("error，请查看日志文件");
            text.setFill(Paint.valueOf("#AE4F13"));
            text.setLayoutX(75);
            image.setImage(new Image("/image/export-error.png"));
            image.setLayoutX(30);
        }

        tableAdvanceSetUpStage.show();
    }

    /**
     * 关闭
     */
    @FXML
    public void close() {
        tableAdvanceSetUpStage.close();
    }
}
