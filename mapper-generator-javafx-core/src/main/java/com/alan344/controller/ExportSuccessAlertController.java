package com.alan344.controller;

import com.alan344.constants.BaseConstants;
import com.fasterxml.jackson.databind.ser.Serializers;
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
import java.awt.*;
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
     * @param primaryStage 主窗口
     * @throws IOException e
     */
    void openTableAdvancedSetUP(Stage primaryStage, boolean isExportSuccess) throws IOException {
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
            tableAdvanceSetUpStage.initOwner(primaryStage);
        }

        if (isExportSuccess) {
            tableAdvanceSetUpStage.setTitle("导出成功");
            text.setText("successful!!!");
            image.setImage(new Image("/image/export-success.png"));
        } else {
            tableAdvanceSetUpStage.setTitle("导出失败");
            text.setText("error!!!");
            text.setFill(Paint.valueOf("#AE4F13"));
            image.setImage(new Image("/image/export-error.png"));
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

    /**
     * 打开文件夹
     */
    @FXML
    public void openDir() {
        if (Desktop.isDesktopSupported()) {
            Desktop desktop = Desktop.getDesktop();
            try {
                desktop.open(BaseConstants.getConfigFile());
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

    }
}
