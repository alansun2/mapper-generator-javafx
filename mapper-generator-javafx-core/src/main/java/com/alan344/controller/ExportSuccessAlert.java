package com.alan344.controller;

import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.layout.AnchorPane;
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
public class ExportSuccessAlert {
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

        tableAdvanceSetUpStage.show();
    }
}
