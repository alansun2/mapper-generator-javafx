package com.alan344.controller;

import com.alan344.factory.FxmlLoadFactory;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;


/**
 * @author AlanSun
 * @date 2019/8/20 18:01
 */
@Controller
public class AboutController {

    @Autowired
    private BeanFactory beanFactory;

    private Stage aboutStage;

    /**
     * 打开关于
     *
     * @param primaryStage 主窗口
     */
    void openWindow(Stage primaryStage) {
        if (aboutStage == null) {
            aboutStage = new Stage();
            aboutStage.setScene(new Scene(FxmlLoadFactory.create("/fxml/about.fxml", beanFactory)));
            aboutStage.setResizable(false);
            aboutStage.initStyle(StageStyle.UNDECORATED);
            aboutStage.initOwner(primaryStage);

            aboutStage.focusedProperty().addListener((observable, oldValue, newValue) -> {
                if (!newValue) {
                    aboutStage.close();
                }
            });
        }
        aboutStage.show();
    }
}
