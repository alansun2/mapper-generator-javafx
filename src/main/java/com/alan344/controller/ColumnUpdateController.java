package com.alan344.controller;

import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.stage.Stage;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/19 18:53
 */
@Controller
public class ColumnUpdateController implements Initializable {

    @Autowired
    private BeanFactory beanFactory;

    private Stage columnUpdateStage;

    @Override
    public void initialize(URL location, ResourceBundle resources) {

    }

    /**
     * 打开字段修改页面
     */
    public void openStage() {
        FXMLLoader fxmlLoader = new FXMLLoader();

        fxmlLoader.setLocation(getClass().getResource("/fxml/column-update.fxml"));
        fxmlLoader.setControllerFactory(beanFactory::getBean);

    }
}
