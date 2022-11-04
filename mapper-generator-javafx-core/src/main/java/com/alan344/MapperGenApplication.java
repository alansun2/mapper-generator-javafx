package com.alan344;

import com.alan344.config.BooleanPropertyWriter;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.FxmlLoadFactory;
import com.alibaba.fastjson2.JSON;
import javafx.application.Application;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.stage.Stage;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

/**
 * @author AlanSun
 * @date 2019/8/7 17:07
 */
public class MapperGenApplication extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    private ConfigurableApplicationContext applicationContext;

    @Override
    public void init() {
        applicationContext = new AnnotationConfigApplicationContext("com.alan344");
        JSON.register(SimpleBooleanProperty.class, BooleanPropertyWriter.INSTANCE);
    }

    @Override
    public void start(Stage primaryStage) {
        NodeConstants.primaryStage = primaryStage;
        NodeConstants.hostServices = getHostServices();
        primaryStage.setScene(new Scene(FxmlLoadFactory.create("/fxml/main.fxml", applicationContext)));
        //图标
        primaryStage.getIcons().add(new Image("/image/icon.png"));
        primaryStage.setWidth(1200);
        primaryStage.setHeight(700);
        primaryStage.setTitle("mybatis friend");
        primaryStage.show();
    }
}
