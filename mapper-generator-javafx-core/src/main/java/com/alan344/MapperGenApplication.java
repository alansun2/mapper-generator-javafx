package com.alan344;

import com.alan344.config.BooleanPropertyWriter;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.FxmlLoadFactory;
import com.alibaba.fastjson2.JSON;
import javafx.application.Application;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.input.MouseEvent;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

/**
 * @author AlanSun
 * @date 2019/8/7 17:07
 */
public class MapperGenApplication extends Application {

    public static void main(String[] args) {
        // 捕捉未处理的异常
        Thread.setDefaultUncaughtExceptionHandler((t, e) -> {
            System.out.println("捕捉到未处理的异常：" + e.getMessage());
            // 抛出栈信息
            e.printStackTrace();
        });
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
        this.primaryStage = primaryStage;
        NodeConstants.hostServices = getHostServices();
        final Scene scene = new Scene(FxmlLoadFactory.create("/fxml/main.fxml", applicationContext));
        primaryStage.setScene(scene);
        // primaryStage.initStyle(StageStyle.UNDECORATED);
        primaryStage.setIconified(true);
        //图标
        primaryStage.getIcons().add(new Image("/image/icon.png"));
        primaryStage.setWidth(1200);
        primaryStage.setHeight(700);
        primaryStage.setTitle("Mybatis Friend");
        primaryStage.show();
    }

    private Stage primaryStage;

    private double dragOffsetX;

    private double dragOffsetY;

    protected void handleMousePressed(MouseEvent e) {
        // Store the mouse x and y coordinates with respect to the
        // stage in the reference variables to use them in the drag event
        // 点击鼠标时，获取鼠标在窗体上点击时相对应窗体左上角的偏移
        this.dragOffsetX = e.getScreenX() - primaryStage.getX();
        this.dragOffsetY = e.getScreenY() - primaryStage.getY();
    }

    protected void handleMouseDragged(MouseEvent e) {
        // Move the stage by the drag amount
        // 拖动鼠标后，获取鼠标相对应显示器坐标减去鼠标相对窗体的坐标，并将其设置为窗体在显示器上的坐标
        primaryStage.setX(e.getScreenX() - this.dragOffsetX);
        primaryStage.setY(e.getScreenY() - this.dragOffsetY);
    }
}
