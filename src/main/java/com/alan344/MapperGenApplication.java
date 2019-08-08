package com.alan344;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

/**
 * @author AlanSun
 * @date 2019/8/7 17:07
 */
@SpringBootApplication
public class MapperGenApplication extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    private FXMLLoader fxmlLoader;

    @Override
    public void init() {
        ConfigurableApplicationContext run = SpringApplication.run(MapperGenApplication.class);
        fxmlLoader = new FXMLLoader();
        fxmlLoader.setControllerFactory(run::getBean);
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        fxmlLoader.setLocation(getClass().getResource("/fxml/main.fxml"));
        Parent root = fxmlLoader.load();

//        primaryStage.initStyle(StageStyle.UNDECORATED);
        primaryStage.setScene(new Scene(root));
        primaryStage.setWidth(1000);
        primaryStage.setHeight(700);
        primaryStage.setTitle("mapper自动生成器");
        primaryStage.show();
    }
}
