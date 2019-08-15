package com.alan344.controller;

import com.alan344.bean.GeneratorConfig;
import com.alan344.service.ConfigService;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.SplitPane;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.stage.Modality;
import javafx.stage.Stage;
import lombok.Getter;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/15 17:55
 */
@Controller
public class ConfigController implements Initializable {

    @Autowired
    private ConfigService configService;

    @Autowired
    private BeanFactory beanFactory;

    @Autowired
    private ExportController exportController;

    @Getter
    private Stage configStage;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        System.out.println("initialize");
    }

    /**
     * 打开配置文件设置
     *
     * @param primaryStage 主窗口
     * @throws IOException e
     */
    void addConfig(Stage primaryStage) throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader();
        fxmlLoader.setLocation(getClass().getResource("/fxml/config.fxml"));
        fxmlLoader.setControllerFactory(beanFactory::getBean);

        BorderPane configBorderPane = fxmlLoader.load();
        //加载配置文件
        List<GeneratorConfig> generatorConfigs = configService.loadConfigFromFile();

        if (generatorConfigs.isEmpty()) {
            FXMLLoader exportFxmlLoader = new FXMLLoader();
            exportFxmlLoader.setLocation(getClass().getResource("/fxml/export.fxml"));
            exportFxmlLoader.setControllerFactory(beanFactory::getBean);

            AnchorPane exportAnchorPane = exportFxmlLoader.load();
            BorderPane leftBorderPane = (BorderPane) ((SplitPane) configBorderPane.getCenter()).getItems().get(1);
            leftBorderPane.setCenter(exportAnchorPane);
        } else {
            generatorConfigs.forEach(generatorConfig -> {

            });
        }

        configStage = new Stage();
        configStage.setScene(new Scene(configBorderPane));
        configStage.setTitle("生成mapper");
        configStage.setResizable(false);
        configStage.initModality(Modality.WINDOW_MODAL);
        configStage.initOwner(primaryStage);
        configStage.show();
    }

    @FXML
    public void addConfig() {
        exportController.addConfig();
    }
}
