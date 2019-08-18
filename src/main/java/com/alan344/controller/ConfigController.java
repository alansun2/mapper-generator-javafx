package com.alan344.controller;

import com.alan344.bean.GeneratorConfig;
import com.alan344.service.ConfigService;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SplitPane;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import lombok.Getter;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2019/8/15 17:55
 */
@Controller
public class ConfigController implements Initializable {

    @FXML
    private VBox centerVBox;

    @FXML
    private SplitPane splitPane;

    @Autowired
    private ConfigService configService;

    @Autowired
    private BeanFactory beanFactory;

    @Autowired
    private ExportController exportController;

    @Getter
    private Stage configStage;

    /**
     * 配置信息
     */
    private Map<String, GeneratorConfig> configNameConfigMap;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
    }

    /**
     * 打开配置文件设置
     *
     * @param primaryStage 主窗口
     * @throws IOException e
     */
    void openConfigPane(Stage primaryStage) throws IOException {

        if (configStage == null) {
            FXMLLoader fxmlLoader = new FXMLLoader();

            fxmlLoader.setLocation(getClass().getResource("/fxml/config.fxml"));
            fxmlLoader.setControllerFactory(beanFactory::getBean);

            BorderPane configBorderPane = fxmlLoader.load();

            configStage = new Stage();
            configStage.setScene(new Scene(configBorderPane));
            configStage.setTitle("设置导出");
            configStage.getIcons().add(new Image("/image/setting@32.png"));
            configStage.setResizable(false);
            configStage.initModality(Modality.WINDOW_MODAL);
            configStage.initOwner(primaryStage);
            configStage.show();

            //加载配置文件
            List<GeneratorConfig> generatorConfigs = configService.loadConfigFromFile();

            this.configNameConfigMap = generatorConfigs.stream().collect(Collectors.toMap(GeneratorConfig::getConfigName, o -> o));

            if (generatorConfigs.isEmpty()) {
                FXMLLoader exportFxmlLoader = new FXMLLoader();
                exportFxmlLoader.setLocation(getClass().getResource("/fxml/export.fxml"));
                exportFxmlLoader.setControllerFactory(beanFactory::getBean);

                splitPane.getItems().add(exportFxmlLoader.load());
            } else {
                this.configNameConfigMap.forEach((k, v) -> this.addConfigButton(v));

                FXMLLoader exportFxmlLoader = new FXMLLoader();
                exportFxmlLoader.setLocation(getClass().getResource("/fxml/export.fxml"));
                exportFxmlLoader.setControllerFactory(beanFactory::getBean);

                splitPane.getItems().add(exportFxmlLoader.load());

                GeneratorConfig generatorConfig = generatorConfigs.get(0);
                exportController.showConfig(generatorConfig);
            }
        } else {
            configStage.show();
        }
    }

    /**
     * 新增一个配置
     */
    @FXML
    public void addEmptyExportPane() {
        exportController.clearPane();
    }

    /**
     * 添加配置
     *
     * @param generatorConfig 配置信息
     */
    void addConfig(GeneratorConfig generatorConfig) {
        //写入文件
        int addType = configService.addConfig(generatorConfig);
        //同时更新内存的配置信息
        if (addType == 3) {
            this.addConfigButton(generatorConfig);
            this.configNameConfigMap.put(generatorConfig.getConfigName(), generatorConfig);
        } else if (addType == 1) {
            this.configNameConfigMap.put(generatorConfig.getConfigName(), generatorConfig);
        }
    }

    /**
     * 删除配置
     *
     * @param generatorConfig 配置信息
     */
    private void deleteConfig(Button button, GeneratorConfig generatorConfig) {
        ObservableList<Node> children = centerVBox.getChildren();
        int size = children.size();
        if (size == 1) {
            exportController.clearPane();
        } else {
            int i = children.indexOf(button);
            if (i == 0) {
                exportController.showConfig(configNameConfigMap.get(((Button) children.get(1)).getText()));
            } else {
                exportController.showConfig(configNameConfigMap.get(((Button) children.get(i - 1)).getText()));
            }
        }
        children.remove(button);
        configService.deleteConfig(generatorConfig);
    }

    /**
     * 添加配置文件有左边的button
     *
     * @param generatorConfig 配置信息
     */
    private void addConfigButton(GeneratorConfig generatorConfig) {
        Button button = new Button(generatorConfig.getConfigName());
        MenuItem removeMenuItem = new MenuItem("删除");
        removeMenuItem.setOnAction(event -> this.deleteConfig(button, generatorConfig));
        button.setContextMenu(new ContextMenu(removeMenuItem));
        button.prefWidthProperty().bind(centerVBox.widthProperty());
        button.setOnAction(event -> exportController.showConfig(this.configNameConfigMap.get(generatorConfig.getConfigName())));
        centerVBox.getChildren().add(button);
    }
}
