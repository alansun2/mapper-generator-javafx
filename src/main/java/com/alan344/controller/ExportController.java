package com.alan344.controller;

import com.alan344.bean.GeneratorConfig;
import com.alan344.service.ExportService;
import com.alan344.service.XmlGeneratorService;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Controller;

import java.io.IOException;
import java.util.List;

/**
 * @author AlanSun
 * @date 2019/8/12 15:33
 */
@Controller
public class ExportController {

    @FXML
    private TextField exportLocationText;

    @Autowired
    private ApplicationContext applicationContext;

    @Autowired
    private MainController mainController;

    @Autowired
    private XmlGeneratorService xmlGeneratorService;

    @Autowired
    private ExportService exportService;

    private Stage dateSourceStage;

    void export(Stage primaryStage) throws IOException {
        //加载配置文件
        List<GeneratorConfig> generatorConfigs = exportService.loadConfigFromFile();

        if (generatorConfigs != null) {
            generatorConfigs.forEach(generatorConfig -> {

            });
        }

        FXMLLoader fxmlLoader = new FXMLLoader();
        fxmlLoader.setLocation(getClass().getResource("/fxml/export.fxml"));
        fxmlLoader.setControllerFactory(applicationContext::getBean);

        Parent load = fxmlLoader.load();

        dateSourceStage = new Stage();
        dateSourceStage.setScene(new Scene(load));
        dateSourceStage.setResizable(false);
        dateSourceStage.setTitle("生成mapper");
        dateSourceStage.initModality(Modality.WINDOW_MODAL);
        dateSourceStage.initOwner(primaryStage);
        dateSourceStage.show();
    }

    /**
     * 应用生成xml,并生成bean
     */
    @FXML
    public void apply() throws Exception {
        ListView<VBox> anchorPaneListView = mainController.getAnchorPaneListView();
        ObservableList<VBox> items = anchorPaneListView.getItems();
        xmlGeneratorService.generatorXml(items);
        dateSourceStage.close();
    }

    /**
     * 关闭
     */
    @FXML
    public void cancel() {
        dateSourceStage.close();
    }
}
