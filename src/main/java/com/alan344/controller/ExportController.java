package com.alan344.controller;

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

    private Stage dateSourceStage;

    void export(Stage primaryStage) throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader();
        fxmlLoader.setLocation(getClass().getResource("/fxml/export.fxml"));
        fxmlLoader.setControllerFactory(applicationContext::getBean);

        Parent load = fxmlLoader.load();

        dateSourceStage = new Stage();
        dateSourceStage.setScene(new Scene(load));
        dateSourceStage.setResizable(false);
        dateSourceStage.setTitle("生成");
        dateSourceStage.initModality(Modality.WINDOW_MODAL);
        dateSourceStage.initOwner(primaryStage);
        dateSourceStage.show();
    }

    /**
     * 应用生成xml,并生成bean
     */
    public void apply() throws Exception {
        ListView<VBox> anchorPaneListView = mainController.getAnchorPaneListView();
        ObservableList<VBox> items = anchorPaneListView.getItems();
        xmlGeneratorService.generatorXml(items);
    }
}
