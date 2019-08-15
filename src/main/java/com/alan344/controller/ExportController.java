package com.alan344.controller;

import com.alan344.bean.GeneratorConfig;
import com.alan344.service.ExportService;
import com.alan344.service.XmlGeneratorService;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/12 15:33
 */
@Controller
public class ExportController implements Initializable {
    @FXML
    private TextField beanLocationText;

    @FXML
    private TextField beanPackageText;

    @FXML
    private TextField mapperLocationText;

    @FXML
    private TextField mapperPackageText;

    @FXML
    private TextField xmlLocationText;

    @FXML
    private TextField authorText;

    @FXML
    private TextField configNameText;

    @FXML
    private CheckBox userJava8CheckBox;

    @FXML
    private CheckBox useBigDecimalCheckBox;
    @FXML

    private CheckBox useCommentCheckBox;
    @FXML
    private CheckBox useSwaggerCheckBox;

    @Autowired
    private MainController mainController;

    @Autowired
    private XmlGeneratorService xmlGeneratorService;

    @Autowired
    private ExportService exportService;

    private Stage dateSourceStage;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        System.out.println("ExportController initialize");
    }

    void addConfig() {

    }

    /**
     * 应用生成xml,并生成bean
     */
    @FXML
    public void apply() throws Exception {
        GeneratorConfig generatorConfig = new GeneratorConfig();
        generatorConfig.setConfigName(configNameText.getText());
        generatorConfig.setAuthor(authorText.getText());
        generatorConfig.setConfigName(configNameText.getText());
        generatorConfig.setBeanLocation(beanLocationText.getText());
        generatorConfig.setBeanPackage(beanPackageText.getText());
        generatorConfig.setMapperLocation(mapperLocationText.getText());
        generatorConfig.setMapperPackage(mapperPackageText.getText());
        generatorConfig.setMapperXmlLocation(xmlLocationText.getText());

        generatorConfig.setUserJava8(userJava8CheckBox.isSelected());
        generatorConfig.setUseBigDecimal(useBigDecimalCheckBox.isSelected());
        generatorConfig.setUseComment(useCommentCheckBox.isSelected());
        generatorConfig.setUseSwagger(useSwaggerCheckBox.isSelected());

        ListView<VBox> anchorPaneListView = mainController.getAnchorPaneListView();
        ObservableList<VBox> items = anchorPaneListView.getItems();
        xmlGeneratorService.generatorXml(items, generatorConfig);
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
