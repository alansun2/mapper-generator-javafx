package com.alan344.controller;

import com.alan344.bean.GeneratorConfig;
import com.alan344.service.ConfigService;
import com.alan344.service.XmlGeneratorService;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.io.File;
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
    private ConfigService configService;

    @Autowired
    private ConfigController configController;

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
        //调用 mybatis 生成文件
        xmlGeneratorService.generatorXml(items, generatorConfig);

        configController.getConfigStage().close();

        configService.addConfig(generatorConfig);
    }

    /**
     * 关闭
     */
    @FXML
    public void cancel() {
        configController.getConfigStage().close();
    }


    @FXML
    public void fileScan() {
        FileChooser fileChooser = new FileChooser();
        FileChooser.ExtensionFilter extFilter = new FileChooser.ExtensionFilter("TXT files (*.txt)", "*.txt");
        fileChooser.getExtensionFilters().add(extFilter);
        Stage fileStage = new Stage();
        File file = fileChooser.showOpenDialog(fileStage);
        System.out.println(file);
        fileStage.show();
    }
}
