package com.alan344.controller;

import com.alan344.bean.GeneratorConfig;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.XmlGeneratorService;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;
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
    private ConfigController configController;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
//        System.out.println("ExportController initialize");
    }

    /**
     * 应用生成xml,并生成bean
     */
    @FXML
    public void apply() throws Exception {
        configController.getConfigStage().close();
        GeneratorConfig generatorConfig = new GeneratorConfig();
        generatorConfig.setConfigName(configNameText.getText());
        generatorConfig.setAuthor(authorText.getText());
        generatorConfig.setBeanLocation(beanLocationText.getText());
        generatorConfig.setBeanPackage(beanPackageText.getText());
        generatorConfig.setMapperLocation(mapperLocationText.getText());
        generatorConfig.setMapperPackage(mapperPackageText.getText());
        generatorConfig.setMapperXmlLocation(xmlLocationText.getText());

        generatorConfig.setUserJava8(userJava8CheckBox.isSelected());
        generatorConfig.setUseBigDecimal(useBigDecimalCheckBox.isSelected());
        generatorConfig.setUseComment(useCommentCheckBox.isSelected());
        generatorConfig.setUseSwagger(useSwaggerCheckBox.isSelected());

        configController.addConfig(generatorConfig);

        ListView<VBox> anchorPaneListView = mainController.getAnchorPaneListView();
        ObservableList<VBox> vBoxes = anchorPaneListView.getItems();
        //调用 mybatis 生成文件
        xmlGeneratorService.generatorXml(vBoxes, generatorConfig);
    }

    /**
     * 关闭
     */
    @FXML
    public void cancel() {
        configController.getConfigStage().close();
    }

    //-------------------------文件夹浏览------------------------------------------------------------------------------//

    /**
     * bean 文件夹选择器
     */
    @FXML
    public void beanDirectoryScan() {
        File directory = FileDirChooserFactory.createDirectoryScan(null, null);
        if (directory != null) {
            beanLocationText.setText(directory.getPath());
        }
    }

    /**
     * mapper 文件夹选择器
     */
    @FXML
    public void mapperDirectoryScan() {
        File directory = FileDirChooserFactory.createDirectoryScan(null, null);
        if (directory != null) {
            mapperLocationText.setText(directory.getPath());
        }
    }

    /**
     * mapper 文件夹选择器
     */
    @FXML
    public void xmlDirectoryScan() {
        File directory = FileDirChooserFactory.createDirectoryScan(null, null);
        if (directory != null) {
            xmlLocationText.setText(directory.getPath());
        }
    }

    /**
     * 展示配置信息
     *
     * @param generatorConfig 配置信息
     */
    void showConfig(GeneratorConfig generatorConfig) {
        configNameText.setText(generatorConfig.getConfigName());
        authorText.setText(generatorConfig.getAuthor());
        beanLocationText.setText(generatorConfig.getBeanLocation());
        beanPackageText.setText(generatorConfig.getBeanPackage());
        mapperLocationText.setText(generatorConfig.getMapperLocation());
        mapperPackageText.setText(generatorConfig.getMapperPackage());
        xmlLocationText.setText(generatorConfig.getMapperXmlLocation());

        userJava8CheckBox.setSelected(generatorConfig.isUserJava8());
        useBigDecimalCheckBox.setSelected(generatorConfig.isUseBigDecimal());
        useCommentCheckBox.setSelected(generatorConfig.isUseComment());
        useSwaggerCheckBox.setSelected(generatorConfig.isUseSwagger());
    }

    /**
     * 点击新增配置时清空配置面板
     */
    void clearPane() {
        configNameText.setText(null);
        authorText.setText(null);
        beanLocationText.setText(null);
        beanPackageText.setText(null);
        mapperLocationText.setText(null);
        mapperPackageText.setText(null);
        xmlLocationText.setText(null);

        userJava8CheckBox.setSelected(true);
        useBigDecimalCheckBox.setSelected(false);
        useCommentCheckBox.setSelected(true);
        useSwaggerCheckBox.setSelected(false);
    }
}
