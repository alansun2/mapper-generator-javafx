package com.alan344.controller;

import com.alan344.bean.GeneratorConfig;
import com.alan344.constants.StageConstants;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.ColumnService;
import com.alan344.service.TableService;
import com.alan344.service.generator.MapperGeneratorStrategyContext;
import com.alan344.utils.TextUtils;
import com.alan344happyframework.util.StringUtils;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.stage.Stage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/12 15:33
 */
@Slf4j
@Controller
public class ExportController implements Initializable {
    @FXML
    private TextField authorText;

    @FXML
    private TextField configNameText;

    @FXML
    private CheckBox modelOnlyCheckBox;

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
    private TextField mapperRootInterfaceText;

    /**
     * 官方的 mybatis-generator
     */
    @FXML
    private ToggleGroup targetName;

    @FXML
    private CheckBox userJava8CheckBox;

    @FXML
    private CheckBox useBigDecimalCheckBox;

    @FXML
    private CheckBox useCommentCheckBox;

    @FXML
    private CheckBox useSwaggerCheckBox;

    /**
     * tk.mybatis
     */
    @FXML
    private CheckBox userJava8CheckBox1;

    @FXML
    private CheckBox useBigDecimalCheckBox1;

    @FXML
    private CheckBox useCommentCheckBox1;

    @FXML
    private CheckBox useSwaggerCheckBox1;

    /**
     * mybatis-plus
     */
    @FXML
    private CheckBox userJava8CheckBox2;

    @FXML
    private CheckBox useBigDecimalCheckBox2;

    @FXML
    private CheckBox useCommentCheckBox2;

    @FXML
    private CheckBox useSwaggerCheckBox2;

    @FXML
    private CheckBox generateColumnConstantsCheckbox;

    @FXML
    private TabPane tabPane;

    @Resource
    private MapperGeneratorStrategyContext mapperGeneratorStrategyContext;

    @Resource
    private TableService tableService;

    @Resource
    private ColumnService columnService;

    @Resource
    private ConfigController configController;

    @Resource
    private ExportSuccessAlertController exportSuccessAlertController;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        modelOnlyCheckBox.selectedProperty().addListener((observable, oldValue, newValue) -> {
            mapperLocationText.setDisable(newValue);
            mapperPackageText.setDisable(newValue);
            xmlLocationText.setDisable(newValue);
            mapperRootInterfaceText.setDisable(newValue);
        });
    }

    /**
     * 应用 生成bean
     */
    @FXML
    public void apply() throws IOException {
        Stage configStage = StageConstants.configStage;
        if (TextUtils.checkTextsHasEmpty(configStage, configNameText, authorText, beanLocationText, beanPackageText, mapperLocationText, mapperPackageText, xmlLocationText)) {
            return;
        }

        GeneratorConfig generatorConfig = new GeneratorConfig();
        generatorConfig.setConfigName(configNameText.getText());
        generatorConfig.setAuthor(authorText.getText());
        generatorConfig.setBeanLocation(beanLocationText.getText());
        generatorConfig.setBeanPackage(beanPackageText.getText());
        generatorConfig.setMapperLocation(mapperLocationText.getText());
        generatorConfig.setMapperPackage(mapperPackageText.getText());
        generatorConfig.setMapperXmlLocation(xmlLocationText.getText());
        generatorConfig.setSelectTab(tabPane.getSelectionModel().getSelectedIndex());
        generatorConfig.setMapperRootInterface(mapperRootInterfaceText.getText());
        generatorConfig.setModelOnly(modelOnlyCheckBox.isSelected());

        // 配置信息
        this.selectTabSetup(generatorConfig);

        configController.addConfig(generatorConfig);

        // 导出时，如果 tableNameIsOverrideRecodeMap 不为空，则把 table 配置（如 insert）文件重写
        tableService.downLoadTableIfOverrideModify();

        // 导出时，如果 tableNameIsOverrideRecodeMap 不为空，则把 columns 文件重写
        columnService.downLoadColumnOverride();

        boolean exportSuccess = true;
        // 调用 mybatis generator 生成文件
        try {
            mapperGeneratorStrategyContext.getMapperGeneratorStrategy(generatorConfig).generator(generatorConfig);
        } catch (Throwable e) {
            log.error("export fail", e);
            exportSuccess = false;
        }

        Stage stage;
        if (exportSuccess) {
            // 导出成功，关闭 configStage
            configStage.close();
            stage = StageConstants.primaryStage;
        } else {
            stage = StageConstants.configStage;
        }

        // 成功或失败后的弹窗
        exportSuccessAlertController.openTableAdvancedSetup(stage, exportSuccess, generatorConfig);
    }

    /**
     * 配置选择
     */
    private void selectTabSetup(GeneratorConfig generatorConfig) {
        final int selectedIndex = tabPane.getSelectionModel().getSelectedIndex();
        switch (selectedIndex) {
            case 0:
                GeneratorConfig.MybatisExportConfig mybatisExportConfig = new GeneratorConfig.MybatisExportConfig();
                mybatisExportConfig.setTargetName(((RadioButton) targetName.getSelectedToggle()).getText());
                mybatisExportConfig.setUserJava8(userJava8CheckBox.isSelected());
                mybatisExportConfig.setUseBigDecimal(useBigDecimalCheckBox.isSelected());
                mybatisExportConfig.setUseComment(useCommentCheckBox.isSelected());
                mybatisExportConfig.setUseSwagger(useSwaggerCheckBox.isSelected());

                generatorConfig.setMybatisExportConfig(mybatisExportConfig);
                break;
            case 2:
                GeneratorConfig.MybatisPlusExportConfig mybatisPlusExportConfig = new GeneratorConfig.MybatisPlusExportConfig();
                mybatisPlusExportConfig.setUserJava8(userJava8CheckBox1.isSelected());
                mybatisPlusExportConfig.setUseBigDecimal(useBigDecimalCheckBox1.isSelected());
                mybatisPlusExportConfig.setUseComment(useCommentCheckBox1.isSelected());
                mybatisPlusExportConfig.setUseSwagger(useSwaggerCheckBox1.isSelected());

                generatorConfig.setMybatisPlusExportConfig(mybatisPlusExportConfig);
                break;
            case 1:
                GeneratorConfig.TkMybatisExportConfig tkMybatisExportConfig = new GeneratorConfig.TkMybatisExportConfig();
                tkMybatisExportConfig.setUserJava8(userJava8CheckBox2.isSelected());
                tkMybatisExportConfig.setUseBigDecimal(useBigDecimalCheckBox2.isSelected());
                tkMybatisExportConfig.setUseComment(useCommentCheckBox2.isSelected());
                tkMybatisExportConfig.setUseSwagger(useSwaggerCheckBox2.isSelected());
                tkMybatisExportConfig.setGenerateColumnConsts(generateColumnConstantsCheckbox.isSelected());

                generatorConfig.setTkMybatisExportConfig(tkMybatisExportConfig);
                break;
            default:
        }
    }

    /**
     * 关闭
     */
    @FXML
    public void cancel() {
        StageConstants.configStage.close();
    }

    //-------------------------文件夹浏览------------------------------------------------------------------------------//

    /**
     * 选择文件夹时，记录上次记录的选择的文件夹，方便用户选择
     */
    private String baseDir;

    /**
     * bean 文件夹选择器
     */
    @FXML
    public void beanDirectoryScan() {
        File directory = FileDirChooserFactory.createDirectoryScan(null, StringUtils.isEmpty(this.baseDir) ? null : this.baseDir);
        if (directory != null) {
            beanLocationText.setText(directory.getPath());
            this.baseDir = directory.getPath();
        }
    }

    /**
     * mapper 文件夹选择器
     */
    @FXML
    public void mapperDirectoryScan() {
        File directory = FileDirChooserFactory.createDirectoryScan(null, StringUtils.isEmpty(this.baseDir) ? null : this.baseDir);
        if (directory != null) {
            mapperLocationText.setText(directory.getPath());
            this.baseDir = directory.getPath();
        }
    }

    /**
     * mapper xml 文件夹选择器
     */
    @FXML
    public void xmlDirectoryScan() {
        File directory = FileDirChooserFactory.createDirectoryScan(null, StringUtils.isEmpty(this.baseDir) ? null : this.baseDir);
        if (directory != null) {
            xmlLocationText.setText(directory.getPath());
            this.baseDir = directory.getPath();
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
        mapperRootInterfaceText.setText(generatorConfig.getMapperRootInterface());
        tabPane.getSelectionModel().select(generatorConfig.getSelectTab());
        modelOnlyCheckBox.setSelected(generatorConfig.isModelOnly());

        // 一旦选择 modelOnly 后这四项不用填写
        if (generatorConfig.isModelOnly()) {
            mapperLocationText.setDisable(false);
            mapperPackageText.setDisable(false);
            xmlLocationText.setDisable(false);
            mapperRootInterfaceText.setDisable(false);
        }

        final GeneratorConfig.MybatisExportConfig mybatisExportConfig = generatorConfig.getMybatisExportConfig();
        userJava8CheckBox.setSelected(mybatisExportConfig.isUserJava8());
        useBigDecimalCheckBox.setSelected(mybatisExportConfig.isUseBigDecimal());
        useCommentCheckBox.setSelected(mybatisExportConfig.isUseComment());
        useSwaggerCheckBox.setSelected(mybatisExportConfig.isUseSwagger());
        final ObservableList<Toggle> toggles = targetName.getToggles();
        for (Toggle toggle : toggles) {
            final RadioButton radioButton = (RadioButton) toggle;
            if (radioButton.getText().equals(mybatisExportConfig.getTargetName())) {
                radioButton.setSelected(true);
                break;
            }
        }

//        final GeneratorConfig.MybatisPlusExportConfig mybatisPlusExportConfig = generatorConfig.getMybatisPlusExportConfig();
//        userJava8CheckBox1.setSelected(mybatisPlusExportConfig.isUserJava8());
//        useBigDecimalCheckBox1.setSelected(mybatisPlusExportConfig.isUseBigDecimal());
//        useCommentCheckBox1.setSelected(mybatisPlusExportConfig.isUseComment());
//        useSwaggerCheckBox1.setSelected(mybatisPlusExportConfig.isUseSwagger());

        final GeneratorConfig.TkMybatisExportConfig tkMybatisExportConfig = generatorConfig.getTkMybatisExportConfig();
        userJava8CheckBox2.setSelected(tkMybatisExportConfig.isUserJava8());
        useBigDecimalCheckBox2.setSelected(tkMybatisExportConfig.isUseBigDecimal());
        useCommentCheckBox2.setSelected(tkMybatisExportConfig.isUseComment());
        useSwaggerCheckBox2.setSelected(tkMybatisExportConfig.isUseSwagger());
        generateColumnConstantsCheckbox.setSelected(tkMybatisExportConfig.isGenerateColumnConsts());
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

