package com.alan344.controller;

import com.alan344.bean.GeneratorConfig;
import com.alan344.constants.StageConstants;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.ColumnService;
import com.alan344.service.TableService;
import com.alan344.service.MyMybatisGeneratorService;
import com.alan344.utils.TextUtils;
import com.alan344happyframework.util.StringUtils;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;
import javafx.stage.Stage;
import lombok.extern.slf4j.Slf4j;
import javax.annotation.Resource;
import org.springframework.stereotype.Controller;

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

    @Resource
    private MyMybatisGeneratorService myMybatisGeneratorService;

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
    }

    /**
     * 应用生成xml,并生成bean
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

        generatorConfig.setUserJava8(userJava8CheckBox.isSelected());
        generatorConfig.setUseBigDecimal(useBigDecimalCheckBox.isSelected());
        generatorConfig.setUseComment(useCommentCheckBox.isSelected());
        generatorConfig.setUseSwagger(useSwaggerCheckBox.isSelected());
//        generatorConfig.setUserMerge(useMergeCheckBox.isSelected());

        configController.addConfig(generatorConfig);

        // 导出时，如果 tableNameIsOverrideRecodeMap 不为空，则把 table 配置（如 insert）文件重写
        tableService.downLoadTableIfOverrideModify();

        // 导出时，如果 tableNameIsOverrideRecodeMap 不为空，则把 columns 文件重写
        columnService.downLoadColumnOverride();

        boolean exportSuccess = true;
        // 调用 mybatis generator 生成文件
        try {
            myMybatisGeneratorService.generator(generatorConfig);
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

        exportSuccessAlertController.openTableAdvancedSetUP(stage, exportSuccess);
    }

    /**
     * 关闭
     */
    @FXML
    public void cancel() {
        StageConstants.configStage.close();
    }

    //-------------------------文件夹浏览------------------------------------------------------------------------------//

    // 选择文件夹时，记录上次记录的选择的文件夹，方便用户选择
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

        userJava8CheckBox.setSelected(generatorConfig.isUserJava8());
        useBigDecimalCheckBox.setSelected(generatorConfig.isUseBigDecimal());
        useCommentCheckBox.setSelected(generatorConfig.isUseComment());
        useSwaggerCheckBox.setSelected(generatorConfig.isUseSwagger());
//        useMergeCheckBox.setSelected(generatorConfig.isUserMerge());
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
//        useMergeCheckBox.setSelected(false);
    }
}
