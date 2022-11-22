package com.alan344.controller.component;

import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.componet.FileSelectText;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.utils.StringUtils;
import com.alan344.utils.TextUtils;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Controller;

import java.io.File;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/12 15:33
 */
@Slf4j
@Controller
public class MybatisExportController implements Initializable {
    @FXML
    private BorderPane exportSetupBP;
    @Getter
    @FXML
    private ListView<HBox> exportSetupLV;
    @FXML
    private TextField authorText;
    @FXML
    private TextField configNameText;
    @FXML
    private CheckBox modelOnlyCheckBox;
    @FXML
    private FileSelectText beanLocationText;
    @FXML
    private TextField beanPackageText;
    @FXML
    private TextField beanRootClassText;
    @FXML
    private FileSelectText mapperLocationText;
    @FXML
    private TextField mapperPackageText;
    @FXML
    private FileSelectText xmlLocationText;
    @FXML
    private TextField mapperRootInterfaceText;
    @FXML
    private TextField globalIgnoreFieldText;
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
    private CheckBox useLombokGetSetCheckBox;

    @FXML
    private CheckBox useLombokBuilderCheckBox;
    /**
     * tab
     */
    @FXML
    private TabPane tabPane;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        exportSetupLV.prefWidthProperty().bind(exportSetupBP.prefWidthProperty());

        modelOnlyCheckBox.selectedProperty().addListener((observable, oldValue, newValue) -> {
            mapperLocationText.setDisable(newValue);
            mapperPackageText.setDisable(newValue);
            xmlLocationText.setDisable(newValue);
            mapperRootInterfaceText.setDisable(newValue);
        });

        // 绑定文本框和tab的宽度
        final ObservableList<HBox> hBoxs = exportSetupLV.getItems();
        hBoxs.forEach(node -> {
            final ObservableList<Node> children = node.getChildren();
            if (children.size() > 1) {
                final Node node1 = children.get(1);
                if (node1 instanceof TextField) {
                    ((TextField) node1).prefWidthProperty().bind(exportSetupLV.widthProperty().subtract(220));
                } else if (node1 instanceof FileSelectText) {
                    ((FileSelectText) node1).prefWidthProperty().bind(exportSetupLV.widthProperty().subtract(220));
                }
            }
        });

        tabPane.prefWidthProperty().bind(exportSetupBP.widthProperty());

        mapperLocationText.onAction(actionEvent -> this.mapperDirectoryScan());
        beanLocationText.onAction(actionEvent -> this.beanDirectoryScan());
        xmlLocationText.onAction(actionEvent -> this.xmlDirectoryScan());
    }

    /**
     * 校验配置是否符合要求
     */
    public void validExport() {
        TextUtils.checkTextsHasEmpty(NodeConstants.primaryStage, configNameText, authorText, beanLocationText.getTextField(), beanPackageText, mapperLocationText.getTextField(), mapperPackageText, xmlLocationText.getTextField());
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
        File directory = FileDirChooserFactory.createDirectoryScan(null, !StringUtils.isNotEmpty(this.baseDir) ? null : this.baseDir);
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
        File directory = FileDirChooserFactory.createDirectoryScan(null, !StringUtils.isNotEmpty(this.baseDir) ? null : this.baseDir);
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
        File directory = FileDirChooserFactory.createDirectoryScan(null, !StringUtils.isNotEmpty(this.baseDir) ? null : this.baseDir);
        if (directory != null) {
            xmlLocationText.setText(directory.getPath());
            this.baseDir = directory.getPath();
        }
    }

    /**
     * 展示配置信息
     *
     * @param mybatisExportConfig 配置信息
     */
    public void showConfig(MybatisExportConfig mybatisExportConfig) {
        if (null == mybatisExportConfig) {
            return;
        }
        BaseConstants.currentConfig = mybatisExportConfig.clone();

        configNameText.setText(mybatisExportConfig.getConfigName());
        authorText.setText(mybatisExportConfig.getAuthor());
        beanLocationText.setText(mybatisExportConfig.getBeanLocation());
        beanPackageText.setText(mybatisExportConfig.getBeanPackage());
        beanRootClassText.setText(mybatisExportConfig.getModelRootClass());
        mapperLocationText.setText(mybatisExportConfig.getMapperLocation());
        mapperPackageText.setText(mybatisExportConfig.getMapperPackage());
        xmlLocationText.setText(mybatisExportConfig.getMapperXmlLocation());
        mapperRootInterfaceText.setText(mybatisExportConfig.getMapperRootInterface());
        globalIgnoreFieldText.setText(mybatisExportConfig.getGlobalIgnoreField());
        tabPane.getSelectionModel().select(mybatisExportConfig.getSelectTab());
        modelOnlyCheckBox.setSelected(mybatisExportConfig.isModelOnly());

        // official mybatis
        final MybatisExportConfig.MybatisOfficialExportConfig mybatisOfficialExportConfig = mybatisExportConfig.getMybatisOfficialExportConfig();
        userJava8CheckBox.setSelected(mybatisOfficialExportConfig.isUserJava8());
        useBigDecimalCheckBox.setSelected(mybatisOfficialExportConfig.isUseBigDecimal());
        useCommentCheckBox.setSelected(mybatisOfficialExportConfig.isUseComment());
        useLombokGetSetCheckBox.setSelected(mybatisOfficialExportConfig.isUseLombokGetSet());
        useLombokBuilderCheckBox.setSelected(mybatisOfficialExportConfig.isUseLombokBuilder());
        final ObservableList<Toggle> toggles = targetName.getToggles();
        for (Toggle toggle : toggles) {
            final RadioButton radioButton = (RadioButton) toggle;
            if (radioButton.getText().equals(mybatisOfficialExportConfig.getTargetName())) {
                radioButton.setSelected(true);
                break;
            }
        }
    }

    /**
     * 获取配置信息
     */
    public MybatisExportConfig getConfig(MybatisExportConfig mybatisExportConfig) {
        mybatisExportConfig.setConfigName(configNameText.getText());
        mybatisExportConfig.setAuthor(authorText.getText());
        mybatisExportConfig.setBeanLocation(beanLocationText.getText());
        mybatisExportConfig.setBeanPackage(beanPackageText.getText());
        mybatisExportConfig.setModelRootClass(beanRootClassText.getText());
        mybatisExportConfig.setMapperLocation(mapperLocationText.getText());
        mybatisExportConfig.setMapperPackage(mapperPackageText.getText());
        mybatisExportConfig.setMapperXmlLocation(xmlLocationText.getText());
        mybatisExportConfig.setSelectTab(tabPane.getSelectionModel().getSelectedIndex());
        mybatisExportConfig.setMapperRootInterface(mapperRootInterfaceText.getText());
        mybatisExportConfig.setGlobalIgnoreField(globalIgnoreFieldText.getText());
        mybatisExportConfig.setModelOnly(modelOnlyCheckBox.isSelected());

        final int selectedIndex = tabPane.getSelectionModel().getSelectedIndex();
        switch (selectedIndex) {
            case 0:
                MybatisExportConfig.MybatisOfficialExportConfig mybatisOfficialExportConfig = new MybatisExportConfig.MybatisOfficialExportConfig();
                mybatisOfficialExportConfig.setTargetName(((RadioButton) targetName.getSelectedToggle()).getText());
                mybatisOfficialExportConfig.setUserJava8(userJava8CheckBox.isSelected());
                mybatisOfficialExportConfig.setUseBigDecimal(useBigDecimalCheckBox.isSelected());
                mybatisOfficialExportConfig.setUseComment(useCommentCheckBox.isSelected());
                mybatisOfficialExportConfig.setUseLombokGetSet(useLombokGetSetCheckBox.isSelected());
                mybatisOfficialExportConfig.setUseLombokBuilder(useLombokBuilderCheckBox.isSelected());

                mybatisExportConfig.setMybatisOfficialExportConfig(mybatisOfficialExportConfig);
                break;
            default:
        }

        return mybatisExportConfig;
    }

    /**
     * 点击新增配置时清空配置面板
     */
    public void clearPane() {
        configNameText.setText(null);
        authorText.setText(null);
        beanLocationText.setText(null);
        beanPackageText.setText(null);
        beanRootClassText.setText(null);
        mapperLocationText.setText(null);
        mapperPackageText.setText(null);
        xmlLocationText.setText(null);
        mapperRootInterfaceText.setText(null);
        globalIgnoreFieldText.setText(null);
        modelOnlyCheckBox.setSelected(false);

        // 重置到第 0 个 tab
        tabPane.getSelectionModel().select(0);

        // official mybatis
        targetName.getToggles().get(0).setSelected(true);
        userJava8CheckBox.setSelected(true);
        useBigDecimalCheckBox.setSelected(false);
        useCommentCheckBox.setSelected(true);
        useLombokGetSetCheckBox.setSelected(true);
        useLombokBuilderCheckBox.setSelected(false);
    }
}

