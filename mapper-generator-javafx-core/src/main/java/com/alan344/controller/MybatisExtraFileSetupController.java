package com.alan344.controller;

import com.alan344.bean.config.ExtraFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.componet.ExtraFileLabel;
import com.alan344.componet.FileSelectText;
import com.alan344.componet.PropertyHBox;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ExtraFileTypeEnum;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.ExportService;
import com.alan344.service.node.NodeHandler;
import com.alan344.utils.CollectionUtils;
import com.alan344.utils.StringUtils;
import com.alan344.utils.Toast;
import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.net.URL;
import java.util.List;
import java.util.ResourceBundle;
import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/11/3 9:49
 */
@Service
public class MybatisExtraFileSetupController implements Initializable {
    @Autowired
    private ExportService exportService;
    private final NodeHandler nodeHandler = NodeHandler.getSingleTon(true);
    @FXML
    private ListView<ExtraFileLabel> listView;

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        final List<ExtraFileConfig> extraFileConfigs = currentConfig.getExtraFileConfigs();
        if (CollectionUtils.isNotEmpty(extraFileConfigs)) {
            extraFileConfigs.forEach(this::addExtraFileAfterSubmit);
        }

        // 单选
        listView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
    }

    @FXML
    public void export() {
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        final List<ExtraFileConfig> extraFileConfigs = currentConfig.getExtraFileConfigs();
        for (ExtraFileConfig extraFileConfig : extraFileConfigs) {
            final ExtraFileTypeEnum templateType = extraFileConfig.getExtraFileType();
            if (StringUtils.isEmpty(extraFileConfig.getOutputPath())) {
                Toast.makeText(NodeConstants.primaryStage, extraFileConfig.getName() + "配置中，文件地址必填", 3000, 500, 500, 15, 5);
                return;
            }
            if (StringUtils.isEmpty(extraFileConfig.getPackageName())) {
                Toast.makeText(NodeConstants.primaryStage, extraFileConfig.getName() + "配置中，包名必填", 3000, 500, 500, 15, 5);
                return;
            }
            if (templateType == ExtraFileTypeEnum.CUSTOM_TEMPLATE) {
                if (StringUtils.isEmpty(extraFileConfig.getCustomTemplateDir())) {
                    Toast.makeText(NodeConstants.primaryStage, extraFileConfig.getName() + "配置中，自定义模板路径必填", 3000, 500, 500, 15, 5);
                    return;
                }
            }
        }
        exportService.export(currentConfig);
    }

    @FXML
    public void pre() {
        NodeConstants.borderPaneWrap.setCenter(nodeHandler.getPre());
    }

    /**
     * 添加额外文件
     */
    @FXML
    public void saveSetup() {
        exportService.saveSetup(BaseConstants.currentConfig);
    }

    /**
     * 添加额外文件
     */
    @FXML
    public void addExtraFile() {
        ExtraFileConfig extraFileConfig = new ExtraFileConfig();
        this.openExtraFileSetup(extraFileConfig, o -> {
            this.addExtraFileAfterSubmit(o);
            BaseConstants.currentConfig.getExtraFileConfigs().add(extraFileConfig);
        }, false);
    }

    /**
     * 文件浏览的地址缓存
     */
    private String baseDir;

    /**
     * 打开额外文件设置
     *
     * @param extraFileConfig 额外文件配置
     * @param submitBtnAction 确定按钮操作
     * @param isEdit          true:编辑,false:新增
     */
    private void openExtraFileSetup(ExtraFileConfig extraFileConfig, Consumer<ExtraFileConfig> submitBtnAction, boolean isEdit) {
        Stage addTemplateStage = new Stage();
        BorderPane borderPane = new BorderPane();
        borderPane.getStylesheets().add("/css/common.css");
        VBox vBox = new VBox(5);
        vBox.setPrefWidth(500);
        borderPane.setCenter(vBox);
        borderPane.setPadding(new Insets(10));

        int labelWidth = 130;
        // 属性名称
        final TextField nameTextField = new TextField(extraFileConfig.getName());
        nameTextField.setPromptText("配置名称");
        PropertyHBox nameHbox = new PropertyHBox("配置名称", labelWidth, nameTextField);
        vBox.getChildren().add(nameHbox);

        // 文件类型
        ChoiceBox<ExtraFileTypeEnum> fileTypeCb = new ChoiceBox<>(FXCollections.observableArrayList(ExtraFileTypeEnum.MODEL, ExtraFileTypeEnum.CUSTOM_TEMPLATE));
        fileTypeCb.getItems().addAll();
        fileTypeCb.setValue(extraFileConfig.getExtraFileType());
        if (isEdit) {
            fileTypeCb.setDisable(true);
        }
        PropertyHBox fileTypeHbox = new PropertyHBox("文件类型", labelWidth, fileTypeCb);
        vBox.getChildren().add(fileTypeHbox);

        // 文件地址
        FileSelectText outputPathTextField = new FileSelectText("浏览", extraFileConfig.getOutputPath());
        outputPathTextField.setPromptText("文件输出地址");
        outputPathTextField.setTextTooltip("不包含包名的路径");
        outputPathTextField.onAction(actionEvent -> {
            // 文件导出地址
            baseDir = extraFileConfig.getOutputPath();
            File directory = FileDirChooserFactory.createDirectoryScan(null, StringUtils.getDefaultIfNull(this.baseDir, null));
            if (directory != null) {
                outputPathTextField.setText(directory.getPath());
                this.baseDir = directory.getPath();
            }
        });
        PropertyHBox outputPathHbox = new PropertyHBox("文件输出地址", labelWidth, outputPathTextField);
        vBox.getChildren().add(outputPathHbox);

        // 包名
        TextField packageNameTextField = new TextField(extraFileConfig.getPackageName());
        packageNameTextField.setPromptText("包名");
        PropertyHBox packageNameHbox = new PropertyHBox("包名", labelWidth, packageNameTextField);
        vBox.getChildren().add(packageNameHbox);

        // 父类
        final TextField superClassTextField = new TextField(extraFileConfig.getSuperClass());
        superClassTextField.setPromptText("父类，类全限定名称");
        PropertyHBox superClassHbox = new PropertyHBox("父类", labelWidth, superClassTextField);
        vBox.getChildren().add(superClassHbox);

        // model 后缀
        TextField modelSuffixTextField = new TextField(extraFileConfig.getModelSuffix());
        modelSuffixTextField.setPromptText("model 后缀");
        PropertyHBox modelSuffixHbox = new PropertyHBox("model 后缀", labelWidth, modelSuffixTextField);
        vBox.getChildren().add(modelSuffixHbox);

        // 是否开启 validation 注解
        CheckBox checkBox = new CheckBox();
        checkBox.setSelected(extraFileConfig.isGenerateValidAnnotation());
        PropertyHBox modelValidSuffixHbox = new PropertyHBox("是否开启 Validate 注解", labelWidth, checkBox);
        vBox.getChildren().add(modelValidSuffixHbox);

        // lombok @Getter
        CheckBox lombokGetterCheckBox = new CheckBox();
        lombokGetterCheckBox.setSelected(extraFileConfig.isLombokGetter());
        PropertyHBox lombokGetterHbox = new PropertyHBox("lombok @Getter", labelWidth, lombokGetterCheckBox);
        vBox.getChildren().add(lombokGetterHbox);

        // lombok @Setter
        CheckBox lombokSetterCheckBox = new CheckBox();
        lombokSetterCheckBox.setSelected(extraFileConfig.isLombokSetter());
        PropertyHBox lombokSetterHbox = new PropertyHBox("lombok @Setter", labelWidth, lombokSetterCheckBox);
        vBox.getChildren().add(lombokSetterHbox);

        // lombok @ToString
        CheckBox lombokToStringCheckBox = new CheckBox();
        lombokToStringCheckBox.setSelected(extraFileConfig.isLombokToString());
        PropertyHBox lombokToStringHbox = new PropertyHBox("lombok @ToString", labelWidth, lombokToStringCheckBox);
        vBox.getChildren().add(lombokToStringHbox);

        // 忽略字段
        TextField ignoreColumnTextField = new TextField(extraFileConfig.getModelIgnoreColumns());
        ignoreColumnTextField.setPromptText("忽略字段，逗号分隔");
        PropertyHBox ignoreColumnHbox = new PropertyHBox("忽略字段", labelWidth, ignoreColumnTextField);
        vBox.getChildren().add(ignoreColumnHbox);

        // 自定义模板文件夹
        FileSelectText customTemplatePathTextField = new FileSelectText("浏览", extraFileConfig.getCustomTemplateDir());
        customTemplatePathTextField.setPromptText("模板文件夹");
        customTemplatePathTextField.onAction(actionEvent -> {
            baseDir = extraFileConfig.getCustomTemplateDir();
            // 文件导出地址
            File directory = FileDirChooserFactory.createDirectoryScan(null, StringUtils.getDefaultIfNull(this.baseDir, null));
            if (directory != null) {
                customTemplatePathTextField.setText(directory.getPath());
                this.baseDir = directory.getPath();
            }
        });
        PropertyHBox customTemplatePathHbox = new PropertyHBox("模板文件夹", labelWidth, customTemplatePathTextField);
        vBox.getChildren().add(customTemplatePathHbox);

        // 模板文件名称
        FileSelectText customTemplateFileNameTextField = new FileSelectText("浏览", extraFileConfig.getCustomTemplateFileName());
        customTemplateFileNameTextField.setPromptText("模板文件名称");
        customTemplateFileNameTextField.onAction(actionEvent -> {
            baseDir = extraFileConfig.getCustomTemplateDir();
            // 文件导出地址
            File file = FileDirChooserFactory.createFileScan(null, StringUtils.getDefaultIfNull(this.baseDir, null), "freemarker 文件", "*.ftl");
            if (file != null) {
                customTemplateFileNameTextField.setText(file.getName());
                this.baseDir = file.getParent();
            }
        });
        PropertyHBox customTemplateFileNameHbox = new PropertyHBox("模板文件名称", labelWidth, customTemplateFileNameTextField);
        vBox.getChildren().add(customTemplateFileNameHbox);

        // 展示
        this.showByExtraFileType(extraFileConfig.getExtraFileType(), modelValidSuffixHbox, lombokGetterHbox,
                lombokSetterHbox, lombokToStringHbox, ignoreColumnHbox, customTemplatePathHbox, customTemplateFileNameHbox);

        fileTypeCb.getSelectionModel().selectedIndexProperty().addListener((observable, oldValue, newValue) -> {
            final ExtraFileTypeEnum extraFileTypeEnum = fileTypeCb.getItems().get(newValue.intValue());
            this.showByExtraFileType(extraFileTypeEnum, modelValidSuffixHbox, lombokGetterHbox,
                    lombokSetterHbox, lombokToStringHbox, ignoreColumnHbox, customTemplatePathHbox, customTemplateFileNameHbox);
        });

        // 按钮
        Button cancelButton = new Button("取消");
        cancelButton.setOnAction(actionEvent -> addTemplateStage.close());
        Button submitButton = new Button("确定");
        submitButton.setOnAction(actionEvent -> {
            extraFileConfig.setName(nameTextField.getText());
            extraFileConfig.setExtraFileType(fileTypeCb.getSelectionModel().getSelectedItem());
            extraFileConfig.setOutputPath(outputPathTextField.getText());
            extraFileConfig.setPackageName(packageNameTextField.getText());
            extraFileConfig.setSuperClass(superClassTextField.getText());
            extraFileConfig.setModelSuffix(modelSuffixTextField.getText());
            extraFileConfig.setGenerateValidAnnotation(checkBox.isSelected());
            extraFileConfig.setModelIgnoreColumns(ignoreColumnTextField.getText());
            extraFileConfig.setLombokGetter(lombokGetterCheckBox.isSelected());
            extraFileConfig.setLombokSetter(lombokSetterCheckBox.isSelected());
            extraFileConfig.setLombokToString(lombokToStringCheckBox.isSelected());
            extraFileConfig.setCustomTemplateDir(customTemplatePathTextField.getText());
            extraFileConfig.setCustomTemplateFileName(customTemplateFileNameTextField.getText());

            submitBtnAction.accept(extraFileConfig);
            addTemplateStage.close();
        });
        HBox btnHbox = new HBox(20, cancelButton, submitButton);
        btnHbox.setStyle("-fx-padding: 10 0 0 0");
        btnHbox.setAlignment(Pos.CENTER_RIGHT);
        borderPane.setBottom(btnHbox);

        // ESC 监听
        vBox.setOnKeyPressed(event -> {
            if (KeyCode.ESCAPE.equals(event.getCode())) {
                addTemplateStage.close();
            }
        });
        addTemplateStage.setScene(new Scene(borderPane));
        addTemplateStage.setResizable(false);
        addTemplateStage.getIcons().add(new Image("/image/advanced-set-up.png"));
        addTemplateStage.setTitle((isEdit ? "编辑" : "新增") + "额外文件");
        addTemplateStage.initModality(Modality.WINDOW_MODAL);
        addTemplateStage.initOwner(NodeConstants.primaryStage);
        addTemplateStage.show();
    }

    private void showByExtraFileType(ExtraFileTypeEnum extraFileTypeEnum,
                                     PropertyHBox modelValidSuffixHbox,
                                     PropertyHBox lombokGetterHbox,
                                     PropertyHBox lombokSetterHbox,
                                     PropertyHBox lombokToStringHbox,
                                     PropertyHBox ignoreColumnHbox,
                                     PropertyHBox customTemplatePathHbox,
                                     PropertyHBox templateFileNameHbox) {
        if (extraFileTypeEnum == ExtraFileTypeEnum.MODEL) {
            customTemplatePathHbox.setManaged(false);
            customTemplatePathHbox.setVisible(false);
            templateFileNameHbox.setManaged(false);
            templateFileNameHbox.setVisible(false);
            modelValidSuffixHbox.setManaged(true);
            modelValidSuffixHbox.setVisible(true);
            lombokGetterHbox.setManaged(true);
            lombokGetterHbox.setVisible(true);
            lombokSetterHbox.setManaged(true);
            lombokSetterHbox.setVisible(true);
            lombokToStringHbox.setManaged(true);
            lombokToStringHbox.setVisible(true);
            ignoreColumnHbox.setManaged(true);
            ignoreColumnHbox.setVisible(true);
        } else {
            customTemplatePathHbox.setManaged(true);
            customTemplatePathHbox.setVisible(true);
            templateFileNameHbox.setManaged(true);
            templateFileNameHbox.setVisible(true);
            modelValidSuffixHbox.setManaged(false);
            modelValidSuffixHbox.setVisible(false);
            lombokGetterHbox.setManaged(false);
            lombokGetterHbox.setVisible(false);
            lombokSetterHbox.setManaged(false);
            lombokSetterHbox.setVisible(false);
            lombokToStringHbox.setManaged(false);
            lombokToStringHbox.setVisible(false);
            ignoreColumnHbox.setManaged(false);
            ignoreColumnHbox.setVisible(false);
        }
    }

    private void addExtraFileAfterSubmit(ExtraFileConfig extraFileConfig) {
        listView.getItems().add(this.packageExtraFileLabel(extraFileConfig));
    }

    private void copyItem(ExtraFileLabel old) {
        final ExtraFileConfig extraFileConfigSource = old.getExtraFileConfig();
        final ExtraFileConfig extraFileConfig = extraFileConfigSource.clone();
        extraFileConfig.setName(extraFileConfig.getName() + "copy");
        final ExtraFileLabel extraFileLabel = this.packageExtraFileLabel(extraFileConfig);
        final int i = listView.getItems().indexOf(old);
        listView.getItems().add(i + 1, extraFileLabel);
        BaseConstants.currentConfig.getExtraFileConfigs().add(i + 1, extraFileConfig);
    }

    private ExtraFileLabel packageExtraFileLabel(ExtraFileConfig extraFileConfig) {
        ExtraFileLabel extraFileLabel = new ExtraFileLabel(extraFileConfig.getName(), extraFileConfig.getExtraFileType(),
                extraFileConfig.isEnable(), extraFileConfig::setEnable);
        extraFileLabel.setPrefHeight(23);
        extraFileLabel.setAlignment(Pos.CENTER);
        extraFileLabel.setExtraFileConfig(extraFileConfig);
        extraFileLabel.prefWidthProperty().bind(listView.widthProperty().subtract(220));
        // 编辑
        extraFileLabel.onEditAction(actionEvent -> this.openExtraFileSetup(extraFileConfig,
                extraFileConfig1 -> extraFileLabel.setLabelText(extraFileConfig1.getName()), true));
        // 删除
        extraFileLabel.onDelAction(actionEvent -> {
            listView.getItems().remove(extraFileLabel);
            BaseConstants.currentConfig.getExtraFileConfigs().remove(extraFileConfig);
        });
        // 复制
        extraFileLabel.onCopyAction(actionEvent -> {
            this.copyItem(extraFileLabel);
        });

        return extraFileLabel;
    }
}
