package com.alan344.controller;

import com.alan344.bean.config.ExtraFileConfig;
import com.alan344.componet.ExtraFileLabel;
import com.alan344.componet.FileSelectText;
import com.alan344.componet.PropertyHBox;
import com.alan344.constants.ExtraFileTypeEnum;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.ExtraFileConfigService;
import com.alan344.utils.StringUtils;
import com.alan344.utils.Toast;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2022/11/3 9:49
 */
@Service
public class MybatisExtraFileSetupController {
    @Autowired
    private ExtraFileConfigService extraFileConfigService;
    private Stage stage;
    private ListView<ExtraFileLabel> listView;

    private final Map<Integer, ExtraFileConfig> configTem = new HashMap<>();

    private Button saveBtn;

    public void openExtraFilePageInternal(boolean showCheckBox, Consumer<List<ExtraFileConfig>> consumer) {
        if (null != stage) {
            stage.show();
            return;
        }

        stage = new Stage();
        listView = new ListView<>();
        final List<ExtraFileConfig> extraFileConfigList = extraFileConfigService.getExtraFileConfigList();
        extraFileConfigList.forEach(extraFileConfig -> listView.getItems().add(this.packageExtraFileLabel(showCheckBox, extraFileConfig)));

        BorderPane borderPane = new BorderPane();
        borderPane.getStylesheets().add("/css/common.css");
        borderPane.setStyle("-fx-background-insets: 0");
        borderPane.setPrefHeight(500);
        borderPane.setPrefWidth(700);
        borderPane.addEventHandler(KeyEvent.KEY_RELEASED, event -> {
            if (event.getCode().equals(KeyCode.ESCAPE)) {
                stage.close();
            }
        });

        borderPane.setCenter(listView);

        borderPane.setBottom(this.getBtnHbox(stage, showCheckBox, consumer));
        stage.setScene(new Scene(borderPane));
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/advanced-set-up.png"));
        stage.setTitle("额外文件配置");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(NodeConstants.primaryStage);
        stage.show();
    }

    private HBox getBtnHbox(Stage stage, boolean showCheckBox, Consumer<List<ExtraFileConfig>> consumer) {
        int btnWidth = 50;
        HBox hBox = new HBox(15);
        hBox.setStyle("-fx-padding: 10;");
        hBox.setAlignment(Pos.CENTER_RIGHT);

        Button importBtn = new Button("导入");
        importBtn.setDisable(!showCheckBox);
        importBtn.setPrefWidth(btnWidth);
        importBtn.setOnAction(event -> {
            final ObservableList<ExtraFileLabel> items = listView.getItems();
            if (items.isEmpty()) {
                Toast.makeText(stage, "请先新增", 3000, 500, 500, 15, 5);
                return;
            }

            final List<ExtraFileLabel> selectedList = items.stream().filter(ExtraFileLabel::isSelected).toList();
            if (selectedList.isEmpty()) {
                Toast.makeText(stage, "至少选择一条导入", 3000, 500, 500, 15, 5);
                return;
            }
            final List<ExtraFileConfig> extraFileConfigs = selectedList.stream().map(ExtraFileLabel::getExtraFileConfig).collect(Collectors.toList());
            consumer.accept(extraFileConfigs);
        });

        saveBtn = new Button("保存配置");
        saveBtn.setPrefWidth(70);
        saveBtn.setOnAction(event -> {
            // 添加文件到缓存
            configTem.forEach((integer, extraFileConfig) -> {
                extraFileConfigService.addExtraFileConfig(extraFileConfig, integer < 0 ? null : integer);
            });
            // 保存到磁盘
            extraFileConfigService.saveExtraFileConfig();
            // 清除配置缓存
            configTem.clear();
            saveBtn.setDisable(true);
        });

        Button addBtn = new Button("新增");
        addBtn.setPrefWidth(btnWidth);
        addBtn.setOnAction(event -> {
            ExtraFileConfig extraFileConfig = new ExtraFileConfig();
            this.openExtraFileSetup(extraFileConfig, extraFileConfig1 -> {
                this.addExtraFileAfterSubmit(showCheckBox, extraFileConfig1);
                saveBtn.setDisable(false);
            }, false);
            configTem.put(-listView.getItems().size(), extraFileConfig);
            saveBtn.setDisable(false);
        });

        Button cancelBtn = new Button("取消");
        cancelBtn.setPrefWidth(btnWidth);
        cancelBtn.setOnAction(event -> stage.hide());
        hBox.getChildren().addAll(importBtn, saveBtn, addBtn, cancelBtn);
        return hBox;
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
        borderPane.addEventHandler(KeyEvent.KEY_RELEASED, event -> {
            if (event.getCode().equals(KeyCode.ESCAPE)) {
                addTemplateStage.close();
            }
        });

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
        cancelButton.setPrefWidth(50);
        cancelButton.setOnAction(actionEvent -> addTemplateStage.close());
        Button submitButton = new Button("确定");
        submitButton.setPrefWidth(50);
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

            // 检查文件配置
            this.checkConfig(extraFileConfig);

            submitBtnAction.accept(extraFileConfig);
            addTemplateStage.close();
        });
        HBox btnHbox = new HBox(10, cancelButton, submitButton);
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
        addTemplateStage.initOwner(stage);
        addTemplateStage.show();
    }

    private void checkConfig(ExtraFileConfig extraFileConfig) {
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

    private void addExtraFileAfterSubmit(boolean showCheckBox, ExtraFileConfig extraFileConfig) {
        listView.getItems().add(this.packageExtraFileLabel(showCheckBox, extraFileConfig));
    }

    private void copyItem(ExtraFileLabel old) {
        final ExtraFileConfig extraFileConfigSource = old.getExtraFileConfig();
        final ExtraFileConfig extraFileConfig = extraFileConfigSource.clone();
        extraFileConfig.setName(extraFileConfig.getName() + "copy");
        final ExtraFileLabel extraFileLabel = this.packageExtraFileLabel(old.isShowCheckBox(), extraFileConfig);
        final int i = listView.getItems().indexOf(old);
        listView.getItems().add(i + 1, extraFileLabel);
        configTem.put(i + 1, extraFileConfig);
    }

    private ExtraFileLabel packageExtraFileLabel(boolean showCheckBox, ExtraFileConfig extraFileConfig) {
        ExtraFileLabel extraFileLabel = new ExtraFileLabel(showCheckBox, extraFileConfig);
        extraFileLabel.setPrefHeight(23);
        extraFileLabel.setAlignment(Pos.CENTER);
        extraFileLabel.prefWidthProperty().bind(listView.widthProperty().subtract(220));
        // 编辑
        extraFileLabel.onEditAction(actionEvent -> this.openExtraFileSetup(extraFileConfig,
                extraFileConfig1 -> {
                    extraFileLabel.setLabelText(extraFileConfig1.getName());
                    saveBtn.setDisable(false);
                }, true));
        // 删除
        extraFileLabel.onDelAction(actionEvent -> {
            listView.getItems().remove(extraFileLabel);
            extraFileConfigService.deleteExtraFileConfig(extraFileConfig);
            saveBtn.setDisable(false);
        });
        // 复制
        extraFileLabel.onCopyAction(actionEvent -> {
            this.copyItem(extraFileLabel);
            saveBtn.setDisable(false);
        });

        return extraFileLabel;
    }
}
