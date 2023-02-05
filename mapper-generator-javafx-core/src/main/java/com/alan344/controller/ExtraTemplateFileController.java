package com.alan344.controller;

import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.ExtraTemplateFileGroupConfig;
import com.alan344.componet.*;
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
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2022/11/3 9:49
 */
@Service
public class ExtraTemplateFileController {
    @Autowired
    private ExtraFileConfigService extraFileConfigService;
    private Stage stage;
    private Button saveBtn;
    private LeftRightLinkageBorderPane<ExtraTemplateFileConfig, ExtraTemplateFileGroupConfig, ExtraTemplateFileGroupItemHBox> linkageBorderPane;

    public void openExtraFilePageInternal(boolean showCheckBox, Consumer<List<ExtraTemplateFileConfig>> consumer) {
        if (null != stage) {
            stage.show();
            return;
        }
        stage = new Stage();
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/icon.png"));
        stage.setTitle("额外文件配置");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(NodeConstants.primaryStage);
        stage.setScene(new Scene(this.getBorderPane(showCheckBox, consumer)));
        stage.show();
    }

    public BorderPane getBorderPane(boolean showCheckBox, Consumer<List<ExtraTemplateFileConfig>> consumer) {
        linkageBorderPane = new LeftRightLinkageBorderPane<>(
                ExtraTemplateFileGroupConfig::new,
                ExtraTemplateFileGroupItemHBox::new,
                extraTemplateFileGroupConfig -> this.getCenterListView(showCheckBox, extraTemplateFileGroupConfig),
                NodeConstants.primaryStage,
                this.getBottomBtns(stage, showCheckBox, consumer),
                0.25
        );
        linkageBorderPane.setPrefHeight(550);
        linkageBorderPane.setPrefWidth(800);

        final List<ExtraTemplateFileGroupConfig> extraTemplateFileGroupConfigs = extraFileConfigService.getExtraFileConfigList();
        linkageBorderPane.addLeftItems(extraTemplateFileGroupConfigs, extraTemplateFileGroupConfig -> this.getCenterListView(showCheckBox, extraTemplateFileGroupConfig));
        return linkageBorderPane;
    }

    private ListView<ExtraTemplateFileItemHBox> getCenterListView(boolean showCheckBox, ExtraTemplateFileGroupConfig extraTemplateFileGroupConfig) {
        ListView<ExtraTemplateFileItemHBox> listView = new ListView<>();
        extraTemplateFileGroupConfig.getExtraTemplateFileConfigList().forEach(extraFileConfig -> listView.getItems().add(this.packageextrafilelabel(showCheckBox, extraFileConfig)));
        return listView;
    }

    private List<Button> getBottomBtns(Stage stage, boolean showCheckBox, Consumer<List<ExtraTemplateFileConfig>> consumer) {
        int btnWidth = 70;
        Button importBtn = new Button("导入");
        importBtn.setDisable(!showCheckBox);
        importBtn.setPrefWidth(btnWidth);
        importBtn.setOnAction(event -> {
            final ObservableList<ExtraTemplateFileItemHBox> items = ((ListView<ExtraTemplateFileItemHBox>) linkageBorderPane.getRightBorderPane().getCenter()).getItems();
            if (items.isEmpty()) {
                Toast.makeText(stage, "请先添加", 3000, 500, 500, 15, 5);
                return;
            }

            final List<ExtraTemplateFileItemHBox> selectedList = items.stream().filter(ExtraTemplateFileItemHBox::isSelected).toList();
            if (selectedList.isEmpty()) {
                Toast.makeText(stage, "至少选择一条导入", 3000, 500, 500, 15, 5);
                return;
            }
            final List<ExtraTemplateFileGroupConfig> extraTemplateFileGroupConfigs = linkageBorderPane.getGroupLeftListView().getItems().stream()
                    .map(ExtraTemplateFileGroupItemHBox::getConfig).toList();
            // 保存到磁盘
            extraFileConfigService.saveExtraFileConfig(extraTemplateFileGroupConfigs);
            saveBtn.setDisable(true);
            final List<ExtraTemplateFileConfig> extraTemplateFileConfigs = selectedList.stream().map(ExtraTemplateFileItemHBox::getExtraTemplateFileConfig).collect(Collectors.toList());
            consumer.accept(extraTemplateFileConfigs);
        });

        saveBtn = new Button("保存配置");
        saveBtn.setPrefWidth(70);
        saveBtn.setOnAction(event -> {
            final List<ExtraTemplateFileGroupConfig> items = linkageBorderPane.getGroupLeftListView().getItems().stream()
                    .map(ExtraTemplateFileGroupItemHBox::getConfig).toList();
            // 保存到磁盘
            extraFileConfigService.saveExtraFileConfig(items);
            saveBtn.setDisable(true);
        });

        Button addBtn = new Button("添加");
        addBtn.setPrefWidth(btnWidth);
        addBtn.setOnAction(event -> {
            final ExtraTemplateFileGroupItemHBox selectedItem = linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem();
            if (null == selectedItem) {
                Toast.makeText(stage, "请选择一个分组再添加", 3000, 500, 500, 15, 5);
                return;
            }
            final ExtraTemplateFileGroupConfig config = selectedItem.getConfig();
            if (config.getIsSystem()) {
                Toast.makeText(stage, "不能使用默认分组， 请新建分组后再使用", 3000, 500, 500, 15, 5);
                return;
            }
            ExtraTemplateFileConfig extraTemplateFileConfig = new ExtraTemplateFileConfig();
            this.openExtraFileSetup(extraTemplateFileConfig, false, extraFileConfig1 -> {
                ((ListView<ExtraTemplateFileItemHBox>) linkageBorderPane.getRightBorderPane().getCenter()).getItems().add(this.packageextrafilelabel(showCheckBox, extraFileConfig1));
                saveBtn.setDisable(false);
            });
            saveBtn.setDisable(false);
        });

        Button cancelBtn = new Button("关闭");
        cancelBtn.setPrefWidth(btnWidth);
        cancelBtn.setOnAction(event -> stage.hide());
        return List.of(importBtn, saveBtn, addBtn, cancelBtn);
    }

    /**
     * 文件浏览的地址缓存
     */
    private String baseDir;

    /**
     * 打开额外文件设置
     *
     * @param extraTemplateFileConfig 额外文件配置
     * @param submitBtnAction         确定按钮操作
     * @param isEdit                  true:编辑,false:添加
     */
    private void openExtraFileSetup(ExtraTemplateFileConfig extraTemplateFileConfig, boolean isEdit, Consumer<ExtraTemplateFileConfig> submitBtnAction) {
        Stage addTemplateStage = new Stage();
        BorderPane borderPane = new BorderPane();
        borderPane.getStylesheets().add("/css/common.css");
        VBox vBox = new VBox(5);
        vBox.setPrefWidth(500);
        borderPane.setCenter(vBox);
        borderPane.setPadding(new Insets(10));

        int labelWidth = 130;
        // 属性名称
        final TextField nameTextField = new TextField(extraTemplateFileConfig.getName());
        nameTextField.setPromptText("配置名称");
        if (isEdit) {
            nameTextField.setEditable(false);
        }
        PropertyHBox nameHbox = new PropertyHBox("配置名称", labelWidth, nameTextField);
        vBox.getChildren().add(nameHbox);

        // 文件类型
        ChoiceBox<ExtraFileTypeEnum> fileTypeCb = new ChoiceBox<>(FXCollections.observableArrayList(ExtraFileTypeEnum.MODEL, ExtraFileTypeEnum.CUSTOM_TEMPLATE));
        fileTypeCb.getItems().addAll();
        fileTypeCb.setValue(extraTemplateFileConfig.getExtraFileType());
        if (isEdit) {
            fileTypeCb.setDisable(true);
        }
        PropertyHBox fileTypeHbox = new PropertyHBox("文件类型", labelWidth, fileTypeCb);
        vBox.getChildren().add(fileTypeHbox);

        // 父类
        final TextField superClassTextField = new TextField(extraTemplateFileConfig.getSuperClass());
        superClassTextField.setPromptText("父类，类全限定名称");
        PropertyHBox superClassHbox = new PropertyHBox("父类", labelWidth, superClassTextField);
        vBox.getChildren().add(superClassHbox);

        // model 后缀
        TextField modelSuffixTextField = new TextField(extraTemplateFileConfig.getModelSuffix());
        modelSuffixTextField.setPromptText("model 后缀");
        PropertyHBox modelSuffixHbox = new PropertyHBox("model 后缀", labelWidth, modelSuffixTextField);
        vBox.getChildren().add(modelSuffixHbox);

        // 是否开启 validation 注解
        CheckBox checkBox = new CheckBox();
        checkBox.setSelected(extraTemplateFileConfig.isGenerateValidAnnotation());
        PropertyHBox modelValidSuffixHbox = new PropertyHBox("是否开启 Validate 注解", labelWidth, checkBox);
        vBox.getChildren().add(modelValidSuffixHbox);

        // lombok @Getter
        CheckBox lombokGetterCheckBox = new CheckBox();
        lombokGetterCheckBox.setSelected(extraTemplateFileConfig.isLombokGetter());
        PropertyHBox lombokGetterHbox = new PropertyHBox("lombok @Getter", labelWidth, lombokGetterCheckBox);
        vBox.getChildren().add(lombokGetterHbox);

        // lombok @Setter
        CheckBox lombokSetterCheckBox = new CheckBox();
        lombokSetterCheckBox.setSelected(extraTemplateFileConfig.isLombokSetter());
        PropertyHBox lombokSetterHbox = new PropertyHBox("lombok @Setter", labelWidth, lombokSetterCheckBox);
        vBox.getChildren().add(lombokSetterHbox);

        // lombok @ToString
        CheckBox lombokToStringCheckBox = new CheckBox();
        lombokToStringCheckBox.setSelected(extraTemplateFileConfig.isLombokToString());
        PropertyHBox lombokToStringHbox = new PropertyHBox("lombok @ToString", labelWidth, lombokToStringCheckBox);
        vBox.getChildren().add(lombokToStringHbox);

        // 忽略字段
        TextField ignoreColumnTextField = new TextField(extraTemplateFileConfig.getModelIgnoreColumns());
        ignoreColumnTextField.setPromptText("忽略字段，逗号分隔");
        PropertyHBox ignoreColumnHbox = new PropertyHBox("忽略字段", labelWidth, ignoreColumnTextField);
        vBox.getChildren().add(ignoreColumnHbox);

        // 自定义模板文件夹
        FileSelectTextHBox customTemplatePathTextField = new FileSelectTextHBox("浏览", extraTemplateFileConfig.getCustomTemplateDir());
        customTemplatePathTextField.setPromptText("模板文件夹");
        customTemplatePathTextField.onAction(actionEvent -> {
            baseDir = extraTemplateFileConfig.getCustomTemplateDir();
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
        FileSelectTextHBox customTemplateFileNameTextField = new FileSelectTextHBox("浏览", extraTemplateFileConfig.getCustomTemplateFileName());
        customTemplateFileNameTextField.setPromptText("模板文件名称");
        customTemplateFileNameTextField.onAction(actionEvent -> {
            baseDir = extraTemplateFileConfig.getCustomTemplateDir();
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
        this.showByExtraFileType(extraTemplateFileConfig.getExtraFileType(), modelValidSuffixHbox, lombokGetterHbox,
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
            extraTemplateFileConfig.setName(nameTextField.getText());
            extraTemplateFileConfig.setExtraFileType(fileTypeCb.getSelectionModel().getSelectedItem());
            extraTemplateFileConfig.setSuperClass(superClassTextField.getText());
            extraTemplateFileConfig.setModelSuffix(modelSuffixTextField.getText());
            extraTemplateFileConfig.setGenerateValidAnnotation(checkBox.isSelected());
            extraTemplateFileConfig.setModelIgnoreColumns(ignoreColumnTextField.getText());
            extraTemplateFileConfig.setLombokGetter(lombokGetterCheckBox.isSelected());
            extraTemplateFileConfig.setLombokSetter(lombokSetterCheckBox.isSelected());
            extraTemplateFileConfig.setLombokToString(lombokToStringCheckBox.isSelected());
            extraTemplateFileConfig.setCustomTemplateDir(customTemplatePathTextField.getText());
            extraTemplateFileConfig.setCustomTemplateFileName(customTemplateFileNameTextField.getText());

            // 检查文件配置
            this.checkConfig(extraTemplateFileConfig);

            submitBtnAction.accept(extraTemplateFileConfig);
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
        addTemplateStage.getIcons().add(new Image("/image/icon.png"));
        addTemplateStage.setTitle((isEdit ? "编辑" : "添加") + "额外文件");
        addTemplateStage.initModality(Modality.WINDOW_MODAL);
        addTemplateStage.initOwner(stage);
        addTemplateStage.show();
    }

    private void checkConfig(ExtraTemplateFileConfig extraTemplateFileConfig) {
        final ExtraFileTypeEnum templateType = extraTemplateFileConfig.getExtraFileType();
        if (StringUtils.isEmpty(extraTemplateFileConfig.getOutputPath())) {
            Toast.makeText(NodeConstants.primaryStage, extraTemplateFileConfig.getName() + "配置中，文件地址必填", 3000, 500, 500, 15, 5);
            return;
        }
        if (StringUtils.isEmpty(extraTemplateFileConfig.getPackageName())) {
            Toast.makeText(NodeConstants.primaryStage, extraTemplateFileConfig.getName() + "配置中，包名必填", 3000, 500, 500, 15, 5);
            return;
        }
        if (templateType == ExtraFileTypeEnum.CUSTOM_TEMPLATE) {
            if (StringUtils.isEmpty(extraTemplateFileConfig.getCustomTemplateDir())) {
                Toast.makeText(NodeConstants.primaryStage, extraTemplateFileConfig.getName() + "配置中，自定义模板路径必填", 3000, 500, 500, 15, 5);
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

    private void copyItem(ExtraTemplateFileItemHBox old) {
        final ExtraTemplateFileConfig extraTemplateFileConfigSource = old.getExtraTemplateFileConfig();
        final ExtraTemplateFileConfig clone = extraTemplateFileConfigSource.clone();
        clone.setName(clone.getName() + "COPY");
        final ExtraTemplateFileItemHBox extraTemplateFileItemHBox = this.packageextrafilelabel(old.isShowCheckBox(), clone);
        final int i = ((ListView<ExtraTemplateFileItemHBox>) linkageBorderPane.getRightBorderPane().getCenter()).getItems().indexOf(old);
        ((ListView<ExtraTemplateFileItemHBox>) linkageBorderPane.getRightBorderPane().getCenter()).getItems().add(i + 1, extraTemplateFileItemHBox);
    }

    private ExtraTemplateFileItemHBox packageextrafilelabel(boolean showCheckBox, ExtraTemplateFileConfig extraTemplateFileConfig) {
        ExtraTemplateFileItemHBox extraTemplateFileItemHBox = new ExtraTemplateFileItemHBox(showCheckBox, extraTemplateFileConfig);
        extraTemplateFileItemHBox.setPrefHeight(23);
        extraTemplateFileItemHBox.setAlignment(Pos.CENTER);
        extraTemplateFileItemHBox.prefWidthProperty().bind(((ListView<ExtraTemplateFileItemHBox>) linkageBorderPane.getRightBorderPane().getCenter()).widthProperty());
        // 编辑
        extraTemplateFileItemHBox.onEditAction(actionEvent -> this.openExtraFileSetup(extraTemplateFileConfig, true,
                extraFileConfig1 -> {
                    extraTemplateFileItemHBox.setLabelText(extraFileConfig1.getName());
                    saveBtn.setDisable(false);
                }));
        // 删除
        extraTemplateFileItemHBox.onDelAction(actionEvent -> {
            ((ListView<ExtraTemplateFileItemHBox>) linkageBorderPane.getRightBorderPane().getCenter()).getItems().remove(extraTemplateFileItemHBox);
            saveBtn.setDisable(false);
        });
        // 复制
        extraTemplateFileItemHBox.onCopyAction(actionEvent -> {
            this.copyItem(extraTemplateFileItemHBox);
            saveBtn.setDisable(false);
        });

        return extraTemplateFileItemHBox;
    }
}
