package com.alan344.controller;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.RandomUtil;
import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.ExtraTemplateFileGroupConfig;
import com.alan344.component.*;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.constants.enums.ExtraFileTypeEnum;
import com.alan344.factory.DialogFactory;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.ExtraTemplateFileConfigService;
import com.alan344.utils.NameUtils;
import com.alan344.utils.StringUtils;
import com.alan344.utils.Toast;
import com.jfoenix.controls.JFXCheckBox;
import com.jfoenix.controls.JFXComboBox;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2022/11/3 9:49
 */
@Slf4j
@Service
public class ExtraTemplateFileController {
    @Autowired
    private ExtraTemplateFileConfigService extraTemplateFileConfigService;
    @Autowired
    private ResourceLoader resourceLoader;
    private Stage stage;
    private Button saveBtn;
    private final Map<String, BorderPane> groupNameBorderPaneMapCache = new HashMap<>();
    private LeftRightLinkageBorderPane<ExtraTemplateFileGroupConfig, ExtraTemplateFileGroupItemHBox> linkageBorderPane;
    private ListView<ExtraTemplateFileItemHBox> listView;

    public void openExtraFilePageInternal(boolean showCheckBox, BiConsumer<ExtraTemplateFileGroupConfig, List<ExtraTemplateFileConfig>> consumer) {
        if (null == stage) {
            stage = new Stage();
            stage.setResizable(false);
            stage.getIcons().add(new Image("/image/icon.png"));
            stage.setTitle("额外文件配置");
            stage.initModality(Modality.WINDOW_MODAL);
            stage.initOwner(NodeConstants.primaryStage);
            stage.setScene(new Scene(this.getBorderPane(showCheckBox, consumer)));
        }
        stage.show();
    }

    private BorderPane getBorderPane(boolean showCheckBox, BiConsumer<ExtraTemplateFileGroupConfig, List<ExtraTemplateFileConfig>> consumer) {
        linkageBorderPane = new LeftRightLinkageBorderPane<>(
                ExtraTemplateFileGroupConfig::new,
                ExtraTemplateFileGroupItemHBox::new,
                groupConfig -> this.getCenterBorderPane(showCheckBox, groupConfig),
                NodeConstants.primaryStage,
                this.getBottomBtns(stage, showCheckBox, consumer),
                0.25
        );
        linkageBorderPane.setPrefHeight(550);
        linkageBorderPane.setPrefWidth(800);

        linkageBorderPane.addLeftItems(extraTemplateFileConfigService.getExtraTemplateFileGroupConfig());
        return linkageBorderPane;
    }

    private BorderPane getCenterBorderPane(boolean showCheckBox, ExtraTemplateFileGroupConfig groupConfig) {
        final BorderPane borderPane1 = groupNameBorderPaneMapCache.computeIfAbsent(groupConfig.getGroupName(), s -> {
            ListView<ExtraTemplateFileItemHBox> listView = new ListView<>();
            groupConfig.getExtraTemplateFileConfigList().forEach(extraFileConfig ->
                    listView.getItems().add(this.packageExtraFileLabel(showCheckBox, groupConfig.isSystem(), extraFileConfig, groupConfig.getExtraTemplateFileConfigList())));

            // top 创建全选，全不选, 反选按钮
            final SelectBtnBarHBox selectBtnBarHbox = new SelectBtnBarHBox(listView.getItems());
            BorderPane borderPane = new BorderPane();

            borderPane.setTop(selectBtnBarHbox);
            borderPane.setCenter(listView);
            return borderPane;
        });

        listView = (ListView<ExtraTemplateFileItemHBox>) borderPane1.getCenter();
        return borderPane1;
    }

    private List<Button> getBottomBtns(Stage stage, boolean showCheckBox, BiConsumer<ExtraTemplateFileGroupConfig, List<ExtraTemplateFileConfig>> consumer) {
        int btnWidth = 70;
        Button importBtn = new Button("导入");
        importBtn.setDisable(!showCheckBox);
        importBtn.setPrefWidth(btnWidth);
        importBtn.setOnAction(event -> {
            final ObservableList<ExtraTemplateFileItemHBox> items = listView.getItems();
            if (CollectionUtil.isEmpty(items)) {
                Toast.makeTextDefault(stage, "当前模板列表为空, 请先添加");
                return;
            }

            final List<ExtraTemplateFileItemHBox> selectedList = items.stream().filter(ExtraTemplateFileItemHBox::isSelected).toList();
            if (selectedList.isEmpty()) {
                Toast.makeTextDefault(stage, "至少选择一条导入");
                return;
            }
            final List<ExtraTemplateFileGroupConfig> groupConfigs = linkageBorderPane.getGroupLeftListView().getItems().stream()
                    .map(ExtraTemplateFileGroupItemHBox::getConfig).collect(Collectors.toCollection(ArrayList::new));
            // 保存到磁盘
            extraTemplateFileConfigService.saveExtraFileConfig(groupConfigs);

            final ExtraTemplateFileGroupItemHBox selectedItem = linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem();

            // saveBtn.setDisable(true);
            final List<ExtraTemplateFileConfig> extraTemplateFileConfigs = selectedList.stream()
                    .map(ExtraTemplateFileItemHBox::getExtraTemplateFileConfig).collect(Collectors.toList());
            consumer.accept(selectedItem.getConfig(), extraTemplateFileConfigs);
        });

        saveBtn = new Button("保存配置");
        saveBtn.setPrefWidth(70);
        saveBtn.setOnAction(event -> {
            final List<ExtraTemplateFileGroupConfig> items = linkageBorderPane.getGroupLeftListView().getItems().stream()
                    .map(ExtraTemplateFileGroupItemHBox::getConfig).toList();
            // 保存到磁盘
            extraTemplateFileConfigService.saveExtraFileConfig(items);
            // saveBtn.setDisable(true);

            // 保存成功 dialog
            DialogFactory.successDialog(NodeConstants.primaryStage, "保存成功");
        });

        Button addBtn = new Button("添加");
        addBtn.setPrefWidth(btnWidth);
        addBtn.setOnAction(event -> {
            final ExtraTemplateFileGroupItemHBox selectedItem = linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem();
            if (null == selectedItem) {
                Toast.makeTextDefault(stage, "请选择一个分组再添加");
                return;
            }
            final ExtraTemplateFileGroupConfig config = selectedItem.getConfig();
            if (config.isSystem()) {
                Toast.makeTextDefault(stage, "不能使用默认分组， 请新建分组后再使用");
                return;
            }
            ExtraTemplateFileConfig extraTemplateFileConfig = new ExtraTemplateFileConfig();
            this.openExtraFileSetup(extraTemplateFileConfig, false, config.isSystem(), extraFileConfig1 -> {
                listView.getItems().add(this.packageExtraFileLabel(showCheckBox, config.isSystem(), extraFileConfig1, config.getExtraTemplateFileConfigList()));
                config.getExtraTemplateFileConfigList().add(extraFileConfig1);
                // saveBtn.setDisable(false);
            });
            // saveBtn.setDisable(false);
        });

        Button cancelBtn = new Button("取消");
        cancelBtn.setPrefWidth(btnWidth);
        cancelBtn.setOnAction(event -> stage.hide());
        return List.of(importBtn, saveBtn, addBtn, cancelBtn);
    }

    /**
     * 文件浏览的地址缓存
     */
    private String baseDir, exportDir;

    /**
     * 打开额外文件设置
     *
     * @param extraTemplateFileConfig 额外文件配置
     * @param submitBtnAction         应用按钮操作
     * @param isEdit                  true:编辑,false:添加
     */
    private void openExtraFileSetup(ExtraTemplateFileConfig extraTemplateFileConfig, boolean isEdit,
                                    boolean isSystem, Consumer<ExtraTemplateFileConfig> submitBtnAction) {
        Stage addTemplateStage = new Stage();
        BorderPane borderPane = new BorderPane();
        borderPane.getStylesheets().add("/css/common.css");
        borderPane.setPadding(new Insets(10));

        VBox vBox = new VBox(5);
        vBox.setPrefWidth(500);
        int labelWidth = 130;
        // 属性名称
        final TextField nameTextField = new TextField(extraTemplateFileConfig.getName());
        nameTextField.setPromptText("配置名称");
        nameTextField.setDisable(isSystem);
        if (isEdit) {
            nameTextField.setEditable(false);
        }
        PropertyHBox nameHbox = new PropertyHBox("配置名称", labelWidth, nameTextField);
        vBox.getChildren().add(nameHbox);

        // 文件类型
        JFXComboBox<ExtraFileTypeEnum> fileTypeCb = new JFXComboBox<>(FXCollections.observableArrayList(ExtraFileTypeEnum.MODEL, ExtraFileTypeEnum.CUSTOM_TEMPLATE));
        fileTypeCb.getItems().addAll();
        fileTypeCb.setValue(extraTemplateFileConfig.getExtraFileType());
        fileTypeCb.setDisable(isEdit || isSystem);
        PropertyHBox fileTypeHbox = new PropertyHBox("文件类型", labelWidth, fileTypeCb);
        vBox.getChildren().add(fileTypeHbox);

        // 父类
        final TextField superClassTextField = new TextField(extraTemplateFileConfig.getSuperClass());
        superClassTextField.setPromptText("父类，类全限定名称");
        superClassTextField.setDisable(isSystem);
        PropertyHBox superClassHbox = new PropertyHBox("父类", labelWidth, superClassTextField);
        vBox.getChildren().add(superClassHbox);

        // model 后缀
        TextField modelSuffixTextField = new TextField(extraTemplateFileConfig.getModelSuffix());
        modelSuffixTextField.setPromptText("model 后缀");
        modelSuffixTextField.setDisable(isSystem);
        PropertyHBox modelSuffixHbox = new PropertyHBox("model 后缀", labelWidth, modelSuffixTextField);
        vBox.getChildren().add(modelSuffixHbox);

        // 是否开启 validation 注解
        JFXCheckBox checkBox = new JFXCheckBox();
        checkBox.setDisable(isSystem);
        checkBox.setSelected(extraTemplateFileConfig.isGenerateValidAnnotation());
        PropertyHBox modelValidSuffixHbox = new PropertyHBox("Validate 注解", labelWidth, checkBox);
        vBox.getChildren().add(modelValidSuffixHbox);

        // lombok @Getter
        JFXCheckBox lombokGetterCheckBox = new JFXCheckBox();
        lombokGetterCheckBox.setDisable(isSystem);
        lombokGetterCheckBox.setSelected(extraTemplateFileConfig.isLombokGetter());
        PropertyHBox lombokGetterHbox = new PropertyHBox("lombok @Getter", labelWidth, lombokGetterCheckBox);
        vBox.getChildren().add(lombokGetterHbox);

        // lombok @Setter
        JFXCheckBox lombokSetterCheckBox = new JFXCheckBox();
        lombokSetterCheckBox.setDisable(isSystem);
        lombokSetterCheckBox.setSelected(extraTemplateFileConfig.isLombokSetter());
        PropertyHBox lombokSetterHbox = new PropertyHBox("lombok @Setter", labelWidth, lombokSetterCheckBox);
        vBox.getChildren().add(lombokSetterHbox);

        // lombok @ToString
        JFXCheckBox lombokToStringCheckBox = new JFXCheckBox();
        lombokToStringCheckBox.setDisable(isSystem);
        lombokToStringCheckBox.setSelected(extraTemplateFileConfig.isLombokToString());
        PropertyHBox lombokToStringHbox = new PropertyHBox("lombok @ToString", labelWidth, lombokToStringCheckBox);
        vBox.getChildren().add(lombokToStringHbox);

        // 忽略字段
        TextField ignoreColumnTextField = new TextField(extraTemplateFileConfig.getModelIgnoreColumns());
        ignoreColumnTextField.setPromptText("忽略字段，逗号分隔");
        ignoreColumnTextField.setDisable(isSystem);
        PropertyHBox ignoreColumnHbox = new PropertyHBox("忽略字段", labelWidth, ignoreColumnTextField);
        vBox.getChildren().add(ignoreColumnHbox);

        // 自定义模板文件
        FileTemplateTextHBox customTemplatePathTextField = new FileTemplateTextHBox(extraTemplateFileConfig.getCustomTemplateDir());
        customTemplatePathTextField.disable(isSystem);
        customTemplatePathTextField.setPromptText("模板文件");
        customTemplatePathTextField.getTextField().setEditable(false);
        customTemplatePathTextField.importAction(actionEvent -> {
            baseDir = extraTemplateFileConfig.getCustomTemplateDir();
            if (StringUtils.isNotEmpty(extraTemplateFileConfig.getCustomTemplateDir())) {
                final File file = FileUtils.getFile(extraTemplateFileConfig.getCustomTemplateDir());
                baseDir = file.getParent();
            }
            // 文件地址
            File filePath = FileDirChooserFactory.createFileScan("freemarker 模板选择器", baseDir, "freemarker 文件", "*.ftl");
            if (filePath != null) {
                final String path = filePath.getPath().replace("\\", "/");
                customTemplatePathTextField.setText(path);
                this.baseDir = path;
                BaseConstants.baseFileDir = path;
            }
        });
        customTemplatePathTextField.exportAction(actionEvent -> {
            // 文件地址
            File directory = FileDirChooserFactory.createDirectoryScan("模板导出地址", exportDir);
            if (directory != null) {
                final String path = directory.getPath().replace("\\", "/");
                BaseConstants.baseFileDir = path;
                this.exportDir = path;
                final Resource resource = resourceLoader.getResource(customTemplatePathTextField.getText());
                try {
                    final File file = FileUtils.getFile(customTemplatePathTextField.getText());
                    IOUtils.copy(resource.getInputStream(), new FileOutputStream(path + "/" + file.getName()));

                    DialogFactory.successDialog(addTemplateStage, "导出成功");
                } catch (IOException e) {
                    log.error("导出文件失败", e);
                }
            }
        });

        PropertyHBox customTemplatePathHbox = new PropertyHBox("模板文件", labelWidth, customTemplatePathTextField);
        vBox.getChildren().add(customTemplatePathHbox);

        // 展示
        this.showByExtraFileType(extraTemplateFileConfig.getExtraFileType(), modelValidSuffixHbox, lombokGetterHbox,
                lombokSetterHbox, lombokToStringHbox, ignoreColumnHbox, customTemplatePathHbox);

        fileTypeCb.getSelectionModel().selectedIndexProperty().addListener((observable, oldValue, newValue) -> {
            final ExtraFileTypeEnum extraFileTypeEnum = fileTypeCb.getItems().get(newValue.intValue());
            this.showByExtraFileType(extraFileTypeEnum, modelValidSuffixHbox, lombokGetterHbox,
                    lombokSetterHbox, lombokToStringHbox, ignoreColumnHbox, customTemplatePathHbox);
        });

        // 按钮
        Button cancelButton = new Button("取消");
        cancelButton.setPrefWidth(50);
        cancelButton.setOnAction(actionEvent -> addTemplateStage.close());
        Button applyBtn = new Button("应用");
        applyBtn.getStyleClass().add("apply-btn");
        applyBtn.setDisable(isSystem);
        applyBtn.setPrefWidth(50);
        applyBtn.setOnAction(actionEvent -> {
            extraTemplateFileConfig.setId(UUID.randomUUID().toString());
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

            // 检查文件配置
            this.checkConfig(extraTemplateFileConfig);

            submitBtnAction.accept(extraTemplateFileConfig);
            addTemplateStage.close();
        });
        HBox btnHbox = new HBox(10, cancelButton, applyBtn);
        btnHbox.setStyle("-fx-padding: 10 0 0 0");
        btnHbox.setAlignment(Pos.CENTER_RIGHT);

        // ESC 监听
        vBox.setOnKeyPressed(event -> {
            if (KeyCode.ESCAPE.equals(event.getCode())) {
                addTemplateStage.close();
            }
        });

        borderPane.setCenter(vBox);
        borderPane.setBottom(btnHbox);

        addTemplateStage.setScene(new Scene(borderPane));
        addTemplateStage.setResizable(false);
        addTemplateStage.getIcons().add(new Image("/image/icon.png"));
        addTemplateStage.setTitle((isEdit ? "编辑" : "添加") + "额外文件");
        addTemplateStage.initModality(Modality.WINDOW_MODAL);
        addTemplateStage.initOwner(stage);
        addTemplateStage.show();
    }

    private void checkConfig(ExtraTemplateFileConfig extraTemplateFileConfig) {
        if (StringUtils.isEmpty(extraTemplateFileConfig.getName())) {
            Toast.makeTextDefault(NodeConstants.primaryStage, extraTemplateFileConfig.getName() + "配置中，配置名称必填");
        }

        if (StringUtils.isEmpty(extraTemplateFileConfig.getModelSuffix())) {
            Toast.makeTextDefault(NodeConstants.primaryStage, extraTemplateFileConfig.getName() + "配置中，model 后缀必填");
        }

        final ExtraFileTypeEnum templateType = extraTemplateFileConfig.getExtraFileType();
        if (templateType == ExtraFileTypeEnum.CUSTOM_TEMPLATE) {
            if (StringUtils.isEmpty(extraTemplateFileConfig.getCustomTemplateDir())) {
                Toast.makeTextDefault(NodeConstants.primaryStage, extraTemplateFileConfig.getName() + "配置中，自定义模板路径必填");
            }
        }
    }

    private void showByExtraFileType(ExtraFileTypeEnum extraFileTypeEnum,
                                     PropertyHBox modelValidSuffixHbox,
                                     PropertyHBox lombokGetterHbox,
                                     PropertyHBox lombokSetterHbox,
                                     PropertyHBox lombokToStringHbox,
                                     PropertyHBox ignoreColumnHbox,
                                     PropertyHBox customTemplatePathHbox) {
        if (extraFileTypeEnum == ExtraFileTypeEnum.MODEL) {
            customTemplatePathHbox.setManaged(false);
            customTemplatePathHbox.setVisible(false);
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

    private void copyItem(ExtraTemplateFileItemHBox old, Collection<ExtraTemplateFileConfig> extraTemplateFileConfigList) {
        final ExtraTemplateFileConfig extraTemplateFileConfigSource = old.getExtraTemplateFileConfig();
        final ExtraTemplateFileConfig clone = extraTemplateFileConfigSource.clone();
        clone.setId(UUID.randomUUID().toString());
        clone.setName(NameUtils.generatorName(clone.getName(), extraTemplateFileConfigList));
        final ExtraTemplateFileItemHBox extraTemplateFileItemHbox = this.packageExtraFileLabel(old.isShowCheckBox(), old.isSystem(), clone, extraTemplateFileConfigList);
        final int i = listView.getItems().indexOf(old);
        listView.getItems().add(i + 1, extraTemplateFileItemHbox);
        ((List) extraTemplateFileConfigList).add(i + 1, clone);
    }

    private String getName(String newGroupName, Collection<ExtraTemplateFileConfig> extraTemplateFileConfigList) {
        while (true) {
            String finalNewGroupName = newGroupName;
            final boolean isExist = extraTemplateFileConfigList.stream().anyMatch(gi -> gi.getName().equals(finalNewGroupName));
            if (isExist) {
                newGroupName = newGroupName + RandomUtil.randomLong(1, 999999999);
            } else {
                break;
            }
        }
        return newGroupName;
    }

    private ExtraTemplateFileItemHBox packageExtraFileLabel(boolean showCheckBox, boolean isSystem, ExtraTemplateFileConfig extraTemplateFileConfig,
                                                            Collection<ExtraTemplateFileConfig> extraTemplateFileConfigList) {
        ExtraTemplateFileItemHBox extraTemplateFileItemHbox = new ExtraTemplateFileItemHBox(showCheckBox, isSystem, extraTemplateFileConfig);
        extraTemplateFileItemHbox.setAlignment(Pos.CENTER);
        extraTemplateFileItemHbox.prefWidthProperty().bind(linkageBorderPane.getRightBorderPane().widthProperty().subtract(50));
        // 编辑
        extraTemplateFileItemHbox.onEditAction(actionEvent -> this.openExtraFileSetup(extraTemplateFileConfig, true, isSystem,
                extraFileConfig1 -> {
                    extraTemplateFileItemHbox.setLabelText(extraFileConfig1.getName());
                    // saveBtn.setDisable(false);
                }));
        // 删除
        extraTemplateFileItemHbox.onDelAction(actionEvent -> {
            listView.getItems().remove(extraTemplateFileItemHbox);
            extraTemplateFileConfigList.remove(extraTemplateFileItemHbox.getExtraTemplateFileConfig());
            // saveBtn.setDisable(false);
        });
        // 复制
        extraTemplateFileItemHbox.onCopyAction(actionEvent -> {
            this.copyItem(extraTemplateFileItemHbox, extraTemplateFileConfigList);
            // saveBtn.setDisable(false);
        });

        return extraTemplateFileItemHbox;
    }
}
