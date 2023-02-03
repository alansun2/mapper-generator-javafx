package com.alan344.controller;

import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.ExtraTemplateFileGroupConfig;
import com.alan344.componet.ExtraTemplateFileGroupItem;
import com.alan344.componet.ExtraTemplateFileItem;
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
import javafx.scene.control.cell.TextFieldListCell;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.util.StringConverter;
import org.kordamp.ikonli.javafx.FontIcon;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.util.ArrayList;
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
public class ExtraTemplateFileController {
    @Autowired
    private ExtraFileConfigService extraFileConfigService;
    private Stage stage;
    private ListView<ExtraTemplateFileItem> curCenterListView;

    private final Map<String, ListView<ExtraTemplateFileItem>> listViewCache = new HashMap<>();

    private Button saveBtn;

    private BorderPane borderPane;

    public void openExtraFilePageInternal(boolean showCheckBox, Consumer<List<ExtraTemplateFileConfig>> consumer) {
        if (null != stage) {
            stage.show();
            return;
        }

        stage = new Stage();
        final List<ExtraTemplateFileGroupConfig> extraTemplateFileGroupConfigs = extraFileConfigService.getExtraFileConfigList();
        // 展开第一个
        if (!extraTemplateFileGroupConfigs.isEmpty()) {
            curCenterListView = this.getCenterListView(showCheckBox, extraTemplateFileGroupConfigs.get(0));
        }

        borderPane = new BorderPane();
        borderPane.getStylesheets().add("/css/common.css");
        borderPane.setStyle("-fx-background-insets: 0");
        borderPane.setPrefHeight(550);
        borderPane.setPrefWidth(750);
        borderPane.setCenter(curCenterListView);

        borderPane.setLeft(this.initGroupListView(showCheckBox, extraTemplateFileGroupConfigs));

        borderPane.setBottom(this.getBtnHbox(stage, showCheckBox, consumer));
        stage.setScene(new Scene(borderPane));
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/icon.png"));
        stage.setTitle("额外文件配置");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(NodeConstants.primaryStage);
        stage.show();
    }

    private ListView<ExtraTemplateFileItem> getCenterListView(boolean showCheckBox, ExtraTemplateFileGroupConfig extraTemplateFileGroupConfig) {
        return listViewCache.computeIfAbsent(extraTemplateFileGroupConfig.getGroupName(), s -> {
            ListView<ExtraTemplateFileItem> listView = new ListView<>();
            extraTemplateFileGroupConfig.getExtraTemplateFileConfigList().forEach(extraFileConfig -> listView.getItems().add(this.packageExtraFileLabel(showCheckBox, extraFileConfig)));
            return listView;
        });
    }

    private ListView<ExtraTemplateFileGroupItem> initGroupListView(boolean showCheckBox, List<ExtraTemplateFileGroupConfig> extraTemplateFileGroupConfigs) {
        ListView<ExtraTemplateFileGroupItem> extraTemplateFileConfigGroupListView = new ListView<>();
        extraTemplateFileConfigGroupListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        extraTemplateFileConfigGroupListView.setCellFactory(TextFieldListCell.forListView(new StringConverter<ExtraTemplateFileGroupItem>() {
            @Override
            public String toString(ExtraTemplateFileGroupItem object) {
                return object.getName();
            }

            @Override
            public ExtraTemplateFileGroupItem fromString(String string) {
                return null;
            }
        }));
        extraTemplateFileGroupConfigs.forEach(extraTemplateFileGroupConfig -> extraTemplateFileConfigGroupListView.getItems().add(new ExtraTemplateFileGroupItem(extraTemplateFileGroupConfig)));
        extraTemplateFileConfigGroupListView.addEventHandler(MouseEvent.MOUSE_RELEASED, event -> {
            final ExtraTemplateFileGroupItem selectedItem = extraTemplateFileConfigGroupListView.getSelectionModel().getSelectedItem();
            if (event.getButton() == MouseButton.SECONDARY) {
                // 右键
                final int selectedIndex = extraTemplateFileConfigGroupListView.getSelectionModel().getSelectedIndex();
                // open context menu on current screen position
                MenuItem addMenuItem = new MenuItem("添加");
                addMenuItem.setGraphic(new FontIcon("unil-plus-circle:16:BLUE"));
                addMenuItem.setOnAction(event1 -> this.openGroupConfigStage(null, false, s -> {
                    ExtraTemplateFileGroupConfig extraTemplateFileGroupConfig = new ExtraTemplateFileGroupConfig();
                    extraTemplateFileGroupConfig.setGroupName(s);
                    extraTemplateFileGroupConfig.setExtraTemplateFileConfigList(new ArrayList<>(3));
                    ExtraTemplateFileGroupItem extraTemplateFileGroupItem = new ExtraTemplateFileGroupItem(extraTemplateFileGroupConfig);
                    extraTemplateFileConfigGroupListView.getItems().add(extraTemplateFileGroupItem);
                }));
                MenuItem updateMenuItem = new MenuItem("编辑");
                updateMenuItem.setGraphic(new FontIcon("unil-file-edit-alt:16:ORANGE"));
                updateMenuItem.setOnAction(event1 -> this.openGroupConfigStage(selectedItem.getName(), true, selectedItem::setName));
                MenuItem copyMenuItem = new MenuItem("复制");
                copyMenuItem.setGraphic(new FontIcon("unil-copy:16:GRAY"));
                copyMenuItem.setOnAction(event1 -> {
                    final ExtraTemplateFileGroupConfig extraTemplateFileGroupConfig = selectedItem.getExtraTemplateFileConfigGroup();
                    final ExtraTemplateFileGroupConfig clone = extraTemplateFileGroupConfig.clone();
                    clone.setGroupName(clone.getGroupName() + "COPY");
                    ExtraTemplateFileGroupItem extraTemplateFileGroupItem = new ExtraTemplateFileGroupItem(clone);
                    extraTemplateFileConfigGroupListView.getItems().add(selectedIndex + 1, extraTemplateFileGroupItem);
                });
                MenuItem deleteMenuItem = new MenuItem("删除");
                deleteMenuItem.setGraphic(new FontIcon("unil-times-circle:16:RED"));
                deleteMenuItem.setOnAction(event1 -> extraTemplateFileConfigGroupListView.getItems().remove(selectedItem));

                ContextMenu contextMenu = new ContextMenu(addMenuItem, updateMenuItem, copyMenuItem, deleteMenuItem);
                // 放入  contextMenu
                extraTemplateFileConfigGroupListView.setContextMenu(contextMenu);
            } else if (event.getButton() == MouseButton.PRIMARY) {
                // 左键点击
                if (null != selectedItem) {
                    final ExtraTemplateFileGroupConfig extraTemplateFileGroupConfig = selectedItem.getExtraTemplateFileConfigGroup();
                    curCenterListView = this.getCenterListView(true, extraTemplateFileGroupConfig);
                    borderPane.setCenter(curCenterListView);
                }
            }
        });
        return extraTemplateFileConfigGroupListView;
    }

    /**
     * 打开新增/修改分组页面
     *
     * @param stringConsumer 分组名
     * @param isEdit         是否是编辑
     */
    private void openGroupConfigStage(String name, boolean isEdit, Consumer<String> stringConsumer) {
        Stage stage = new Stage();

        BorderPane borderPane = new BorderPane();
        borderPane.getStyleClass().add("border-pane-padding");
        borderPane.getStylesheets().add("css/common.css");
        borderPane.setPrefWidth(400);
        borderPane.setPrefHeight(150);

        // 分组名称
        Label label = new Label("分组名称: ");
        label.setPrefWidth(80);
        TextField textField = new TextField();
        if (null != name) {
            textField.setText(name);
        }
        textField.setPromptText("分组名称");
        textField.prefWidthProperty().bind(borderPane.prefWidthProperty().subtract(80));
        HBox nameText = new HBox(10, label, textField);
        nameText.setAlignment(Pos.CENTER);
        borderPane.setCenter(nameText);

        // 按钮
        Button cancelBtn = new Button("取消");
        cancelBtn.setOnAction(event -> stage.close());
        Button applyBtn = new Button("应用");
        applyBtn.setOnAction(event -> {
            stringConsumer.accept(textField.getText());
            stage.close();
        });
        HBox btnHbox = new HBox(10, cancelBtn, applyBtn);
        btnHbox.setAlignment(Pos.CENTER_RIGHT);
        borderPane.setBottom(btnHbox);

        stage.setScene(new Scene(borderPane));
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/icon.png"));
        stage.setTitle((isEdit ? "编辑" : "新增") + "分组");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(this.stage);
        stage.show();
    }

    private HBox getBtnHbox(Stage stage, boolean showCheckBox, Consumer<List<ExtraTemplateFileConfig>> consumer) {
        int btnWidth = 50;
        HBox hBox = new HBox(15);
        hBox.setStyle("-fx-padding: 10;");
        hBox.setAlignment(Pos.CENTER_RIGHT);

        Button importBtn = new Button("导入");
        importBtn.setDisable(!showCheckBox);
        importBtn.setPrefWidth(btnWidth);
        importBtn.setOnAction(event -> {
            final ObservableList<ExtraTemplateFileItem> items = curCenterListView.getItems();
            if (items.isEmpty()) {
                Toast.makeText(stage, "请先新增", 3000, 500, 500, 15, 5);
                return;
            }

            final List<ExtraTemplateFileItem> selectedList = items.stream().filter(ExtraTemplateFileItem::isSelected).toList();
            if (selectedList.isEmpty()) {
                Toast.makeText(stage, "至少选择一条导入", 3000, 500, 500, 15, 5);
                return;
            }
            final List<ExtraTemplateFileGroupConfig> extraTemplateFileGroupConfigs = ((ListView<ExtraTemplateFileGroupItem>) borderPane.getLeft()).getItems().stream()
                    .map(ExtraTemplateFileGroupItem::getExtraTemplateFileConfigGroup).toList();
            // 保存到磁盘
            extraFileConfigService.saveExtraFileConfig(extraTemplateFileGroupConfigs);
            saveBtn.setDisable(true);
            final List<ExtraTemplateFileConfig> extraTemplateFileConfigs = selectedList.stream().map(ExtraTemplateFileItem::getExtraTemplateFileConfig).collect(Collectors.toList());
            consumer.accept(extraTemplateFileConfigs);
        });

        saveBtn = new Button("保存配置");
        saveBtn.setPrefWidth(70);
        saveBtn.setOnAction(event -> {
            final List<ExtraTemplateFileGroupConfig> items = ((ListView<ExtraTemplateFileGroupItem>) borderPane.getLeft()).getItems().stream()
                    .map(ExtraTemplateFileGroupItem::getExtraTemplateFileConfigGroup).toList();
            // 保存到磁盘
            extraFileConfigService.saveExtraFileConfig(items);
            saveBtn.setDisable(true);
        });

        Button addBtn = new Button("新增");
        addBtn.setPrefWidth(btnWidth);
        addBtn.setOnAction(event -> {
            ExtraTemplateFileConfig extraTemplateFileConfig = new ExtraTemplateFileConfig();
            this.openExtraFileSetup(extraTemplateFileConfig, false, extraFileConfig1 -> {
                curCenterListView.getItems().add(this.packageExtraFileLabel(showCheckBox, extraFileConfig1));
                saveBtn.setDisable(false);
            });
            saveBtn.setDisable(false);
        });

        Button cancelBtn = new Button("关闭");
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
     * @param extraTemplateFileConfig 额外文件配置
     * @param submitBtnAction         确定按钮操作
     * @param isEdit                  true:编辑,false:新增
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
        FileSelectText customTemplatePathTextField = new FileSelectText("浏览", extraTemplateFileConfig.getCustomTemplateDir());
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
        FileSelectText customTemplateFileNameTextField = new FileSelectText("浏览", extraTemplateFileConfig.getCustomTemplateFileName());
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
        addTemplateStage.setTitle((isEdit ? "编辑" : "新增") + "额外文件");
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

    private void copyItem(ExtraTemplateFileItem old) {
        final ExtraTemplateFileConfig extraTemplateFileConfigSource = old.getExtraTemplateFileConfig();
        final ExtraTemplateFileConfig clone = extraTemplateFileConfigSource.clone();
        clone.setName(clone.getName() + "COPY");
        final ExtraTemplateFileItem extraTemplateFileItem = this.packageExtraFileLabel(old.isShowCheckBox(), clone);
        final int i = curCenterListView.getItems().indexOf(old);
        curCenterListView.getItems().add(i + 1, extraTemplateFileItem);
    }

    private ExtraTemplateFileItem packageExtraFileLabel(boolean showCheckBox, ExtraTemplateFileConfig extraTemplateFileConfig) {
        ExtraTemplateFileItem extraTemplateFileItem = new ExtraTemplateFileItem(showCheckBox, extraTemplateFileConfig);
        extraTemplateFileItem.setPrefHeight(23);
        extraTemplateFileItem.setAlignment(Pos.CENTER);
        extraTemplateFileItem.prefWidthProperty().bind(curCenterListView.widthProperty().subtract(220));
        // 编辑
        extraTemplateFileItem.onEditAction(actionEvent -> this.openExtraFileSetup(extraTemplateFileConfig, true,
                extraFileConfig1 -> {
                    extraTemplateFileItem.setLabelText(extraFileConfig1.getName());
                    saveBtn.setDisable(false);
                }));
        // 删除
        extraTemplateFileItem.onDelAction(actionEvent -> {
            curCenterListView.getItems().remove(extraTemplateFileItem);
            saveBtn.setDisable(false);
        });
        // 复制
        extraTemplateFileItem.onCopyAction(actionEvent -> {
            this.copyItem(extraTemplateFileItem);
            saveBtn.setDisable(false);
        });

        return extraTemplateFileItem;
    }
}
