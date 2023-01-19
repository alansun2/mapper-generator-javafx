package com.alan344.controller;

import com.alan344.bean.config.ExtraFileConfig;
import com.alan344.bean.config.ExtraFileGroupConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.componet.*;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.ExportService;
import com.alan344.service.ExtraFileConfigService;
import com.alan344.service.node.NodeHandler;
import com.alan344.utils.CollectionUtils;
import com.jfoenix.controls.JFXTextField;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.net.URL;
import java.util.*;
import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/11/3 9:49
 */
@Service
public class MybatisExtraFileGroupController implements Initializable {
    @FXML
    private ListView<ExtraFileGroup> groupListView;
    @FXML
    private BorderPane borderPane;
    @Autowired
    private ExportService exportService;
    @Autowired
    private MybatisExtraFileSetupController mybatisExtraFileSetupController;
    @Autowired
    private ExtraFileConfigService extraFileConfigService;
    private final NodeHandler nodeHandler = NodeHandler.getSingleTon(true);

    private final Map<String, ListView<ExtraFileOfGroupLabel>> groupNameListViewMap = new HashMap<>();

    /**
     * 当前选中的分组中
     */
    private ExtraFileGroupConfig curExtraFileGroupConfig;

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        // 清空缓存
        groupNameListViewMap.clear();

        groupListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        // 初始化分组
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        List<ExtraFileGroupConfig> extraFileGroupConfigs = currentConfig.getExtraFileGroupConfigs();
        if (CollectionUtils.isNotEmpty(extraFileGroupConfigs)) {
            extraFileGroupConfigs.forEach(extraFileGroupConfig ->
                    groupListView.getItems().add(this.generatorExtraFileGroup(extraFileGroupConfig)));
            groupListView.getSelectionModel().select(0);
            curExtraFileGroupConfig = extraFileGroupConfigs.get(0);
            borderPane.setCenter(this.getRightListView(curExtraFileGroupConfig));
        } else {
            groupListView.setPlaceholder(new Label("没有数据"));
            extraFileGroupConfigs = new ArrayList<>(2);
            currentConfig.setExtraFileGroupConfigs(extraFileGroupConfigs);
        }

        // 自定义属性
        final LinkedHashMap<String, String> customProperties = currentConfig.getCustomProperties();
        if (customProperties != null && !customProperties.isEmpty()) {

        } else {

        }

        // groupListView 设置右键
        MenuItem addMenuItem = new MenuItem("新增");
        List<ExtraFileGroupConfig> finalExtraFileGroupConfigs = extraFileGroupConfigs;
        addMenuItem.setOnAction(event -> {
            this.openGroupConfigStage(null, false, s -> {
                ExtraFileGroupConfig extraFileGroupConfig = new ExtraFileGroupConfig();
                extraFileGroupConfig.setGroupName(s);
                // 如果为空，第一个添加的选为默认
                final boolean empty = groupListView.getItems().isEmpty();
                // 添加到 listView
                groupListView.getItems().add(this.generatorExtraFileGroup(extraFileGroupConfig));
                if (empty) {
                    groupListView.getSelectionModel().select(0);
                    curExtraFileGroupConfig = extraFileGroupConfig;
                }
                // 添加到配置
                finalExtraFileGroupConfigs.add(extraFileGroupConfig);
            });
        });

        MenuItem editMenuItem = new MenuItem("修改");
        editMenuItem.setOnAction(event -> {
            final ExtraFileGroup selectedItem = groupListView.getSelectionModel().getSelectedItem();
            if (null != selectedItem) {
                this.openGroupConfigStage(selectedItem.getName(), true, selectedItem::setName);
            }
        });

        MenuItem delMenuItem = new MenuItem("删除");
        delMenuItem.setOnAction(event -> {
            final ExtraFileGroup selectedItem = groupListView.getSelectionModel().getSelectedItem();
            if (null != selectedItem) {
                groupListView.getItems().remove(selectedItem);
                finalExtraFileGroupConfigs.remove(selectedItem.getExtraFileGroupConfig());
            }
        });

        // JFXPopup
        groupListView.setContextMenu(new ContextMenu(addMenuItem, editMenuItem, delMenuItem));
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
        JFXTextField textField = new JFXTextField();
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
        stage.getIcons().add(new Image("/image/advanced-set-up.png"));
        stage.setTitle((isEdit ? "编辑" : "新增") + "分组");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(NodeConstants.primaryStage);
        stage.show();
    }

    @FXML
    public void export() {
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        final List<ExtraFileGroupConfig> extraFileGroupConfigs = currentConfig.getExtraFileGroupConfigs();
        final Optional<ExtraFileGroupConfig> first = extraFileGroupConfigs.stream().filter(ExtraFileGroupConfig::isEnable).findFirst();
        if (first.isPresent()) {
            final ExtraFileGroupConfig extraFileGroupConfig = first.get();
            final Map<String, ExtraFileConfig> extraFileConfigMap = extraFileConfigService.getExtraFileConfigMap();
            ConfigConstants.extraFileConfigs = extraFileGroupConfig.getExtraFileConfigNames().stream()
                    .filter(ExtraFileGroupConfig.ExtraFileConfig::isEnable).map(extraFileConfig -> {
                        final ExtraFileConfig extraFileConfig1 = extraFileConfigMap.get(extraFileConfig.getName());
                        extraFileConfig1.setOutputPath(extraFileConfig.getOutputPath());
                        extraFileConfig1.setPackageName(extraFileConfig.getPackageName());
                        return extraFileConfig1;
                    }).toList();
        } else {
            ConfigConstants.extraFileConfigs = null;
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
    public void openExtraFilePage() {
        mybatisExtraFileSetupController.openExtraFilePageInternal(groupListView.getSelectionModel().getSelectedItem() != null, extraFileConfigs -> {
            Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigNames = curExtraFileGroupConfig.getExtraFileConfigNames();
            if (null == extraFileConfigNames) {
                extraFileConfigNames = new HashSet<>();
                curExtraFileGroupConfig.setExtraFileConfigNames(extraFileConfigNames);
            }
            final ListView<ExtraFileOfGroupLabel> rightListView = this.getRightListView(curExtraFileGroupConfig);
            Set<ExtraFileGroupConfig.ExtraFileConfig> finalExtraFileConfigNames = extraFileConfigNames;

            final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
            final List<ExtraFileGroupConfig> extraFileGroupConfigs = currentConfig.getExtraFileGroupConfigs();
            Map<String, ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigMap = new HashMap<>();
            if (extraFileGroupConfigs != null) {
                extraFileGroupConfigs.forEach(extraFileGroupConfig -> {
                    final Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigNames1 = extraFileGroupConfig.getExtraFileConfigNames();
                    extraFileConfigNames1.forEach(extraFileConfig -> extraFileConfigMap.put(extraFileConfig.getName(), extraFileConfig));
                });
            }
            extraFileConfigs.forEach(extraFileConfig -> {
                ExtraFileGroupConfig.ExtraFileConfig extraFileConfigNew = new ExtraFileGroupConfig.ExtraFileConfig();
                extraFileConfigNew.setName(extraFileConfig.getName());
                final boolean notExist = finalExtraFileConfigNames.add(extraFileConfigNew);
                if (notExist) {
                    // 填充文件输出地址和包名防止重复填写
                    final ExtraFileGroupConfig.ExtraFileConfig original = extraFileConfigMap.get(extraFileConfig.getName());
                    if (null != original) {
                        extraFileConfigNew.setOutputPath(original.getOutputPath());
                        extraFileConfigNew.setPackageName(original.getPackageName());
                    }
                    rightListView.getItems().add(this.packageExtraFileLabel(rightListView, finalExtraFileConfigNames, extraFileConfig, extraFileConfigNew));
                    if (borderPane.getCenter() == null) {
                        borderPane.setCenter(rightListView);
                    }
                }
            });
        });
    }

    /**
     * 打开添加自定义属性页面
     */
    @FXML
    public void openExtraFileCustomProperties() {
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        LinkedHashMap<String, String> customProperties = currentConfig.getCustomProperties();
        if (null == customProperties) {
            customProperties = new LinkedHashMap<>();
            currentConfig.setCustomProperties(customProperties);
        }
        this.addCustomProperty(customProperties);
    }

    private void addCustomProperty(LinkedHashMap<String, String> customProperties) {
        Stage stage = new Stage();

        BorderPane borderPane = new BorderPane();
        borderPane.getStyleClass().add("border-pane-padding");
        borderPane.getStylesheets().add("css/common.css");
        borderPane.setPrefWidth(400);
        borderPane.setPrefHeight(400);

        ListView<CustomPropertyHBox> lv = new ListView<>();

        customProperties.forEach((key, value) -> {
            CustomPropertyHBox customPropertyHBox = new CustomPropertyHBox(key, value);
            customPropertyHBox.delOnAction(event -> {
                lv.getItems().remove(customPropertyHBox);
            });
            lv.getItems().add(customPropertyHBox);
        });

        borderPane.setCenter(lv);

        // 按钮
        Button addBtn = new Button("添加");
        addBtn.setOnAction(event -> this.addCustomProperty(customPropertyHBox -> {
            lv.getItems().add(customPropertyHBox);
            customPropertyHBox.delOnAction(event1 -> lv.getItems().remove(customPropertyHBox));
        }));
        Button cancelBtn = new Button("取消");
        cancelBtn.setOnAction(event -> stage.close());
        Button applyBtn = new Button("应用");
        applyBtn.setOnAction(event -> {
            customProperties.clear();
            lv.getItems().forEach(customPropertyHBox -> customProperties.put(customPropertyHBox.getKey(), customPropertyHBox.getValue()));
            stage.close();
        });
        HBox btnHbox = new HBox(10, addBtn, cancelBtn, applyBtn);
        btnHbox.setAlignment(Pos.CENTER_RIGHT);

        borderPane.setBottom(btnHbox);

        stage.setScene(new Scene(borderPane));
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/advanced-set-up.png"));
        stage.setTitle("设置自定义属性");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(NodeConstants.primaryStage);
        stage.show();
    }

    private ExtraFileGroup generatorExtraFileGroup(ExtraFileGroupConfig extraFileGroupConfig) {
        return new ExtraFileGroup(extraFileGroupConfig, mouseEvent -> {
            // 鼠标点击事件
            if (mouseEvent.getButton() == MouseButton.PRIMARY) {
                final ExtraFileGroup extraFileGroup = (ExtraFileGroup) mouseEvent.getSource();
                borderPane.setCenter(this.getRightListView(extraFileGroup.getExtraFileGroupConfig()));
                this.curExtraFileGroupConfig = extraFileGroupConfig;
            }
        });
    }

    private ListView<ExtraFileOfGroupLabel> getRightListView(ExtraFileGroupConfig extraFileGroupConfig) {
        return groupNameListViewMap.computeIfAbsent(extraFileGroupConfig.getGroupName(), s -> {
            ListView<ExtraFileOfGroupLabel> extraFileConfigListView = new ListView<>();
            final Map<String, ExtraFileConfig> extraFileConfigMap = extraFileConfigService.getExtraFileConfigMap();
            final Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigNames = extraFileGroupConfig.getExtraFileConfigNames();
            if (CollectionUtils.isNotEmpty(extraFileConfigNames)) {
                extraFileConfigNames.forEach(extraFileConfig -> extraFileConfigListView.getItems()
                        .add(this.packageExtraFileLabel(extraFileConfigListView, extraFileConfigNames, extraFileConfigMap.get(extraFileConfig.getName()), extraFileConfig)));
            }
            return extraFileConfigListView;
        });
    }

    private ExtraFileOfGroupLabel packageExtraFileLabel(ListView<ExtraFileOfGroupLabel> listView,
                                                        Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileNames,
                                                        ExtraFileConfig extraFileConfig,
                                                        ExtraFileGroupConfig.ExtraFileConfig extraFileConfig1) {
        ExtraFileOfGroupLabel extraFileLabel = new ExtraFileOfGroupLabel(extraFileConfig.getName(), extraFileConfig.getExtraFileType(),
                extraFileConfig1.isEnable(), extraFileConfig1::setEnable);
        extraFileLabel.setPrefHeight(23);
        extraFileLabel.setAlignment(Pos.CENTER);
        extraFileLabel.setExtraFileConfig(extraFileConfig);
        extraFileLabel.prefWidthProperty().bind(listView.widthProperty().subtract(220));
        // 编辑
        extraFileLabel.onEditAction(actionEvent -> {
            this.openEdit(extraFileConfig1);
        });
        // 删除
        extraFileLabel.onDelAction(actionEvent -> {
            listView.getItems().remove(extraFileLabel);
            extraFileNames.remove(extraFileConfig1);
        });

        return extraFileLabel;
    }

    private void openEdit(ExtraFileGroupConfig.ExtraFileConfig extraFileConfig) {
        Stage stage = new Stage();

        BorderPane borderPane = new BorderPane();
        borderPane.getStyleClass().add("border-pane-padding");
        borderPane.getStylesheets().add("css/common.css");
        borderPane.setPrefWidth(400);
        borderPane.setPrefHeight(150);

        int labelWidth = 80;

        VBox vBox = new VBox();
        vBox.setSpacing(10);
        // 文件地址
        FileSelectText outputPathTextField = new FileSelectText("浏览", extraFileConfig.getOutputPath());
        outputPathTextField.setPromptText("文件输出地址");
        outputPathTextField.setTextTooltip("不包含包名的路径");
        outputPathTextField.onAction(actionEvent -> {
            // 文件导出地址
            BaseConstants.baseFileDir = extraFileConfig.getOutputPath();
            File directory = FileDirChooserFactory.createDirectoryScan(null, com.alan344.utils.StringUtils.getDefaultIfNull(BaseConstants.baseFileDir, null));
            if (directory != null) {
                outputPathTextField.setText(directory.getPath());
                BaseConstants.baseFileDir = directory.getPath();
            }
        });
        PropertyHBox outputPathHbox = new PropertyHBox("文件输出地址", labelWidth, outputPathTextField);
        vBox.getChildren().add(outputPathHbox);

        // 包名
        TextField packageNameTextField = new TextField(extraFileConfig.getPackageName());
        packageNameTextField.setPromptText("包名");
        PropertyHBox packageNameHbox = new PropertyHBox("包名", labelWidth, packageNameTextField);
        vBox.getChildren().add(packageNameHbox);

        borderPane.setCenter(vBox);

        // 按钮
        Button cancelBtn = new Button("取消");
        cancelBtn.setOnAction(event -> stage.close());
        Button applyBtn = new Button("应用");
        applyBtn.setOnAction(event -> {
            extraFileConfig.setOutputPath(outputPathTextField.getText());
            extraFileConfig.setPackageName(packageNameTextField.getText());
            stage.close();
        });
        HBox btnHbox = new HBox(10, cancelBtn, applyBtn);
        btnHbox.setAlignment(Pos.CENTER_RIGHT);

        borderPane.setBottom(btnHbox);

        stage.setScene(new Scene(borderPane));
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/advanced-set-up.png"));
        stage.setTitle("编辑");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(NodeConstants.primaryStage);
        stage.show();
    }

    private void addCustomProperty(Consumer<CustomPropertyHBox> consumer) {
        Stage stage = new Stage();

        BorderPane borderPane = new BorderPane();
        borderPane.getStyleClass().add("border-pane-padding");
        borderPane.getStylesheets().add("css/common.css");
        borderPane.setPrefWidth(400);
        borderPane.setPrefHeight(150);

        int labelWidth = 80;

        VBox vBox = new VBox();
        vBox.setSpacing(10);

        // key
        TextField keyTextField = new TextField();
        keyTextField.setPromptText("key");
        PropertyHBox keyHbox = new PropertyHBox("key", labelWidth, keyTextField);
        vBox.getChildren().add(keyHbox);

        // value
        TextField valueTextField = new TextField();
        valueTextField.setPromptText("value");
        PropertyHBox valueHbox = new PropertyHBox("value", labelWidth, valueTextField);
        vBox.getChildren().add(valueHbox);

        borderPane.setCenter(vBox);

        // 按钮
        Button cancelBtn = new Button("取消");
        cancelBtn.setOnAction(event -> stage.close());
        Button applyBtn = new Button("应用");
        applyBtn.setOnAction(event -> {
            CustomPropertyHBox customPropertyHBox = new CustomPropertyHBox(keyTextField.getText(), valueTextField.getText());
            consumer.accept(customPropertyHBox);
            stage.close();
        });
        HBox btnHbox = new HBox(10, cancelBtn, applyBtn);
        btnHbox.setAlignment(Pos.CENTER_RIGHT);

        borderPane.setBottom(btnHbox);

        stage.setScene(new Scene(borderPane));
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/advanced-set-up.png"));
        stage.setTitle("编辑");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(NodeConstants.primaryStage);
        stage.show();
    }
}
