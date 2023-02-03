package com.alan344.controller;

import com.alan344.bean.config.ExtraFileGroupConfig;
import com.alan344.bean.config.ExtraTemplateFileConfig;
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
import org.kordamp.ikonli.javafx.FontIcon;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.net.URL;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author AlanSun
 * @date 2022/11/3 9:49
 * <p>
 * 添加额外文件
 */
@Service
public class ExtraFileController implements Initializable {
    @FXML
    private ListView<ExtraFileGroupItem> groupListView;
    @FXML
    private BorderPane borderPane;
    @Autowired
    private ExportService exportService;
    @Autowired
    private ExtraTemplateFileController extraTemplateFileController;
    @Autowired
    private ExtraFileConfigService extraFileConfigService;
    private final NodeHandler nodeHandler = NodeHandler.getSingleTon(true);

    private final Map<String, ListView<ExtraFileItem>> groupNameListViewMapCache = new HashMap<>();

    /**
     * 当前选中的分组中
     */
    private ExtraFileGroupConfig curExtraFileGroupConfig;

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        // 清空缓存
        groupNameListViewMapCache.clear();
        groupListView.getItems().clear();
        // 设置单选
        groupListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        // 初始化分组
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        List<ExtraFileGroupConfig> extraFileGroupConfigs = currentConfig.getExtraFileGroupConfigs();
        // 分组为空
        if (CollectionUtils.isEmpty(extraFileGroupConfigs)) {
            extraFileGroupConfigs = this.getDefaults();
            currentConfig.setExtraFileGroupConfigs(extraFileGroupConfigs);
        }
        extraFileGroupConfigs.forEach(extraFileGroupConfig ->
                groupListView.getItems().add(this.generatorExtraFileGroup(extraFileGroupConfig)));
        groupListView.getSelectionModel().select(0);
        curExtraFileGroupConfig = extraFileGroupConfigs.get(0);
        borderPane.setCenter(this.getRightListView(curExtraFileGroupConfig));

        // groupListView 设置右键
        MenuItem addMenuItem = new MenuItem("添加");
        addMenuItem.setGraphic(new FontIcon("unil-plus-circle:16:BLUE"));
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
        editMenuItem.setGraphic(new FontIcon("unil-file-edit-alt:16:ORANGE"));
        editMenuItem.setOnAction(event -> {
            final ExtraFileGroupItem selectedItem = groupListView.getSelectionModel().getSelectedItem();
            if (null != selectedItem) {
                this.openGroupConfigStage(selectedItem.getName(), true, selectedItem::setName);
            }
        });

        MenuItem delMenuItem = new MenuItem("删除");
        delMenuItem.setGraphic(new FontIcon("unil-times-circle:16:RED"));
        delMenuItem.setOnAction(event -> {
            final ExtraFileGroupItem selectedItem = groupListView.getSelectionModel().getSelectedItem();
            if (null != selectedItem) {
                groupListView.getItems().remove(selectedItem);
                finalExtraFileGroupConfigs.remove(selectedItem.getExtraFileGroupConfig());
            }
        });

        // JFXPopup
        groupListView.setContextMenu(new ContextMenu(addMenuItem, editMenuItem, delMenuItem));
    }

    private List<ExtraFileGroupConfig> getDefaults() {
        List<ExtraFileGroupConfig> extraFileGroupConfigs = new ArrayList<>();
        ExtraFileGroupConfig extraFileGroupConfig1 = new ExtraFileGroupConfig();
        extraFileGroupConfig1.setEnable(false);
        extraFileGroupConfig1.setGroupName("usual test");
        final Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigSet1 = Stream.of("Controller",
                "ServiceI", "ServiceImpl").map(s -> {
            ExtraFileGroupConfig.ExtraFileConfig extraFileConfig = new ExtraFileGroupConfig.ExtraFileConfig();
            extraFileConfig.setEnable(true);
            extraFileConfig.setName(s);
            extraFileConfig.setOutputPath("/data/mybatis-friend-test/com/test/usual");
            extraFileConfig.setPackageName("com.test");
            return extraFileConfig;
        }).collect(Collectors.toSet());
        extraFileGroupConfig1.setExtraFileConfigNames(extraFileConfigSet1);
        extraFileGroupConfigs.add(extraFileGroupConfig1);

        ExtraFileGroupConfig extraFileGroupConfig = new ExtraFileGroupConfig();
        extraFileGroupConfig.setEnable(false);
        extraFileGroupConfig.setGroupName("cola架构 test");
        final Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigSet = Stream.of("AddCmdExe", "ByIdQryExe", "Controller", "DelByIdCmdExe", "DOConvertMapper",
                "GatewayI", "GatewayImpl", "PageQryExe", "ServiceI", "ServiceImpl", "UpdateCmdExe").map(s -> {
            ExtraFileGroupConfig.ExtraFileConfig extraFileConfig = new ExtraFileGroupConfig.ExtraFileConfig();
            extraFileConfig.setEnable(true);
            extraFileConfig.setName(s);
            extraFileConfig.setOutputPath("/data/mybatis-friend-test/com/test/cola");
            extraFileConfig.setPackageName("com.test");
            return extraFileConfig;
        }).collect(Collectors.toSet());
        extraFileGroupConfig.setExtraFileConfigNames(extraFileConfigSet);
        extraFileGroupConfigs.add(extraFileGroupConfig);

        return extraFileGroupConfigs;
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
            final List<String> groupNameNames = extraFileGroupConfig.getExtraFileConfigNames().stream()
                    .map(extraFileConfig -> extraFileConfig.getGroupName() + ":" + extraFileConfig.getName()).toList();
            final Map<String, ExtraTemplateFileConfig> extraFileConfigMap = extraFileConfigService.getExtraFileConfigMap(groupNameNames);
            ConfigConstants.extraTemplateFileConfigs = extraFileGroupConfig.getExtraFileConfigNames().stream()
                    .filter(ExtraFileGroupConfig.ExtraFileConfig::isEnable).map(extraFileConfig -> {
                        final ExtraTemplateFileConfig extraTemplateFileConfig1 = extraFileConfigMap.get(extraFileConfig.getGroupName() + ":" + extraFileConfig.getName());
                        extraTemplateFileConfig1.setOutputPath(extraFileConfig.getOutputPath());
                        extraTemplateFileConfig1.setPackageName(extraFileConfig.getPackageName());
                        return extraTemplateFileConfig1;
                    }).toList();
        } else {
            ConfigConstants.extraTemplateFileConfigs = null;
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
        extraTemplateFileController.openExtraFilePageInternal(groupListView.getSelectionModel().getSelectedItem() != null, extraTemplateFileConfigs -> {
            Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigNames = curExtraFileGroupConfig.getExtraFileConfigNames();
            if (null == extraFileConfigNames) {
                extraFileConfigNames = new HashSet<>();
                curExtraFileGroupConfig.setExtraFileConfigNames(extraFileConfigNames);
            }
            final ListView<ExtraFileItem> rightListView = this.getRightListView(curExtraFileGroupConfig);
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
            extraTemplateFileConfigs.forEach(extraTemplateFileConfig -> {
                ExtraFileGroupConfig.ExtraFileConfig extraFileConfigNew = new ExtraFileGroupConfig.ExtraFileConfig();
                extraFileConfigNew.setGroupName(extraTemplateFileConfig.getName());
                extraFileConfigNew.setName(extraTemplateFileConfig.getName());
                final boolean notExist = finalExtraFileConfigNames.add(extraFileConfigNew);
                if (notExist) {
                    // 填充文件输出地址和包名防止重复填写
                    final ExtraFileGroupConfig.ExtraFileConfig original = extraFileConfigMap.get(extraTemplateFileConfig.getName());
                    if (null != original) {
                        extraFileConfigNew.setOutputPath(original.getOutputPath());
                        extraFileConfigNew.setPackageName(original.getPackageName());
                    }
                    rightListView.getItems().add(this.packageExtraFileLabel(rightListView, finalExtraFileConfigNames, extraTemplateFileConfig, extraFileConfigNew));
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
        stage.getIcons().add(new Image("/image/icon.png"));
        stage.setTitle("设置自定义属性");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(NodeConstants.primaryStage);
        stage.show();
    }

    private ExtraFileGroupItem generatorExtraFileGroup(ExtraFileGroupConfig extraFileGroupConfig) {
        return new ExtraFileGroupItem(extraFileGroupConfig, mouseEvent -> {
            // 鼠标点击事件
            if (mouseEvent.getButton() == MouseButton.PRIMARY) {
                final ExtraFileGroupItem extraFileGroupItem = (ExtraFileGroupItem) mouseEvent.getSource();
                borderPane.setCenter(this.getRightListView(extraFileGroupItem.getExtraFileGroupConfig()));
                this.curExtraFileGroupConfig = extraFileGroupConfig;
            }
        });
    }

    private ListView<ExtraFileItem> getRightListView(ExtraFileGroupConfig extraFileGroupConfig) {
        final List<String> groupNameNames = extraFileGroupConfig.getExtraFileConfigNames().stream()
                .map(extraFileConfig -> extraFileConfig.getGroupName() + ":" + extraFileConfig.getName()).toList();
        return groupNameListViewMapCache.computeIfAbsent(extraFileGroupConfig.getGroupName(), s -> {
            ListView<ExtraFileItem> extraFileConfigListView = new ListView<>();
            final Map<String, ExtraTemplateFileConfig> extraFileConfigMap = extraFileConfigService.getExtraFileConfigMap(groupNameNames);
            final Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigNames = extraFileGroupConfig.getExtraFileConfigNames();
            if (CollectionUtils.isNotEmpty(extraFileConfigNames)) {
                extraFileConfigNames.stream().filter(extraFileConfig -> extraFileConfigMap.containsKey(extraFileConfig.getGroupName() + ":" + extraFileConfig.getName())).forEach(extraFileConfig -> extraFileConfigListView.getItems()
                        .add(this.packageExtraFileLabel(extraFileConfigListView, extraFileConfigNames, extraFileConfigMap.get(extraFileConfig.getGroupName() + ":" + extraFileConfig.getName()), extraFileConfig)));
            }
            return extraFileConfigListView;
        });
    }

    private ExtraFileItem packageExtraFileLabel(ListView<ExtraFileItem> listView,
                                                Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileNames,
                                                ExtraTemplateFileConfig extraTemplateFileConfig,
                                                ExtraFileGroupConfig.ExtraFileConfig extraFileConfig1) {
        ExtraFileItem extraFileLabel = new ExtraFileItem(extraTemplateFileConfig.getName(), extraTemplateFileConfig.getExtraFileType(),
                extraFileConfig1.isEnable(), extraFileConfig1::setEnable);
        extraFileLabel.setPrefHeight(23);
        extraFileLabel.setAlignment(Pos.CENTER);
        extraFileLabel.setExtraFileConfig(extraTemplateFileConfig);
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
        stage.getIcons().add(new Image("/image/icon.png"));
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
        stage.getIcons().add(new Image("/image/icon.png"));
        stage.setTitle("编辑");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(NodeConstants.primaryStage);
        stage.show();
    }
}
