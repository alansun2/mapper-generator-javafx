package com.alan344.controller;

import com.alan344.bean.config.ExtraFileGroupConfig;
import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.componet.*;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.ConfigService;
import com.alan344.service.ExportService;
import com.alan344.service.ExtraFileConfigService;
import com.alan344.service.node.NodeHandler;
import com.alan344.utils.CollectionUtils;
import com.alan344.utils.Toast;
import javafx.fxml.FXML;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
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
import java.util.*;
import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/11/3 9:49
 * <p>
 * 添加额外文件
 */
@Service
public class ExtraFileController {
    @Autowired
    private ExportService exportService;
    @Autowired
    private ConfigService configService;
    @Autowired
    private ExtraTemplateFileController extraTemplateFileController;
    @Autowired
    private ExtraFileConfigService extraFileConfigService;
    private final NodeHandler nodeHandler = NodeHandler.getSingleTon(true);
    private final Map<String, ListView<ExtraFileItemHBox>> groupNameListViewMapCache = new HashMap<>();
    private LeftRightLinkageBorderPane<ExtraFileGroupConfig.ExtraFileConfig, ExtraFileGroupConfig, ExtraFileGroupItemHBox> linkageBorderPane;

    public BorderPane getBorderPane() {
        linkageBorderPane = new LeftRightLinkageBorderPane<>(
                ExtraFileGroupConfig::new,
                this::convert2ExtraFileGroupItem,
                this::getRightListView,
                NodeConstants.primaryStage,
                this.getBottomBtns(),
                0.25
        );

        final List<ExtraFileGroupConfig> extraFileGroupConfigs = configService.getExtraFileGroupConfigs();
        linkageBorderPane.addLeftItems(extraFileGroupConfigs, this::getRightListView);
        return linkageBorderPane;
    }

    private List<Button> getBottomBtns() {
        Button openExtraPropertyStageBtn = new Button("添加额外属性");
        openExtraPropertyStageBtn.setOnAction(event -> this.openExtraFileCustomProperties());
        openExtraPropertyStageBtn.setPrefWidth(100);
        Button openExtraTemplateFileStageBtn = new Button("添加额外文件");
        openExtraTemplateFileStageBtn.setOnAction(event -> {
            final ExtraFileGroupItemHBox selectedItem = linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem();
            if (null == selectedItem) {
                Toast.makeText(NodeConstants.primaryStage, "请选择一个分组再添加", 3000, 500, 500, 15, 5);
                return;
            }
            final ExtraFileGroupConfig config = selectedItem.getConfig();
            if (config.getIsSystem()) {
                Toast.makeText(NodeConstants.primaryStage, "不能使用默认分组， 请新建分组后再使用", 3000, 500, 500, 15, 5);
                return;
            }
            this.openExtraFilePage();
        });
        openExtraTemplateFileStageBtn.setPrefWidth(100);
        Button saveBtn = new Button("保存配置");
        saveBtn.setOnAction(event -> this.saveSetup());
        saveBtn.setPrefWidth(70);
        Button exportBtn = new Button("导出");
        exportBtn.getStyleClass().add("export");
        exportBtn.setOnAction(event -> this.export());
        exportBtn.setPrefWidth(70);
        Button preBtn = new Button("返回");
        preBtn.setOnAction(event -> this.pre());
        preBtn.setPrefWidth(70);
        return List.of(openExtraPropertyStageBtn, openExtraTemplateFileStageBtn, saveBtn, exportBtn, preBtn);
    }

    /**
     * 当前选中的分组中
     */
    private ExtraFileGroupConfig curExtraFileGroupConfig;

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
        extraTemplateFileController.openExtraFilePageInternal(linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem() != null, extraTemplateFileConfigs -> {
            Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigNames = curExtraFileGroupConfig.getExtraFileConfigNames();
            if (null == extraFileConfigNames) {
                extraFileConfigNames = new HashSet<>();
                curExtraFileGroupConfig.setExtraFileConfigNames(extraFileConfigNames);
            }
            final ListView<ExtraFileItemHBox> rightListView = this.getRightListView(curExtraFileGroupConfig);
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
                    rightListView.getItems().add(this.convert2ExtraFileItem(rightListView, finalExtraFileConfigNames, extraTemplateFileConfig, extraFileConfigNew));
                    if (linkageBorderPane.getRightBorderPane().getCenter() == null) {
                        linkageBorderPane.getRightBorderPane().setCenter(rightListView);
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

    private ExtraFileGroupItemHBox convert2ExtraFileGroupItem(ExtraFileGroupConfig extraFileGroupConfig) {
        return new ExtraFileGroupItemHBox(extraFileGroupConfig, mouseEvent -> {
            // 鼠标点击事件
            if (mouseEvent.getButton() == MouseButton.PRIMARY) {
                final ExtraFileGroupItemHBox extraFileGroupItemHBox = (ExtraFileGroupItemHBox) mouseEvent.getSource();
                linkageBorderPane.getRightBorderPane().setCenter(this.getRightListView(extraFileGroupItemHBox.getConfig()));
                this.curExtraFileGroupConfig = extraFileGroupConfig;
            }
        });
    }

    private ListView<ExtraFileItemHBox> getRightListView(ExtraFileGroupConfig extraFileGroupConfig) {
        final List<String> groupNameNames = extraFileGroupConfig.getExtraFileConfigNames().stream()
                .map(extraFileConfig -> extraFileConfig.getGroupName() + ":" + extraFileConfig.getName()).toList();
        return groupNameListViewMapCache.computeIfAbsent(extraFileGroupConfig.getGroupName(), s -> {
            ListView<ExtraFileItemHBox> extraFileConfigListView = new ListView<>();
            final Map<String, ExtraTemplateFileConfig> extraFileConfigMap = extraFileConfigService.getExtraFileConfigMap(groupNameNames);
            final Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigNames = extraFileGroupConfig.getExtraFileConfigNames();
            if (CollectionUtils.isNotEmpty(extraFileConfigNames)) {
                extraFileConfigNames.stream().filter(extraFileConfig -> extraFileConfigMap.containsKey(extraFileConfig.getGroupName() + ":" + extraFileConfig.getName())).forEach(extraFileConfig -> extraFileConfigListView.getItems()
                        .add(this.convert2ExtraFileItem(extraFileConfigListView, extraFileConfigNames, extraFileConfigMap.get(extraFileConfig.getGroupName() + ":" + extraFileConfig.getName()), extraFileConfig)));
            }
            return extraFileConfigListView;
        });
    }

    private ExtraFileItemHBox convert2ExtraFileItem(ListView<ExtraFileItemHBox> listView,
                                                    Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileNames,
                                                    ExtraTemplateFileConfig extraTemplateFileConfig,
                                                    ExtraFileGroupConfig.ExtraFileConfig extraFileConfig1) {
        ExtraFileItemHBox extraFileLabel = new ExtraFileItemHBox(extraTemplateFileConfig.getName(), extraTemplateFileConfig.getExtraFileType(),
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
        FileSelectTextHBox outputPathTextField = new FileSelectTextHBox("浏览", extraFileConfig.getOutputPath());
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