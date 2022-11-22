package com.alan344.controller;

import com.alan344.bean.config.ExtraFileConfig;
import com.alan344.bean.config.ExtraFileGroupConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.componet.ExtraFileGroup;
import com.alan344.componet.ExtraFileOfGroupLabel;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.NodeConstants;
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
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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
                    .filter(ExtraFileGroupConfig.ExtraFileConfig::isEnable).map(extraFileConfig -> extraFileConfigMap.get(extraFileConfig.getName())).toList();
        } else {
            ConfigConstants.extraFileConfigs = null;
        }
        exportService.export(currentConfig);
    }

    @FXML
    public void pre() {
        NodeConstants.borderPaneWrap.setCenter(nodeHandler.getPre());
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        currentConfig.setExportExtraFile(false);
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
            extraFileConfigs.forEach(extraFileConfig -> {
                ExtraFileGroupConfig.ExtraFileConfig extraFileConfig1 = new ExtraFileGroupConfig.ExtraFileConfig();
                extraFileConfig1.setName(extraFileConfig.getName());
                final boolean notExist = finalExtraFileConfigNames.add(extraFileConfig1);
                if (notExist) {
                    rightListView.getItems().add(this.packageExtraFileLabel(rightListView, finalExtraFileConfigNames, extraFileConfig, extraFileConfig1));
                    if (borderPane.getCenter() == null) {
                        borderPane.setCenter(rightListView);
                    }
                }
            });
        });
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
        // 删除
        extraFileLabel.onDelAction(actionEvent -> {
            listView.getItems().remove(extraFileLabel);
            extraFileNames.remove(extraFileConfig1);
        });

        return extraFileLabel;
    }
}
