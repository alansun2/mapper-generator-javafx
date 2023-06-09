package com.alan344.controller;

import cn.hutool.core.util.StrUtil;
import com.alan344.bean.config.ExtraFileGroupConfig;
import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.component.*;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.DialogFactory;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.ConfigService;
import com.alan344.service.ExportService;
import com.alan344.service.ExtraTemplateFileConfigService;
import com.alan344.service.node.NodeHandler;
import com.alan344.utils.Assert;
import com.alan344.utils.CollectionUtils;
import com.alan344.utils.Toast;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

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
    private ExtraTemplateFileConfigService extraTemplateFileConfigService;
    private final NodeHandler nodeHandler = NodeHandler.getSingleTon(true);
    private final Map<String, BorderPane> groupNameBorderPaneMapCache = new HashMap<>();
    private LeftRightLinkageBorderPane<ExtraFileGroupConfig, ExtraFileGroupItemHBox> linkageBorderPane;
    private static final Map<String, LeftRightLinkageBorderPane<ExtraFileGroupConfig, ExtraFileGroupItemHBox>> CACHE = new HashMap<>(8);

    public BorderPane getBorderPane(MybatisExportConfig mybatisExportConfig) {
        linkageBorderPane = CACHE.computeIfAbsent(mybatisExportConfig.getConfigName(), s -> {
            final LeftRightLinkageBorderPane<ExtraFileGroupConfig, ExtraFileGroupItemHBox> linkageBorderPane1 = new LeftRightLinkageBorderPane<>(
                    ExtraFileGroupConfig::new,
                    this::convert2ExtraFileGroupItem,
                    this::getRightBorderPane,
                    NodeConstants.primaryStage,
                    this.getBottomBtns(mybatisExportConfig),
                    0.25
            );

            linkageBorderPane1.addLeftItems(configService.getExtraFileGroupConfigs(mybatisExportConfig));
            return linkageBorderPane1;
        });
        return linkageBorderPane;
    }

    private List<Button> getBottomBtns(MybatisExportConfig mybatisExportConfig) {
        Button openExtraTemplateFileStageBtn = new Button("导入额外文件");
        openExtraTemplateFileStageBtn.setOnAction(event -> {
            final ExtraFileGroupItemHBox selectedItem = linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem();
            Assert.isTrue(Objects.nonNull(selectedItem), "请选择一个分组再添加", NodeConstants.primaryStage);
            final ExtraFileGroupConfig config = selectedItem.getConfig();
            Assert.isTrue(!config.isSystem(), "不能使用默认分组， 请新建分组后再使用", NodeConstants.primaryStage);
            this.openExtraFilePage(mybatisExportConfig);
        });
        Button openExtraPropertyStageBtn = new Button("添加额外属性");
        openExtraPropertyStageBtn.setOnAction(event -> this.openExtraFileCustomProperties(mybatisExportConfig));
        openExtraPropertyStageBtn.setPrefWidth(100);
        openExtraTemplateFileStageBtn.setPrefWidth(100);
        Button saveBtn = new Button("保存配置");
        saveBtn.setOnAction(event -> this.saveSetup());
        saveBtn.setPrefWidth(70);
        Button exportBtn = new Button("导出");
        exportBtn.getStyleClass().add("export");
        exportBtn.setOnAction(event -> {
            final ExtraFileGroupItemHBox selectedItem = linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem();
            final Collection<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigs = selectedItem.getConfig().getExtraFileConfigs();
            final String s = extraFileConfigs.stream().filter(extraFileConfig -> StrUtil.hasEmpty(extraFileConfig.getOutputPath(), extraFileConfig.getPackageName()))
                    .map(ExtraFileGroupConfig.ExtraFileConfig::getName).collect(Collectors.joining(","));
            if (StrUtil.isNotEmpty(s)) {
                Toast.makeTextDefault(NodeConstants.primaryStage, s + ", 未设置输出路径或包名");
                return;
            }
            this.export(mybatisExportConfig);
        });
        exportBtn.setPrefWidth(70);
        Button preBtn = new Button("返回");
        preBtn.setOnAction(event -> this.pre());
        preBtn.setPrefWidth(70);
        return List.of(openExtraTemplateFileStageBtn, openExtraPropertyStageBtn, saveBtn, exportBtn, preBtn);
    }

    private void export(MybatisExportConfig mybatisExportConfig) {
        final Optional<ExtraFileGroupConfig> enabledConfig = linkageBorderPane.getGroupLeftListView().getItems().stream()
                .map(ExtraFileGroupItemHBox::getConfig)
                .filter(ExtraFileGroupConfig::isEnable).findFirst();
        if (enabledConfig.isPresent()) {
            // selected group
            final ExtraFileGroupConfig extraFileGroupConfig = enabledConfig.get();
            // 获取模板 id
            final List<String> templateIds = extraFileGroupConfig.getExtraFileConfigs().stream()
                    .map(ExtraFileGroupConfig.ExtraFileConfig::getTemplateId).toList();
            final Map<String, ExtraTemplateFileConfig> extraFileConfigMap = extraTemplateFileConfigService.getExtraFileConfigMap(templateIds);

            ConfigConstants.extraTemplateFileConfigs = extraFileGroupConfig.getExtraFileConfigs().stream()
                    .filter(ExtraFileGroupConfig.ExtraFileConfig::isEnable)
                    .map(extraFileConfig -> {
                        final ExtraTemplateFileConfig extraTemplateFileConfig1 = extraFileConfigMap.get(extraFileConfig.getTemplateId());
                        extraTemplateFileConfig1.setOutputPath(extraFileConfig.getOutputPath());
                        extraTemplateFileConfig1.setPackageName(extraFileConfig.getPackageName());
                        return extraTemplateFileConfig1;
                    }).toList();
        } else {
            ConfigConstants.extraTemplateFileConfigs = null;
        }

        if (null != mybatisExportConfig.getCustomProperties()) {
            ConfigConstants.globalParam.putAll(mybatisExportConfig.getCustomProperties());
        }
        exportService.export(BaseConstants.currentConfig);
    }

    private void pre() {
        NodeConstants.borderPaneWrap.setCenter(nodeHandler.getPre());
    }

    /**
     * 保存额外文件
     */
    private void saveSetup() {
        exportService.saveSetup(BaseConstants.currentConfig);

        // 保存成功 dialog
        DialogFactory.successDialog(NodeConstants.primaryStage, "保存成功");
    }

    /**
     * 添加额外文件
     */
    private void openExtraFilePage(MybatisExportConfig mybatisExportConfig) {
        extraTemplateFileController.openExtraFilePageInternal(linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem() != null
                , extraTemplateFileConfigs -> {
                    // 获取选中的分组
                    final ExtraFileGroupConfig curExtraFileGroupConfig = linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem().getConfig();
                    // 获取已经存在的配置
                    Collection<ExtraFileGroupConfig.ExtraFileConfig> existExtraFileConfigs = curExtraFileGroupConfig.getExtraFileConfigs();
                    if (null == existExtraFileConfigs) {
                        // 不存在则创建
                        existExtraFileConfigs = new ArrayList<>();
                        curExtraFileGroupConfig.setExtraFileConfigs(existExtraFileConfigs);
                    }

                    // 获取已经存在的模板 id, 防止重复添加
                    final Set<String> existTemplateIdSet = existExtraFileConfigs.stream()
                            .map(ExtraFileGroupConfig.ExtraFileConfig::getTemplateId).collect(Collectors.toSet());

                    // 根据分组获取所有的配置，用于用户少写配置
                    final Map<String, ExtraFileGroupConfig.ExtraFileConfig> allExistTemplateIdMap = linkageBorderPane.getGroupLeftListView()
                            .getItems().stream()
                            .filter(extraFileGroupItemHbox -> !extraFileGroupItemHbox.getConfig().isSystem())
                            .flatMap(extraFileGroupItemHbox -> extraFileGroupItemHbox.getConfig().getList().stream())
                            .collect(Collectors.toMap(ExtraFileGroupConfig.ExtraFileConfig::getTemplateId, Function.identity(), (extraFileConfig, extraFileConfig2) -> extraFileConfig));

                    // 获取当前选中的 listView
                    ListView<ExtraFileItemHBox> rightListView = ((ListView<ExtraFileItemHBox>) this.getRightBorderPane(curExtraFileGroupConfig).getCenter());

                    // 添加新的模板
                    final List<ExtraTemplateFileConfig> add = extraTemplateFileConfigs.stream()
                            .filter(extraTemplateFileConfig -> !existTemplateIdSet.contains(extraTemplateFileConfig.getId())).toList();
                    final int curSerialNumber = existTemplateIdSet.size();
                    for (int i = 0; i < add.size(); i++) {
                        ExtraTemplateFileConfig extraTemplateFileConfig = add.get(i);
                        ExtraFileGroupConfig.ExtraFileConfig extraFileConfigNew = new ExtraFileGroupConfig.ExtraFileConfig();
                        extraFileConfigNew.setExtraFileType(extraTemplateFileConfig.getExtraFileType().name());
                        extraFileConfigNew.setSerialNumber(String.valueOf(curSerialNumber + i + 1));
                        extraFileConfigNew.setTemplateId(extraTemplateFileConfig.getId());
                        extraFileConfigNew.setName(extraTemplateFileConfig.getName());
                        extraFileConfigNew.setEnable(true);
                        existExtraFileConfigs.add(extraFileConfigNew);
                        // 填充文件输出地址和包名防止重复填写
                        final ExtraFileGroupConfig.ExtraFileConfig original = allExistTemplateIdMap.get(extraTemplateFileConfig.getId());
                        if (null != original) {
                            extraFileConfigNew.setOutputPath(original.getOutputPath());
                            extraFileConfigNew.setPackageName(original.getPackageName());
                        } else {
                            extraFileConfigNew.setOutputPath(StrUtil.addSuffixIfNot(mybatisExportConfig.getProjectDir(), StrUtil.SLASH) + mybatisExportConfig.getBeanLocation());
                        }
                        rightListView.getItems().add(this.convert2ExtraFileItem(rightListView, existExtraFileConfigs,
                                extraFileConfigNew, curExtraFileGroupConfig));
                        if (linkageBorderPane.getRightBorderPane().getCenter() == null) {
                            linkageBorderPane.getRightBorderPane().setCenter(rightListView);
                        }
                    }
                });
    }

    /**
     * 打开添加自定义属性页面
     */
    private void openExtraFileCustomProperties(MybatisExportConfig mybatisExportConfig) {
        LinkedHashMap<String, String> customProperties = mybatisExportConfig.getCustomProperties();
        if (null == customProperties) {
            customProperties = new LinkedHashMap<>();
            mybatisExportConfig.setCustomProperties(customProperties);
        }
        PropertyPane.open(customProperties);
    }

    private ExtraFileGroupItemHBox convert2ExtraFileGroupItem(ExtraFileGroupConfig extraFileGroupConfig) {
        return new ExtraFileGroupItemHBox(extraFileGroupConfig);
    }

    private BorderPane getRightBorderPane(ExtraFileGroupConfig extraFileGroupConfig) {
        return groupNameBorderPaneMapCache.computeIfAbsent(extraFileGroupConfig.getGroupName(), s -> {
            BorderPane borderPane = new BorderPane();

            ListView<ExtraFileItemHBox> extraFileItemHboxListView = new ListView<>();

            // 根据 id 获取模板列表
            final List<String> templateIds = extraFileGroupConfig.getExtraFileConfigs().stream().map(ExtraFileGroupConfig.ExtraFileConfig::getTemplateId).toList();
            final Map<String, ExtraTemplateFileConfig> extraFileConfigMap = extraTemplateFileConfigService.getExtraFileConfigMap(templateIds);
            final Collection<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigs = extraFileGroupConfig.getExtraFileConfigs();
            if (CollectionUtils.isNotEmpty(extraFileConfigs)) {
                final List<ExtraFileGroupConfig.ExtraFileConfig> list = extraFileConfigs.stream().filter(extraFileConfig -> extraFileConfigMap.containsKey(extraFileConfig.getTemplateId())).toList();
                for (int i = 0; i < list.size(); i++) {
                    ExtraFileGroupConfig.ExtraFileConfig extraFileConfig = list.get(i);
                    extraFileConfig.setSerialNumber(String.valueOf(i + 1));
                    extraFileItemHboxListView.getItems().add(this.convert2ExtraFileItem(
                            extraFileItemHboxListView,
                            extraFileConfigs,
                            extraFileConfig,
                            extraFileGroupConfig));
                }

            }

            // top 创建全选，全不选, 反选按钮
            final SelectBtnBarHBox selectBtnBarHbox = new SelectBtnBarHBox(extraFileItemHboxListView.getItems());
            borderPane.setTop(selectBtnBarHbox);
            borderPane.setCenter(extraFileItemHboxListView);
            return borderPane;
        });
    }

    private ExtraFileItemHBox convert2ExtraFileItem(ListView<ExtraFileItemHBox> listView,
                                                    Collection<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigCollection,
                                                    ExtraFileGroupConfig.ExtraFileConfig extraFileConfigNew,
                                                    ExtraFileGroupConfig extraFileGroupConfig) {
        ExtraFileItemHBox extraFileLabel = new ExtraFileItemHBox(extraFileConfigNew);
        extraFileLabel.disable(extraFileGroupConfig.isSystem());
        extraFileLabel.setAlignment(Pos.CENTER);
        extraFileLabel.prefWidthProperty().bind(listView.widthProperty().subtract(25));
        // 编辑
        extraFileLabel.onEditAction(actionEvent -> {
            this.openEdit(extraFileConfigNew, extraFileGroupConfig.isSystem());
        });
        // 删除
        extraFileLabel.onDelAction(actionEvent -> {
            listView.getItems().remove(extraFileLabel);
            extraFileConfigCollection.remove(extraFileConfigNew);
            extraFileConfigCollection.forEach(extraFileConfig1 -> {
                if (Integer.parseInt(extraFileConfig1.getSerialNumber()) > Integer.parseInt(extraFileConfigNew.getSerialNumber())) {
                    extraFileConfig1.setSerialNumber(String.valueOf(Integer.parseInt(extraFileConfig1.getSerialNumber()) - 1));
                }
            });
        });

        return extraFileLabel;
    }

    private Stage stage;
    private FileSelectTextHBox outputPathTextField;
    private TextField packageNameTextField;
    private Button applyBtn;
    private final StyleClassValidationDecoration styleClassValidationDecoration = new StyleClassValidationDecoration();
    ValidationSupport validationSupport = new ValidationSupport();

    private void openEdit(ExtraFileGroupConfig.ExtraFileConfig extraFileConfig, boolean isSystem) {
        if (stage == null) {
            validationSupport.setValidationDecorator(styleClassValidationDecoration);
            stage = new Stage();
            stage.setResizable(false);
            stage.getIcons().add(new Image("/image/icon.png"));
            stage.setTitle("编辑");
            stage.initModality(Modality.WINDOW_MODAL);
            stage.initOwner(NodeConstants.primaryStage);

            BorderPane borderPane = new BorderPane();
            borderPane.getStyleClass().add("border-pane-padding");
            borderPane.getStylesheets().add("css/common.css");
            borderPane.setPrefWidth(400);
            borderPane.setPrefHeight(150);
            stage.setScene(new Scene(borderPane));

            int labelWidth = 80;

            VBox vBox = new VBox();
            vBox.setSpacing(10);
            // 文件地址
            outputPathTextField = new FileSelectTextHBox("浏览", extraFileConfig.getOutputPath());
            outputPathTextField.setPromptText("文件输出地址");
            outputPathTextField.setTextTooltip("不包含包名的路径");
            outputPathTextField.onAction(actionEvent -> {
                // 文件导出地址
                BaseConstants.baseFileDir = extraFileConfig.getOutputPath();
                File directory = FileDirChooserFactory.createDirectoryScan(null, BaseConstants.baseFileDir);
                if (directory != null) {
                    final String path = directory.getPath().replace("\\", "/");
                    outputPathTextField.setText(path);
                    BaseConstants.baseFileDir = path;
                }
            });
            PropertyHBox outputPathHbox = new PropertyHBox("文件输出地址", labelWidth, outputPathTextField);
            validationSupport.registerValidator(outputPathTextField.getTextField(), Validator.createEmptyValidator("文件输出地址不能为空"));
            vBox.getChildren().add(outputPathHbox);

            // 包名
            packageNameTextField = new TextField(extraFileConfig.getPackageName());
            packageNameTextField.setPromptText("包名");
            PropertyHBox packageNameHbox = new PropertyHBox("包名", labelWidth, packageNameTextField);
            validationSupport.registerValidator(packageNameTextField, Validator.createEmptyValidator("包名不能为空"));
            vBox.getChildren().add(packageNameHbox);

            borderPane.setCenter(vBox);

            // 按钮
            Button cancelBtn = new Button("取消");
            cancelBtn.setOnAction(event -> stage.close());
            applyBtn = new Button("应用");
            applyBtn.setDisable(isSystem);
            applyBtn.setOnAction(event -> {
                if (validationSupport.isInvalid()) {
                    return;
                }
                extraFileConfig.setOutputPath(outputPathTextField.getText());
                extraFileConfig.setPackageName(packageNameTextField.getText());
                stage.close();
            });
            HBox btnHbox = new HBox(10, cancelBtn, applyBtn);
            btnHbox.setAlignment(Pos.CENTER_RIGHT);
            borderPane.setBottom(btnHbox);
        }

        outputPathTextField.setText(extraFileConfig.getOutputPath());
        packageNameTextField.setText(extraFileConfig.getPackageName());
        applyBtn.setDisable(isSystem);
        stage.show();
    }
}
