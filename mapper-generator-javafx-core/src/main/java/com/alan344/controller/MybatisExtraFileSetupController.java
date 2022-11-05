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
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.image.Image;
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
    private ListView<HBox> listView;

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        final List<ExtraFileConfig> extraFileConfigs = currentConfig.getExtraFileConfigs();
        if (CollectionUtils.isNotEmpty(extraFileConfigs)) {
            extraFileConfigs.forEach(this::addExtraFileAfterSubmit);
        }
    }

    @FXML
    public void export() {
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        final List<ExtraFileConfig> extraFileConfigs = currentConfig.getExtraFileConfigs();
        for (ExtraFileConfig extraFileConfig : extraFileConfigs) {
            final ExtraFileTypeEnum templateType = extraFileConfig.getTemplateType();
            if (StringUtils.isEmpty(extraFileConfig.getOutputPath())) {
                Toast.makeText(NodeConstants.primaryStage, extraFileConfig.getName() + "配置中，文件地址必填", 3000, 500, 500, 15, 5);
                return;
            }
            if (StringUtils.isEmpty(extraFileConfig.getPackageName())) {
                Toast.makeText(NodeConstants.primaryStage, extraFileConfig.getName() + "配置中，包名必填", 3000, 500, 500, 15, 5);
                return;
            }
            if (templateType == ExtraFileTypeEnum.CUSTOM_TEMPLATE) {
                if (StringUtils.isEmpty(extraFileConfig.getCustomTemplateInputPath())) {
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
    public void addExtraFile() {
        ExtraFileConfig extraFileConfig = new ExtraFileConfig();
        this.openExtraFileSetup(extraFileConfig, o -> {
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
    private void openExtraFileSetup(ExtraFileConfig extraFileConfig, Consumer<Object> submitBtnAction, boolean isEdit) {
        final ExtraFileTypeEnum templateType = extraFileConfig.getTemplateType();
        Stage addTemplateStage = new Stage();
        VBox vBox = new VBox(5);
        vBox.setPrefWidth(400);
        vBox.getStylesheets().add("/css/common.css");

        int labelWidth = 110;
        // 属性名称
        final TextField nameTextField = new TextField(extraFileConfig.getName());
        nameTextField.setPromptText("配置名称");
        PropertyHBox nameHbox = new PropertyHBox("配置名称", labelWidth, nameTextField);
        vBox.getChildren().add(nameHbox);

        // 文件类型
        ChoiceBox<ExtraFileTypeEnum> fileTypeCb = new ChoiceBox<>(FXCollections.observableArrayList(ExtraFileTypeEnum.MODEL, ExtraFileTypeEnum.CUSTOM_TEMPLATE));
        fileTypeCb.getItems().addAll();
        fileTypeCb.setValue(ExtraFileTypeEnum.MODEL);
        if (isEdit) {
            fileTypeCb.setDisable(true);
        }
        PropertyHBox fileTypeHbox = new PropertyHBox("文件类型", labelWidth, fileTypeCb);
        vBox.getChildren().add(fileTypeHbox);

        // 文件地址
        FileSelectText outputPathTextField = new FileSelectText("浏览", extraFileConfig.getOutputPath());
        outputPathTextField.setPromptText("文件地址");
        PropertyHBox outputPathHbox = new PropertyHBox("文件地址", labelWidth, outputPathTextField);
        vBox.getChildren().add(outputPathHbox);

        // 包名
        TextField packageNameTextField = new TextField(extraFileConfig.getPackageName());
        packageNameTextField.setPromptText("包名");
        PropertyHBox packageNameHbox = new PropertyHBox("包名", labelWidth, packageNameTextField);
        vBox.getChildren().add(packageNameHbox);

        TextField modelSuffixTextField = null, ignoreColumnTextField = null;
        CheckBox checkBox = null;
        FileSelectText customTemplatePathTextField = null;
        if (templateType == ExtraFileTypeEnum.MODEL) {
            // model 后缀
            modelSuffixTextField = new TextField(extraFileConfig.getModelSuffix());
            modelSuffixTextField.setPromptText("model 后缀");
            PropertyHBox modelSuffixHbox = new PropertyHBox("model 后缀", labelWidth, modelSuffixTextField);
            vBox.getChildren().add(modelSuffixHbox);

            // 是否开启 validation 注解
            checkBox = new CheckBox();
            checkBox.setSelected(extraFileConfig.isGenerateValidAnnotation());
            PropertyHBox modelValidSuffixHbox = new PropertyHBox("是否开启 Validate 注解", labelWidth, modelSuffixTextField);
            vBox.getChildren().add(modelValidSuffixHbox);

            // 忽略字段
            ignoreColumnTextField = new TextField(extraFileConfig.getModelIgnoreColumns());
            ignoreColumnTextField.setPromptText("忽略字段，逗号分隔");
            PropertyHBox ignoreColumnHbox = new PropertyHBox("忽略字段", labelWidth, ignoreColumnTextField);
            vBox.getChildren().add(ignoreColumnHbox);
        } else {
            // 自定义模板地址
            customTemplatePathTextField = new FileSelectText("浏览", extraFileConfig.getCustomTemplateInputPath());
            customTemplatePathTextField.setPromptText("自定义模板地址");
            FileSelectText finalCustomTemplatePathTextField = customTemplatePathTextField;
            customTemplatePathTextField.onAction(actionEvent -> {
                // 文件导出地址
                File directory = FileDirChooserFactory.createDirectoryScan(null, !StringUtils.isNotEmpty(this.baseDir) ? null : this.baseDir);
                if (directory != null) {
                    finalCustomTemplatePathTextField.setText(directory.getPath());
                    this.baseDir = directory.getPath();
                }
            });
            PropertyHBox customTemplatePathHbox = new PropertyHBox("自定义模板地址", labelWidth, customTemplatePathTextField);
            vBox.getChildren().add(customTemplatePathHbox);
        }

        // 按钮
        Button cancelButton = new Button("取消");
        cancelButton.setOnAction(actionEvent -> addTemplateStage.close());
        Button submitButton = new Button("确定");
        TextField finalModelSuffixTextField = modelSuffixTextField;
        CheckBox finalCheckBox = checkBox;
        TextField finalIgnoreColumnTextField = ignoreColumnTextField;
        FileSelectText finalCustomTemplatePathTextField1 = customTemplatePathTextField;
        submitButton.setOnAction(actionEvent -> {
            extraFileConfig.setName(nameTextField.getText());
            extraFileConfig.setTemplateType(fileTypeCb.getSelectionModel().getSelectedItem());
            extraFileConfig.setOutputPath(outputPathTextField.getText());
            extraFileConfig.setPackageName(packageNameTextField.getText());
            if (templateType == ExtraFileTypeEnum.MODEL) {
                extraFileConfig.setModelSuffix(finalModelSuffixTextField.getText());
                extraFileConfig.setGenerateValidAnnotation(finalCheckBox.isSelected());
                extraFileConfig.setModelIgnoreColumns(finalIgnoreColumnTextField.getText());
            } else {
                extraFileConfig.setCustomTemplateInputPath(finalCustomTemplatePathTextField1.getText());
            }

            submitBtnAction.accept(extraFileConfig);
            addTemplateStage.close();
        });
        HBox btnHbox = new HBox(20, cancelButton, submitButton);
        btnHbox.setStyle("-fx-padding: 10 10");
        btnHbox.setAlignment(Pos.CENTER_RIGHT);
        vBox.getChildren().add(btnHbox);

        addTemplateStage.setScene(new Scene(vBox));
        addTemplateStage.setResizable(false);
        addTemplateStage.getIcons().add(new Image("/image/database@32.png"));
        addTemplateStage.setTitle("额外文件" + (isEdit ? "编辑" : "新增"));
        addTemplateStage.initModality(Modality.WINDOW_MODAL);
        addTemplateStage.initOwner(NodeConstants.primaryStage);
        addTemplateStage.show();
    }

    private void addExtraFileAfterSubmit(ExtraFileConfig extraFileConfig) {
        ExtraFileLabel extraFileLabel = new ExtraFileLabel(extraFileConfig.getName(), extraFileConfig.getTemplateType(),
                extraFileConfig.isEnable(), extraFileConfig::setEnable);
        extraFileLabel.prefWidthProperty().bind(listView.widthProperty().subtract(220));
        extraFileLabel.onAction(actionEvent -> this.openExtraFileSetup(extraFileConfig, o -> {
        }, true));
        extraFileLabel.setPrefHeight(23);
        extraFileLabel.setAlignment(Pos.CENTER);
        listView.getItems().add(extraFileLabel);
    }
}
