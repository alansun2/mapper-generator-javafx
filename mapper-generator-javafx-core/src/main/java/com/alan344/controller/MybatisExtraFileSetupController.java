package com.alan344.controller;

import com.alan344.bean.config.ExtraFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.componet.ExtraFileLabel;
import com.alan344.componet.FileSelectText;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ExtraFileTypeEnum;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.ExportService;
import com.alan344.service.node.NodeHandler;
import com.alan344.utils.*;
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
            extraFileConfigs.forEach(this::addTemplateAfterSubmit);
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
        Stage addExtraFileStage = new Stage();
        VBox vBox = new VBox();
        vBox.setSpacing(10);
        vBox.setPrefWidth(400);
        vBox.getStylesheets().add("/css/common.css");

        // 属性名称
        Label firstLabel = new Label("配置名称");
        firstLabel.setPrefWidth(90);
        TextField firstTextField = new TextField();
        firstTextField.setPromptText("配置名称");
        firstTextField.prefWidthProperty().bind(vBox.widthProperty().subtract(90));
        HBox firstHbox = new HBox(10, firstLabel, firstTextField);
        firstHbox.setAlignment(Pos.CENTER);
        firstHbox.setStyle("-fx-padding: 0 10");

        // 文件类型
        Label secondLabel = new Label("文件类型");
        secondLabel.setPrefWidth(90);
        ChoiceBox<ExtraFileTypeEnum> cb = new ChoiceBox<>();
        cb.getItems().addAll(ExtraFileTypeEnum.MODEL, ExtraFileTypeEnum.CUSTOM_TEMPLATE);
        cb.setValue(ExtraFileTypeEnum.MODEL);
        cb.prefWidthProperty().bind(vBox.widthProperty().subtract(90));
        HBox secondHbox = new HBox(10, secondLabel, cb);
        secondHbox.setAlignment(Pos.CENTER);
        secondHbox.setStyle("-fx-padding: 0 10");

        // 按钮
        Button cancelButton = new Button("取消");
        cancelButton.setOnAction(actionEvent -> {
            addExtraFileStage.close();
        });
        Button submitButton = new Button("确定");
        submitButton.setOnAction(actionEvent -> {
            TextUtils.checkTextsHasEmpty(NodeConstants.primaryStage, firstTextField);
            ExtraFileConfig extraFileConfig = new ExtraFileConfig();
            extraFileConfig.setName(firstTextField.getText());
            extraFileConfig.setTemplateType(cb.getSelectionModel().getSelectedItem());
            BaseConstants.currentConfig.getExtraFileConfigs().add(extraFileConfig);
            this.addTemplateAfterSubmit(extraFileConfig);
            addExtraFileStage.close();
        });
        HBox hBox1 = new HBox(20, cancelButton, submitButton);
        hBox1.setStyle("-fx-padding: 10 10");
        hBox1.setAlignment(Pos.CENTER_RIGHT);

        vBox.getChildren().addAll(firstHbox, secondHbox, hBox1);

        addExtraFileStage.setScene(new Scene(vBox));
        addExtraFileStage.setResizable(false);
        addExtraFileStage.getIcons().add(new Image("/image/database@32.png"));
        addExtraFileStage.setTitle("新增模板");
        addExtraFileStage.initModality(Modality.WINDOW_MODAL);
        addExtraFileStage.initOwner(NodeConstants.primaryStage);
        addExtraFileStage.show();
    }

    private void addTemplateAfterSubmit(ExtraFileConfig extraFileConfig) {
        Label label = new Label(extraFileConfig.getName());
        label.setPrefHeight(23);
        label.setPrefWidth(130);
        ExtraFileLabel templateText = new ExtraFileLabel(extraFileConfig.getTemplateType(), extraFileConfig.isEnable(), extraFileConfig::setEnable);
        templateText.prefWidthProperty().bind(listView.widthProperty().subtract(220));
        templateText.onAction(actionEvent -> this.openTemplateSetup(extraFileConfig));
        HBox hBox = new HBox(10, label, templateText);
        hBox.setPrefHeight(23);
        hBox.setAlignment(Pos.CENTER);
        listView.getItems().add(hBox);
    }

    private String baseDir;

    private void openTemplateSetup(ExtraFileConfig extraFileConfig) {
        final ExtraFileTypeEnum templateType = extraFileConfig.getTemplateType();
        Stage addTemplateStage = new Stage();
        VBox vBox = new VBox();
        vBox.setSpacing(10);
        vBox.setPrefWidth(400);
        vBox.getStylesheets().add("/css/common.css");

        // 文件地址
        Label outputPathLabel = new Label("文件地址");
        outputPathLabel.setPrefWidth(110);
        TextField outputPathTextField = new TextField();
        outputPathTextField.setText(extraFileConfig.getOutputPath());
        outputPathTextField.setPromptText("文件地址");
        outputPathTextField.prefWidthProperty().bind(vBox.widthProperty().subtract(110));
        HBox outputPathHbox = new HBox(10, outputPathLabel, outputPathTextField);
        outputPathHbox.setAlignment(Pos.CENTER);
        outputPathHbox.setStyle("-fx-padding: 0 10");
        vBox.getChildren().add(outputPathHbox);

        // 包名
        Label packageNameLabel = new Label("包名");
        packageNameLabel.setPrefWidth(110);
        TextField packageNameTextField = new TextField();
        packageNameTextField.setText(extraFileConfig.getPackageName());
        packageNameTextField.setPromptText("包名");
        packageNameTextField.prefWidthProperty().bind(vBox.widthProperty().subtract(110));
        HBox packageNameHbox = new HBox(10, packageNameLabel, packageNameTextField);
        packageNameHbox.setAlignment(Pos.CENTER);
        packageNameHbox.setStyle("-fx-padding: 0 10");
        vBox.getChildren().add(packageNameHbox);

        TextField modelSuffixTextField = null, ignoreColumnTextField = null;
        CheckBox checkBox = null;
        FileSelectText customTemplatePathTextField = null;
        if (templateType == ExtraFileTypeEnum.MODEL) {
            // model 后缀
            Label modelSuffixLabel = new Label("model 后缀");
            modelSuffixLabel.setPrefWidth(110);
            modelSuffixTextField = new TextField(extraFileConfig.getModelSuffix());
            modelSuffixTextField.setPromptText("model 后缀");
            modelSuffixTextField.prefWidthProperty().bind(vBox.widthProperty().subtract(110));
            HBox modelSuffixHbox = new HBox(10, modelSuffixLabel, modelSuffixTextField);
            modelSuffixHbox.setAlignment(Pos.CENTER);
            modelSuffixHbox.setStyle("-fx-padding: 0 10");
            vBox.getChildren().add(modelSuffixHbox);

            // 是否开启 validation 注解
            Label modelValidAnnoSuffixLabel = TooltipWrapper.wrap(new Label("是否开启 Validate 注解"), "是否开启 Validate 注解");
            modelValidAnnoSuffixLabel.setPrefWidth(110);
            checkBox = new CheckBox();
            checkBox.setSelected(extraFileConfig.isGenerateValidAnnotation());
            HBox checkBoxContainer = new HBox(checkBox);
            checkBoxContainer.prefWidthProperty().bind(vBox.widthProperty().subtract(110));
            HBox modelValidSuffixHbox = new HBox(10, modelValidAnnoSuffixLabel, checkBoxContainer);
            modelValidSuffixHbox.setAlignment(Pos.CENTER);
            modelValidSuffixHbox.setStyle("-fx-padding: 0 10");
            vBox.getChildren().add(modelValidSuffixHbox);

            // 忽略字段
            Label ignoreColumnLabel = new Label("忽略字段");
            ignoreColumnLabel.setPrefWidth(110);
            ignoreColumnTextField = new TextField();
            ignoreColumnTextField.setText(extraFileConfig.getModelIgnoreColumns());
            ignoreColumnTextField.setPromptText("忽略字段，逗号分隔");
            ignoreColumnTextField.prefWidthProperty().bind(vBox.widthProperty().subtract(110));
            HBox ignoreColumnHbox = new HBox(10, ignoreColumnLabel, ignoreColumnTextField);
            ignoreColumnHbox.setAlignment(Pos.CENTER);
            ignoreColumnHbox.setStyle("-fx-padding: 0 10");
            vBox.getChildren().add(ignoreColumnHbox);
        } else {
            // 自定义模板地址
            Label customTemplatePathLabel = new Label("自定义模板地址");
            customTemplatePathLabel.setPrefWidth(110);
            customTemplatePathTextField = new FileSelectText();
            customTemplatePathTextField.setText(extraFileConfig.getCustomTemplateInputPath());
            customTemplatePathTextField.setPromptText("自定义模板地址");
            customTemplatePathTextField.prefWidthProperty().bind(vBox.widthProperty().subtract(110));
            FileSelectText finalCustomTemplatePathTextField = customTemplatePathTextField;
            customTemplatePathTextField.onAction(actionEvent -> {
                // 文件导出地址
                File directory = FileDirChooserFactory.createDirectoryScan(null, !StringUtils.isNotEmpty(this.baseDir) ? null : this.baseDir);
                if (directory != null) {
                    finalCustomTemplatePathTextField.setText(directory.getPath());
                    this.baseDir = directory.getPath();
                }
            });
            HBox customTemplatePathHbox = new HBox(10, customTemplatePathLabel, customTemplatePathTextField);
            customTemplatePathHbox.setAlignment(Pos.CENTER);
            customTemplatePathHbox.setStyle("-fx-padding: 0 10");
            vBox.getChildren().add(customTemplatePathHbox);
        }

        // 按钮
        Button cancelButton = new Button("取消");
        cancelButton.setOnAction(actionEvent -> {
            addTemplateStage.close();
        });
        Button submitButton = new Button("确定");
        TextField finalModelSuffixTextField = modelSuffixTextField;
        CheckBox finalCheckBox = checkBox;
        TextField finalIgnoreColumnTextField = ignoreColumnTextField;
        FileSelectText finalCustomTemplatePathTextField1 = customTemplatePathTextField;
        submitButton.setOnAction(actionEvent -> {
            extraFileConfig.setOutputPath(outputPathTextField.getText());
            extraFileConfig.setPackageName(packageNameTextField.getText());
            if (templateType == ExtraFileTypeEnum.MODEL) {
                extraFileConfig.setModelSuffix(finalModelSuffixTextField.getText());
                extraFileConfig.setGenerateValidAnnotation(finalCheckBox.isSelected());
                extraFileConfig.setModelIgnoreColumns(finalIgnoreColumnTextField.getText());
            } else {
                extraFileConfig.setCustomTemplateInputPath(finalCustomTemplatePathTextField1.getText());
            }

            addTemplateStage.close();
        });
        HBox btnHbox = new HBox(20, cancelButton, submitButton);
        btnHbox.setStyle("-fx-padding: 10 10");
        btnHbox.setAlignment(Pos.CENTER_RIGHT);
        vBox.getChildren().add(btnHbox);

        addTemplateStage.setScene(new Scene(vBox));
        addTemplateStage.setResizable(false);
        addTemplateStage.getIcons().add(new Image("/image/database@32.png"));
        addTemplateStage.setTitle("额外文件设置");
        addTemplateStage.initModality(Modality.WINDOW_MODAL);
        addTemplateStage.initOwner(NodeConstants.primaryStage);
        addTemplateStage.show();
    }
}
