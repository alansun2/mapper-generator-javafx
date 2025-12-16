package com.alan344.component;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.alan344.bean.config.MybatisPluginConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.utils.Toast;
import com.jfoenix.controls.JFXCheckBox;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;
import org.kordamp.ikonli.javafx.FontIcon;

import java.io.File;
import java.util.List;
import java.util.function.Consumer;

/**
 * @author AlanSun
 * @since 2022/8/21 15:01
 */
public class MybatisPluginItemHBox extends HBox {
    private final Button deleteButton;
    private final MybatisPluginConfig mybatisPluginConfig;
    private final JFXCheckBox jfxCheckBox;

    public MybatisPluginItemHBox(MybatisPluginConfig mybatisPluginConfig, Stage ownerStage, Consumer<MybatisPluginConfig> acceptConsumer) {
        this.mybatisPluginConfig = mybatisPluginConfig;
        int jfxCheckBoxWidth = 30, nameWidth = 200, btnWidth = 64;

        jfxCheckBox = new JFXCheckBox();
        jfxCheckBox.selectedProperty().bindBidirectional(mybatisPluginConfig.enableProperty());
        jfxCheckBox.setPrefWidth(jfxCheckBoxWidth);

        Label nameLabel = new Label(mybatisPluginConfig.getName());
        nameLabel.setStyle("-fx-background-insets: 0");
        nameLabel.prefWidthProperty().bind(this.widthProperty().subtract(jfxCheckBoxWidth + nameWidth + (btnWidth * 3)));
        nameLabel.textProperty().bindBidirectional(mybatisPluginConfig.nameProperty());
        nameLabel.prefHeightProperty().bind(this.heightProperty());

        Label fileNameLabel = new Label(mybatisPluginConfig.getFileName());
        fileNameLabel.setStyle("-fx-background-insets: 0");
        fileNameLabel.setPrefWidth(nameWidth);
        fileNameLabel.textProperty().bindBidirectional(mybatisPluginConfig.fileNameProperty());

        Button editButton = new Button("Edit");
        editButton.setOnAction(event -> openEdItStage(false, ownerStage, mybatisPluginConfig, acceptConsumer));
        editButton.setStyle("-fx-background-insets: 0; -fx-background-color: #7070fd; -fx-text-fill: white");
        editButton.setPrefWidth(btnWidth);
        editButton.prefHeightProperty().bind(this.heightProperty());

        deleteButton = new Button("Del");
        deleteButton.setStyle("-fx-background-insets: 0; -fx-background-color: #E35252; -fx-text-fill: white");
        deleteButton.setPrefWidth(btnWidth);
        deleteButton.prefHeightProperty().bind(this.heightProperty());

        this.getChildren().addAll(jfxCheckBox, nameLabel, fileNameLabel, editButton, deleteButton);
        this.setSpacing(10);
        this.setAlignment(Pos.CENTER);
        this.setStyle("-fx-background-insets: 0");
    }

    private static Stage stage;

    public static void openEdItStage(boolean isAdd, Stage ownerStage, MybatisPluginConfig mybatisPluginConfig, Consumer<MybatisPluginConfig> acceptConsumer) {
        if (stage == null) {
            stage = new Stage();
            stage.initModality(Modality.WINDOW_MODAL);
            stage.initOwner(ownerStage);
            stage.setResizable(false);
            stage.getIcons().add(new Image("/image/icon.png"));
        }
        stage.setTitle(isAdd ? "新增" : "编辑");
        stage.setScene(new Scene(getEditParent(isAdd, mybatisPluginConfig, stage, acceptConsumer)));
        stage.show();
    }

    private static File fileScanGlobal;

    private static Parent getEditParent(boolean isAdd, MybatisPluginConfig mybatisPluginConfig, Stage stage, Consumer<MybatisPluginConfig> acceptConsumer) {
        fileScanGlobal = null;
        int labelWidth = 80;
        ValidationSupport validationSupport = new ValidationSupport();
        validationSupport.setValidationDecorator(new StyleClassValidationDecoration());
        BorderPane borderPane = new BorderPane();
        borderPane.getStylesheets().add("css/common.css");
        borderPane.setPadding(new Insets(10));
        borderPane.setPrefWidth(400);
        VBox vBox = new VBox(10);
        vBox.setAlignment(Pos.TOP_CENTER);
        vBox.prefWidthProperty().bind(borderPane.widthProperty());
        borderPane.setCenter(vBox);

        Label label = new Label("名称: ");
        label.setPrefWidth(labelWidth);
        TextField textField = new TextField(mybatisPluginConfig.getName());
        textField.prefWidthProperty().bind(vBox.widthProperty().subtract(labelWidth + 10));
        HBox hBox = new HBox(10, label, textField);
        hBox.setAlignment(Pos.CENTER);
        validationSupport.registerValidator(textField, Validator.createEmptyValidator("名称不能为空"));
        Label classNameLabel = new Label("全限定类名: ");
        classNameLabel.setPrefWidth(labelWidth);
        final FontIcon fontIcon = new FontIcon("unil-question-circle:16:#f7df05");
        Tooltip tooltip = new Tooltip("全限定类名，如：com.github.mybatis.plugin.MybatisPlugin");
        tooltip.setStyle("-fx-background-color: #f7df05; -fx-text-fill: black");
        fontIcon.addEventHandler(MouseEvent.MOUSE_ENTERED, event -> {
            if (tooltip.isShowing()) {
                tooltip.hide();
            } else {
                tooltip.show(fontIcon, event.getScreenX(), event.getScreenY());
            }
        });
        CustomTextField classNameTextField = new CustomTextField();
        classNameTextField.setText(mybatisPluginConfig.getClassName());
        classNameTextField.setRight(fontIcon);
        classNameTextField.prefWidthProperty().bind(vBox.widthProperty().subtract(labelWidth + 10));
        HBox classNameHbox = new HBox(10, classNameLabel, classNameTextField);
        classNameHbox.setAlignment(Pos.CENTER);
        validationSupport.registerValidator(classNameTextField, Validator.createEmptyValidator("全限定类名不能为空"));

        Label label1 = new Label("文件: ");
        label1.setPrefWidth(labelWidth);
        TextField fileNameTextField = new TextField();
        fileNameTextField.setText(mybatisPluginConfig.getFileName());
        fileNameTextField.setEditable(false);
        fileNameTextField.setDisable(true);
        fileNameTextField.prefWidthProperty().bind(vBox.widthProperty().subtract(labelWidth + 90));
        fileNameTextField.textProperty().bindBidirectional(mybatisPluginConfig.fileNameProperty());
        Button button = new Button("导入");
        button.setPrefWidth(70);
        button.setOnAction(event -> {
            // 选择文件
            final File fileScan = FileDirChooserFactory.createFileScan("选择 mybatis 插件", BaseConstants.baseFileDir, "plugin class", "*.java", "*.class", "*.jar");
            if (null != fileScan) {
                fileScanGlobal = fileScan;
                BaseConstants.baseFileDir = fileScan.getParent().replace(StrUtil.BACKSLASH, StrUtil.SLASH);
                fileNameTextField.setText(fileScan.getName());
                if (StrUtil.endWithAny(fileScan.getName(), ".java", ".class")) {
                    classNameTextField.setText(fileScan.getName().replace("." + FileUtil.extName(fileScan.getName()), ""));
                }
            } else {
                fileScanGlobal = null;
            }
        });
        validationSupport.registerValidator(fileNameTextField, Validator.createEmptyValidator("文件不能为空"));
        HBox hBox1 = new HBox(10, label1, fileNameTextField, button);
        hBox1.setAlignment(Pos.CENTER);

        vBox.getChildren().addAll(hBox, hBox1, classNameHbox);

        // 点击应用按钮,新增插件
        Button cancel = new Button("取消");
        cancel.setOnAction(event -> ((Stage) cancel.getScene().getWindow()).close());
        Button apply = new Button("应用");
        apply.getStyleClass().add("apply-btn");
        apply.setOnAction(event -> {
            if (validationSupport.isInvalid()) {
                Toast.makeTextDefault(stage, "有必填项未填");
                return;
            }

            mybatisPluginConfig.setName(textField.getText());
            mybatisPluginConfig.setFilePath(BaseConstants.PLUGIN_DIR + fileNameTextField.getText());
            mybatisPluginConfig.setFileName(fileNameTextField.getText());
            mybatisPluginConfig.setClassName(classNameTextField.getText());

            if (fileScanGlobal != null) {
                // 如果是 java 文件则去除包那一行
                if (fileNameTextField.getText().endsWith(".java")) {
                    final List<String> strings = FileUtil.readUtf8Lines(fileScanGlobal);
                    if (strings.stream().anyMatch(s -> s.contains("package"))) {
                        strings.removeIf(s -> s.contains("package"));
                        FileUtil.writeUtf8Lines(strings, new File(BaseConstants.PLUGIN_DIR + fileNameTextField.getText()));
                        return;
                    }
                }
                // 导入文件至用户目录
                FileUtil.copy(fileScanGlobal, new File(BaseConstants.PLUGIN_DIR + fileNameTextField.getText()), true);
            }

            if (isAdd || fileScanGlobal != null) {
                acceptConsumer.accept(mybatisPluginConfig);
            }
            // 关闭窗口
            ((Stage) apply.getScene().getWindow()).close();
        });

        HBox btnHbox = new HBox(10, cancel, apply);
        btnHbox.setAlignment(Pos.BASELINE_RIGHT);
        btnHbox.setPadding(new Insets(10, 0, 0, 0));
        borderPane.setBottom(btnHbox);
        return borderPane;
    }

    public void onDelAction(Consumer<ActionEvent> consumer) {
        this.deleteButton.setOnAction(consumer::accept);
    }

    public void setSelected(boolean selected) {
        this.jfxCheckBox.setSelected(selected);
    }

    public MybatisPluginConfig getPluginConfig() {
        return this.mybatisPluginConfig;
    }

    public boolean isSelected() {
        return this.jfxCheckBox.isSelected();
    }
}
