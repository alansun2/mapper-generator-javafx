package com.alan344.component;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.alan344.bean.config.MybatisPluginConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.factory.FileDirChooserFactory;
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
import org.kordamp.ikonli.javafx.FontIcon;

import java.io.File;
import java.util.List;
import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/8/21 15:01
 */
public class MybatisPluginItemHBox extends HBox {
    private final Button editButton;
    private final Button deleteButton;
    private final Button copyBtn;
    private final Label nameLabel;
    private final MybatisPluginConfig mybatisPluginConfig;
    private final JFXCheckBox jfxCheckBox;

    private final Stage ownerStage;

    private final Consumer<MybatisPluginConfig> acceptConsumer;

    public MybatisPluginItemHBox(MybatisPluginConfig mybatisPluginConfig, Stage ownerStage, Consumer<MybatisPluginConfig> acceptConsumer) {
        this.ownerStage = ownerStage;
        this.mybatisPluginConfig = mybatisPluginConfig;
        this.acceptConsumer = acceptConsumer;
        int jfxCheckBoxWidth = 30, nameWidth = 100, btnWidth = 64;

        jfxCheckBox = new JFXCheckBox();
        jfxCheckBox.selectedProperty().bindBidirectional(mybatisPluginConfig.enableProperty());
        jfxCheckBox.setPrefWidth(jfxCheckBoxWidth);

        nameLabel = new Label(mybatisPluginConfig.getName());
        nameLabel.setStyle("-fx-background-insets: 0");
        nameLabel.setPrefWidth(nameWidth);
        nameLabel.textProperty().bindBidirectional(mybatisPluginConfig.nameProperty());
        nameLabel.prefHeightProperty().bind(this.heightProperty());

        TextField fileNameTextField = new TextField(mybatisPluginConfig.getFileName());
        fileNameTextField.setEditable(false);
        fileNameTextField.setStyle("-fx-background-insets: 0");
        fileNameTextField.prefWidthProperty().bind(this.widthProperty().subtract(jfxCheckBoxWidth + nameWidth + (btnWidth * 3)));
        fileNameTextField.textProperty().bindBidirectional(mybatisPluginConfig.fileNameProperty());

        editButton = new Button("Edit");
        editButton.setOnAction(event -> this.openEdItStage());
        editButton.setStyle("-fx-background-insets: 0; -fx-background-color: #7070fd; -fx-text-fill: white");
        editButton.setPrefWidth(btnWidth);
        editButton.prefHeightProperty().bind(this.heightProperty());

        deleteButton = new Button("Del");
        deleteButton.setStyle("-fx-background-insets: 0; -fx-background-color: #E35252; -fx-text-fill: white");
        deleteButton.setPrefWidth(btnWidth);
        deleteButton.prefHeightProperty().bind(this.heightProperty());

        copyBtn = new Button("Copy");
        copyBtn.setStyle("-fx-background-insets: 0");
        copyBtn.setPrefWidth(btnWidth);
        copyBtn.prefHeightProperty().bind(this.heightProperty());

        this.getChildren().addAll(jfxCheckBox, nameLabel, fileNameTextField, editButton, deleteButton, copyBtn);
        this.setSpacing(10);
        this.setAlignment(Pos.CENTER);
        this.setStyle("-fx-background-insets: 0");
    }

    private void openEdItStage() {
        Stage stage = new Stage();
        stage.setTitle("编辑");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(ownerStage);
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/icon.png"));
        stage.setScene(new Scene(this.getEditParent(this.mybatisPluginConfig)));
        stage.show();
    }

    private Parent getEditParent(MybatisPluginConfig mybatisPluginConfig) {
        ValidationSupport validationSupport = new ValidationSupport();
        BorderPane borderPane = new BorderPane();
        borderPane.getStylesheets().add("css/common.css");
        borderPane.setPadding(new Insets(10));
        borderPane.setPrefWidth(400);
        VBox vBox = new VBox(10);
        vBox.setAlignment(Pos.TOP_CENTER);
        vBox.prefWidthProperty().bind(borderPane.widthProperty());
        borderPane.setCenter(vBox);

        Label label = new Label("名称: ");
        label.setPrefWidth(100);
        TextField textField = new TextField(mybatisPluginConfig.getName());
        textField.textProperty().bindBidirectional(mybatisPluginConfig.nameProperty());
        textField.prefWidthProperty().bind(vBox.widthProperty().subtract(110));
        HBox hBox = new HBox(10, label, textField);
        hBox.setAlignment(Pos.CENTER);
        validationSupport.registerValidator(textField, Validator.createEmptyValidator("名称不能为空"));

        Label label1 = new Label("文件: ");
        label1.setPrefWidth(100);
        TextField fileNameTextField = new TextField(FileUtil.getName(mybatisPluginConfig.getFilePath()));
        fileNameTextField.setEditable(false);
        fileNameTextField.setDisable(true);
        fileNameTextField.prefWidthProperty().bind(vBox.widthProperty().subtract(190));
        fileNameTextField.textProperty().bindBidirectional(mybatisPluginConfig.fileNameProperty());
        Button button = new Button("导入");
        button.setPrefWidth(70);
        button.setOnAction(event -> {
            // 选择文件
            final File fileScan = FileDirChooserFactory.createFileScan("选择 mybatis 插件", BaseConstants.baseFileDir, "plugin class", "*.java", "*.class", "*.jar");
            if (null != fileScan) {
                BaseConstants.baseFileDir = fileScan.getParent().replace("\\", "/");
                fileNameTextField.setText(fileScan.getName());
                mybatisPluginConfig.setFilePath(BaseConstants.PLUGIN_DIR + fileScan.getName());

                if (StrUtil.endWithAny(fileScan.getName(), ".java", ".class")) {
                    mybatisPluginConfig.setClassName(FileUtil.getName(fileScan).replace("." + FileUtil.extName(fileScan), ""));
                }

                // 如果时 java 文件获取包名
                if (fileScan.getName().endsWith(".java")) {
                    final List<String> strings = FileUtil.readUtf8Lines(fileScan);
                    if (strings.stream().anyMatch(s -> s.contains("package"))) {
                        strings.removeIf(s -> s.contains("package"));
                        FileUtil.writeUtf8Lines(strings, new File(BaseConstants.PLUGIN_DIR + fileScan.getName()));
                        return;
                    }
                }

                // 导入文件至用户目录
                FileUtil.copy(fileScan, new File(BaseConstants.PLUGIN_DIR + fileScan.getName()), true);
            }
        });
        validationSupport.registerValidator(fileNameTextField, Validator.createEmptyValidator("文件不能为空"));
        HBox hBox1 = new HBox(10, label1, fileNameTextField, button);
        hBox1.setAlignment(Pos.CENTER);

        Label classNameLabel = new Label("全限定类名: ");
        classNameLabel.setPrefWidth(100);
        CustomTextField classNameTextField = new CustomTextField();
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
        classNameTextField.setRight(fontIcon);
        classNameTextField.setText(mybatisPluginConfig.getClassName());
        classNameTextField.textProperty().bindBidirectional(mybatisPluginConfig.classNameProperty());
        classNameTextField.prefWidthProperty().bind(vBox.widthProperty().subtract(110));
        HBox classNameHbox = new HBox(10, classNameLabel, classNameTextField);
        classNameHbox.setAlignment(Pos.CENTER);
        validationSupport.registerValidator(classNameTextField, Validator.createEmptyValidator("全限定类名不能为空"));

        vBox.getChildren().addAll(hBox, hBox1, classNameHbox);

        // 按钮
        HBox btnHbox = new HBox(10);
        Button save = new Button("确定");
        save.setOnAction(event -> {
            if (validationSupport.isInvalid()) {
                return;
            }

            acceptConsumer.accept(mybatisPluginConfig);
            // 关闭窗口
            ((Stage) save.getScene().getWindow()).close();
        });

        btnHbox.setAlignment(Pos.BASELINE_RIGHT);
        btnHbox.getChildren().addAll(save);
        btnHbox.setPadding(new Insets(10, 0, 0, 0));
        borderPane.setBottom(btnHbox);
        return borderPane;
    }

    public void onDelAction(Consumer<ActionEvent> consumer) {
        this.deleteButton.setOnAction(consumer::accept);
    }

    public void onCopyAction(Consumer<ActionEvent> consumer) {
        this.copyBtn.setOnAction(consumer::accept);
    }

    public void setSelected(boolean selected) {
        this.jfxCheckBox.setSelected(selected);
    }

    public void setReverseSelected() {
        this.jfxCheckBox.setSelected(!jfxCheckBox.isSelected());
    }

    public MybatisPluginConfig getPluginConfig() {
        return this.mybatisPluginConfig;
    }

    public boolean isSelected() {
        return this.jfxCheckBox.isSelected();
    }
}
